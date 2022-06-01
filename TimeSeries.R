# install.packages("dplyr")
# install.packages("signal")
# install.packages("dygraphs") 
# install.packages("Rssa")
# install.packages("htmltools")
# install.packages("psd")
library(dplyr)
library(signal)
library(dygraphs)
library(Rssa)
library(htmltools)
library(psd)


# Set the directory
#setwd("C:/Users/Usuario/OneDrive/UFU/Doutorado/PROJETO JOGO PARKINSON/Sinais/Parkinson/Alaor/COM_F1")

# Reading the file
df <- read.table("53_3_1.txt", header = T, sep = "\t", dec = ",") 

# Change the name of the column PITCH with ROLL
df <- rename(df, pitch = roll, roll = pitch)

# Subtracts each element by the first element to start at zero
df$iTimeStampHand <- df$iTimeStampHand-df$iTimeStampHand[1] 

# Scale from milliseconds to seconds
df$iTimeStampHand <- df$iTimeStampHand*0.001


# RESAMPLING ------------------------------------------------------------

fs1 <- 250 # Hz
dt1 <- 1/fs1 # New time resolution
t1 <- seq(from=df$iTimeStampHand[1], 
          to=df$iTimeStampHand[length(df$iTimeStampHand)], 
          by=dt1)

time_original_signal <-  df$iTimeStampHand
amplitude_original_signal <- df$roll
desired_discret_time <- t1

# Vector with the result of the interpolation
y1 <- pchip(x = time_original_signal, 
            y = amplitude_original_signal, 
            xi = desired_discret_time)

# Final signal with the interpolation
df1 <- data.frame(time = desired_discret_time, y = y1) # Variables: time and y

dygraph(df1, main = "Original Signal") %>% dyRangeSelector() 


# SINGULAR SPECTRUM ANALYSIS ------------------------------------------------------

x <- ts(df1$y, start = 0, frequency = 250, deltat = 0.004)

s <- ssa(x, L=250) # Decomposition
# (s$sigma)^2 
# plot(s) 
# plot(wcor(s)) 

r <- reconstruct(s, groups = list(VoluntaryMovem = c(1:3, 50), 
                                  Tremor = c(6, 7, 10, 11, 26, 27, 48:50))) # Reconstruction (2 components)

# OBS: As componentes do sinal 25_4_1 sao: VoluntaryMovem = c(1:3, 50), 
#                                          Tremor = c(6, 7, 9:12, 44:49)

df2 <- data.frame(Time=df1$time, Original = df1$y, Voluntary_Movement=r$VoluntaryMovem, Tremor=r$Tremor)

plotMultiPanelDataout <- function(df2)
{
  browsable(
    tagList(list(
      tags$div(
        dygraph(data.frame(Time=df2$Time, Original=df2$Original), group = "ensync", height = 200, width = "100%") %>%
          dyLegend(show="always")%>%dyOptions(colors = c("rgb(0,0,0)"))%>%dyRangeSelector(),
        dygraph(data.frame(Time=df2$Time, VoluntaryMovem=df2$Voluntary_Movement), group = "ensync", height = 200, width = "100%") %>%
          dyLegend(show="always"),
        dygraph(data.frame(Time=df2$Time, Tremor=df2$Tremor), group = "ensync", height = 200, width = "100%") %>%
          dyLegend(show="always")%>%dyOptions(colors = c("rgb(100,0,100)")),
        
      )
    )
    ))
}

#Plot multi data 
plotMultiPanelDataout(df2)


# POWER SPECTRUM OF THE TREMOR ------------------------------------------------

# psdTremsenData 
#             Estimate the power spectrum of the input data
# Input: 
#       df: dataframe resulting from LoadTREMSENFile
# Output:
#       power spectrum based on the 'Adaptive sine multitaper power spectral density estimation'. See the function
#       pspectrum in the package psd
#
# Example of use:
#
#       pp <- psdTremsenData(df.nonlineardetrended) 


psf <- function(vec, fs) {
  
  # library(psd)
  sss <- pspectrum(vec, verbose = FALSE, niter = 10, 
                   AR = FALSE, x.frqsamp = fs, plot = FALSE) 
  
  return (data.frame("freq" = sss$freq, "spec" = sss$spec))
}

psdTremsenData <- function(df, startColRange=2, endColRange=39) {
  
  psf <- function(vec, fs) {
    
    sss <- pspectrum(vec, verbose = FALSE, niter = 10, 
                     AR = FALSE, x.frqsamp = fs, plot = FALSE) 
    
    return (data.frame("freq" = sss$freq, "spec" = sss$spec))
  }
  
  
  X <- data.matrix(df)
  
  fs <- 1 / (df$X.Time.[2] - df$X.Time.[1])
  
  Nwindows <- nlevels(df$X.PULSE.LABEL)-1
  
  dx <- list()
  
  for (i in 1:Nwindows){
    
    indx <- which(df$X.PULSE.LABEL==i)
    
    dx[[i]] <- apply(X[indx, c(startColRange:endColRange)], 2, psf, fs=fs)
  }
  
  return(dx)
}

v <- vector()
v <- df2$Tremor

dfPSD <- psf(v,50)

dygraph(dfPSD,main = "Espectro de Potencia", group = "G1") %>% dyRangeSelector() %>%
  dyAxis("x", label = "frequecia(Hz)")%>%
  dyAxis("y", label = "densidade")

