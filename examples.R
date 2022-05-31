library(stringr)
aa <- names(df)

aa <- str_replace(string = aa, pattern = c("\\["), replacement = "")
aa <- str_replace(string = aa, pattern = "\\]", replacement = "")
aa <- str_replace(string = aa, pattern = "\\.", replacement = "")
names(df) <- aa


library(ggplot2)

gg <- ggplot() # create the graphic object
gg <- gg + ggplot2::geom_line(data = df, 
                              mapping = aes(x = Time, y = G1X)) # adding layers to the graphic object
print(gg) # display the graphic on the screen

gg <- gg + theme_minimal() #changing the appearance of the theme 
gg # display the graphic on the screen

gg <- gg + ggplot2::ggtitle(label = "gyroscope data - sensor 1")

gg

#################
library(tidyr)
df.long <- pivot_longer(data = df, cols = (2:length(df)), 
                        names_to = "variable",
                        values_to = "value")
head(df.long, n = 10L)


g <- ggplot(data = df.long,
            mapping = aes(x = Time, y = value)) +
  geom_line() + 
  facet_wrap( ~ variable, ncol = 3, scales = "free_y") + 
  theme_light()

g

df.long$variable <- factor(df.long$variable, 
                           levels = df.long$variable %>% unique()) 
g <- ggplot(data = df.long,
            mapping = aes(x = Time, y = value)) +
  geom_line() + 
  facet_wrap( ~ variable, ncol = 3, scales = "free_y") + 
  theme_light()

g


library(dplyr)
df.filtered <- df.long %>% filter(variable == "G1X" |
                                    variable == "G1Y" |
                                    variable == "G1Z")

library(ggplot2)
g <- ggplot(data = df.filtered,
            mapping = aes(x = Time,
                          y = value,
                          color = interaction(variable))) +
    geom_line() + theme_minimal() + labs(color = "signal",
                                         x = "time (s)",
                                         y = "angular velocity (º/s)") 
g


df.filtered <- df.long %>% filter(variable == "G1X" |
                                    variable == "G1Y" |
                                    variable == "G1Z" |
                                    variable == "A1X" |
                                    variable == "A1Y" |
                                    variable == "A1Z" |
                                    variable == "M1X" |
                                    variable == "M1Y" |
                                    variable == "M1Z")

df.filtered$sensor <- str_sub(df.filtered$variable, 1, 1)

g <- ggplot(data = df.filtered,
            mapping = aes(x = Time,
                          y = value,
                          color = interaction(variable))) +
  geom_line() +
  facet_wrap( ~ sensor, ncol = 3, scales = "free_y",
              strip.position = "left", 
              labeller = as_labeller(c(A = "linear acceleration (g)", 
                                       G = "angular velocity (º/s)",
                                       M = "magnetic field (Gauss)"))) +
  
    theme_minimal() + labs(color = "signal",
                                       x = "time (s)",
                                       y = "") +
  theme(strip.background = element_blank(),
        strip.placement = "outside")
g
