##Load relevant libraries
library(ggplot2)

##Read in data
bat_data <- read.csv("./Data/2020-01_Swanson-et-al_Bats-Forest-Str_Wing-Loading-Aspect-Ratio.csv")

##Create aspect ratio vs. loading plot
ggplot(bat_data, aes(x = AspectRatio, y = Wingloading, label = Species)) +
  geom_point() +
  labs(x = "Aspect Ratio", y = "Wing Loading") +
  geom_text(hjust = 0.01, nudge_x = 0.005) +
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black", size = 2)) 

  
  