##Load relevant libraries
library(ggplot2)

##Read in data
bat_data <- read.csv("G:/Thesis/Data/LoadingAspectRatio.csv")

##Create aspect ratio vs. loading plot
ggplot(bat_data, aes(x = AspectRatio, y = Wingloading, label = Spp)) +
  geom_point() +
  labs(x = "Aspect Ratio", y = "Wing Loading") +
  geom_text(hjust = 0.01, nudge_x = 0.005) +
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black", size = 2)) 

  
  