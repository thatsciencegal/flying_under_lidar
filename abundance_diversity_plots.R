library(ggplot2)

bat.data <- read.csv("./Data/BatDataNewCallID.csv",header=T)
boxplot(bat.data$TotalID ~ bat.data$ï..Cluster, ylim = c(0, 125))


ggplot(bat.data, aes(x = ï..Cluster, y = TotalID)) +
  geom_boxplot(aes(group = ï..Cluster)) +
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black")) + 
  ylim(0,125) +
  xlab("") +
  ylab("Bat Activity")


ggplot(bat.data, aes(x = ï..Cluster, y = SpeciesNumber)) +
  geom_boxplot(aes(group = ï..Cluster)) +
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black")) +
  xlab("Cluster") +
  ylab("Species Richness")

