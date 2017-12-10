library(ggplot2)

bat.data <- read.csv("G:/Thesis/Data/BatMasterDataFINALworking2.csv")
boxplot(bat.data$TotalID ~ bat.data$Cluster, ylim = c(0, 1000))


ggplot(bat.data, aes(x = Cluster, y = TotalID)) +
  geom_boxplot(aes(group = Cluster)) +
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black")) + 
  ylim(0,1000) +
  xlab("") +
  ylab("Bat Activity")


ggplot(bat.data, aes(x = Cluster, y = ShanBatDiv)) +
  geom_boxplot(aes(group = Cluster)) +
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black")) +
  xlab("Cluster") +
  ylab("Diversity")

