library(vegan)
library(ggplot2)
library(viridis)

##Read in bad data
bat.data <- read.csv("./Data/2020-01_Swanson-et-al_Bats-Forest-Str_Master-Datasheet.csv")

##Select bats and cluster information
bat.data.subset <- subset(bat.data, select = c(as.numeric(EPFU), as.numeric(LANO), LABO, LACI, LAIN, MYAU, NYHU, PESU, TABR, ï..Cluster))

##Change to numeric data
bat.data.subset$EPFU <- as.numeric(bat.data.subset$EPFU)
bat.data.subset$LANO <- as.numeric(bat.data.subset$LANO)

##Change NA values to 0
bat.data.subset[is.na(bat.data.subset)]<-0

##Subset data by clusters
bat.data.subset1 <- subset(bat.data.subset, ï..Cluster == 1)
bat.data.subset2 <- subset(bat.data.subset, ï..Cluster == 2)
bat.data.subset3 <- subset(bat.data.subset, ï..Cluster == 3)
bat.data.subset4 <- subset(bat.data.subset, ï..Cluster == 4)
bat.data.subset5 <- subset(bat.data.subset, ï..Cluster == 5)
bat.data.subset6 <- subset(bat.data.subset, ï..Cluster == 6)

##Species accumulation curves for each cluster
bat.specaccum1 <- specaccum(bat.data.subset1, na.rm = TRUE, method = "rarefaction")
bat.specaccum2 <- specaccum(bat.data.subset2, method = "rarefaction")
bat.specaccum3 <- specaccum(bat.data.subset3, method = "rarefaction")
bat.specaccum4 <- specaccum(bat.data.subset4, method = "rarefaction")
bat.specaccum5 <- specaccum(bat.data.subset5, method = "rarefaction")
bat.specaccum6 <- specaccum(bat.data.subset6, method = "rarefaction")

##Summarize richness and standard deviation data from species accumulation curves
spac1 <- with(bat.specaccum1, data.frame(sites,richness,sd))
spac2 <- with(bat.specaccum2, data.frame(sites,richness,sd))
spac3 <- with(bat.specaccum3, data.frame(sites,richness,sd))
spac4 <- with(bat.specaccum4, data.frame(sites,richness,sd))
spac5 <- with(bat.specaccum5, data.frame(sites,richness,sd))
spac6 <- with(bat.specaccum6, data.frame(sites,richness,sd))

##Add a cluster column
spac1$cluster <- 1
spac2$cluster <- 2
spac3$cluster <- 3
spac4$cluster <- 4
spac5$cluster <- 5
spac6$cluster <- 6

##Combine the six species accumulation data frames
spac <- rbind(spac1,spac2,spac3,spac4,spac5,spac6)

##Make the species richness plots
ggplot(spac, aes(x=sites,y=richness,color=factor(cluster)))+ 
  geom_line(size=1.5)+
  xlab("Sample nights")+
  ylab("Richness")+
  theme_classic()+
  theme(legend.title = element_text(size=16), legend.text=element_text(size=12), axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title = element_text(size = 16), 
        panel.background = element_rect(fill = "white", color = "black"))+
  scale_color_manual(values=c("#000000","#FDE725FF","#238A8DFF","#404788FF","#55C667FF", "#A9A9A9"))+
  labs(color="Cluster")

  