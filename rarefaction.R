library(vegan)
library(ggplot2)
library(viridis)

bat.data <- read.csv("../Data/BatMasterDataFINALworking2.csv")

bat.data.subset <- subset(bat.data, select = c(as.numeric(EPFU), as.numeric(LANO), LABO, LACI, LAIN, MYAU, NYHU, PESU, TABR, ï..Cluster))
bat.data.subset$EPFU <- as.numeric(bat.data.subset$EPFU)
bat.data.subset$LANO <- as.numeric(bat.data.subset$LANO)
bat.data.subset[is.na(bat.data.subset)]<-0
bat.data.subset1 <- subset(bat.data.subset, ï..Cluster == 1)
bat.data.subset2 <- subset(bat.data.subset, ï..Cluster == 2)
bat.data.subset3 <- subset(bat.data.subset, ï..Cluster == 3)
bat.data.subset4 <- subset(bat.data.subset, ï..Cluster == 4)
bat.data.subset5 <- subset(bat.data.subset, ï..Cluster == 5)
bat.data.subset6 <- subset(bat.data.subset, ï..Cluster == 6)

bat.specaccum1 <- specaccum(bat.data.subset1, na.rm = TRUE, method = "rarefaction")
bat.specaccum2 <- specaccum(bat.data.subset2, method = "rarefaction")
bat.specaccum3 <- specaccum(bat.data.subset3, method = "rarefaction")
bat.specaccum4 <- specaccum(bat.data.subset4, method = "rarefaction")
bat.specaccum5 <- specaccum(bat.data.subset5, method = "rarefaction")
bat.specaccum6 <- specaccum(bat.data.subset6, method = "rarefaction")

spac1 <- with(bat.specaccum1, data.frame(sites,richness,sd))
spac2 <- with(bat.specaccum2, data.frame(sites,richness,sd))
spac3 <- with(bat.specaccum3, data.frame(sites,richness,sd))
spac4 <- with(bat.specaccum4, data.frame(sites,richness,sd))
spac5 <- with(bat.specaccum5, data.frame(sites,richness,sd))
spac6 <- with(bat.specaccum6, data.frame(sites,richness,sd))

spac1$cluster <- 1
spac2$cluster <- 2
spac3$cluster <- 3
spac4$cluster <- 4
spac5$cluster <- 5
spac6$cluster <- 6

spac <- rbind(spac1,spac2,spac3,spac4,spac5,spac6)

ggplot(spac, aes(x=sites,y=richness,color=factor(cluster)))+
  geom_line(size=1)+
  xlab("Sample nights")+
  ylab("Richness")+
  theme_classic()+
  scale_color_viridis_d()
