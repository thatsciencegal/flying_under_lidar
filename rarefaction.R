library(vegan)
library(ggplot2)

bat.data <- read.csv("../Data/BatMasterDataFINALworking2.csv")

bat.data.subset <- subset(bat.data, select = c(EPFU, LANO, LABO, LACI, MYAU, NYHU, PESU, Cluster))
bat.data.subset1 <- subset(bat.data.subset, Cluster == 1)
bat.data.subset2 <- subset(bat.data.subset, Cluster == 2)
bat.data.subset3 <- subset(bat.data.subset, Cluster == 3)
bat.data.subset4 <- subset(bat.data.subset, Cluster == 4)
bat.data.subset5 <- subset(bat.data.subset, Cluster == 5)
bat.data.subset6 <- subset(bat.data.subset, Cluster == 6)

bat.specaccum1 <- specaccum(bat.data.subset1, method = "rarefaction")
bat.specaccum2 <- specaccum(bat.data.subset2, method = "rarefaction")
bat.specaccum3 <- specaccum(bat.data.subset3, method = "rarefaction")
bat.specaccum4 <- specaccum(bat.data.subset4, method = "rarefaction")
bat.specaccum5 <- specaccum(bat.data.subset5, method = "rarefaction")
bat.specaccum6 <- specaccum(bat.data.subset6, method = "rarefaction")
