library(corrplot)
library(grDevices)

#Read in data file
bat.data<-read.csv("./Data/2020-01_Swanson-et-al_Bats-Forest-Str_Master-Datasheet.csv", header=T)
names(bat.data)

#Select columns to check for correlation
bat.subset<-subset(bat.data,select=c(CanHeight,CanMean,Rugosity,Prop015,Prop156,Prop612,PropAb12,TimeSinceRain,TimeSinceFire,AreaWater,RoadLength,LandHeterogeneity,PropUrban,PropAg,PropForest,Entropy))

#Correlation matrix
mcor<-cor(bat.subset)

#Select colors for correlation matrix
col<-colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))

#Plot correlation matrix
corrplot(mcor,tl.col="black",tl.srt=60,col=col(200),order="AOE")

