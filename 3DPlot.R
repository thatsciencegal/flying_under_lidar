setwd("G:/Thesis/Data")
data<-read.csv("BatMasterDataFINALworking2.csv", header=T)
attach(data)
library(scatterplot3d)
x= seq(min(Entropy),max(Entropy),0.05)
y =seq(min(TimeSinceFire),max(TimeSinceFire),10)
datos <- array(0,c(length(x)*length(y),3))

r=0
for (i in 1:length(x)) {
  for (j in 1:length(y)){
    r<-r+1
    datos[r,1] <- predict(cand.models[[16]],list(vifEntropy=x[i], vifTimeSinceFire=y[j],vifCanMean=mean(CanMean), 
                          vifAreaWater=mean(AreaWater),vifProp156=mean(Prop156),vifProp612=mean(Prop612),vifRoadLength=mean(RoadLength),vifPropUrban=mean(PropUrban)))
    datos[r,2] <- x[i]
    datos[r,3] <- y[j]
  }
}

datfplot <- cbind(vifEntropy,vifTimeSinceFire,logbat)
s3d <- scatterplot3d(datos[,2],datos[,3],datos[,1], xlab="Entropy", ylab="Time Since Fire (ln)", zlab="Diversity",zlim=c(0,2))
s3d$points3d(datfplot, col="blue", pch=16)

