##read data
osdata<-read.csv("./Data/2020-01_Swanson-et-al_Bats-Forest-Str_Quant-Lidar.csv", header=T)

#write the k means cluster
model3<-kmeans(data.frame(ElevMin,ElevMax,ElevMean,ElevStdev,ElevSkew,ElevKurt,Ret3Ab3,PerAb3,PropStrat5, Prop515, Prop153, Prop36, Prop69, Prop912, PropAb12), centers = 6, algorithm="Lloyd", iter.max=1000)
model4<-kmeans(data.frame(ElevMin,ElevMax,ElevMean,ElevStdev,ElevSkew,ElevKurt,Ret3Ab3,PerAb3,PropStrat5, Prop515, Prop153, Prop36, Prop69, Prop912, PropAb12), centers = 5, algorithm="Lloyd", iter.max=1000)

#write the model to a file
library(MASS)
write.matrix(model3,file="kmeans6-1.txt", sep =",")
write.matrix(model4,file="kmeans5-1.txt", sep =",")

##import the kmeans data
clusterdata6<-scan("kmeans6-1-1.txt", what=numeric(), sep=",")
clusterdata5<-scan("kmeans5-1-1.txt", what=numeric(), sep=",")

##transponse the kmeans data
t(clusterdata6)
t(clusterdata5)

##add clusters to original data
osdata$Cluster6<-clusterdata6
osdata$Cluster5<-clusterdata5

##write table with clusters included
write.table(unclass(osdata), "OSclusters.txt", sep=",", col.names=T, row.names=F)
write.table(unclass(osbsdata), "OSBSclusters5-1.txt", sep=",", col.names=T, row.names=F)

#write the k means cluster
model2<-kmeans(data.frame(TotalReturnCount,ElevMinim,ElevMax,ElevMean,ElevStdDev,ElevSkew,ElevKurtosis,Return3Above3,PercentAllAbove3,MaxHeight,P0to3,P3to6,P6to9,P9to12,P12to15,P15to18,P18to21,P21to24,P24to27,P27to30,P30to33), centers = 10, algorithm="Lloyd", iter.max=1000)

#write the model to a file
library(MASS)
write.matrix(model2,file="kmeans10-2.txt", sep =",")

##import the kmeans data
kmeans10<-scan("kmeans10-2.txt", what=numeric(), sep=",")

##transponse the kmeans data
t(kmeans10)

##add clusters to original data
data$Clusters10<-kmeans10

##write table with clusters included
write.table(unclass(data), "OSBSclusters10-2.txt", sep=",", col.names=T, row.names=F)

##model with 7 clusters
model3<-kmeans(data.frame(TotalReturnCount,ElevMinim,ElevMax,ElevMean,ElevStdDev,ElevSkew,ElevKurtosis,Return3Above3,PercentAllAbove3,MaxHeight,P0to3,P3to6,P6to9,P9to12,P12to15,P15to18,P18to21,P21to24,P24to27,P27to30,P30to33), centers = 7, algorithm="Lloyd", iter.max=1000)

#write the model to a file
library(MASS)
write.matrix(model3,file="kmeans7-1.txt", sep =",")

##import the kmeans data
kmeans7<-scan("kmeans7-1.txt", what=numeric(), sep=",")

##transponse the kmeans data
t(kmeans7)

##add clusters to original data
data$Clusters7<-kmeans7

##write table with clusters included
write.table(unclass(data), "OSBSclusters7-1.txt", sep=",", col.names=T, row.names=F)

names(data)

##Remove minimum elevation to counteract effects of topography
data$ElevMax2<-(ElevMax-ElevMinim)
data$ElevMean2<-(ElevMean-ElevMinim)
model4<-kmeans(data.frame(TotalReturnCount,ElevMax2,ElevMean2,ElevStdDev,ElevSkew,ElevKurtosis,Return3Above3,PercentAllAbove3,MaxHeight,P0to3,P3to6,P6to9,P9to12,P12to15,P15to18,P18to21,P21to24,P24to27,P27to30,P30to33), centers = 7, algorithm="Lloyd", iter.max=1000)
write.matrix(model4,file="kmeans7-2.txt", sep =",")
kmeans72<-scan("kmeans7-2.txt", what=numeric(), sep=",")
t(kmeans72)
data$Clusters72<-kmeans72
write.table(unclass(data), "OSBSclusters7-2.txt", sep=",", col.names=T, row.names=F)

##run kmeans without total return count (this was messing up the clusters with remnants of flight lines) and with adjusted elevation metrics
model5<-kmeans(data.frame(ElevMax2,ElevMean2,ElevStdDev,ElevSkew,ElevKurtosis,Return3Above3,PercentAllAbove3,MaxHeight,P0to3,P3to6,P6to9,P9to12,P12to15,P15to18,P18to21,P21to24,P24to27,P27to30,P30to33), centers = 7, algorithm="Lloyd", iter.max=1000)
kmeans73<-scan("kmeans7-3.txt", what=numeric(), sep=",")
t(kmeans73)
data$Clusters73<-kmeans73

write.table(unclass(data), "OSBSclusters7-3.txt", sep=",", col.names=T, row.names=F)

##segregate rows by cluster
##note: clusters 1 and 7 were excluded because no data and lakes, respectively
data.sub1<-subset(osclusters, Cluster6==1)
write.table(unclass(data.sub1), "OSclustersSub6-1-1.txt", sep=",", col.names=T, row.names=F)

data.sub2<-subset(osclusters, Cluster6==2)
write.table(unclass(data.sub2), "OSBSclustersSub6-1-2.txt", sep=",", col.names=T, row.names=F)

data.sub3<-subset(osclusters, Cluster6==3)
write.table(unclass(data.sub3), "OSBSclustersSub6-1-3.txt", sep=",", col.names=T, row.names=F)

data.sub4<-subset(osclusters, Cluster6==4)
write.table(unclass(data.sub4), "OSBSclustersSub6-1-4.txt", sep=",", col.names=T, row.names=F)

data.sub5<-subset(osclusters, Cluster6==5)
write.table(unclass(data.sub5), "OSBSclustersSub6-1-5.txt", sep=",", col.names=T, row.names=F)

data.sub6<-subset(osclusters, Cluster6==6)
write.table(unclass(data.sub6), "OSBSclustersSub6-5-6.txt", sep=",", col.names=T, row.names=F)

##randomly select sites from the subsets, extra sites were selected to ensure that there were enough sites within the boundaries of Ordway-Swisher
set.seed(0)
random1<-data.sub1[sample(nrow(data.sub1), 30), ]
random2<-data.sub2[sample(nrow(data.sub2), 30), ]
random3<-data.sub3[sample(nrow(data.sub3), 30), ]
random4<-data.sub4[sample(nrow(data.sub4), 30), ]
random5<-data.sub5[sample(nrow(data.sub5), 30), ]
random6<-data.sub6[sample(nrow(data.sub6), 30), ]

##write random samples to a table
write.table(unclass(random1), "OSRandom6-1-1.txt", sep=",", col.names=T, row.names=F)
write.table(unclass(random2), "OSRandom6-1-2.txt", sep=",", col.names=T, row.names=F)
write.table(unclass(random3), "OSRandom6-1-3.txt", sep=",", col.names=T, row.names=F)
write.table(unclass(random4), "OSRandom6-1-4.txt", sep=",", col.names=T, row.names=F)
write.table(unclass(random5), "OSRandom6-1-5.txt", sep=",", col.names=T, row.names=F)
write.table(unclass(random6), "OSRandom6-1-6.txt", sep=",", col.names=T, row.names=F)
model6<-kmeans(data.frame(ElevMinim,ElevMax,ElevMean,ElevStdDev,ElevSkew,ElevKurtosis,Return3Above3,PercentAllAbove3,MaxHeight,P0to3,P3to6,P6to9,P9to12,P12to15,P15to18,P18to21,P21to24,P24to27,P27to30,P30to33), centers = 6, algorithm="Lloyd", iter.max=1000)
write.table(unclass(model6), "OSBSclusters6-2.txt", sep=",", col.names=T, row.names=F)


data.sub61<-subset(data, Clusters62==1)
write.table(unclass(data.sub61), "OSBSclustersSub6-2-1.txt", sep=",", col.names=T, row.names=F)

data.sub62<-subset(data, Clusters62==2)
write.table(unclass(data.sub62), "OSBSclustersSub6-2-2.txt", sep=",", col.names=T, row.names=F)

data.sub63<-subset(data, Clusters62==3)
write.table(unclass(data.sub63), "OSBSclustersSub6-2-3.txt", sep=",", col.names=T, row.names=F)

data.sub64<-subset(data, Clusters62==4)
write.table(unclass(data.sub64), "OSBSclustersSub6-2-4.txt", sep=",", col.names=T, row.names=F)

data.sub65<-subset(data, Clusters62==5)
write.table(unclass(data.sub65), "OSBSclustersSub6-2-5.txt", sep=",", col.names=T, row.names=F)

data.sub66<-subset(data, Clusters62==6)
write.table(unclass(data.sub66), "OSBSclustersSub6-2-6.txt", sep=",", col.names=T, row.names=F)
lib