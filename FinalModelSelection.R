setwd("G:/Thesis/Data")

##Read and attach the data
bat.data<-read.csv("BatMasterDataFINALworking2.csv",header=T)
names(bat.data)
bat<-subset(bat.data,ShanBatDiv != 0)

##Call package for AIC comparisons
library(AICcmodavg)

##Set up variable for diversity
Bat.Div<-bat$ShanBatDiv

##models
cand.models<-list()

##1) Stand-level attributes will contribute to bat diversity. Expected contributions would be canopy height, entropy, rugosity, and proportion of returns in height bins.

cand.models[[1]]<-lm(logbat~vifCanMean+vifEntropy+vifRugosity+vifProp156+vifProp612, data=bat)
cand.models[[2]]<-lm(logbat~vifCanMean*vifEntropy+vifRugosity+vifProp156+vifProp612, data=bat)
cand.models[[3]]<-lm(logbat~vifCanMean+vifEntropy+vifRugosity, data=bat)
cand.models[[4]]<-lm(logbat~vifCanMean*vifEntropy+vifRugosity, data=bat)

##2) Time since fire will relate to bat community diversity

cand.models[[5]]<-lm(logbat~vifCanMean*vifTimeSinceFire, data=bat)
cand.models[[6]]<-lm(logbat~vifCanMean*vifTimeSinceFire+vifEntropy+vifRugosity+vifProp156+vifProp612, data=bat)
cand.models[[7]]<-lm(logbat~vifCanMean*vifEntropy+vifRugosity+vifProp156+vifProp612+vifCanMean*vifTimeSinceFire, data=bat)

##3) Stand-level attributes will relate to bat community diversity

cand.models[[8]]<-lm(logbat~vifCanMean*vifTimeSinceFire+vifEntropy+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifCanMean+vifRoadLength, data=bat)
cand.models[[9]]<-lm(logbat~vifCanMean*vifTimeSinceFire+vifEntropy*vifRoadLength+vifProp156+vifProp612+vifAreaWater*vifCanMean+vifEntropy*vifCanMean, data=bat)
cand.models[[10]]<-lm(logbat~vifCanMean*vifTimeSinceFire+vifCanMean*vifAreaWater, data=bat)
cand.models[[11]]<-lm(logbat~vifCanMean*vifTimeSinceFire+vifEntropy*vifTimeSinceFire+vifProp156+vifProp612+vifAreaWater*vifCanMean+vifEntropy*vifCanMean, data=bat)
cand.models[[12]]<-lm(logbat~vifCanMean*vifTimeSinceFire+vifEntropy*vifTimeSinceFire+vifProp156+vifProp612+vifAreaWater*vifCanMean+vifPropUrban, data=bat)
cand.models[[13]]<-lm(logbat~vifCanMean*vifTimeSinceFire+vifEntropy*vifTimeSinceFire+vifCanMean*vifAreaWater+vifProp156+vifEntropy*vifCanMean, data=bat)
cand.models[[14]]<-lm(logbat~vifCanMean*vifTimeSinceFire+vifEntropy*vifTimeSinceFire+vifCanMean*vifAreaWater+vifProp612+vifEntropy*vifCanMean, data=bat)
cand.models[[15]]<-lm(logbat~vifCanMean*vifTimeSinceFire+vifEntropy*vifTimeSinceFire+vifCanMean*vifAreaWater+vifEntropy:vifCanMean, data=bat)
cand.models[[16]]<-lm(logbat~vifCanMean*vifTimeSinceFire+vifEntropy*vifTimeSinceFire+vifCanMean*vifAreaWater+vifProp156+vifProp612+vifEntropy*vifCanMean+vifRoadLength+vifPropUrban, data=bat)
cand.models[[17]]<-lm(logbat~vifCanMean*vifTimeSinceFire+vifEntropy*vifTimeSinceFire+vifCanMean*vifAreaWater+vifProp156+vifProp612+vifEntropy*vifCanMean+vifRoadLength*vifEntropy+vifPropUrban, data=bat)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(cand.models), sep=" ")

##Generate AICc table
aictab(cand.set=cand.models, modnames=modelnames, sort=TRUE)

##Set up variable for diversity
labo<-bat.data$LABOPres

##models
labo.models<-list()

##1) Stand-level attributes will contribute to bat diversity. Expected contributions would be canopy height, entropy, rugosity, and proportion of returns in height bins.

labo.models[[1]]<-glm(labo~CanMean+Entropy+Rugosity+Prop015+Prop156+Prop612, family=binomial, data=bat.data)
labo.models[[2]]<-glm(labo~CanMean*Entropy+Rugosity+Prop015+Prop156+Prop612, family=binomial, data=bat.data)
labo.models[[3]]<-glm(labo~CanMean+Entropy+Rugosity, family=binomial, data=bat.data)
labo.models[[4]]<-glm(labo~CanMean*Entropy+Rugosity, family=binomial, data=bat.data)

##2) Time since fire will relate to bat community diversity

labo.models[[5]]<-glm(labo~CanMean*TimeSinceFire, family=binomial, data=bat.data)
labo.models[[6]]<-glm(labo~CanMean*TimeSinceFire+Entropy+Rugosity+Prop015+Prop156+Prop612, family=binomial, data=bat.data)
labo.models[[7]]<-glm(labo~CanMean*Entropy+Rugosity+Prop015+Prop156+Prop612+CanMean*TimeSinceFire, family=binomial, data=bat.data)

##3) Stand-level attributes will relate to bat community diversity

labo.models[[8]]<-glm(labo~CanMean*TimeSinceFire+Entropy+Prop156+Prop612+PropUrban+AreaWater*CanMean+Prop015+RoadLength, family=binomial, data=bat.data)
labo.models[[9]]<-glm(labo~CanMean*TimeSinceFire+Entropy*RoadLength+Prop156+Prop612+AreaWater*CanMean+Prop015+Entropy*CanMean, family=binomial, data=bat.data)
labo.models[[10]]<-glm(labo~CanMean*TimeSinceFire+CanMean*AreaWater, family=binomial, data=bat.data)
labo.models[[11]]<-glm(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+Prop156+Prop612+AreaWater*CanMean+Entropy*CanMean, family=binomial, data=bat.data)
labo.models[[12]]<-glm(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+Prop156+Prop612+AreaWater*CanMean+PropUrban, family=binomial, data=bat.data)
labo.models[[13]]<-glm(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Prop156+Entropy*CanMean, family=binomial, data=bat.data)
labo.models[[14]]<-glm(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Prop612+Entropy*CanMean, family=binomial, data=bat.data)
labo.models[[15]]<-glm(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Entropy:CanMean, family=binomial, data=bat.data)
labo.models[[16]]<-glm(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+CanMean*AreaWater+Prop015+Prop156+Prop612+Entropy*CanMean+RoadLength+PropUrban, family=binomial, data=bat.data)
labo.models[[17]]<-glm(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+CanMean*AreaWater+Prop015+Prop156+Prop612+Entropy*CanMean+RoadLength*Entropy+PropUrban, family=binomial, data=bat.data)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(labo.models), sep=" ")

##Generate AICc table
aictab(cand.set=labo.models, modnames=modelnames, sort=TRUE)

##Set up variable for diversity
pesu<-bat.data$PESUPres

##models
pesu.models<-list()

##1) Stand-level attributes will contribute to bat diversity. Expected contributions would be canopy height, entropy, rugosity, and proportion of returns in height bins.

pesu.models[[1]]<-glm(pesu~CanMean+Entropy+Rugosity+Prop015+Prop156+Prop612, family=binomial, data=bat.data)
pesu.models[[2]]<-glm(pesu~CanMean*Entropy+Rugosity+Prop015+Prop156+Prop612, family=binomial, data=bat.data)
pesu.models[[3]]<-glm(pesu~CanMean+Entropy+Rugosity, family=binomial, data=bat.data)
pesu.models[[4]]<-glm(pesu~CanMean*Entropy+Rugosity, family=binomial, data=bat.data)

##2) Time since fire will relate to bat community diversity

pesu.models[[5]]<-glm(pesu~CanMean*TimeSinceFire, family=binomial, data=bat.data)
pesu.models[[6]]<-glm(pesu~CanMean*TimeSinceFire+Entropy+Rugosity+Prop015+Prop156+Prop612, family=binomial, data=bat.data)
pesu.models[[7]]<-glm(pesu~CanMean*Entropy+Rugosity+Prop015+Prop156+Prop612+CanMean*TimeSinceFire, family=binomial, data=bat.data)

##3) Stand-level attributes will relate to bat community diversity

pesu.models[[8]]<-glm(pesu~CanMean*TimeSinceFire+Entropy+Prop156+Prop612+PropUrban+AreaWater*CanMean+RoadLength+Prop015, family=binomial, data=bat.data)
pesu.models[[9]]<-glm(pesu~CanMean*TimeSinceFire+Entropy*RoadLength+Prop156+Prop612+AreaWater*CanMean+Prop015+Entropy*CanMean, family=binomial, data=bat.data)
pesu.models[[10]]<-glm(pesu~CanMean*TimeSinceFire+CanMean*AreaWater, family=binomial, data=bat.data)
pesu.models[[11]]<-glm(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+Prop156+Prop612+AreaWater*CanMean+Entropy*CanMean, family=binomial, data=bat.data)
pesu.models[[12]]<-glm(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+Prop156+Prop612+AreaWater*CanMean+PropUrban, family=binomial, data=bat.data)
pesu.models[[13]]<-glm(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Prop156+Entropy*CanMean, family=binomial, data=bat.data)
pesu.models[[14]]<-glm(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Prop612+Entropy*CanMean, family=binomial, data=bat.data)
pesu.models[[15]]<-glm(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Entropy:CanMean, family=binomial, data=bat.data)
pesu.models[[16]]<-glm(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+CanMean*AreaWater+Prop015+Prop156+Prop612+Entropy*CanMean+RoadLength+PropUrban, family=binomial, data=bat.data)
pesu.models[[17]]<-glm(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+CanMean*AreaWater+Prop015+Prop156+Prop612+Entropy*CanMean+RoadLength*Entropy+PropUrban, family=binomial, data=bat.data)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(pesu.models), sep=" ")

##Generate AICc table
aictab(cand.set=pesu.models, modnames=modelnames, sort=TRUE)

bat.total<-subset(bat.data,TotalID != 0)
##Set up variable for diversity
total<-bat.total$TotalID

##models
total.models<-list()

##1) Stand-level attributes will contribute to bat diversity. Expected contributions would be canopy height, entropy, rugosity, and proportion of returns in height bins.

total.models[[1]]<-lm(total~CanMean+Entropy+Rugosity+Prop015+Prop156+Prop612, data=bat.total)
total.models[[2]]<-lm(total~CanMean*Entropy+Rugosity+Prop015+Prop156+Prop612, data=bat.total)
total.models[[3]]<-lm(total~CanMean+Entropy+Rugosity, data=bat.total)
total.models[[4]]<-lm(total~CanMean*Entropy+Rugosity, data=bat.total)

##2) Time since fire will relate to bat community diversity

total.models[[5]]<-lm(total~CanMean*TimeSinceFire, data=bat.total)
total.models[[6]]<-lm(total~CanMean*TimeSinceFire+Entropy+Rugosity+Prop015+Prop156+Prop612, data=bat.total)
total.models[[7]]<-lm(total~CanMean*Entropy+Rugosity+Prop015+Prop156+Prop612+CanMean*TimeSinceFire, data=bat.total)

##3) Stand-level attributes will relate to bat community diversity

total.models[[8]]<-lm(total~CanMean*TimeSinceFire+Entropy+Prop156+Prop612+PropUrban+AreaWater*Prop015+RoadLength, data=bat.total)
total.models[[9]]<-lm(total~CanMean*TimeSinceFire+Entropy*RoadLength+Prop156+Prop612+AreaWater*CanMean+Prop015+Entropy*CanMean, data=bat.total)
total.models[[10]]<-lm(total~CanMean*TimeSinceFire+CanMean*AreaWater, data=bat.total)
total.models[[11]]<-lm(total~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+Prop156+Prop612+AreaWater*CanMean+Entropy*CanMean, data=bat.total)
total.models[[12]]<-lm(total~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+Prop156+Prop612+AreaWater*CanMean+PropUrban, data=bat.total)
total.models[[13]]<-lm(total~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Prop156+Entropy*CanMean, data=bat.total)
total.models[[14]]<-lm(total~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Prop612+Entropy*CanMean, data=bat.total)
total.models[[15]]<-lm(total~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Entropy:CanMean, data=bat.total)
total.models[[16]]<-lm(total~CanMean*TimeSinceFire+Entropy*TimeSinceFire+CanMean*AreaWater+Prop015+Prop156+Prop612+Entropy*CanMean+RoadLength+PropUrban, data=bat.total)
total.models[[17]]<-lm(total~CanMean*TimeSinceFire+Entropy*TimeSinceFire+CanMean*AreaWater+Prop015+Prop156+Prop612+Entropy*CanMean+RoadLength*Entropy+PropUrban, data=bat.total)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(total.models), sep=" ")

##Generate AICc table
aictab(cand.set=total.models, modnames=modelnames, sort=TR

       ##Set up variable for diversity
       labo<-bat.data$LABOPres
       
       ##models
       labo.models<-list()
       
       ##1) Stand-level attributes will contribute to bat diversity. Expected contributions would be canopy height, entropy, rugosity, and proportion of returns in height bins.
       
       labo.models[[1]]<-logistf(labo~CanMean+Entropy+Rugosity+Prop015+Prop156+Prop612, data=bat.data)
       labo.models[[2]]<-logistf(labo~CanMean*Entropy+Rugosity+Prop015+Prop156+Prop612, data=bat.data)
       labo.models[[3]]<-logistf(labo~CanMean+Entropy+Rugosity, data=bat.data)
       labo.models[[4]]<-logistf(labo~CanMean*Entropy+Rugosity, data=bat.data)
       
       ##2) Time since fire will relate to bat community diversity
       
       labo.models[[5]]<-logistf(labo~CanMean*TimeSinceFire, family=binomial, data=bat.data)
       labo.models[[6]]<-logistf(labo~CanMean*TimeSinceFire+Entropy+Rugosity+Prop015+Prop156+Prop612, family=binomial, data=bat.data)
       labo.models[[7]]<-logistf(labo~CanMean*Entropy+Rugosity+Prop015+Prop156+Prop612+CanMean*TimeSinceFire, family=binomial, data=bat.data)
       
       ##3) Stand-level attributes will relate to bat community diversity
       
       labo.models[[8]]<-logistf(labo~CanMean*TimeSinceFire+Entropy+Prop156+Prop612+PropUrban+AreaWater*CanMean+Prop015+RoadLength, family=binomial, data=bat.data)
       labo.models[[9]]<-logistf(labo~CanMean*TimeSinceFire+Entropy*RoadLength+Prop156+Prop612+AreaWater*CanMean+Prop015+Entropy*CanMean, family=binomial, data=bat.data)
       labo.models[[10]]<-logistf(labo~CanMean*TimeSinceFire+CanMean*AreaWater, family=binomial, data=bat.data)
       labo.models[[11]]<-logistf(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+Prop156+Prop612+AreaWater*CanMean+Entropy*CanMean, family=binomial, data=bat.data)
       labo.models[[12]]<-logistf(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+Prop156+Prop612+AreaWater*CanMean+PropUrban, family=binomial, data=bat.data)
       labo.models[[13]]<-logistf(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Prop156+Entropy*CanMean, family=binomial, data=bat.data)
       labo.models[[14]]<-logistf(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Prop612+Entropy*CanMean, family=binomial, data=bat.data)
       labo.models[[15]]<-logistf(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Entropy:CanMean, family=binomial, data=bat.data)
       labo.models[[16]]<-logistf(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+CanMean*AreaWater+Prop015+Prop156+Prop612+Entropy*CanMean+RoadLength+PropUrban, family=binomial, data=bat.data)
       labo.models[[17]]<-logistf(labo~CanMean*TimeSinceFire+Entropy*TimeSinceFire+CanMean*AreaWater+Prop015+Prop156+Prop612+Entropy*CanMean+RoadLength*Entropy+PropUrban, family=binomial, data=bat.data)
       
       ##Create a vector of names to trace back models in set
       modelnames<-paste("Model", 1:length(labo.models), sep=" ")
       
       ##Generate AICc table
       aictab(cand.set=labo.models, modnames=modelnames, sort=TRUE)       

       pesu.models<-list()
       
       ##1) Stand-level attributes will contribute to bat diversity. Expected contributions would be canopy height, entropy, rugosity, and proportion of returns in height bins.
       
       pesu.models[[1]]<-logistf(pesu~CanMean+Entropy+Rugosity+Prop015+Prop156+Prop612, family=binomial, data=bat.data)
       pesu.models[[2]]<-logistf(pesu~CanMean*Entropy+Rugosity+Prop015+Prop156+Prop612, family=binomial, data=bat.data)
       pesu.models[[3]]<-logistf(pesu~CanMean+Entropy+Rugosity, family=binomial, data=bat.data)
       pesu.models[[4]]<-logistf(pesu~CanMean*Entropy+Rugosity, family=binomial, data=bat.data)
       
       ##2) Time since fire will relate to bat community diversity
       
       pesu.models[[5]]<-logistf(pesu~CanMean*TimeSinceFire, family=binomial, data=bat.data)
       pesu.models[[6]]<-logistf(pesu~CanMean*TimeSinceFire+Entropy+Rugosity+Prop015+Prop156+Prop612, family=binomial, data=bat.data)
       pesu.models[[7]]<-logistf(pesu~CanMean*Entropy+Rugosity+Prop015+Prop156+Prop612+CanMean*TimeSinceFire, family=binomial, data=bat.data)
       
       ##3) Stand-level attributes will relate to bat community diversity
       
       pesu.models[[8]]<-logistf(pesu~CanMean*TimeSinceFire+Entropy+Prop156+Prop612+PropUrban+AreaWater*CanMean+RoadLength+Prop015, family=binomial, data=bat.data)
       pesu.models[[9]]<-logistf(pesu~CanMean*TimeSinceFire+Entropy*RoadLength+Prop156+Prop612+AreaWater*CanMean+Prop015+Entropy*CanMean, family=binomial, data=bat.data)
       pesu.models[[10]]<-logistf(pesu~CanMean*TimeSinceFire+CanMean*AreaWater, family=binomial, data=bat.data)
       pesu.models[[11]]<-logistf(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+Prop156+Prop612+AreaWater*CanMean+Entropy*CanMean, family=binomial, data=bat.data)
       pesu.models[[12]]<-logistf(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+Prop156+Prop612+AreaWater*CanMean+PropUrban, family=binomial, data=bat.data)
       pesu.models[[13]]<-logistf(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Prop156+Entropy*CanMean, family=binomial, data=bat.data)
       pesu.models[[14]]<-logistf(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Prop612+Entropy*CanMean, family=binomial, data=bat.data)
       pesu.models[[15]]<-logistf(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+Prop015+CanMean*AreaWater+Entropy:CanMean, family=binomial, data=bat.data)
       pesu.models[[16]]<-logistf(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+CanMean*AreaWater+Prop015+Prop156+Prop612+Entropy*CanMean+RoadLength+PropUrban, family=binomial, data=bat.data)
       pesu.models[[17]]<-logistf(pesu~CanMean*TimeSinceFire+Entropy*TimeSinceFire+CanMean*AreaWater+Prop015+Prop156+Prop612+Entropy*CanMean+RoadLength*Entropy+PropUrban, family=binomial, data=bat.data)
       
vifCanMean<-bat$CanMean-mean(bat$CanMean)
vifEntropy<-bat$Entropy-mean(bat$Entropy)
vifRugosity<-bat$Rugosity-mean(bat$Rugosity)
vifProp015<-bat$Prop015-mean(bat$Prop015)
vifProp156<-bat$Prop156-mean(bat$Prop156)
vifProp612<-bat$Prop612-mean(bat$Prop612)
vifTimeSinceFire<-bat$TimeSinceFire-mean(bat$TimeSinceFire)
vifPropUrban<-bat$PropUrban-mean(bat$PropUrban)
vifAreaWater<-bat$AreaWater-mean(bat$AreaWater)
vifRoadLength<-bat$RoadLength-mean(bat$RoadLength)
       