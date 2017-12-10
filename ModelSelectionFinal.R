##Set the Working directory
setwd("G:/Thesis/Data")

##Read and attach the data
bat.data<-read.csv("BatMasterDataFINALworking2.csv",header=T)
names(bat.data)
bat<-subset(bat.data,ShanBatDiv != 0 & !is.na(Rugosity) & !is.na(TimeSinceFire))

##Call package for AIC comparisons
library(AICcmodavg)

##Set up variable for diversity
Bat.Div<-bat.data$ShanBatDiv

##models for patch-specific variables
cand.models<-list()
cand.models[[1]]<-lm(Bat.Div~Prop015)
cand.models[[2]]<-lm(Bat.Div~Entropy)
cand.models[[3]]<-lm(Bat.Div~Prop015+Entropy)
cand.models[[4]]<-lm(Bat.Div~Prop015*Entropy)

##Models including weather
cand.models[[4]]<-lm(Bat.Div~Prop015*Entropy+TimeSinceRain)
cand.models[[5]]<-lm(Bat.Div~Prop015*Entropy+Entropy*TimeSinceRain)

##Models including landscape variables
cand.models[[6]]<-lm(Bat.Div~Prop015*Entropy+PropAg+PropUrban+PropForest+PercentWater+RoadLength+TimeSinceRain)
cand.models[[7]]<-lm(Bat.Div~Prop015*Entropy+PropAg*PercentWater+PropForest*Entropy+PercentWater*RoadLength+Entropy*RoadLength+PercentWater*Entropy)
cand.models[[8]]<-lm(Bat.Div~Prop015*Entropy+PropAg*PercentWater+PropForest*Entropy+PropForest*Prop015+RoadLength)
cand.models[[9]]<-lm(Bat.Div~Prop015*Entropy+PropAg+PropForest*Entropy+RoadLength*Entropy+RoadLength*PercentWater+PropForest*PercentWater)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(cand.models), sep=" ")

##Generate AICc table
aictab(cand.set=cand.models, modnames=modelnames, sort=TRUE)

##Round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set=cand.models, modnames=modelnames, sort=TRUE), digits=4, LL=TRUE)

##Set variable for LABO presence/absence
labo<-bat.data$LABOPres

##models for patch-specific variables
labo.models<-list()
labo.models[[1]]<-glm(labo~Prop015, family=binomial)
labo.models[[2]]<-glm(labo~Entropy, family=binomial)
labo.models[[3]]<-glm(labo~Prop015+Entropy, family=binomial)
labo.models[[4]]<-glm(labo~Prop015*Entropy, family=binomial)
labo.models[[5]]<-glm(labo~Prop156, family=binomial)
labo.models[[12]]<-glm(labo~Prop156*Entropy, family=binomial)
labo.models[[13]]<-glm(labo~Prop156*Entropy+Prop015, family=binomial)

##Models including landscape variables
labo.models[[6]]<-glm(labo~Entropy+PropUrban+PropForest, family=binomial)
labo.models[[7]]<-glm(labo~Entropy*PropUrban+PropForest, family=binomial)
labo.models[[8]]<-glm(labo~Entropy*PropForest+PropUrban, family=binomial)
labo.models[[9]]<-glm(labo~Entropy*PropForest+Entropy*PropUrban, family=binomial)
labo.models[[10]]<-glm(labo~Entropy+PropForest*PropUrban, family=binomial)
labo.models[[11]]<-glm(labo~Entropy+PropForest*PropUrban+E,family=binomial)
labo.models[[14]]<-glm(labo~Entropy+PropForest*PropUrban+Prop156, family=binomial)
labo.models[[15]]<-glm(labo~PropForest*PropUrban+Prop156, family=binomial)
labo.models[[16]]<-glm(labo~PropForest+PropUrban+Prop156, family=binomial)
labo.models[[17]]<-glm(labo~PropForest*Prop156+PropUrban,family=binomial)
labo.models[[18]]<-glm(labo~PropForest*PropUrban+Prop156+Entropy, family=binomial)

##Create a vector of names to trace back models in set
labomodelnames<-paste("LABO Model", 1:length(labo.models), sep=" ")

##Generate AICc table
aictab(cand.set=labo.models, modnames=labomodelnames, sort=TRUE)

##Round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set=labo.models, modnames=modelnames, sort=TRUE), digits=4, LL=TRUE)

##Set variable for MYAU presence/absence
myau<-bat.data$MYAUPres

##models for patch-specific variables
myau.models<-list()
myau.models[[1]]<-glm(myau~Prop015+E, family=binomial)
myau.models[[2]]<-glm(myau~Prop612+E, family=binomial)
myau.models[[3]]<-glm(myau~Prop015+Prop612+E, family=binomial)
myau.models[[4]]<-glm(myau~Prop015*Prop612+E, family=binomial)

##Models including weather
myau.models[[5]]<-glm(myau~Prop612+PropForest+E, family=binomial)
myau.models[[6]]<-glm(myau~PropForest+E, family=binomial)
myau.models[[7]]<-glm(myau~Prop612*PropForest+E, family=binomial)
myau.models[[8]]<-glm(myau~Prop015+PropForest+E, family=binomial)
myau.models[[9]]<-glm(myau~Prop015*PropForest+E, family=binomial)
myau.models[[10]]<-glm(myau~Prop015+Prop612+PropForest+E, family=binomial)
myau.models[[11]]<-glm(myau~Prop015*PropForest+Prop612, family=binomial)
myau.models[[12]]<-glm(myau~Prop015+Prop612*PropForest+E, family=binomial)
myau.models[[13]]<-glm(myau~Prop015*PropForest+Prop612+E,family=binomial)
myau.models[[14]]<-glm(myau~PropForest,family=binomial)

##Create a vector of names to trace back models in set
myaumodelnames<-paste("MYAU Model", 1:length(myau.models), sep=" ")

##Generate AICc table
aictab(cand.set=myau.models, modnames=myaumodelnames, sort=TRUE)

##Round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set=labo.models, modnames=modelnames, sort=TRUE), digits=4, LL=TRUE)
