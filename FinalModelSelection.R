library(AICcmodavg)
library(dplyr)
library(caret)
library(broom)

##Read in bat call data
bat.data<-read.csv("./Data/2020-01_Swanson-et-al_Bats-Forest-Str_Master-Datasheet.csv",header=T)

###################################################################################
###########################      Confusion Matrix      ############################
###################################################################################
man.dat <- read.csv("./Data/2020-01_Swanson-et-al_Bats-Forest-Str_Manual-ID.csv.csv")

man.dat.sub <- subset(man.dat, AUTO.ID. != "NoID")
confusionMatrix(man.dat.sub$AUTO.ID.,man.dat.sub$MANUAL.ID)
confusionMatrix(man.dat$AUTO.ID., man.dat$MANUAL.ID)

##Calculate normalized variables
vifCanMean<-bat.data$CanMean-mean(bat.data$CanMean)
vifEntropy<-bat.data$Entropy-mean(bat.data$Entropy)
vifRugosity<-bat.data$Rugosity-mean(bat.data$Rugosity)
vifProp015<-bat.data$Prop015-mean(bat.data$Prop015)
vifProp156<-bat.data$Prop156-mean(bat.data$Prop156)
vifProp612<-bat.data$Prop612-mean(bat.data$Prop612)
vifTimeSinceFire<-bat.data$TimeSinceFire-mean(bat.data$TimeSinceFire)
vifPropUrban<-bat.data$PropUrban-mean(bat.data$PropUrban)
vifAreaWater<-bat.data$AreaWater-mean(bat.data$AreaWater)
vifRoadLength<-bat.data$RoadLength-mean(bat.data$RoadLength)
vifLandHet<-bat.data$LandHeterogeneity-mean(bat.data$LandHeterogeneity)

##models
cand.models<-list()

######################################
#######    Abundance Models    #######
######################################

##1) Stand-level models

cand.models[[1]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
cand.models[[2]]<-lm(Total~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
cand.models[[3]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
cand.models[[4]]<-lm(Total~vifCanMean*vifEntropy+vifRugosity, data=bat.data)

##2) Stand- and landscape-level models

cand.models[[5]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet, data=bat.data)
cand.models[[6]]<-lm(Total~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet, data=bat.data)
cand.models[[7]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet, data=bat.data)
cand.models[[8]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength, data=bat.data)
cand.models[[9]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength, data=bat.data)
cand.models[[10]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet, data=bat.data)

##3) Landscape-level models
cand.models[[11]]<-lm(Total~vifLandHet, data=bat.data)
cand.models[[12]]<-lm(Total~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet, data=bat.data)
cand.models[[13]]<-lm(Total~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength, data=bat.data)
cand.models[[14]]<-lm(Total~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength, data=bat.data)
cand.models[[15]]<-lm(Total~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet, data=bat.data)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(cand.models), sep=" ")

##Generate AICc table
aictab(cand.set=cand.models, modnames=modelnames, sort=TRUE)

cand.tidy<-lapply(cand.models,tidy)
lapply(1:length(cand.tidy), function(i) write.csv(cand.tidy[[i]],
                                        file= paste0("./Results/abundance",i,".csv"),
                                        row.names=FALSE))

##Calculate richness
bat.data.rich <- bat.data %>% 
  mutate(richness = (EPFUPres+LANOPres+LABOPres+LACIPres+LAINPres+MYAUPres+
                        NYHUPres+PESUPres+TABRPres))

##Richness data

##models
rich.models<-list()

##1) Stand-level models
rich.models[[1]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data.rich)
rich.models[[2]]<-lm(richness~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data.rich)
rich.models[[3]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity, data=bat.data.rich)
rich.models[[4]]<-lm(richness~vifCanMean*vifEntropy+vifRugosity, data=bat.data.rich)

##2) Stand- and landscape-level models
rich.models[[5]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet, data=bat.data.rich)
rich.models[[6]]<-lm(richness~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet, data=bat.data.rich)
rich.models[[7]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet, data=bat.data.rich)
rich.models[[8]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength, data=bat.data.rich)
rich.models[[9]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength, data=bat.data.rich)
rich.models[[10]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet, data=bat.data.rich)

##3) Landscape-level models
rich.models[[11]]<-lm(richness~vifLandHet, data=bat.data.rich)
rich.models[[12]]<-lm(richness~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet, data=bat.data.rich)
rich.models[[13]]<-lm(richness~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength, data=bat.data.rich)
rich.models[[14]]<-lm(richness~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength, data=bat.data.rich)
rich.models[[15]]<-lm(richness~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet, data=bat.data.rich)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(rich.models), sep=" ")

##Generate AICc table
aictab(cand.set=rich.models, modnames=modelnames, sort=TRUE)

rich.tidy<-lapply(rich.models,tidy)
lapply(1:length(rich.tidy), function(i) write.csv(rich.tidy[[i]],
                                                  file= paste0("./Results/richness",i,".csv"),
                                                  row.names=FALSE))


#################################################################################
########################      LABO Presence      ################################
#################################################################################
##Set up variable for diversity
labo<-bat.data$LABOPres

##models
labo.models<-list()

##1) Stand-level 

labo.models[[1]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
labo.models[[2]]<-lm(labo~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
labo.models[[3]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
labo.models[[4]]<-lm(labo~vifCanMean*vifEntropy+vifRugosity, data=bat.data)

##3) Stand- and landscape-level

labo.models[[5]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
labo.models[[6]]<-lm(labo~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
labo.models[[7]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
labo.models[[8]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
labo.models[[9]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
labo.models[[10]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##3) Landscape-level

labo.models[[11]]<-lm(labo~vifLandHet)
labo.models[[12]]<-lm(labo~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
labo.models[[13]]<-lm(labo~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
labo.models[[14]]<-lm(labo~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
labo.models[[15]]<-lm(labo~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(labo.models), sep=" ")

##Generate AICc table
aictab(cand.set=labo.models, modnames=modelnames, sort=TRUE)

labo.tidy<-lapply(labo.models,tidy)
lapply(1:length(labo.tidy), function(i) write.csv(labo.tidy[[i]],
                                                  file= paste0("./Results/labo",i,".csv"),
                                                  row.names=FALSE))

#################################################################################
########################      LACI Presence      ################################
#################################################################################
##Set up variable for diversity
laci<-bat.data$LACIPres

##models
laci.models<-list()

##1) Stand-level 

laci.models[[1]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
laci.models[[2]]<-lm(laci~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
laci.models[[3]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
laci.models[[4]]<-lm(laci~vifCanMean*vifEntropy+vifRugosity, data=bat.data)

##2) Stand- and landscape-level

laci.models[[5]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
laci.models[[6]]<-lm(laci~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
laci.models[[7]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
laci.models[[8]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
laci.models[[9]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
laci.models[[10]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

laci.models[[11]]<-lm(laci~vifLandHet)
laci.models[[12]]<-lm(laci~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
laci.models[[13]]<-lm(laci~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
laci.models[[14]]<-lm(laci~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
laci.models[[15]]<-lm(laci~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(laci.models), sep=" ")

##Generate AICc table
aictab(cand.set=laci.models, modnames=modelnames, sort=TRUE)

laci.tidy<-lapply(laci.models,tidy)
lapply(1:length(laci.tidy), function(i) write.csv(laci.tidy[[i]],
                                                  file= paste0("./Results/laci",i,".csv"),
                                                  row.names=FALSE))


#################################################################################
########################      LAIN Presence      ################################
#################################################################################
##Set up variable for diversity
lain<-bat.data$LAINPres

##models
lain.models<-list()

##1) Stand-level 

lain.models[[1]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
lain.models[[2]]<-lm(lain~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
lain.models[[3]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
lain.models[[4]]<-lm(lain~vifCanMean*vifEntropy+vifRugosity, data=bat.data)

##2) Stand- and landscape-level

lain.models[[5]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
lain.models[[6]]<-lm(lain~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
lain.models[[7]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
lain.models[[8]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
lain.models[[9]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
lain.models[[10]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##3) Landscape-level
lain.models[[11]]<-lm(lain~vifLandHet)
lain.models[[12]]<-lm(lain~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
lain.models[[13]]<-lm(lain~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
lain.models[[14]]<-lm(lain~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
lain.models[[15]]<-lm(lain~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(lain.models), sep=" ")

##Generate AICc table
aictab(cand.set=lain.models, modnames=modelnames, sort=TRUE)

lain.tidy<-lapply(lain.models,tidy)
lapply(1:length(lain.tidy), function(i) write.csv(lain.tidy[[i]],
                                                  file= paste0("./Results/lain",i,".csv"),
                                                  row.names=FALSE))

#################################################################################
########################      MYAU Presence      ################################
#################################################################################
##Set up variable for diversity
myau<-bat.data$MYAUPres

##models
myau.models<-list()

##1) Stand-level 

myau.models[[1]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
myau.models[[2]]<-lm(myau~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
myau.models[[3]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
myau.models[[4]]<-lm(myau~vifCanMean*vifEntropy+vifRugosity, data=bat.data)

##2) Stand- and landscape-level

myau.models[[5]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
myau.models[[6]]<-lm(myau~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
myau.models[[7]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
myau.models[[8]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
myau.models[[9]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
myau.models[[10]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##3) Landscape-level

myau.models[[11]]<-lm(myau~vifLandHet)
myau.models[[12]]<-lm(myau~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
myau.models[[13]]<-lm(myau~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
myau.models[[14]]<-lm(myau~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
myau.models[[15]]<-lm(myau~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(myau.models), sep=" ")

##Generate AICc table
aictab(cand.set=myau.models, modnames=modelnames, sort=TRUE)

myau.tidy<-lapply(myau.models,tidy)
lapply(1:length(myau.tidy), function(i) write.csv(myau.tidy[[i]],
                                                  file= paste0("./Results/myau",i,".csv"),
                                                  row.names=FALSE))

#################################################################################
########################      NYHU Presence      ################################
#################################################################################
##Set up variable for diversity
nyhu<-bat.data$NYHUPres

##models
nyhu.models<-list()

##1) Stand-level

nyhu.models[[1]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
nyhu.models[[2]]<-lm(nyhu~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
nyhu.models[[3]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
nyhu.models[[4]]<-lm(nyhu~vifCanMean*vifEntropy+vifRugosity, data=bat.data)

##2) Stand- and landscape-level

nyhu.models[[5]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
nyhu.models[[6]]<-lm(nyhu~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
nyhu.models[[7]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
nyhu.models[[8]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
nyhu.models[[9]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
nyhu.models[[10]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##3) Landscape-level

nyhu.models[[11]]<-lm(nyhu~vifLandHet)
nyhu.models[[12]]<-lm(nyhu~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
nyhu.models[[13]]<-lm(nyhu~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
nyhu.models[[14]]<-lm(nyhu~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
nyhu.models[[15]]<-lm(nyhu~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(nyhu.models), sep=" ")

##Generate AICc table
aictab(cand.set=nyhu.models, modnames=modelnames, sort=TRUE)

nyhu.tidy<-lapply(nyhu.models,tidy)
lapply(1:length(nyhu.tidy), function(i) write.csv(nyhu.tidy[[i]],
                                                  file= paste0("./Results/nyhu",i,".csv"),
                                                  row.names=FALSE))

#################################################################################
########################      PESU Presence      ################################
#################################################################################
##Set up variable for diversity
pesu<-bat.data$PESUPres

##models
pesu.models<-list()

##1) Stand-level 

pesu.models[[1]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
pesu.models[[2]]<-lm(pesu~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
pesu.models[[3]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
pesu.models[[4]]<-lm(pesu~vifCanMean*vifEntropy+vifRugosity, data=bat.data)

##2) Stand- and landscape-level

pesu.models[[5]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
pesu.models[[6]]<-lm(pesu~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
pesu.models[[7]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
pesu.models[[8]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
pesu.models[[9]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
pesu.models[[10]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##3) Landscape-level

pesu.models[[11]]<-lm(pesu~vifLandHet)
pesu.models[[12]]<-lm(pesu~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
pesu.models[[13]]<-lm(pesu~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
pesu.models[[14]]<-lm(pesu~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
pesu.models[[15]]<-lm(pesu~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(pesu.models), sep=" ")

##Generate AICc table
aictab(cand.set=pesu.models, modnames=modelnames, sort=TRUE)

pesu.tidy<-lapply(pesu.models,tidy)
lapply(1:length(pesu.tidy), function(i) write.csv(pesu.tidy[[i]],
                                                  file= paste0("./Results/pesu",i,".csv"),
                                                  row.names=FALSE))


#################################################################################
########################      TABR Presence      ################################
#################################################################################
##Set up variable for diversity
tabr<-bat.data$TABRPres

##models
tabr.models<-list()

##1) Stand-level 

tabr.models[[1]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
tabr.models[[2]]<-lm(tabr~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
tabr.models[[3]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
tabr.models[[4]]<-lm(tabr~vifCanMean*vifEntropy+vifRugosity, data=bat.data)

##2) Stand- and landscape-level

tabr.models[[5]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
tabr.models[[6]]<-lm(tabr~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
tabr.models[[7]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
tabr.models[[8]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
tabr.models[[9]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
tabr.models[[10]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##3) Landscape-level

tabr.models[[11]]<-lm(tabr~vifLandHet)
tabr.models[[12]]<-lm(tabr~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
tabr.models[[13]]<-lm(tabr~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
tabr.models[[14]]<-lm(tabr~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
tabr.models[[15]]<-lm(tabr~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)

##Create a vector of names to trace back models in set
modelnames<-paste("Model", 1:length(tabr.models), sep=" ")

##Generate AICc table
aictab(cand.set=tabr.models, modnames=modelnames, sort=TRUE)

tabr.tidy<-lapply(tabr.models,tidy)
lapply(1:length(tabr.tidy), function(i) write.csv(tabr.tidy[[i]],
                                                  file= paste0("./Results/tabr",i,".csv"),
                                                  row.names=FALSE))
