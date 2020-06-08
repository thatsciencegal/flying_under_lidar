library(dplyr)
library(caret)
library(broom)
library(corrplot)
library(BAS)
library(patchwork)

##Read in bat call data
bat.data<-read.csv("./Data/2020-01_Swanson-et-al_Bats-Forest-Str_Master-Datasheet.csv",header=T)

##Normalize variables
bat.data <- bat.data %>% mutate(TotalStd = (TotalID-mean(TotalID))/sd(TotalID),
                                CanMeanStd = (CanMean-mean(CanMean))/sd(CanMean),
                                EntropyStd = (Entropy-mean(Entropy))/sd(Entropy),
                                Prop015Std = (Prop015-mean(Prop015))/sd(Prop015),
                                Prop156Std = (Prop156-mean(Prop156))/sd(Prop156),
                                Prop612Std = (Prop612-mean(Prop612))/sd(Prop612),
                                AreaWaterStd = (AreaWater-mean(AreaWater))/sd(AreaWater),
                                RoadLengthStd = (RoadLength-mean(RoadLength))/sd(RoadLength),
                                LandHetStd = (LandHeterogeneity-mean(LandHeterogeneity))/sd(LandHeterogeneity),
                                PropUrbanStd = (PropUrban-mean(PropUrban))/sd(PropUrban))


##Model selection using BAS method
##Total acitivity model
total.bat<-bat.data %>% select(TotalStd,CanMeanStd,EntropyStd,Prop015Std,Prop156Std,Prop612Std,
                               AreaWaterStd,RoadLengthStd,LandHetStd,PropUrbanStd)

total.lm <- bas.lm(TotalStd~.,data=total.bat,prior="BIC",modelprior = uniform())
##Get coefficient estimates and credible intervals
total.coef<-coef(total.lm)
tot.confint<-confint(total.coef)
tot.ci <- as.data.frame(as.matrix(tot.confint[]))
tot.ci$postprob <- total.coef$probne0
##Put credible intervals into bins for graphing
tot.ci<-tibble::rownames_to_column(tot.ci,"Param") %>% 
  mutate(cond = case_when(postprob < 0.9 ~ "No",
                          postprob >=0.9 & postprob < 0.975 ~ "Maybe",
                          postprob >= 0.975 ~ "Yes"))
names(tot.ci)<-c("param","lower","upper","mu","postProb","cond")
tot.ci <- tot.ci[-1,]
tot.ci$param<-factor(lain.ci$param,levels=c("LandHetStd","PropUrbanStd","RoadLengthStd",
                                               "AreaWaterStd","Prop612Std","Prop156Std","Prop015Std",
                                               "EntropyStd","CanMeanStd"))

##Plot coefficient estimates and inclusion probabilities
tot.p<-ggplot(tot.ci)+
  geom_errorbar(aes(x=mu,y=param,xmin=lower,xmax=upper),width=.2, color="black")+
  geom_point(aes(x=mu,y=param, fill=cond),shape=21,colour="black",size=3)+
  geom_vline(xintercept=0,linetype=3)+
  scale_fill_manual(values=c("#fde725ff","white","#404788ff"))+
  theme(axis.title.x = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "white", color = "white"))+
  scale_y_discrete(labels=c("LandHet","PropUrban","RoadLength","AreaWater","Prop6-12",
                            "Prop1.5-6","Prop0-1.5","VertDiversity","MeanCanopy"))+
  xlim(-1.5,0.5)+
  xlab(label="")+
  ylab(label="")


##BAS for species richness
bat.data.rich <- bat.data %>% 
  mutate(richness = (EPFUPres+LANOPres+LABOPres+LACIPres+LAINPres+MYAUPres+
                     NYHUPres+PESUPres+TABRPres)) %>% 
  select(richness,CanMeanStd,EntropyStd,Prop015Std,Prop156Std,Prop612Std,AreaWaterStd,
         RoadLengthStd,PropUrbanStd,LandHetStd) 

rich.lm <- bas.lm(richness~.,data=bat.data.rich,prior="BIC",modelprior = uniform())
rich.coef <- coef(rich.lm)
rich.confint<-confint(rich.coef)
rich.ci <- as.data.frame(as.matrix(rich.confint[]))
rich.ci$postprob <- rich.coef$probne0
rich.ci<-tibble::rownames_to_column(rich.ci,"Param") %>% 
  mutate(cond = case_when(postprob < 0.9 ~ "No",
                          postprob >=0.9 & postprob < 0.975 ~ "Maybe",
                          postprob >= 0.975 ~ "Yes"))
names(rich.ci)<-c("param","lower","upper","mu","postProb","cond")
rich.ci <- rich.ci[-1,]
rich.ci$param<-factor(rich.ci$param,levels=c("LandHetStd","PropUrbanStd","RoadLengthStd",
                                               "AreaWaterStd","Prop612Std","Prop156Std","Prop015Std",
                                               "EntropyStd","CanMeanStd"))

rich.p<-ggplot(rich.ci)+
  geom_errorbar(aes(x=mu,y=param,xmin=lower,xmax=upper),width=.2)+
  geom_point(aes(x=mu,y=param, fill=cond),shape=21,colour="black",size=3)+
  geom_vline(xintercept = 0,linetype=3)+
  scale_fill_manual(values=c("white","#404788ff"))+
  theme(axis.title.x = element_blank(), 
        panel.background = element_rect(fill = "white", color = "white"))+
  scale_y_discrete(labels=c("LandHet","PropUrban","RoadLength","AreaWater","Prop6-12",
  "Prop1.5-6","Prop0-1.5","VertDiversity","MeanCanopy"))+
  xlim(-1.5,0.5)+
  xlab(label="")+
  ylab(label="")

tot.p/rich.p
ggsave("./Images/total_rich2.png",dpi=300)
##LABO BAS models
labo.dat <- bat.data %>% 
  select(LABOPres,CanMeanStd,EntropyStd,Prop015Std,Prop156Std,Prop612Std,AreaWaterStd,
         RoadLengthStd,PropUrbanStd,LandHetStd)

labo.lm <- bas.glm(LABOPres~.,data=labo.dat,family=binomial(link="logit"))
labo.coef<-coef(labo.lm)
labo.confint<-confint(labo.coef)
labo.ci <- as.data.frame(as.matrix(labo.confint[]))
labo.ci$postprob <- labo.coef$probne0
labo.ci<-tibble::rownames_to_column(labo.ci,"Param") %>% 
  mutate(cond = case_when(postprob < 0.9 ~ "No",
                          postprob >=0.9 & postprob < 0.975 ~ "Maybe",
                          postprob >= 0.975 ~ "Yes"))
names(labo.ci)<-c("param","lower","upper","mu","postProb","cond")
labo.ci <- labo.ci[-1,]
labo.ci$param<-factor(labo.ci$param,levels=c("LandHetStd","PropUrbanStd","RoadLengthStd",
                                               "AreaWaterStd","Prop612Std","Prop156Std","Prop015Std",
                                               "EntropyStd","CanMeanStd"))

labo.p<-ggplot(labo.ci)+
  geom_errorbar(aes(x=mu,y=param,xmin=lower,xmax=upper),width=.2)+
  geom_point(aes(x=mu,y=param, fill=cond),shape=21,colour="black",size=3)+
  geom_vline(xintercept = 0,linetype=3)+
  scale_fill_manual(values=c("white","#404788ff"))+
  theme(axis.title.x = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "white", color = "white"))+
  scale_y_discrete(labels=c("LandHet","PropUrban","RoadLength","AreaWater","Prop6-12",
        "Prop1.5-6","Prop0-1.5","VertDiversity","MeanCanopy"))+
  xlim(-6,3)+
  xlab(label="")+
  ylab(label="")


##LAIN Model
lain.dat <- bat.data %>% 
  select(LAINPres,CanMeanStd,EntropyStd,Prop015Std,Prop156Std,Prop612Std,AreaWaterStd,
         RoadLengthStd,PropUrbanStd,LandHetStd)
lain.lm <- bas.glm(LAINPres~.,data=lain.dat,family=binomial(link="logit"))
lain.coef<-coef(lain.lm)
lain.confint<-confint(lain.coef)
lain.ci <- as.data.frame(as.matrix(lain.confint[]))
lain.ci$postprob <- lain.coef$probne0
lain.ci<-tibble::rownames_to_column(lain.ci,"Param") %>% 
  mutate(cond = case_when(postprob < 0.9 ~ "No",
                          postprob >=0.9 & postprob < 0.975 ~ "Maybe",
                          postprob >= 0.975 ~ "Yes"))
names(lain.ci)<-c("param","lower","upper","mu","postProb","cond")
lain.ci <- lain.ci[-1,]
lain.ci$param<-factor(lain.ci$param,levels=c("LandHetStd","PropUrbanStd","RoadLengthStd",
                                           "AreaWaterStd","Prop612Std","Prop156Std","Prop015Std",
                                           "EntropyStd","CanMeanStd"))

lain.p<-ggplot(lain.ci)+
  geom_errorbar(aes(x=mu,y=param,xmin=lower,xmax=upper),width=.2, color="black")+
  geom_point(aes(x=mu,y=param, fill=cond),shape=21,colour="black",size=3)+
  geom_vline(xintercept=0,linetype=3)+
  scale_fill_manual(values=c("#fde725ff","white","#404788ff"))+
  theme(axis.title.x = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "white", color = "white"))+
  scale_y_discrete(labels=c("LandHet","PropUrban","RoadLength","AreaWater","Prop6-12",
                            "Prop1.5-6","Prop0-1.5","VertDiversity","MeanCanopy"))+
  xlim(-6,3)+
  xlab(label="")+
  ylab(label="")

labo.p/lain.p
ggsave("./Images/labo_lain2.png",dpi=300)

##MYAU model
myau.dat <- bat.data %>% 
  select(MYAUPres,CanMeanStd,EntropyStd,Prop015Std,Prop156Std,Prop612Std,AreaWaterStd,
         RoadLengthStd,PropUrbanStd,LandHetStd)
myau.lm <- bas.glm(MYAUPres~.,data=myau.dat,family=binomial(link="logit"))
myau.coef<-coef(myau.lm)
myau.confint<-confint(myau.coef)
myau.ci <- as.data.frame(as.matrix(myau.confint[]))
myau.ci$postprob <- myau.coef$probne0
myau.ci<-tibble::rownames_to_column(myau.ci,"Param") %>% 
  mutate(cond = case_when(postprob < 0.9 ~ "No",
                          postprob >=0.9 & postprob < 0.975 ~ "Maybe",
                          postprob >= 0.975 ~ "Yes"))
names(myau.ci)<-c("param","lower","upper","mu","postProb","cond")
myau.ci <- myau.ci[-1,]
myau.ci$param<-factor(myau.ci$param,levels=c("LandHetStd","PropUrbanStd","RoadLengthStd",
                                             "AreaWaterStd","Prop612Std","Prop156Std","Prop015Std",
                                             "EntropyStd","CanMeanStd"))

myau.p<-ggplot(myau.ci)+
  geom_errorbar(aes(x=mu,y=param,xmin=lower,xmax=upper),width=.2, color="black")+
  geom_point(aes(x=mu,y=param, fill=cond),shape=21,colour="black",size=3)+
  geom_vline(xintercept=0,linetype=3)+
  scale_fill_manual(values=c("white"))+
  theme(axis.title.x = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "white", color = "white"))+
  scale_y_discrete(labels=c("LandHet","PropUrban","RoadLength","AreaWater","Prop6-12",
                            "Prop1.5-6","Prop0-1.5","VertDiversity","MeanCanopy"))+
  xlim(-6,3)+
  xlab(label="")+
  ylab(label="")

##NYHU model
nyhu.dat <- bat.data %>% 
  select(NYHUPres,CanMeanStd,EntropyStd,Prop015Std,Prop156Std,Prop612Std,AreaWaterStd,
         RoadLengthStd,PropUrbanStd,LandHetStd)
nyhu.lm <- bas.glm(NYHUPres~.,data=nyhu.dat,family=binomial(link="logit"))
nyhu.coef<-coef(nyhu.lm)
nyhu.confint<-confint(nyhu.coef)
nyhu.ci <- as.data.frame(as.matrix(nyhu.confint[]))
nyhu.ci$postprob <- nyhu.coef$probne0
nyhu.ci<-tibble::rownames_to_column(nyhu.ci,"Param") %>% 
  mutate(cond = case_when(postprob < 0.9 ~ "No",
                          postprob >=0.9 & postprob < 0.975 ~ "Maybe",
                          postprob >= 0.975 ~ "Yes"))
names(nyhu.ci)<-c("param","lower","upper","mu","postProb","cond")
nyhu.ci <- nyhu.ci[-1,]
nyhu.ci$param<-factor(nyhu.ci$param,levels=c("LandHetStd","PropUrbanStd","RoadLengthStd",
                                             "AreaWaterStd","Prop612Std","Prop156Std","Prop015Std",
                                             "EntropyStd","CanMeanStd"))

nyhu.p<-ggplot(nyhu.ci)+
  geom_errorbar(aes(x=mu,y=param,xmin=lower,xmax=upper),width=.2, color="black")+
  geom_point(aes(x=mu,y=param, fill=cond),shape=21,colour="black",size=3)+
  geom_vline(xintercept=0,linetype=3)+
  scale_fill_manual(values=c("#fde725ff","white","#404788ff"))+
  theme(axis.title.x = element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "white", color = "white"))+
  scale_y_discrete(labels=c("LandHet","PropUrban","RoadLength","AreaWater","Prop6-12",
                            "Prop1.5-6","Prop0-1.5","VertDiversity","MeanCanopy"))+
  xlim(-6,3)+
  xlab(label="")+
  ylab(label="")

myau.p/nyhu.p
ggsave("./Images/myau_nyhu2.png",)

pesu.dat <- bat.data %>% 
  select(PESUPres,CanMeanStd,EntropyStd,Prop015Std,Prop156Std,Prop612Std,AreaWaterStd,
         RoadLengthStd,PropUrbanStd,LandHetStd)
pesu.lm <- bas.glm(PESUPres~.,data=pesu.dat,family=binomial(link="logit"))
pesu.coef <- coef(pesu.lm)
pesu.confint<-confint(pesu.coef)
pesu.ci <- as.data.frame(as.matrix(pesu.confint[]))
pesu.ci$postprob <- pesu.coef$probne0
pesu.ci<-tibble::rownames_to_column(pesu.ci,"Param") %>% 
  mutate(cond = case_when(postprob < 0.9 ~ "No",
                          postprob >=0.9 & postprob < 0.975 ~ "Maybe",
                          postprob >= 0.975 ~ "Yes"))
names(pesu.ci)<-c("param","lower","upper","mu","postProb","cond")
pesu.ci <- pesu.ci[-1,]
pesu.ci$param<-factor(pesu.ci$param,levels=c("LandHetStd","PropUrbanStd","RoadLengthStd",
                                             "AreaWaterStd","Prop612Std","Prop156Std","Prop015Std",
                                             "EntropyStd","CanMeanStd"))

pesu.p<-ggplot(pesu.ci)+
  geom_errorbar(aes(x=mu,y=param,xmin=lower,xmax=upper),width=.2, color="black")+
  geom_point(aes(x=mu,y=param, fill=cond),shape=21,colour="black",size=3)+
  geom_vline(xintercept=0,linetype=3)+
  scale_fill_manual(values=c("white","#404788ff"))+
  theme(axis.title.x = element_blank(), 
        panel.background = element_rect(fill = "white", color = "white"))+
  scale_y_discrete(labels=c("LandHet","PropUrban","RoadLength","AreaWater","Prop6-12",
                            "Prop1.5-6","Prop0-1.5","VertDiversity","MeanCanopy"))+
  xlim(-6,3)+
  xlab(label="")+
  ylab(label="")

###################################################################################
###########################      Confusion Matrix      ############################
###################################################################################
man.dat <- read.csv("./Data/2020-01_Swanson-et-al_Bats-Forest-Str_Manual-ID.csv")
for.dat <- bat.data %>% select(CanHeight,CanMean,Rugosity,Prop015,Prop156,Prop612,PropAb12,
                               Entropy,AreaWater,RoadLength,LandHeterogeneity,PropUrban)

for.cor <- cor(for.dat,method="pearson")
corrplot(for.cor)

names(man.dat)
##Add levels missing from the different datasets so you can do the confusion matrix
levels(man.dat$AUTOID)<-c(levels(man.dat$AUTOID),"EPTFUS")
levels(man.dat$MANUALID)<-c(levels(man.dat$MANUALID),"LASCIN")

##confusion matrix ignoring no ID calls and with no ID calls
man.dat.sub <- subset(man.dat, AUTOID != "NoID" & AUTOID != "NOISE")
confusionMatrix(man.dat.sub$AUTOID,man.dat.sub$MANUALID)
confusionMatrix(man.dat$AUTOID, man.dat$MANUALID)

######################################################################################
## The following code is from a previous version of the manuscript and was not used ##
## in final publication.                                                            ##
######################################################################################
###Calculate normalized variables
# vifCanMean<-bat.data$CanMean-mean(bat.data$CanMean)
# vifEntropy<-bat.data$Entropy-mean(bat.data$Entropy)
# vifRugosity<-bat.data$Rugosity-mean(bat.data$Rugosity)
# vifProp015<-bat.data$Prop015-mean(bat.data$Prop015)
# vifProp156<-bat.data$Prop156-mean(bat.data$Prop156)
# vifProp612<-bat.data$Prop612-mean(bat.data$Prop612)
# vifTimeSinceFire<-bat.data$TimeSinceFire-mean(bat.data$TimeSinceFire)
# vifPropUrban<-bat.data$PropUrban-mean(bat.data$PropUrban)
# vifAreaWater<-bat.data$AreaWater-mean(bat.data$AreaWater)
# vifRoadLength<-bat.data$RoadLength-mean(bat.data$RoadLength)
# vifLandHet<-bat.data$LandHeterogeneity-mean(bat.data$LandHeterogeneity)
# 
# ##models
# cand.models<-list()
# 
# ######################################
# #######    Abundance Models    #######
# ######################################
# 
# ##1) Stand-level models
# 
# cand.models[[1]]<-lm(Total~1, data=bat.data)
# cand.models[[2]]<-lm(Total~vifCanMean)
# cand.models[[1]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
# cand.models[[2]]<-lm(Total~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
# cand.models[[3]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
# cand.models[[4]]<-lm(Total~vifCanMean*vifEntropy+vifRugosity, data=bat.data)
# 
# ##2) Stand- and landscape-level models
# 
# cand.models[[5]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet, data=bat.data)
# cand.models[[6]]<-lm(Total~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet, data=bat.data)
# cand.models[[7]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet, data=bat.data)
# cand.models[[8]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength, data=bat.data)
# cand.models[[9]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength, data=bat.data)
# cand.models[[10]]<-lm(Total~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet, data=bat.data)
# 
# ##3) Landscape-level models
# cand.models[[11]]<-lm(Total~vifLandHet, data=bat.data)
# cand.models[[12]]<-lm(Total~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet, data=bat.data)
# cand.models[[13]]<-lm(Total~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength, data=bat.data)
# cand.models[[14]]<-lm(Total~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength, data=bat.data)
# cand.models[[15]]<-lm(Total~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet, data=bat.data)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(cand.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=cand.models, modnames=modelnames, sort=TRUE)
# 
# cand.tidy<-lapply(cand.models,tidy)
# lapply(1:length(cand.tidy), function(i) write.csv(cand.tidy[[i]],
#                                                   file= paste0("./Results/abundance",i,".csv"),
#                                                   row.names=FALSE))
# 
# ##Calculate richness
# bat.data.rich <- bat.data %>% 
#   mutate(richness = (EPFUPres+LANOPres+LABOPres+LACIPres+LAINPres+MYAUPres+
#                        NYHUPres+PESUPres+TABRPres))
# 
# ##Richness data
# 
# ##models
# rich.models<-list()
# 
# ##1) Stand-level models
# rich.models[[1]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data.rich)
# rich.models[[2]]<-lm(richness~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data.rich)
# rich.models[[3]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity, data=bat.data.rich)
# rich.models[[4]]<-lm(richness~vifCanMean*vifEntropy+vifRugosity, data=bat.data.rich)
# 
# ##2) Stand- and landscape-level models
# rich.models[[5]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet, data=bat.data.rich)
# rich.models[[6]]<-lm(richness~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet, data=bat.data.rich)
# rich.models[[7]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet, data=bat.data.rich)
# rich.models[[8]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength, data=bat.data.rich)
# rich.models[[9]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength, data=bat.data.rich)
# rich.models[[10]]<-lm(richness~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet, data=bat.data.rich)
# 
# ##3) Landscape-level models
# rich.models[[11]]<-lm(richness~vifLandHet, data=bat.data.rich)
# rich.models[[12]]<-lm(richness~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet, data=bat.data.rich)
# rich.models[[13]]<-lm(richness~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength, data=bat.data.rich)
# rich.models[[14]]<-lm(richness~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength, data=bat.data.rich)
# rich.models[[15]]<-lm(richness~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet, data=bat.data.rich)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(rich.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=rich.models, modnames=modelnames, sort=TRUE)
# 
# rich.tidy<-lapply(rich.models,tidy)
# lapply(1:length(rich.tidy), function(i) write.csv(rich.tidy[[i]],
#                                                   file= paste0("./Results/richness",i,".csv"),
#                                                   row.names=FALSE))
# 
# 
# #################################################################################
# ########################      LABO Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# labo<-bat.data$LABOPres
# 
# ##models
# labo.models<-list()
# 
# ##1) Stand-level 
# 
# labo.models[[1]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
# labo.models[[2]]<-lm(labo~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
# labo.models[[3]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
# labo.models[[4]]<-lm(labo~vifCanMean*vifEntropy+vifRugosity, data=bat.data)
# 
# ##3) Stand- and landscape-level
# 
# labo.models[[5]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# labo.models[[6]]<-lm(labo~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# labo.models[[7]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# labo.models[[8]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# labo.models[[9]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# labo.models[[10]]<-lm(labo~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##3) Landscape-level
# 
# labo.models[[11]]<-lm(labo~vifLandHet)
# labo.models[[12]]<-lm(labo~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# labo.models[[13]]<-lm(labo~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# labo.models[[14]]<-lm(labo~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# labo.models[[15]]<-lm(labo~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(labo.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=labo.models, modnames=modelnames, sort=TRUE)
# 
# labo.tidy<-lapply(labo.models,tidy)
# lapply(1:length(labo.tidy), function(i) write.csv(labo.tidy[[i]],
#                                                   file= paste0("./Results/labo",i,".csv"),
#                                                   row.names=FALSE))
# 
# #################################################################################
# ########################      LACI Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# laci<-bat.data$LACIPres
# 
# ##models
# laci.models<-list()
# 
# ##1) Stand-level 
# 
# laci.models[[1]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
# laci.models[[2]]<-lm(laci~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
# laci.models[[3]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
# laci.models[[4]]<-lm(laci~vifCanMean*vifEntropy+vifRugosity, data=bat.data)
# 
# ##2) Stand- and landscape-level
# 
# laci.models[[5]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# laci.models[[6]]<-lm(laci~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# laci.models[[7]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# laci.models[[8]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# laci.models[[9]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# laci.models[[10]]<-lm(laci~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# laci.models[[11]]<-lm(laci~vifLandHet)
# laci.models[[12]]<-lm(laci~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# laci.models[[13]]<-lm(laci~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# laci.models[[14]]<-lm(laci~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# laci.models[[15]]<-lm(laci~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(laci.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=laci.models, modnames=modelnames, sort=TRUE)
# 
# laci.tidy<-lapply(laci.models,tidy)
# lapply(1:length(laci.tidy), function(i) write.csv(laci.tidy[[i]],
#                                                   file= paste0("./Results/laci",i,".csv"),
#                                                   row.names=FALSE))
# 
# 
# #################################################################################
# ########################      LAIN Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# lain<-bat.data$LAINPres
# 
# ##models
# lain.models<-list()
# 
# ##1) Stand-level 
# 
# lain.models[[1]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
# lain.models[[2]]<-lm(lain~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
# lain.models[[3]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
# lain.models[[4]]<-lm(lain~vifCanMean*vifEntropy+vifRugosity, data=bat.data)
# 
# ##2) Stand- and landscape-level
# 
# lain.models[[5]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# lain.models[[6]]<-lm(lain~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# lain.models[[7]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# lain.models[[8]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# lain.models[[9]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# lain.models[[10]]<-lm(lain~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##3) Landscape-level
# lain.models[[11]]<-lm(lain~vifLandHet)
# lain.models[[12]]<-lm(lain~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# lain.models[[13]]<-lm(lain~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# lain.models[[14]]<-lm(lain~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# lain.models[[15]]<-lm(lain~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(lain.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=lain.models, modnames=modelnames, sort=TRUE)
# 
# lain.tidy<-lapply(lain.models,tidy)
# lapply(1:length(lain.tidy), function(i) write.csv(lain.tidy[[i]],
#                                                   file= paste0("./Results/lain",i,".csv"),
#                                                   row.names=FALSE))
# 
# #################################################################################
# ########################      MYAU Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# myau<-bat.data$MYAUPres
# 
# ##models
# myau.models<-list()
# 
# ##1) Stand-level 
# 
# myau.models[[1]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
# myau.models[[2]]<-lm(myau~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
# myau.models[[3]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
# myau.models[[4]]<-lm(myau~vifCanMean*vifEntropy+vifRugosity, data=bat.data)
# 
# ##2) Stand- and landscape-level
# 
# myau.models[[5]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# myau.models[[6]]<-lm(myau~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# myau.models[[7]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# myau.models[[8]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# myau.models[[9]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# myau.models[[10]]<-lm(myau~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##3) Landscape-level
# 
# myau.models[[11]]<-lm(myau~vifLandHet)
# myau.models[[12]]<-lm(myau~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# myau.models[[13]]<-lm(myau~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# myau.models[[14]]<-lm(myau~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# myau.models[[15]]<-lm(myau~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(myau.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=myau.models, modnames=modelnames, sort=TRUE)
# 
# myau.tidy<-lapply(myau.models,tidy)
# lapply(1:length(myau.tidy), function(i) write.csv(myau.tidy[[i]],
#                                                   file= paste0("./Results/myau",i,".csv"),
#                                                   row.names=FALSE))
# 
# #################################################################################
# ########################      NYHU Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# nyhu<-bat.data$NYHUPres
# 
# ##models
# nyhu.models<-list()
# 
# ##1) Stand-level
# 
# nyhu.models[[1]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
# nyhu.models[[2]]<-lm(nyhu~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
# nyhu.models[[3]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
# nyhu.models[[4]]<-lm(nyhu~vifCanMean*vifEntropy+vifRugosity, data=bat.data)
# 
# ##2) Stand- and landscape-level
# 
# nyhu.models[[5]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# nyhu.models[[6]]<-lm(nyhu~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# nyhu.models[[7]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# nyhu.models[[8]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# nyhu.models[[9]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# nyhu.models[[10]]<-lm(nyhu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##3) Landscape-level
# 
# nyhu.models[[11]]<-lm(nyhu~vifLandHet)
# nyhu.models[[12]]<-lm(nyhu~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# nyhu.models[[13]]<-lm(nyhu~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# nyhu.models[[14]]<-lm(nyhu~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# nyhu.models[[15]]<-lm(nyhu~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(nyhu.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=nyhu.models, modnames=modelnames, sort=TRUE)
# 
# nyhu.tidy<-lapply(nyhu.models,tidy)
# lapply(1:length(nyhu.tidy), function(i) write.csv(nyhu.tidy[[i]],
#                                                   file= paste0("./Results/nyhu",i,".csv"),
#                                                   row.names=FALSE))
# 
# #################################################################################
# ########################      PESU Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# pesu<-bat.data$PESUPres
# 
# ##models
# pesu.models<-list()
# 
# ##1) Stand-level 
# 
# pesu.models[[1]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
# pesu.models[[2]]<-lm(pesu~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
# pesu.models[[3]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
# pesu.models[[4]]<-lm(pesu~vifCanMean*vifEntropy+vifRugosity, data=bat.data)
# 
# ##2) Stand- and landscape-level
# 
# pesu.models[[5]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# pesu.models[[6]]<-lm(pesu~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# pesu.models[[7]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# pesu.models[[8]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# pesu.models[[9]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# pesu.models[[10]]<-lm(pesu~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##3) Landscape-level
# 
# pesu.models[[11]]<-lm(pesu~vifLandHet)
# pesu.models[[12]]<-lm(pesu~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# pesu.models[[13]]<-lm(pesu~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# pesu.models[[14]]<-lm(pesu~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# pesu.models[[15]]<-lm(pesu~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(pesu.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=pesu.models, modnames=modelnames, sort=TRUE)
# 
# pesu.tidy<-lapply(pesu.models,tidy)
# lapply(1:length(pesu.tidy), function(i) write.csv(pesu.tidy[[i]],
#                                                   file= paste0("./Results/pesu",i,".csv"),
#                                                   row.names=FALSE))
# 
# 
# #################################################################################
# ########################      TABR Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# tabr<-bat.data$TABRPres
# 
# ##models
# tabr.models<-list()
# 
# ##1) Stand-level 
# 
# tabr.models[[1]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612)
# tabr.models[[2]]<-lm(tabr~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612, data=bat.data)
# tabr.models[[3]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity, data=bat.data)
# tabr.models[[4]]<-lm(tabr~vifCanMean*vifEntropy+vifRugosity, data=bat.data)
# 
# ##2) Stand- and landscape-level
# 
# tabr.models[[5]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# tabr.models[[6]]<-lm(tabr~vifCanMean*vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifLandHet)
# tabr.models[[7]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# tabr.models[[8]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# tabr.models[[9]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# tabr.models[[10]]<-lm(tabr~vifCanMean+vifEntropy+vifRugosity+vifProp015+vifProp156+vifProp612+vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##3) Landscape-level
# 
# tabr.models[[11]]<-lm(tabr~vifLandHet)
# tabr.models[[12]]<-lm(tabr~vifPropUrban+vifAreaWater+vifRoadLength+vifLandHet)
# tabr.models[[13]]<-lm(tabr~vifPropUrban*vifLandHet+vifAreaWater+vifRoadLength)
# tabr.models[[14]]<-lm(tabr~vifPropUrban+vifAreaWater*vifLandHet+vifRoadLength)
# tabr.models[[15]]<-lm(tabr~vifPropUrban+vifAreaWater+vifRoadLength*vifLandHet)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(tabr.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=tabr.models, modnames=modelnames, sort=TRUE)
# 
# tabr.tidy<-lapply(tabr.models,tidy)
# lapply(1:length(tabr.tidy), function(i) write.csv(tabr.tidy[[i]],
#                                                   file= paste0("./Results/tabr",i,".csv"),
#                                                   row.names=FALSE))
# 
# cand.models<-list()
# 
# ######################################
# #######    Abundance Models    #######
# ######################################
# 
# ##1) Stand-level models
# 
# cand.models[[1]]<-lm(Total~1, data=bat.data)
# cand.models[[2]]<-lm(Total~CanMean, data=bat.data)
# cand.models[[3]]<-lm(Total~Rugosity, data=bat.data)
# cand.models[[4]]<-lm(Total~Entropy, data=bat.data)
# cand.models[[5]]<-lm(Total~Prop015, data=bat.data)
# cand.models[[6]]<-lm(Total~Prop156, data=bat.data)
# cand.models[[7]]<-lm(Total~Prop612, data=bat.data)
# cand.models[[8]]<-lm(Total~CanMean+Entropy, data=bat.data)
# cand.models[[9]]<-lm(Total~CanMean+Prop156+Prop612, data=bat.data)
# cand.models[[10]]<-lm(Total~CanMean+Entropy+Prop612, data=bat.data)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(cand.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=cand.models, modnames=modelnames, sort=TRUE)
# 
# cand.tidy<-lapply(cand.models,tidy)
# lapply(1:length(cand.tidy), function(i) write.csv(cand.tidy[[i]],
#                                                   file= paste0("./Results/abundance2",i,".csv"),
#                                                   row.names=FALSE))
# 
# ##Calculate richness
# bat.data.rich <- bat.data %>% 
#   mutate(richness = (EPFUPres+LANOPres+LABOPres+LACIPres+LAINPres+MYAUPres+
#                        NYHUPres+PESUPres+TABRPres))
# 
# ##Richness data
# 
# ##models
# rich.models<-list()
# 
# rich.models[[1]]<-lm(richness~1, data=bat.data.rich)
# rich.models[[2]]<-lm(richness~CanMean, data=bat.data.rich)
# rich.models[[3]]<-lm(richness~Rugosity, data=bat.data.rich)
# rich.models[[4]]<-lm(richness~Entropy, data=bat.data.rich)
# rich.models[[5]]<-lm(richness~Prop015, data=bat.data.rich)
# rich.models[[6]]<-lm(richness~Prop156, data=bat.data.rich)
# rich.models[[7]]<-lm(richness~Prop612, data=bat.data.rich)
# rich.models[[8]]<-lm(richness~CanMean+Entropy, data=bat.data.rich)
# rich.models[[9]]<-lm(richness~CanMean+Prop156+Prop612, data=bat.data.rich)
# rich.models[[10]]<-lm(richness~CanMean+Entropy+Prop612, data=bat.data.rich)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(rich.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=rich.models, modnames=modelnames, sort=TRUE)
# 
# rich.tidy<-lapply(rich.models,tidy)
# lapply(1:length(rich.tidy), function(i) write.csv(rich.tidy[[i]],
#                                                   file= paste0("./Results/richness2",i,".csv"),
#                                                   row.names=FALSE))
# 
# 
# #################################################################################
# ########################      LABO Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# labo<-bat.data$LABOPres
# 
# ##models
# labo.models<-list()
# 
# labo.models[[1]]<-lm(labo~1, data=bat.data.rich)
# labo.models[[2]]<-lm(labo~CanMean, data=bat.data.rich)
# labo.models[[3]]<-lm(labo~Rugosity, data=bat.data.rich)
# labo.models[[4]]<-lm(labo~Entropy, data=bat.data.rich)
# labo.models[[5]]<-lm(labo~Prop015, data=bat.data.rich)
# labo.models[[6]]<-lm(labo~Prop156, data=bat.data.rich)
# labo.models[[7]]<-lm(labo~Prop612, data=bat.data.rich)
# labo.models[[8]]<-lm(labo~CanMean+Entropy, data=bat.data.rich)
# labo.models[[9]]<-lm(labo~CanMean+Prop156+Prop612, data=bat.data.rich)
# labo.models[[10]]<-lm(labo~CanMean+Entropy+Prop612, data=bat.data.rich)
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(labo.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=labo.models, modnames=modelnames, sort=TRUE)
# 
# labo.tidy<-lapply(labo.models,tidy)
# lapply(1:length(labo.tidy), function(i) write.csv(labo.tidy[[i]],
#                                                   file= paste0("./Results/labo2",i,".csv"),
#                                                   row.names=FALSE))
# 
# #################################################################################
# ########################      LACI Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# laci<-bat.data$LACIPres
# 
# ##models
# laci.models<-list()
# 
# laci.models[[1]]<-lm(laci~1, data=bat.data.rich)
# laci.models[[2]]<-lm(laci~CanMean, data=bat.data.rich)
# laci.models[[3]]<-lm(laci~Rugosity, data=bat.data.rich)
# laci.models[[4]]<-lm(laci~Entropy, data=bat.data.rich)
# laci.models[[5]]<-lm(laci~Prop015, data=bat.data.rich)
# laci.models[[6]]<-lm(laci~Prop156, data=bat.data.rich)
# laci.models[[7]]<-lm(laci~Prop612, data=bat.data.rich)
# laci.models[[8]]<-lm(laci~CanMean+Entropy, data=bat.data.rich)
# laci.models[[9]]<-lm(laci~CanMean+Prop156+Prop612, data=bat.data.rich)
# laci.models[[10]]<-lm(laci~CanMean+Entropy+Prop612, data=bat.data.rich)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(laci.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=laci.models, modnames=modelnames, sort=TRUE)
# 
# laci.tidy<-lapply(laci.models,tidy)
# lapply(1:length(laci.tidy), function(i) write.csv(laci.tidy[[i]],
#                                                   file= paste0("./Results/laci2",i,".csv"),
#                                                   row.names=FALSE))
# 
# 
# #################################################################################
# ########################      LAIN Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# lain<-bat.data$LAINPres
# 
# ##models
# lain.models<-list()
# 
# lain.models[[1]]<-lm(lain~1, data=bat.data.rich)
# lain.models[[2]]<-lm(lain~CanMean, data=bat.data.rich)
# lain.models[[3]]<-lm(lain~Rugosity, data=bat.data.rich)
# lain.models[[4]]<-lm(lain~Entropy, data=bat.data.rich)
# lain.models[[5]]<-lm(lain~Prop015, data=bat.data.rich)
# lain.models[[6]]<-lm(lain~Prop156, data=bat.data.rich)
# lain.models[[7]]<-lm(lain~Prop612, data=bat.data.rich)
# lain.models[[8]]<-lm(lain~CanMean+Entropy, data=bat.data.rich)
# lain.models[[9]]<-lm(lain~CanMean+Prop156+Prop612, data=bat.data.rich)
# lain.models[[10]]<-lm(lain~CanMean+Entropy+Prop612, data=bat.data.rich)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(lain.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=lain.models, modnames=modelnames, sort=TRUE)
# 
# lain.tidy<-lapply(lain.models,tidy)
# lapply(1:length(lain.tidy), function(i) write.csv(lain.tidy[[i]],
#                                                   file= paste0("./Results/lain2",i,".csv"),
#                                                   row.names=FALSE))
# 
# #################################################################################
# ########################      MYAU Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# myau<-bat.data$MYAUPres
# 
# ##models
# myau.models<-list()
# 
# myau.models[[1]]<-lm(myau~1, data=bat.data.rich)
# myau.models[[2]]<-lm(myau~CanMean, data=bat.data.rich)
# myau.models[[3]]<-lm(myau~Rugosity, data=bat.data.rich)
# myau.models[[4]]<-lm(myau~Entropy, data=bat.data.rich)
# myau.models[[5]]<-lm(myau~Prop015, data=bat.data.rich)
# myau.models[[6]]<-lm(myau~Prop156, data=bat.data.rich)
# myau.models[[7]]<-lm(myau~Prop612, data=bat.data.rich)
# myau.models[[8]]<-lm(myau~CanMean+Entropy, data=bat.data.rich)
# myau.models[[9]]<-lm(myau~CanMean+Prop156+Prop612, data=bat.data.rich)
# myau.models[[10]]<-lm(myau~CanMean+Entropy+Prop612, data=bat.data.rich)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(myau.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=myau.models, modnames=modelnames, sort=TRUE)
# 
# myau.tidy<-lapply(myau.models,tidy)
# lapply(1:length(myau.tidy), function(i) write.csv(myau.tidy[[i]],
#                                                   file= paste0("./Results/myau2",i,".csv"),
#                                                   row.names=FALSE))
# 
# #################################################################################
# ########################      NYHU Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# nyhu<-bat.data$NYHUPres
# 
# ##models
# nyhu.models<-list()
# 
# nyhu.models[[1]]<-lm(nyhu~1, data=bat.data.rich)
# nyhu.models[[2]]<-lm(nyhu~CanMean, data=bat.data.rich)
# nyhu.models[[3]]<-lm(nyhu~Rugosity, data=bat.data.rich)
# nyhu.models[[4]]<-lm(nyhu~Entropy, data=bat.data.rich)
# nyhu.models[[5]]<-lm(nyhu~Prop015, data=bat.data.rich)
# nyhu.models[[6]]<-lm(nyhu~Prop156, data=bat.data.rich)
# nyhu.models[[7]]<-lm(nyhu~Prop612, data=bat.data.rich)
# nyhu.models[[8]]<-lm(nyhu~CanMean+Entropy, data=bat.data.rich)
# nyhu.models[[9]]<-lm(nyhu~CanMean+Prop156+Prop612, data=bat.data.rich)
# nyhu.models[[10]]<-lm(nyhu~CanMean+Entropy+Prop612, data=bat.data.rich)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(nyhu.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=nyhu.models, modnames=modelnames, sort=TRUE)
# 
# nyhu.tidy<-lapply(nyhu.models,tidy)
# lapply(1:length(nyhu.tidy), function(i) write.csv(nyhu.tidy[[i]],
#                                                   file= paste0("./Results/nyhu2",i,".csv"),
#                                                   row.names=FALSE))
# 
# #################################################################################
# ########################      PESU Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# pesu<-bat.data$PESUPres
# 
# ##models
# pesu.models<-list()
# 
# pesu.models[[1]]<-lm(pesu~1, data=bat.data.rich)
# pesu.models[[2]]<-lm(pesu~CanMean, data=bat.data.rich)
# pesu.models[[3]]<-lm(pesu~Rugosity, data=bat.data.rich)
# pesu.models[[4]]<-lm(pesu~Entropy, data=bat.data.rich)
# pesu.models[[5]]<-lm(pesu~Prop015, data=bat.data.rich)
# pesu.models[[6]]<-lm(pesu~Prop156, data=bat.data.rich)
# pesu.models[[7]]<-lm(pesu~Prop612, data=bat.data.rich)
# pesu.models[[8]]<-lm(pesu~CanMean+Entropy, data=bat.data.rich)
# pesu.models[[9]]<-lm(pesu~CanMean+Prop156+Prop612, data=bat.data.rich)
# pesu.models[[10]]<-lm(pesu~CanMean+Entropy+Prop612, data=bat.data.rich)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(pesu.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=pesu.models, modnames=modelnames, sort=TRUE)
# 
# pesu.tidy<-lapply(pesu.models,tidy)
# lapply(1:length(pesu.tidy), function(i) write.csv(pesu.tidy[[i]],
#                                                   file= paste0("./Results/pesu2",i,".csv"),
#                                                   row.names=FALSE))
# 
# 
# #################################################################################
# ########################      TABR Presence      ################################
# #################################################################################
# ##Set up variable for diversity
# tabr<-bat.data$TABRPres
# 
# ##models
# tabr.models<-list()
# 
# tabr.models[[1]]<-lm(tabr~1, data=bat.data.rich)
# tabr.models[[2]]<-lm(tabr~CanMean, data=bat.data.rich)
# tabr.models[[3]]<-lm(tabr~Rugosity, data=bat.data.rich)
# tabr.models[[4]]<-lm(tabr~Entropy, data=bat.data.rich)
# tabr.models[[5]]<-lm(tabr~Prop015, data=bat.data.rich)
# tabr.models[[6]]<-lm(tabr~Prop156, data=bat.data.rich)
# tabr.models[[7]]<-lm(tabr~Prop612, data=bat.data.rich)
# tabr.models[[8]]<-lm(tabr~CanMean+Entropy, data=bat.data.rich)
# tabr.models[[9]]<-lm(tabr~CanMean+Prop156+Prop612, data=bat.data.rich)
# tabr.models[[10]]<-lm(tabr~CanMean+Entropy+Prop612, data=bat.data.rich)
# 
# ##Create a vector of names to trace back models in set
# modelnames<-paste("Model", 1:length(tabr.models), sep=" ")
# 
# ##Generate AICc table
# aictab(cand.set=tabr.models, modnames=modelnames, sort=TRUE)
# 
# tabr.tidy<-lapply(tabr.models,tidy)
# lapply(1:length(tabr.tidy), function(i) write.csv(tabr.tidy[[i]],
#                                                   file= paste0("./Results/tabr",i,".csv"),
#                                                   row.names=FALSE))