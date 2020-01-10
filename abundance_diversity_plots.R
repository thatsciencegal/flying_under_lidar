library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(raster)

#Read in bat call data
bat.data <- read.csv("./Data/2020-01_Swanson-et-al_Bats-Forest-Str_Master-Datasheet.csv",header=T)

#Box plot of bat activity per cluster
ggplot(bat.data, aes(x = factor(ï..Cluster), y = TotalID)) +
  geom_boxplot(aes(group = ï..Cluster)) +
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black")) + 
  ylim(0,125) +
  xlab("Cluster") +
  ylab("Bat Activity")

#Box plot of species richness per cluster
ggplot(bat.data, aes(x = factor(ï..Cluster), y = SpeciesNumber)) +
  geom_boxplot(aes(group = ï..Cluster)) +
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black")) +
  xlab("Cluster") +
  ylab("Species Richness")

#Change to long format grouping total species detected in each cluster
bats <- bat.data %>% dplyr::select(ï..Cluster, LABO, LACI, LAIN, MYAU, NYHU, PESU, TABR,NoID) %>% 
  group_by(ï..Cluster) %>% summarise(LABO=sum(LABO, na.rm=TRUE), LACI=sum(LACI,na.rm=TRUE),
                                     LAIN=sum(LAIN,na.rm=TRUE), MYAU=sum(MYAU,na.rm=TRUE),
                                     NYHU=sum(NYHU,na.rm=TRUE), PESU=sum(PESU,na.rm=TRUE),
                                     TABR=sum(TABR,na.rm=TRUE))%>% 
  gather(key="bats", value="total", LABO,LACI,LAIN,MYAU,NYHU,PESU,TABR)

#Rename columns
names(bats)<- c("Cluster","Species","Total")

#Change species into factor levels
bats$Species <- factor(bats$Species, c("LABO","LACI","LAIN","MYAU","NYHU","PESU","TABR"))

#Plot total number of species within each cluster
ggplot(bats, aes(x=Cluster,y=Total,fill=Species))+
  geom_bar(stat="identity")+
  theme(axis.title = element_text(size = 16), panel.background = element_rect(fill = "white", color = "black"))+
  scale_fill_manual(values=c("#440154FF","#FDE725FF","#238A8DFF","#404788FF","#55C667FF", "#A9A9A9", "#000000" ))+
  scale_x_continuous("Cluster", labels=as.character(bats$Cluster), breaks=bats$Cluster)

veg<-readOGR("E:/Thesis/osbs_veg_bound", layer="os_fnai_veg14")
plot(veg)
clusts<-readOGR("E:/Project_Home/Products/GIS Layers",layer="OSclusters")
newcrs<-crs(clusts)
veg<-spTransform(veg,newcrs)
veg_clust<-clusts%over%veg

clusts@data$veg<-veg_clust$DESCRIPTIO
veg.pts<-clusts@data 
veg.pts$veg<-as.factor(veg.pts$veg)
veg.clust<-veg.pts %>% dplyr::select(Cluster6,veg) %>% plyr::count() 
veg.freq<-veg.clust %>% dplyr::group_by(Cluster6) %>% dplyr::mutate(percent=(freq/sum(freq))*100)

aspect<-read.csv("E:/Thesis/Data/LoadingAspectRatio.csv")
ggplot(aspect, aes(x=AspectRatio, y=Wingloading, label=Spp))+
  geom_point()+
  theme(axis.title=element_text(size=16), panel.background=element_rect(fill="white", color="black"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  geom_text(aes(label=Spp), hjust=0.5, vjust=1.3)

