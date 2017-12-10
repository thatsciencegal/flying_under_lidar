TSF<-subset(bat, TimeSinceFire<600)
table1<-table(TSF$TimeSinceFire)
table2<-table(TSF$TimeSinceFire, TSF$ShanBatDiv)
data015<-as.numeric(rownames(table2))
dataBatDiv<-as.numeric(colnames(table2))
d<-dim(table2)
plot(TSF$TimeSinceFire,TSF$ShanBatDiv,type="n")
for(i in 1:d[1]){
  for(j in 1:d[2]){
    if(table2[i,j]>0){
    points(data015[i],dataBatDiv[j],cex=table2[i,j]/2,pch=17, xlab='Time Since Fire (in months)', ylab='Bat Diversity')
  }
  }
}
?points