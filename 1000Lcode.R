rm(list = ls())

library(lattice)
library(latticeExtra)
library(readxl)
library(ggplot2)
library(scales)
library(rJava)
library(ReporteRs)
library(psych)
library(grid)
library(gridExtra)
library(car)

##In depth for 1000L##
setwd("C:/Users/Pratyay.Sengupta/Documents/RLS/In depth analysis/")

d1 <- as.data.frame(read_excel("Master_data_upstream_29082017.xlsx",sheet=1))
d1000<-subset(d1,d1$Scale=='1000')
colnames(d1000)
library(rsm)
d1000<-d1000[,c(1:3,23,29,48,146,114,97,157,187,189,195,196)]
colnames(d1000)
rsm10<-rsm(Yield~SO(averagepHbeforetemp.shift,AverageDO2,Temp.shiftloghour),data=d1000)
summary(rsm10)
rsm11<-rsm(Yield~FO(SeedtransferVCC1000L,Max.IVCC1000Lproduction,Avg.osmo1000L)+TWI(SeedtransferVCC1000L,Max.IVCC1000Lproduction,Avg.osmo1000L),data=d1000)
summary(rsm11)
rsm11a<-rsm(Yield~SO(Dextroselevel),data=d1000)
summary(rsm11a)
anovamodel1<-aov(Yield~Dextroselevel,data=d1000)
summary(anovamodel1)
##For building functions with culture supernatant##
rsm33<-rsm(CultureSupernatant~FO(averagepHbeforetemp.shift,AverageDO2,Temp.shiftloghour)+
                              TWI(formula = ~ averagepHbeforetemp.shift*AverageDO2+AverageDO2*Temp.shiftloghour)+
                              PQ(averagepHbeforetemp.shift,AverageDO2,Temp.shiftloghour),data=d1000)
summary(rsm33)
rsm34<-rsm(CultureSupernatant~FO(SeedtransferVCC1000L,Max.IVCC1000Lproduction,Avg.osmo1000L)+
                              TWI(SeedtransferVCC1000L,Max.IVCC1000Lproduction,Avg.osmo1000L)+
                              PQ(Max.IVCC1000Lproduction,Avg.osmo1000L),data=d1000)
summary(rsm34)
rsm35<-rsm(CultureSupernatant~SO(Dextroselevel),data=d1000)
summary(rsm35)
anovamodel2<-aov(CultureSupernatant~Dextroselevel,data=d1000)
summary(anovamodel2)
rsm37 <- lm(FilteredSupernetant ~ CultureSupernatant -1,data = d1000)
summary(rsm37)
##For seed batches##
d1 <- as.data.frame(read_excel("Master_data_upstream_29082017.xlsx",sheet=1))
d1000<-subset(d1,d1$Scale=='1000')
colnames(d1000)
d160<-d1000[,c(1:3,145,37,44,154,146)]
colnames(d160)
rsm12<-rsm(SeedtransferVCC1000L~SO(SeedtransferVCC160L,Max.IVCC160Lseed),data=d160)
summary(rsm12)
rsm13<-rsm(SeedtransferVCC1000L~SO(Avg.pH160Lseed,Avg.DO2160Lseed),data=d160)
summary(rsm13)

d40<-d1000[,c(1:3,36,43,143,153,145)]
colnames(d40)
rsm14<-rsm(SeedtransferVCC160L~SO(SeedtransferVCC40L,Max.IVCC40Lseed),data=d40)
summary(rsm14)
rsm15<-rsm(SeedtransferVCC160L~SO(Avg.pH40Lseed,Avg.DO240Lseed),data=d40)
summary(rsm15)

d10<-d1000[,c(1:3,35,42,142,152,143)]
colnames(d10)
rsm16<-rsm(SeedtransferVCC40L~SO(SeedtransferVCC10L,Max.IVCC10Lseed),data=d10)
summary(rsm16)
rsm17<-rsm(SeedtransferVCC40L~SO(Avg.pH10Lseed,Avg.DO210Lseed),data=d10)
summary(rsm17)

##Inoc parameters##
d1 <- as.data.frame(read_excel("Master_data_upstream_29082017_1.xlsx",sheet=1))
d1000<-subset(d1,d1$Scale=='1000')
colnames(d1000)
dinoc<-d1000[,c(1:9,145)]
colnames(dinoc)
rsm18<-rsm(SeedtransferVCC10L~FO(Vialviablecellcount,EndVCCsubculture1,EndVCCsubculture2,EndVCCsubculture3)+TWI(Vialviablecellcount,EndVCCsubculture1,EndVCCsubculture2,EndVCCsubculture3),data=dinoc)
summary(rsm18)
#rsm19<-rsm(SeedtransferVCC10L~FO(EndVCCsubculture2,EndVCCsubculture3)+TWI(EndVCCsubculture2,EndVCCsubculture3),data=dinoc)
#summary(rsm19)

##Media parameters##
d1 <- as.data.frame(read_excel("Master_data_upstream_29082017.xlsx",sheet=1))
d1000<-subset(d1,d1$Scale=='1000')
d1000media<-d1000[,c(1:3,28,115,85,187,190,195,196)]
colnames(d1000media)
rsm20<-rsm(Yield~FO(mediaholdpH1000L,Mediaosmolalityproduction,CO21000medialevel)+TWI(mediaholdpH1000L,Mediaosmolalityproduction,CO21000medialevel),data=d1000media)
summary(rsm20)
d160media<-d1000[,c(1:3,11,17,27,84,187)]
rsm24<-rsm(Yield~FO(mediaholdpH160L,Mediaduration160L,Mediahold160L)+TWI(mediaholdpH160L,Mediaduration160L,Mediahold160L),data=d160media)
summary(rsm24)
##For building functions with culture supernanatant##
rsm36<-rsm(CultureSupernatant~FO(mediaholdpH1000L,Mediaosmolalityproduction,CO21000medialevel)+
                              TWI(mediaholdpH1000L,Mediaosmolalityproduction,CO21000medialevel)+
                              PQ(mediaholdpH1000L),data=d1000media)
summary(rsm36)
##Contour plots##
contour(rsm10,~averagepHbeforetemp.shift+AverageDO2+Temp.shiftloghour,image=TRUE,at = summary(rsm10)$canonical$xs)
#jpeg(filename="c1.jpeg")
#dev.off()
contour(rsm11,~SeedtransferVCC1000L+Max.IVCC1000Lproduction+Avg.osmo1000L,image=TRUE,at = summary(rsm11)$canonical$xs)
contour(rsm11a,~Dextroselevel,image=TRUE,at = summary(rsm11a)$canonical$xs)
contour(rsm12,~SeedtransferVCC160L+Max.IVCC160Lseed,image=TRUE,at = summary(rsm12)$canonical$xs)
contour(rsm13,~Avg.pH160Lseed+Avg.DO2160Lseed,image=TRUE,at = summary(rsm13)$canonical$xs)
contour(rsm14,~SeedtransferVCC40L+Max.IVCC40Lseed,image=TRUE,at = summary(rsm14)$canonical$xs)
contour(rsm15,~Avg.pH40Lseed+Avg.DO240Lseed,image=TRUE,at = summary(rsm15)$canonical$xs)
contour(rsm16,~SeedtransferVCC10L+Max.IVCC10Lseed,image=TRUE,at = summary(rsm16)$canonical$xs)
contour(rsm17,~Avg.pH10Lseed+Avg.DO210Lseed,image=TRUE,at = summary(rsm17)$canonical$xs)
contour(rsm18,~Vialviablecellcount+EndVCCsubculture1+EndVCCsubculture2+EndVCCsubculture3,image=TRUE,at = summary(rsm18)$canonical$xs)
contour(rsm20,~mediaholdpH1000L+Mediaosmolalityproduction+CO21000medialevel,image=TRUE,at = summary(rsm20)$canonical$xs)


##Writing the model coefficients in csv format##
write.csv(rsm10$coefficients,"coefficients1.csv")
write.csv(rsm11$coefficients,"coefficients2.csv")
write.csv(rsm11a$coefficients,"coefficients3.csv")
write.csv(rsm12$coefficients,"coefficients4.csv")
write.csv(rsm13$coefficients,"coefficients5.csv")
write.csv(rsm14$coefficients,"coefficients6.csv")
write.csv(rsm15$coefficients,"coefficients7.csv")
write.csv(rsm16$coefficients,"coefficients8.csv")
write.csv(rsm17$coefficients,"coefficients9.csv")
write.csv(rsm18$coefficients,"coefficients10.csv")
write.csv(rsm19$coefficients,"coefficients11.csv")
write.csv(rsm20$coefficients,"coefficients12.csv")

##For optimizing rate of change of VCC w.r.t.VCC and IVCC##
drate<-as.data.frame(read_excel("Upstream VCC.xlsx",sheet=1))
drate1000<-subset(drate,drate$Batch.Capacity=='1000')
drate1000<-na.omit(drate1000)
rsm21<-rsm(dVCC/dt~SO(VCC,IVCC),data=drate1000)
summary(rsm21)
contour(rsm21,~VCC+IVCC,image=TRUE,at = summary(rsm21)$canonical$xs)
drate160to1000<-subset(drate,drate$Train=='160->1000')
drate160to1000<-na.omit(drate160to1000)
rsm22<-rsm(dVCC/dt~SO(VCC),data=drate160to1000)
summary(rsm22)
steepest(rsm22)
drate40to160<-subset(drate,drate$Train=='40->160')
drate40to160<-na.omit(drate40to160)
rsm23<-rsm(dVCC/dt~SO(VCC),data=drate40to160)
summary(rsm23)
drate10to40<-subset(drate,drate$Train=='10->40')
drate10to40<-na.omit(drate10to40)
rsm23a<-rsm(dVCC/dt~SO(VCC),data=drate10to40)
summary(rsm23a)
###Partial Correlation checks###
library(ppcor)
j=NULL
for(i in c(4:10,12)){
  #plot(upd[,c(i,40)],main = paste(colnames(upd)[i],cor(upd[,c(i,40)],use = "pairwise.complete.obs")[[2]],sep = " --->>> "))
  print(paste(colnames(d1000)[i],pcor(d1000[,c(i,11)])[[2]],sep = " --->>> "))
  j <- c(j,ifelse(abs(pcor(d1000[,c(i,11)])[[2]]) >= 0.3,i,NA))
}
j <- j[!is.na(j)]
for(k in j){
  plot(d1000[,c(k,11)],main = paste(colnames(d1000)[k],pcor(d1000[,c(k,11)])[[2]],sep = " --->>> "))
}

library(ppcor)
j=NULL
for(i in c(4:6,8)){
  #plot(upd[,c(i,40)],main = paste(colnames(upd)[i],cor(upd[,c(i,40)],use = "pairwise.complete.obs")[[2]],sep = " --->>> "))
  print(paste(colnames(d1000media)[i],pcor(d1000media[,c(i,7)])[[2]],sep = " --->>> "))
  j <- c(j,ifelse(abs(pcor(d1000media[,c(i,7)])[[2]]) >= 0.3,i,NA))
}
j <- j[!is.na(j)]
for(k in j){
  plot(d1000media[,c(k,11)],main = paste(colnames(d1000media)[k],pcor(d1000media[,c(k,11)])[[2]],sep = " --->>> "))
}

library(ppcor)
j=NULL
for(i in c(4:8)){
  #plot(upd[,c(i,40)],main = paste(colnames(upd)[i],cor(upd[,c(i,40)],use = "pairwise.complete.obs")[[2]],sep = " --->>> "))
  print(paste(colnames(dinoc)[i],pcor(dinoc[,c(i,10)])[[2]],sep = " --->>> "))
  j <- c(j,ifelse(abs(pcor(dinoc[,c(i,10)])[[2]]) >= 0.3,i,NA))
}
j <- j[!is.na(j)]
for(k in j){
  plot(dinoc[,c(k,10)],main = paste(colnames(dinoc)[k],pcor(dinoc[,c(k,10)])[[2]],sep = " --->>> "))
}

library(ppcor)
j=NULL
for(i in c(4:7)){
  #plot(upd[,c(i,40)],main = paste(colnames(upd)[i],cor(upd[,c(i,40)],use = "pairwise.complete.obs")[[2]],sep = " --->>> "))
  print(paste(colnames(d10)[i],pcor(d10[,c(i,8)])[[2]],sep = " --->>> "))
  j <- c(j,ifelse(abs(pcor(d10[,c(i,8)])[[2]]) >= 0.3,i,NA))
}
j <- j[!is.na(j)]
for(k in j){
  plot(d10[,c(k,8)],main = paste(colnames(d10)[k],pcor(d1000[,c(k,8)])[[2]],sep = " --->>> "))
}

library(ppcor)
j=NULL
for(i in c(4:7)){
  #plot(upd[,c(i,40)],main = paste(colnames(upd)[i],cor(upd[,c(i,40)],use = "pairwise.complete.obs")[[2]],sep = " --->>> "))
  print(paste(colnames(d40)[i],pcor(d40[,c(i,8)])[[2]],sep = " --->>> "))
  j <- c(j,ifelse(abs(pcor(d40[,c(i,8)])[[2]]) >= 0.3,i,NA))
}
j <- j[!is.na(j)]
for(k in j){
  plot(d40[,c(k,8)],main = paste(colnames(d40)[k],pcor(d40[,c(k,8)])[[2]],sep = " --->>> "))
}

library(ppcor)
j=NULL
for(i in c(4:7)){
  #plot(upd[,c(i,40)],main = paste(colnames(upd)[i],cor(upd[,c(i,40)],use = "pairwise.complete.obs")[[2]],sep = " --->>> "))
  print(paste(colnames(d160)[i],pcor(d160[,c(i,8)])[[2]],sep = " --->>> "))
  j <- c(j,ifelse(abs(pcor(d160[,c(i,8)])[[2]]) >= 0.3,i,NA))
}
j <- j[!is.na(j)]
for(k in j){
  plot(d160[,c(k,8)],main = paste(colnames(d160)[k],pcor(d160[,c(k,8)])[[2]],sep = " --->>> "))
}

##In depth for 100L##
d1 <- as.data.frame(read_excel("Master_data_upstream_29082017_3.xlsx",sheet=1))
d100<-subset(d1,d1$Scale=='100')
colnames(d100)
d100<-d100[,c(1:3,23,29,48,144,112,99,155,187,191,190)]
#d100<-na.omit(d100)
rsm24<-rsm(Yield~FO(averagepHbeforetemp.shift,AverageDO2)+TWI(averagepHbeforetemp.shift,AverageDO2),data=d100)
summary(rsm24)
rsm25<-rsm(Yield~FO(SeedtransferVCC100L,Max.IVCC100Lproduction)+TWI(SeedtransferVCC100L,Max.IVCC100Lproduction),data=d100)
summary(rsm25)
rsm26<-rsm(Yield~SO(Temp.shiftloghour),data=d100)
summary(rsm26)
rsm26a<-rsm(Yield~FO(Avg.osmo100L,Dextroselevel100)+TWI(Avg.osmo100L,Dextroselevel100),data=d100)
summary(rsm26a)


d1 <- as.data.frame(read_excel("Master_data_upstream_29082017_3.xlsx",sheet=1))
d100<-subset(d1,d1$Scale=='100')
colnames(d100)
d10<-d100[,c(1:3,35,42,142,152,144)]
colnames(d10)
rsm27<-rsm(SeedtransferVCC100L~SO(SeedtransferVCC10L,Max.IVCC10Lseed),data=d10)
summary(rsm27)
##Media parameters##
d1 <- as.data.frame(read_excel("Master_data_upstream_29082017_4.xlsx",sheet=1))
d100<-subset(d1,d1$Scale=='100')
d100media<-d100[,c(1:3,26,83,115,187)]
colnames(d100media)
rsm28<-rsm(Yield~SO(Mediaosmolalityproduction,mediaholdpH100L),data=d100media)
summary(rsm28)
##Inoc parameters##
d1 <- as.data.frame(read_excel("Master_data_upstream_29082017_1.xlsx",sheet=1))
d100<-subset(d1,d1$Scale=='100')
colnames(d100)
dinoc<-d100[,c(1:9,145)]
colnames(dinoc)
rsm29<-rsm(InoculumendVCC~SO(Vialviablecellcount,EndVCCsubculture3),data=dinoc)
summary(rsm29)
##For optimizing rate of change of VCC w.r.t.VCC and IVCC##
drate<-as.data.frame(read_excel("Upstream VCC.xlsx",sheet=1))
drate100<-subset(drate,drate$Batch.Capacity=='100')
drate100<-na.omit(drate100)
rsm30<-rsm(dVCC/dt~SO(VCC),data=drate100)
pred<-predict(rsm30,newdata=drate100)
summary(rsm30)
#contour(rsm30~VCC,image=TRUE,at = summary(rsm30)$canonical$xs)
drate10to100<-subset(drate,drate$Train=='10->100')
drate10to100<-na.omit(drate10to100)
rsm32<-rsm(dVCC/dt~SO(VCC),data=drate10to100)
summary(rsm32)
steepest(rsm32)
write.csv(rsm24$coefficients,"coefficients.csv")
write.csv(rsm25$coefficients,"coefficients2.csv")
write.csv(rsm26$coefficients,"coefficients3.csv")
write.csv(rsm26a$coefficients,"coefficients4.csv")
write.csv(rsm27$coefficients,"coefficients5.csv")
write.csv(rsm28$coefficients,"coefficients6.csv")
write.csv(rsm14$coefficients,"coefficients.csv")
write.csv(rsm30$coefficients,"coefficients8.csv")
write.csv(rsm32$coefficients,"coefficients9.csv")


