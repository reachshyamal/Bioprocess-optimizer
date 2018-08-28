rm(list = ls())

library(rsm)
library(readxl)

##In depth for 160L##

setwd("C:/Users/Pratyay.Sengupta/Documents/RLS/In depth analysis/")

d1 <- as.data.frame(read_excel("Master_data_upstream_29082017.xlsx",sheet=1))
d160<-subset(d1,d1$Scale == 160)
#d160<-subset(d1,d1$DS != 'EPODS13006')
colnames(d160)

d160<-d160[,c(1:3,23,29,48,145,99,156,187,189,195,196)]
colnames(d160)

rsm10<-rsm(Yield~SO(AverageDO2,Temp.shiftloghour),data=d160)
summary(rsm10)
rsm11<-rsm(Yield~FO(SeedtransferVCC160L,averagepHbeforetemp.shift)+TWI(SeedtransferVCC160L,averagepHbeforetemp.shift),data=d160)
summary(rsm11)
rsm11a<-rsm(Yield~SO(Max.IVCC160Lproduction,Dextroselevel),data=d160)
summary(rsm11a)

rsm25 <- rsm(CultureSupernatant~FO(AverageDO2,Temp.shiftloghour)+TWI(AverageDO2,Temp.shiftloghour)+
                                PQ(AverageDO2),data=d160)
summary(rsm25)
rsm26<-rsm(CultureSupernatant~FO(SeedtransferVCC160L,averagepHbeforetemp.shift)+
                                TWI(SeedtransferVCC160L,averagepHbeforetemp.shift),data=d160)
summary(rsm26)
rsm27<-rsm(CultureSupernatant~FO(Max.IVCC160Lproduction,Dextroselevel)+
                              PQ(Max.IVCC160Lproduction),data=d160)
summary(rsm27)


##For seed batches##

##For 40-160 trail ##

d1 <- as.data.frame(read_excel("Master_data_upstream_29082017.xlsx",sheet=1))
d160<-subset(d1,d1$Scale=='160')
colnames(d160)
d160<-d160[,c(1:3,143,36,43,153,145)]
colnames(d160)
rsm12<-rsm(SeedtransferVCC160L~SO(SeedtransferVCC40L,Avg.pH40Lseed),data=d160)
summary(rsm12)
rsm13<-rsm(SeedtransferVCC160L~SO(Max.IVCC40Lseed,Avg.DO240Lseed),data=d160)
summary(rsm13)

##For 10-40 trail ##

d1 <- as.data.frame(read_excel("Master_data_upstream_29082017.xlsx",sheet=1))
d160<-subset(d1,d1$Scale=='160')
colnames(d160)
d10<-d160[,c(1:3,35,42,142,152,143)]
colnames(d10)
rsm16<-rsm(SeedtransferVCC40L~FO(Max.IVCC10Lseed,Avg.DO210Lseed)+TWI(Max.IVCC10Lseed,Avg.DO210Lseed),data=d10)
summary(rsm16)
rsm17<-rsm(SeedtransferVCC40L~SO(SeedtransferVCC10L,Avg.pH10Lseed),data=d10)
summary(rsm17)

##Inoc parameters##

d1 <- as.data.frame(read_excel("Master_data_upstream_29082017.xlsx",sheet=1))
d160<-subset(d1,d1$Scale=='160')
colnames(d160)
dinoc<-d160[,c(1:4,191:193,6,142)]
colnames(dinoc)
rsm18<-rsm(SeedtransferVCC10L~FO(Vialviablecellcount,EndVCCsubculture2,EndVCCsubculture3)+TWI(Vialviablecellcount,EndVCCsubculture2,EndVCCsubculture3)+PQ(Vialviablecellcount),data=dinoc)
summary(rsm18)
#rsm19<-rsm(SeedtransferVCC10L~FO(EndVCCsubculture2,EndVCCsubculture3)+TWI(EndVCCsubculture2,EndVCCsubculture3),data=dinoc)
#summary(rsm19)

##Media parameters##

d1 <- as.data.frame(read_excel("Master_data_upstream_29082017.xlsx",sheet=1))
d160<-subset(d1,d1$Scale=='160')
colnames(d160)
d160media<-d160[,c(1:3,28,115,85,187,190)]
colnames(d160media)
rsm20<-rsm(Yield~FO(mediaholdpH160L,Mediaosmolalityproduction,CO2160medialevel)+TWI(mediaholdpH160L,Mediaosmolalityproduction,CO2160medialevel),data=d160media)
summary(rsm20)
d160media<-d160[,c(1:3,11,17,27,84,187)]
rsm24<-rsm(Yield~FO(mediaholdpH160L,Mediaduration160L,Mediahold160L)+TWI(mediaholdpH160L,Mediaduration160L,Mediahold160L),data=d160media)
summary(rsm24)
##Contour plots##
contour(rsm10,~AverageDO2+Temp.shiftloghour,image=TRUE,at = summary(rsm10)$canonical$xs)
#jpeg(filename="c1.jpeg")
#dev.off()
contour(rsm11,~SeedtransferVCC160L+averagepHbeforetemp.shift,image=TRUE,at = summary(rsm11)$canonical$xs)
contour(rsm11a,~Max.IVCC160Lproduction+Dextroselevel,image=TRUE,at = summary(rsm11a)$canonical$xs)
contour(rsm12,~SeedtransferVCC40L+Avg.pH40Lseed,image=TRUE,at = summary(rsm12)$canonical$xs)
contour(rsm13,~Max.IVCC40Lseed+Avg.DO240Lseed,image=TRUE,at = summary(rsm13)$canonical$xs)
contour(rsm14,~SeedtransferVCC40L+Max.IVCC40Lseed,image=TRUE,at = summary(rsm14)$canonical$xs)
contour(rsm15,~Avg.pH40Lseed+Avg.DO240Lseed,image=TRUE,at = summary(rsm15)$canonical$xs)
contour(rsm16,~Max.IVCC10Lseed+Avg.DO210Lseed,image=TRUE,at = summary(rsm16)$canonical$xs)
contour(rsm17,~SeedtransferVCC10L+Avg.pH10Lseed,image=TRUE,at = summary(rsm17)$canonical$xs)
contour(rsm18,~Vialviablecellcount+EndVCCsubculture2+EndVCCsubculture3,image=TRUE,at = summary(rsm18)$canonical$xs)
contour(rsm20,~mediaholdpH160L+Mediaosmolalityproduction+CO2160medialevel,image=TRUE,at = summary(rsm20)$canonical$xs)

##For optimizing rate of change of VCC w.r.t.VCC and IVCC##

drate<-as.data.frame(read_excel("Upstream VCC.xlsx",sheet=1))
drate160<-subset(drate,drate$Train=='160')
drate160<-na.omit(drate160)
rsm21<-rsm(dVCC/dt~SO(VCC,IVCC),data=drate160)
summary(rsm21)
contour(rsm21,~VCC+IVCC,image=TRUE,at = summary(rsm21)$canonical$xs)
drate40to160<-subset(drate,drate$Train=='40->160')
drate40to160<-na.omit(drate40to160)
rsm22<-rsm(dVCC/dt~SO(VCC,IVCC),data=drate40to160)
summary(rsm22)
drate10to40<-subset(drate,drate$Train=='10->40')
drate10to40<-na.omit(drate10to40)
rsm23<-rsm(dVCC/dt~SO(VCC,IVCC),data=drate10to40)
summary(rsm23)
