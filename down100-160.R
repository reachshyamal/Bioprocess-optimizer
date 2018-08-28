rm(list = ls())

library(rsm)
library(readxl)
library(devtools)
#install_github("dgrtwo/broom")
library(broom)
library(stringi)


###### Model 1 (for 100 & 160 L) ######

setwd("C:/Users/Pratyay.Sengupta/Documents/RLS/In depth analysis/")
d <- as.data.frame(read_excel("Master_data_upstream_29082017.xlsx",sheet="Downstream structured",skip=1))
dh <- as.data.frame(read_excel("Master_data_upstream_29082017.xlsx",sheet="HA chromatography"))
d <- d[d$X__2 < 640,]
dh <- dh[dh$Batch.Capacity.L < 640,]
colnames(d)

##Iteration 1##

do <- d[,c(1:2,64,3,19,26,42,56,58,61)]
colnames(do)
colnames(do)[1:9] <- c("BMR","Batch Capacity","Culture Supernetant","Filtered Supernetant","Sepharose","HA","Sephacel","Superdex","Nano")
colnames(do) <- stri_replace_all(colnames(do),".", fixed=" ")

do <- do[do$Batch.Capacity < 640,]
# `Filtered Supernetant`,`DEAE Sepharose Elute Pool (Output)`,
# `HA Elute Pool all lots (Output)`,`DEAE Sephacel Elute Pool (Output)`,
# `Superdex 75 Fraction pool (Output)`,`nano Load`


############################################################### 

d1 <- d[,c(1:19,26,42,56,58,61,64,65)]
colnames(d1)
colnames(d1)[c(1:3,18:23)] <- c("BMR","Batch Capacity","Supernetant","Sepharose.Load","Sepharose","HA","Sephacel","Superdex","Nano")
colnames(d1) <- stri_replace_all(colnames(d1),".", fixed=" ")
d1$Max.OD <- as.numeric(d1$Max.OD)

plot(d1$Sepharose.Load,d1$Sepharose)

# Filtered Supernetant = f( Culture Supernetant )
rsm0<-lm(Filtered.Supernetant ~ Culture.Supernetant -1,data = d1)
summary(rsm0)

# Sepharose = f( Supernetant , Sepharose.Load )
rsm8 <- lm(Sepharose~Supernetant -1,data=d1)
summary(rsm8)
rsm8a <- rsm(Sepharose~FO(Sepharose.Load)+
                      PQ(Sepharose.Load),data=d1)
summary(rsm8a)

# Sepharose = f( TFF1 parameters , Sepharose parameters )
#TFF1 parameters = DV,Cassette.Area,Flux,TMP,pH,Conductivity
rsm9 <- rsm(Sepharose~FO(DV,Flux,TMP)+
                      PQ(DV,Flux,TMP),data=d1)
summary(rsm9)
rsm10 <- rsm(Sepharose~FO(Conductivity,pH)+PQ(Conductivity,pH),data=d1)
summary(rsm10)

#Sepharose chromeatography parameters = Column.Volume,Binding.Capacity,Flow.rate,Max.Pressure,
#                                       Max.OD,Sepharose.Load.pH,Sepharose.Load.Conductivity
rsm11 <- rsm(Sepharose~FO(Flow.rate,Max.Pressure),data=d1)
summary(rsm11)
rsm12 <- rsm(Sepharose~FO(Sepharose.Load.pH,Sepharose.Load.Conductivity,Max.OD)+
                      TWI(formula = ~ Sepharose.Load.pH*Sepharose.Load.Conductivity+Sepharose.Load.Conductivity*Max.OD)+
                      PQ(Max.OD),data=d1)
summary(rsm12)


############################################################### 

d2 <- dh[,c(1:2,12,5:10,3,13:19,23)]
colnames(d2)
colnames(d2)[c(1:3,10,17:18)] <- c("BMR","Batch Capacity","Sepharose","Column Volume","HA Load","HA")
colnames(d2) <- stri_replace_all(colnames(d2),".", fixed=" ")
colnames(d2) <- stri_replace_all(colnames(d2),"", fixed=".Loading")
colnames(d2) <- stri_replace_all(colnames(d2),"", fixed=".LPH")
d2$Max.OD <- as.numeric(d2$Max.OD)


plot(d2$HA,d2$HA)

# HA = f( HA , HA.Load )
rsm13 <- rsm(HA~FO(Sepharose,HA.Load)+PQ(Sepharose,HA.Load),data=d2)
summary(rsm13)

# HA = f( TFF3 parameters , HA parameters )
#TFF2 parameters = DV,Cassette.Area,Flux,TMP,pH,Conductivity
rsm14 <- rsm(HA~FO(DV,Cassette.Area,Flux)+PQ(Flux),data=d2)
summary(rsm14)
rsm15 <- rsm(HA~FO(Conductivity,pH,TMP)+
               PQ(Conductivity),data=d2)
summary(rsm15)

#HA chromeatography parameters = Column.Volume,Binding.Capacity,Flow.Rate,Max.Pressure,
#                                       Max.OD,HA.Load.pH,HA.Load.Conductivity
rsm16 <- rsm(HA~FO(Column.Volume,Binding.Capacity,Flow.Rate,Max.OD)+
               TWI(Flow.Rate,Max.OD)+
               PQ(Binding.Capacity,Max.OD),data=d2)
#summary(update(rsm16, . ~ . - 1))
summary(rsm16)
rsm17 <- rsm(HA~FO(Load.pH,Load.conductivity,Max.Pressure)+
               PQ(Load.pH,Load.conductivity,Max.Pressure),data=d2)
summary(rsm17)

############################################################### 

d3 <- d[,c(1:3,19,26,28:42,56,58,61)]
colnames(d3)
colnames(d3)[c(1:5,19:22)] <- c("BMR","Batch Capacity","Supernetant","Sepharose","HA","Sephacel.Load","Sephacel","Superdex","Nano")
colnames(d3) <- stri_replace_all(colnames(d3),".", fixed=" ")
colnames(d3) <- stri_replace_all(colnames(d3),"", fixed="__1")
colnames(d3) <- stri_replace_all(colnames(d3),"", fixed="__2")


plot(d3$HA,d3$Sephacel)

# Sephacel = f( HA , Sephacel.Load )
rsm18 <- rsm(Sephacel~FO(HA,Sephacel.Load)+PQ(Sephacel.Load),data=d3)
summary(rsm18)

# Sephacel = f( TFF3 parameters , Sephacel parameters )
#TFF3 parameters = DV,Cassette.Area,Flux,TMP,pH,Conductivity
rsm19 <- rsm(Sephacel~FO(DV,Flux,TMP)+TWI(DV,TMP)+PQ(DV,TMP),data=d3)
summary(rsm19)
rsm20 <- rsm(Sephacel~FO(Conductivity,pH,Cassette.Area)+
               TWI(formula = ~ Conductivity*Cassette.Area+pH*Cassette.Area)+
               PQ(Cassette.Area),data=d3)
summary(rsm20)

#Sephacel chromeatography parameters = Column.Volume,Binding.Capacity,Flow.rate,Max.Pressure,
#                                       Max.OD,Sephacel.Load.pH,Sephacel.Load.Conductivity
rsm21 <- rsm(Sephacel~FO(Column.Volume,Binding.Capacity,Flow.Rate)+TWI(formula = ~ Column.Volume*Binding.Capacity+Binding.Capacity*Flow.Rate)+PQ(Flow.Rate),data=d3)
#summary(update(rsm21, . ~ . - 1))
summary(rsm21)
rsm22 <- rsm(Sephacel~FO(Sephacel.Load.pH,Sephacel.Load.conductivity,Max.Pressure)+
               PQ(Sephacel.Load.pH),data=d3)
summary(rsm22)


############################################################### 

d4 <- d[,c(1:3,19,26,42,44:56,58,61)]
colnames(d4)
colnames(d4)[c(1:6,18:20)] <- c("BMR","Batch Capacity","Supernetant","Sepharose","HA","Sephacel","Superdex.Load","Superdex","Nano")
colnames(d4) <- stri_replace_all(colnames(d4),".", fixed=" ")
colnames(d4) <- stri_replace_all(colnames(d4),"", fixed="__1")
colnames(d4) <- stri_replace_all(colnames(d4),"", fixed="__2")
colnames(d4) <- stri_replace_all(colnames(d4),"", fixed="__3")
d4$Max.OD <- as.numeric(d4$Max.OD)

plot(d4$Superdex.Load,d4$Sephacel)

# Superdex = f( HA , Superdex.Load )
#lm23a <- lm(Superdex~Superdex.Load-1,data=d4)
#summary(lm23a)
#lm23b <- lm(Superdex~Sephacel-1,data=d4)
#summary(lm23b)
rsm23 <- rsm(Superdex~FO(Sephacel,Superdex.Load)+TWI(Sephacel,Superdex.Load)+PQ(Superdex.Load),data=d4)
summary(rsm23)
# Superdex = f( TFF3 parameters , Superdex parameters )
#TFF4 parameters = DV,Cassette.Area,Flux,TMP,pH,Conductivity
rsm24 <- rsm(Superdex~FO(DV,Cassette.Area,Flux)+TWI(formula = ~ DV*Cassette.Area+DV*Flux),data=d4)
summary(rsm24)
rsm25 <- rsm(Superdex~FO(Conductivity,pH,TMP)+TWI(formula = ~ Conductivity*pH+pH*TMP)+
               PQ(Conductivity,pH,TMP),data=d4)
summary(rsm25)

#Superdex chromeatography parameters = Column.Volume,Binding.Capacity,Flow.rate,Max.Pressure,
#                                       Max.OD,Superdex.Load.pH,Superdex.Load.Conductivity
rsm26 <- rsm(Superdex~FO(Column.Volume,Flow.rate),data=d4)
#summary(update(rsm26, . ~ . - 1))
summary(rsm26)
rsm27 <- rsm(Superdex~FO(Superdex.Load.pH,Superdex.Load.conductivity)+PQ(Superdex.Load.pH),data=d4)
summary(rsm27)

#####################################################################################

rsm28 <- lm(Yield ~ Superdex-1,data = d4)
summary(rsm28)
