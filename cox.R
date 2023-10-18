#cox
rm(list = ls())
library(openxlsx)
library(DataExplorer)
library(car)
library(reshape)
library(tidyr)
library(plyr)
library(survminer)
library(survival)
library(mice)
library(foreign)
library(rms)


df2<-read.xlsx('hos_data.xlsx',sheet = 1)

BaSurv<-Surv(time = df2$los_hospital,event = df2$label)
df2$BaSurv<-with(df2,BaSurv)

GCox<-coxph(BaSurv~df2$admission_age,data = df2)
GSum<-summary(GCox)
GSum
HR<-round(GSum$coefficients[,2],2)
Pvalue<-round(GSum$coefficients[,5],3)
CI<-paste0(round(GSum$conf.int[,3:4],2),collapse = '-')
Unicox<-data.frame('Characteristics' = 'Gender',
                   'Hazard Ratio' = HR,
                   'CI95' = CI,
                   'P Value' = Pvalue)

UniCox<- function(x){
  FML<-as.formula(paste0('BaSurv~',x))
  GCox<-coxph(FML,data = df2)
  GSum<-summary(GCox)
  HR<-round(GSum$coefficients[,2],2)
  Pvalue<-round(GSum$coefficients[,5],3)
  CI<-paste0(round(GSum$conf.int[,3:4],2),collapse = '-')
  Unicox<-data.frame('Characteristics' = x,
                     'Hazard Ratio' = HR,
                     'CI95' = CI,
                     'P Value' = Pvalue)
  return(Unicox)
}

UniCox('ethnicity')
VarNames<-colnames(df2)[-c(76:78)]
VarNames
UnVar<-lapply(VarNames,UniCox)
UnVar<-ldply(UnVar,data.frame)
write.csv(UnVar,"single_cox.csv")


muldata<-df2[,(which(UnVar$P.Value<0.05& UnVar$Hazard.Ratio!=1))]

muldata$time<-df2$los_hospital
muldata$label<-df2$label
#BaSurv<-Surv(time = muldata$time,event = muldata$aki)
fml<-as.formula(paste0('BaSurv~',paste0(colnames(muldata[,-c(50:51)]),collapse = '+'))) 
fml
MultiCox<- coxph(fml,data = muldata)
MultiSum<-summary(MultiCox)
MultiName<-colnames(muldata)[-c(1,2)]
MHR<-round(MultiSum$coefficients[,2],2)
MPV<-round(MultiSum$coefficients[,5],3)
MCIL<-round(MultiSum$conf.int[,3],2)
MCIU<-round(MultiSum$conf.int[,4],2)
MCI<-paste0(MCIL,'-',MCIU)
MultiCox<-data.frame('Hazard Ratio' = MHR,
                     'CI95' = MCI,
                     'p Value' = MPV)


write.csv(MultiCox,'MCoxReg.csv')




Cox_data<-df2[,c("sofa","gcs_verbal", "sbp_min","spo2_max","los_hospital","label")]

Cox_data$sbp_min[Cox_data$sbp_min<90]<-0
Cox_data$sbp_min[Cox_data$sbp_min>=90]<-1


f<-coxph(Surv(los_hospital, label == 1)~sofa + gcs_verbal + sbp_min + spo2_max,
           data=Cox_data)
summary(f)
sum.surv<-summary(f)
c_index_se<- sum.surv$concordance
c_index <- c_index_se[1]
# C-index的95%置信区间
c_index.ci_low <- c_index - c_index_se[2]*1.96
c_index.ci_hig <- c_index + c_index_se[2]*1.96
c_index
c_index.ci_low
c_index.ci_hig

#Cox树状图
ggforest(f,fontsize = 1.4)


