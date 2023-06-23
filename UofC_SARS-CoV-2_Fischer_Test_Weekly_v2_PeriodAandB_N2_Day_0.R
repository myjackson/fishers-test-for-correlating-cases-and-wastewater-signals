setwd("C:/Users/myjac/OneDrive - University of Calgary/3. Jangwoo_U of C_SARS-Cov-2/2. Data_Analysis/7_Fischer_Test/GitHub") # to set current working directory
data_folder = "data"
result_folder = "result"

case.dat<- read.delim(file.path(data_folder,"UofC_Covid_Tracking_Info_Cases_Data_Summarized_PeriodTotal_v3_WeeklyCases.csv"),sep=",",row.names = NULL) # data for cases

ww.dat<- read.delim(file.path(data_folder,"UofC_Covid_Tracking_Info_Weekly_Data.csv"),sep=",",row.names = NULL) # data for wastewater signals
ww.dat<- subset(ww.dat,Location!='WWTP')

########################################## Fischer's test for each location
## The following codes will generate a result for Fischer's test for each location. The results for all the locations then will be consolidated using the codes (L218-227)
# CR - One of the residence halls (RH1)
ww.dat.CR<-subset(ww.dat,Location=='CR')

new.dat.matrix<-cbind(case.dat$CR_Weekly_Avr_Cases,ww.dat.CR$N2_Weekly_Avr)
new.dat<-as.data.frame(new.dat.matrix)
colnames(new.dat)<-c('Cases','N2_Weekly_Avr')

Q.value.ww<-quantile(new.dat$N2_Weekly_Avr,na.rm = TRUE)[[3]]
Q.value.cases<-quantile(new.dat$Cases,na.rm = TRUE)[[3]]

Quad.1<-c() # + cases & - ww
Quad.2<-c() # + cases & + ww
Quad.3<-c() # - cases & + ww
Quad.4<-c() # - cases & - ww

for (i in c(1:(nrow(new.dat)))){
  num.cases<-new.dat$Cases[i]
  num.ww<-new.dat$N2_Weekly_Avr[i]
  if (!is.na(num.cases) & !is.na(num.ww)) {
    if (num.cases>Q.value.cases & num.ww>Q.value.ww){
      Quad.2<-c(Quad.2,1)
    } else if (num.cases<=Q.value.cases & num.ww>Q.value.ww){
      Quad.3<-c(Quad.3,1)
    } else if (num.cases<=Q.value.cases & num.ww<=Q.value.ww){
      Quad.4<-c(Quad.4,1)
    } else if (num.cases>Q.value.cases & num.ww<=Q.value.ww){
      Quad.1<-c(Quad.1,1)
    } else {
      print(paste(i,'error'))
    }
  } else {
    print(paste(i,'NA'))
  }
}

tab.cont.dat.CR <- matrix(c(sum(Quad.2),sum(Quad.1),sum(Quad.3),sum(Quad.4)),ncol=2,
                          dimnames=list(c("pos.cases","neg.tases"),c("pos.ww","neg.ww")),byrow=TRUE)

dat.test.CR <- fisher.test(tab.cont.dat.CR,alternative='greater')
dat.test.CR

# YA - One of the residence halls (RH2)
ww.dat.YA<-subset(ww.dat,Location=='YA')

new.dat.matrix<-cbind(case.dat$YA_Weekly_Avr_Cases,ww.dat.YA$N2_Weekly_Avr)
new.dat<-as.data.frame(new.dat.matrix)
colnames(new.dat)<-c('Cases','N2_Weekly_Avr')

Q.value.ww<-quantile(new.dat$N2_Weekly_Avr,na.rm = TRUE)[[3]]
Q.value.cases<-quantile(new.dat$Cases,na.rm = TRUE)[[3]]

Quad.1<-c() # + cases & - ww
Quad.2<-c() # + cases & + ww
Quad.3<-c() # - cases & + ww
Quad.4<-c() # - cases & - ww

for (i in c(1:(nrow(new.dat)))){
  num.cases<-new.dat$Cases[i]
  num.ww<-new.dat$N2_Weekly_Avr[i]
  if (!is.na(num.cases) & !is.na(num.ww)) {
    if (num.cases>Q.value.cases & num.ww>Q.value.ww){
      Quad.2<-c(Quad.2,1)
    } else if (num.cases<=Q.value.cases & num.ww>Q.value.ww){
      Quad.3<-c(Quad.3,1)
    } else if (num.cases<=Q.value.cases & num.ww<=Q.value.ww){
      Quad.4<-c(Quad.4,1)
    } else if (num.cases>Q.value.cases & num.ww<=Q.value.ww){
      Quad.1<-c(Quad.1,1)
    } else {
      print(paste(i,'error'))
    }
  } else {
    print(paste(i,'NA'))
  }
}

tab.cont.dat.YA <- matrix(c(sum(Quad.2),sum(Quad.1),sum(Quad.3),sum(Quad.4)),ncol=2,
                          dimnames=list(c("pos.cases","neg.tases"),c("pos.ww","neg.ww")),byrow=TRUE)

dat.test.YA <- fisher.test(tab.cont.dat.YA,alternative='greater')
dat.test.YA

# UCE - University eastern catchment (NE)
ww.dat.UCE<-subset(ww.dat,Location=='UCE')

new.dat.matrix<-cbind(case.dat$UCE_Weekly_Avr_Cases,ww.dat.UCE$N2_Weekly_Avr)
new.dat<-as.data.frame(new.dat.matrix)
colnames(new.dat)<-c('Cases','N2_Weekly_Avr')

Q.value.ww<-quantile(new.dat$N2_Weekly_Avr,na.rm = TRUE)[[3]]
Q.value.cases<-quantile(new.dat$Cases,na.rm = TRUE)[[3]]

Quad.1<-c() # + cases & - ww
Quad.2<-c() # + cases & + ww
Quad.3<-c() # - cases & + ww
Quad.4<-c() # - cases & - ww

for (i in c(1:(nrow(new.dat)))){
  num.cases<-new.dat$Cases[i]
  num.ww<-new.dat$N2_Weekly_Avr[i]
  if (!is.na(num.cases) & !is.na(num.ww)) {
    if (num.cases>Q.value.cases & num.ww>Q.value.ww){
      Quad.2<-c(Quad.2,1)
    } else if (num.cases<=Q.value.cases & num.ww>Q.value.ww){
      Quad.3<-c(Quad.3,1)
    } else if (num.cases<=Q.value.cases & num.ww<=Q.value.ww){
      Quad.4<-c(Quad.4,1)
    } else if (num.cases>Q.value.cases & num.ww<=Q.value.ww){
      Quad.1<-c(Quad.1,1)
    } else {
      print(paste(i,'error'))
    }
  } else {
    print(paste(i,'NA'))
  }
}

tab.cont.dat.UCE <- matrix(c(sum(Quad.2),sum(Quad.1),sum(Quad.3),sum(Quad.4)),ncol=2,
                           dimnames=list(c("pos.cases","neg.tases"),c("pos.ww","neg.ww")),byrow=TRUE)

dat.test.UCE <- fisher.test(tab.cont.dat.UCE,alternative='greater')
dat.test.UCE

# UCS - University southern catchment (SO)
ww.dat.UCS<-subset(ww.dat,Location=='UCS')

new.dat.matrix<-cbind(case.dat$UCS_Weekly_Avr_Cases,ww.dat.UCS$N2_Weekly_Avr)
new.dat<-as.data.frame(new.dat.matrix)
colnames(new.dat)<-c('Cases','N2_Weekly_Avr')

Q.value.ww<-quantile(new.dat$N2_Weekly_Avr,na.rm = TRUE)[[3]]
Q.value.cases<-quantile(new.dat$Cases,na.rm = TRUE)[[3]]

Quad.1<-c() # + cases & - ww
Quad.2<-c() # + cases & + ww
Quad.3<-c() # - cases & + ww
Quad.4<-c() # - cases & - ww

for (i in c(1:(nrow(new.dat)))){
  num.cases<-new.dat$Cases[i]
  num.ww<-new.dat$N2_Weekly_Avr[i]
  if (!is.na(num.cases) & !is.na(num.ww)) {
    if (num.cases>Q.value.cases & num.ww>Q.value.ww){
      Quad.2<-c(Quad.2,1)
    } else if (num.cases<=Q.value.cases & num.ww>Q.value.ww){
      Quad.3<-c(Quad.3,1)
    } else if (num.cases<=Q.value.cases & num.ww<=Q.value.ww){
      Quad.4<-c(Quad.4,1)
    } else if (num.cases>Q.value.cases & num.ww<=Q.value.ww){
      Quad.1<-c(Quad.1,1)
    } else {
      print(paste(i,'error'))
    }
  } else {
    print(paste(i,'NA'))
  }
}

tab.cont.dat.UCS <- matrix(c(sum(Quad.2),sum(Quad.1),sum(Quad.3),sum(Quad.4)),ncol=2,
                           dimnames=list(c("pos.cases","neg.tases"),c("pos.ww","neg.ww")),byrow=TRUE)

dat.test.UCS <- fisher.test(tab.cont.dat.UCS,alternative='greater')
dat.test.UCS

# UCW - University western catchment (NW)
ww.dat.UCW<-subset(ww.dat,Location=='UCW')

new.dat.matrix<-cbind(case.dat$UCW_Weekly_Avr_Cases,ww.dat.UCW$N2_Weekly_Avr)
new.dat<-as.data.frame(new.dat.matrix)
colnames(new.dat)<-c('Cases','N2_Weekly_Avr')

Q.value.ww<-quantile(new.dat$N2_Weekly_Avr,na.rm = TRUE)[[3]]
Q.value.cases<-quantile(new.dat$Cases,na.rm = TRUE)[[3]]

Quad.1<-c() # + cases & - ww
Quad.2<-c() # + cases & + ww
Quad.3<-c() # - cases & + ww
Quad.4<-c() # - cases & - ww

for (i in c(1:(nrow(new.dat)))){
  num.cases<-new.dat$Cases[i]
  num.ww<-new.dat$N2_Weekly_Avr[i]
  if (!is.na(num.cases) & !is.na(num.ww)) {
    if (num.cases>Q.value.cases & num.ww>Q.value.ww){
      Quad.2<-c(Quad.2,1)
    } else if (num.cases<=Q.value.cases & num.ww>Q.value.ww){
      Quad.3<-c(Quad.3,1)
    } else if (num.cases<=Q.value.cases & num.ww<=Q.value.ww){
      Quad.4<-c(Quad.4,1)
    } else if (num.cases>Q.value.cases & num.ww<=Q.value.ww){
      Quad.1<-c(Quad.1,1)
    } else {
      print(paste(i,'error'))
    }
  } else {
    print(paste(i,'NA'))
  }
}

tab.cont.dat.UCW <- matrix(c(sum(Quad.2),sum(Quad.1),sum(Quad.3),sum(Quad.4)),ncol=2,
                           dimnames=list(c("pos.cases","neg.tases"),c("pos.ww","neg.ww")),byrow=TRUE)

dat.test.UCW <- fisher.test(tab.cont.dat.UCW,alternative='greater')
dat.test.UCW

########################################## To save a result file
ls.dat<-c(dat.test.CR$p.value,dat.test.YA$p.value,dat.test.UCE$p.value,dat.test.UCS$p.value,dat.test.UCW$p.value)
ls.loc<-c('CR','YA','UCE','UCS','UCW')

ls.dat<-cbind(ls.dat,ls.loc)
colnames(ls.dat)<-c('p.value','Location')

dat.result<-as.data.frame(ls.dat)
dat.result$p.value<-as.numeric(dat.result$p.value)

write.table(dat.result,file.path(result_folder,"Fisher.Results.for.N2_PeriodAandB_Day+0.csv"),sep=',',row.names = FALSE) 
