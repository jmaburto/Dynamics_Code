## robustness check
rm(list=ls(all=TRUE))
library(data.table)
library(reshape)
library(latticeExtra)
library(lmtest)
library(urca)
library(vars)

# Loading data
load("Data/HMD_Data.RData")

# Be careful with the missing years of Belgium
Data  <- HMDL[HMDL$Year >= 1900 & HMDL$PopName !="BEL",]
# Get data for belgium in consecutive years
Bel   <- HMDL[HMDL$Year >= 1919 & HMDL$PopName=="BEL",]
Data  <- data.table(rbind(Data,Bel))
Data[, Sex1:= ifelse(Sex == 'f', 'Females', 'Males')]

gdata::keep(Data,sure = T)

source('Functions_1.R')
############## Calculate lifespan equality measures
Results           <- Data[,list(h=h.frommx(mx = mx,sex = Sex[1]), 
                                v=my.cv.frommx(mx = mx,sex = Sex[1]),
                                g=log.G.frommx(mx = mx,sex = Sex[1]),
                                eo = ex[1],
                                Period = cut(Year,breaks = c(1900,1920,1959,Inf),labels = c("1900-1921","1921-1959","1960 onwards"),include.lowest = T)),
                          by = list(PopName,Sex,Sex1,Year)]

#Calculate differences on life expectancy and lifespan equality indicators
Dif.data           <- Results[,list(dif.h = diff(h),
                                    dif.g = diff(g),
                                    dif.v = diff(v),
                                    dif.eo = diff(eo),
                                    dif.year= Y(Year,lag.2 = 1), 
                                    Period = cut(Year[-1],breaks = c(1900,1920,1959,Inf),labels = c("1900-1921","1921-1959","1960 onwards"),include.lowest = T)),
                              by = list(PopName,Sex)]


# Testing for unit root in original series for all the measures of variation ---------------------------------------------------
nms <- unique(Results$PopName)
sex <- unique(Results$Sex)

KPSS_test <- NULL
cv_kpss   <- NULL

for (i in nms){
  for (j in sex){
    d1        <- subset(Results, PopName == i & Sex == j)
    d1        <- d1[with(d1, order(Year))]
    ini.year  <- range(d1$Year)[1]
    last.year <- range(d1$Year)[2]
    
    #for life expectancy
    ts1       <- ts(d1$eo,start = ini.year, end = last.year)
    #for h
    ts2       <- ts(d1$h,start = ini.year, end = last.year)
    # for g
    ts3       <- ts(d1$g,start = ini.year, end = last.year)
    # for v
    ts4       <- ts(d1$v,start = ini.year, end = last.year)
    
    #Kwiatkowski-Phillips-Schmidt-Shin test
    kpss.eo   <- ur.kpss(ts1,type="tau", lags = "nil")
    kpss.h    <- ur.kpss(ts2,type="tau", lags = "nil")
    kpss.g    <- ur.kpss(ts3,type="tau", lags = "nil")
    kpss.v    <- ur.kpss(ts4,type="tau", lags = "nil")
    
    msr       <- c("eo","h","g","v")
    Name      <- rep(i,4)
    Sex       <- rep(j,4)
    
    Kpss_t    <- c(kpss.eo@teststat[1],kpss.h@teststat[1],kpss.g@teststat[1],kpss.v@teststat[1])
    b         <- cbind.data.frame(Name,Sex,msr,Kpss_t,stringsAsFactors = FALSE)
    KPSS_test <- rbind(KPSS_test,b)
    
    #Critical values
    cv_kpss <- rbind(cv_kpss,kpss.eo@cval[1,])
    
    ts1 <- ts2 <- ts3 <- ts4 <- NULL
  
  }
}

critical.values.kpss<- kpss.eo@cval


# Figure of density plot for Figure A.2------------------------------------------
KPSS_test$prop <- KPSS_test$Kpss_t > critical.values.kpss[2]

# I will focus on h for this robustness check
p1 <- subset(KPSS_test, msr=="eo")
p2 <- subset(KPSS_test, msr=="h")

# Combine datasets
Data2 <- cbind(p1,prop2 = p2$prop)
Data2 <- data.table(Data2)

# How many mismatches do we have
conflict <- Data2[(prop == 'TRUE' & prop2 == 'FALSE') |
                    (prop == 'FALSE' & prop2 == 'TRUE') |
                    (prop == 'FALSE' & prop2 == 'FALSE')]

conflict.name <- unique(conflict$Name)

# now check that there are integrated of order 1 --------------------------

ADF_test2 <- NULL
cv_adf2 <- NULL
for (i in nms[!(nms%in%conflict.name)]){
  for (j in sex){
      d1       <- subset(Dif.data, PopName == i & Sex == j)
      d1        <- d1[order(dif.year),]
      ini.year  <- range(d1$dif.year)[1]
      last.year <- range(d1$dif.year)[2]
      
      
      #for life expectancy
      ts1       <- ts(d1$dif.eo,start = ini.year, end = last.year)
      #for eta
      ts2       <- ts(d1$dif.h,start = ini.year, end = last.year)
      # for log.G
      ts3       <- ts(d1$dif.g,start = ini.year, end = last.year)
      # for cv
      ts4       <- ts(d1$dif.v,start = ini.year, end = last.year)
      
    #Dickey fuller test
    df.test.eo <- ur.df(ts1,type='drift',selectlags = "AIC")
    df.test.h  <- ur.df(ts2,type='drift',selectlags = "AIC")
    df.test.g  <- ur.df(ts3,type='drift',selectlags = "AIC")
    df.test.v  <- ur.df(ts4,type='drift',selectlags = "AIC")
    
    ADF_t     <- c(df.test.eo@teststat[1],df.test.h@teststat[1],df.test.g@teststat[1],df.test.v@teststat[1])
    msr       <- c("eo","h","g","v")
    Name      <- rep(i,4)
    Sex       <- rep(j,4)
    a         <- cbind.data.frame(Name,Sex,msr,ADF_t,stringsAsFactors = FALSE)
    ADF_test2  <- rbind(ADF_test2,a)
    
    #Critical values
    cv_adf2 <- rbind(cv_adf2,df.test.eo@cval[1,])
    
    
  }
}

critical.values.adf <- df.test.eo@cval

ADF_test2$prop <- ADF_test2$ADF_t < critical.values.adf[1,2]


# Get proportions of non stationary processes
p1.2 <- subset(ADF_test2, msr=="eo")
p2.2 <- subset(ADF_test2, msr=="h")

# Combine datasets
Data3 <- cbind(p1.2,prop2 = p2.2$prop)
Data3 <- data.table(Data3)

# How many mismatches do we have
conflict <- Data3[(prop == 'TRUE' & prop2 == 'FALSE') |
                    (prop == 'FALSE' & prop2 == 'TRUE') |
                    (prop == 'FALSE' & prop2 == 'FALSE')]
