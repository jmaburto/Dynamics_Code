    ####### Testing for cointegration
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
nms <- unique(Results$PopName)[-26]
sex <- unique(Results$Sex)

zivot  <- NULL
cv_zivot <- NULL
for (i in nms){
  for (j in sex){
    #print(c(i,j))
    
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
    
    #Zivot-Andrew  test
    df.test.eo    <- ur.za(ts1,model = 'trend',lag = NULL)
    df.test.h     <- ur.za(ts2,model = 'trend',lag = NULL)
    df.test.g     <- ur.za(ts3,model = 'trend',lag = NULL)
    df.test.v     <- ur.za(ts4,model = 'trend',lag = NULL)
    
    ADF_t     <- c(df.test.eo@teststat[1],df.test.h@teststat[1],df.test.g@teststat[1],df.test.v@teststat[1])
    msr       <- c("eo","h","g","v")
    Name      <- rep(i,4)
    Sex       <- rep(j,4)
    a         <- cbind.data.frame(Name,Sex,msr,ADF_t,stringsAsFactors = FALSE)
    zivot  <- rbind(zivot,a)
    
    #summary(df.test.eo)

    #Critical values
    cv_zivot <- rbind(cv_zivot,df.test.eo@cval[1])
    
    ts1 <- ts2 <- ts3 <- ts4 <- NULL
  
  }
}

critical.values.zivot <- df.test.eo@cval


# Figure of density plot for Figure A.2------------------------------------------

unique(zivot$msr)
zivot$prop <- zivot$ADF_t < critical.values.zivot[2]

# Get proportions of non stationary processes
p1 <- subset(zivot, msr=="eo")
p2 <- subset(zivot, msr=="h")
p3 <- subset(zivot, msr=="g")
p4 <- subset(zivot, msr=="v")

prop1 <- table(p1$prop)["TRUE"]/dim(p1)[1]*100
prop2 <- table(p2$prop)["TRUE"]/dim(p2)[1]*100
prop3 <- table(p3$prop)["TRUE"]/dim(p3)[1]*100
prop4 <- table(p4$prop)["TRUE"]/dim(p4)[1]*100

cbind(eo=prop1,h=prop2,g=prop3,v=prop4)

