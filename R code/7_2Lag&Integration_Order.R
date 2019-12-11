    ####### Testing for cointegration
rm(list=ls(all=TRUE))
library(data.table)
library(reshape)
library(latticeExtra)
library(lmtest)
library(urca)
library(vars)

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

ADF_test  <- NULL
KPSS_test <- NULL
cv_adf    <- NULL
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
    
    #Dickey fuller test
    df.test.eo    <- ur.df(ts1,lags=5,type='trend',selectlags = "AIC")
    df.test.h     <- ur.df(ts2,lags=5,type='trend',selectlags = "AIC")
    df.test.g     <- ur.df(ts3,lags=5,type='trend',selectlags = "AIC")
    df.test.v     <- ur.df(ts4,lags=5,type='trend',selectlags = "AIC")
    
    ADF_t     <- c(df.test.eo@teststat[1],df.test.h@teststat[1],df.test.g@teststat[1],df.test.v@teststat[1])
    msr       <- c("eo","h","g","v")
    Name      <- rep(i,4)
    Sex       <- rep(j,4)
    a         <- cbind.data.frame(Name,Sex,msr,ADF_t,stringsAsFactors = FALSE)
    ADF_test  <- rbind(ADF_test,a)
    
    #Kwiatkowski-Phillips-Schmidt-Shin test
    kpss.eo   <- ur.kpss(ts1,type="tau", lags = "nil")
    kpss.h    <- ur.kpss(ts2,type="tau", lags = "nil")
    kpss.g    <- ur.kpss(ts3,type="tau", lags = "nil")
    kpss.v    <- ur.kpss(ts4,type="tau", lags = "nil")
    
    Kpss_t    <- c(kpss.eo@teststat[1],kpss.h@teststat[1],kpss.g@teststat[1],kpss.v@teststat[1])
    b         <- cbind.data.frame(Name,Sex,msr,Kpss_t,stringsAsFactors = FALSE)
    KPSS_test <- rbind(KPSS_test,b)
    
    #Critical values
    cv_adf <- rbind(cv_adf,df.test.eo@cval[1,])
    cv_kpss <- rbind(cv_kpss,kpss.eo@cval[1,])
    
    ts1 <- ts2 <- ts3 <- ts4 <- NULL
  
  }
}

critical.values.adf <- df.test.eo@cval
critical.values.kpss<- kpss.eo@cval


# Figure of density plot for Figure A.2------------------------------------------

unique(ADF_test$msr)
KPSS_test$prop <- KPSS_test$Kpss_t > critical.values.kpss[2]

# Get proportions of non stationary processes
p1 <- subset(KPSS_test, msr=="eo")
p2 <- subset(KPSS_test, msr=="h")
p3 <- subset(KPSS_test, msr=="g")
p4 <- subset(KPSS_test, msr=="v")

prop1 <- table(p1$prop)["TRUE"]/dim(p1)[1]*100
prop2 <- table(p2$prop)["TRUE"]/dim(p2)[1]*100
prop3 <- table(p3$prop)["TRUE"]/dim(p3)[1]*100
prop4 <- table(p4$prop)["TRUE"]/dim(p4)[1]*100

cbind(eo=prop1,h=prop2,g=prop3,v=prop4)

text2 <- c(expression(v),expression(h),expression(e[o]),expression(g))

KPSS.eta  <- densityplot(~ Kpss_t|msr ,data=KPSS_test,
                  pch=19,col=c("red","blue"),cex=1.9,
                  strip=strip.custom(factor.levels=text2),
                  main=list("a) Kwiatkowski-Phillips-Schimidt-Shin test",cex=2),
                  par.strip.text=list(cex=2),col.line="black",
                  lwd=3,layout=c(2,2),
                  ylab=list("Density",cex=1.6),       
                  scales=list(x=list(at=c(seq(-.5,2.5,.25))),
                              y=list(relation = 'free')),
                  xlab=list("95% critical value",cex=1.6),
                  par.settings=my.settings1,
                  key = list(x=0.001,y=.9, title="Sex",background="transparent", 
                             text=list(c("Females","Males"))
                             ,cex=1.5,
                             points=list(pch=19,col=c("red","blue"))),             
                  panel = function(x, y, ...){ 
                    panel.abline(h=c(0),col='black',lty=1) 
                    panel.text(.8,.28,labels=c(paste(as.character(round(prop4,2)),"% of unit root"),
                                                paste(as.character(round(prop2,2)),"% of unit root"),
                                                paste(as.character(round(prop1,2)),"% of unit root"),
                                                paste(as.character(round(prop3,2)),"% of unit root"))[panel.number()],cex=1)
                    panel.text(.78,.15,labels="non stationary",cex=1)
                    panel.rect(xleft=-.25, xright=critical.values.kpss[2],ybottom=-.05, ytop=1.5,col=makeTransparent("blue",30))
                    panel.densityplot(x,...)
                    panel.abline(v=c(critical.values.kpss[2]),col='black',lty=1,lwd=2)
                  }) 

KPSS.eta



# now check that there are integrated of order 1 --------------------------

ADF_test2 <- NULL
cv_adf2 <- NULL


for (i in nms){
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
p1 <- subset(ADF_test2, msr=="eo")
p2 <- subset(ADF_test2, msr=="h")
p3 <- subset(ADF_test2, msr=="g")
p4 <- subset(ADF_test2, msr=="v")

prop1.1 <- table(p1$prop)["TRUE"]/dim(p1)[1]*100
prop2.1 <- table(p2$prop)["TRUE"]/dim(p2)[1]*100
prop3.1 <- table(p3$prop)["TRUE"]/dim(p3)[1]*100
prop4.1 <- table(p4$prop)["TRUE"]/dim(p4)[1]*100

cbind(eo=prop1.1,h=prop2.1,g=prop3.1,v=prop4.1)


# Figure of density plot for ADF (Appendix)------------------------------------------

ADF.eta  <- densityplot(~ ADF_t|msr ,data=ADF_test2,
                         pch=19,col=c("red","blue"),cex=1.9,
                        strip=strip.custom(factor.levels=text2),
                         main=list("b) ADF test on first differences",cex=2),
                         par.strip.text=list(cex=2),col.line="black",
                         lwd=3,layout=c(2,2),
                         ylab=list("Density",cex=1.6),       
                         scales=list(y=list(relation = 'free')),
                         xlab=list("95% critical value",cex=1.6),
                         par.settings=my.settings1,
                         key = NULL,             
                         panel = function(x, y, ...){ 
                           panel.abline(h=c(0),col='black',lty=1) 
                           panel.text(-7,.1,labels=c(paste(as.character(round(prop4.1,2)),"% I(1)"),
                                                      paste(as.character(round(prop2.1,2)),"% I(1)"),
                                                      paste(as.character(round(prop1.1,2)),"% I(1)"),
                                                      paste(as.character(round(prop3.1,2)),"% I(1)"))[panel.number()],cex=1)
                           panel.rect(xleft=-.25, xright=critical.values.adf[1,2],ybottom=-.05, ytop=1.5,col=makeTransparent("blue",30))
                           panel.densityplot(x,...)
                           panel.abline(v=c(critical.values.adf[1,2]),col='black',lty=1,lwd=2)
                         }) 

ADF.eta
