####### Testing for cointegration
###### We use 2 test, trace and max eigen johansen tests 
rm(list=ls(all=TRUE))
library(egcm)
library(latticeExtra)
library(urca)
library(reshape)
library(vars)
library(FitAR)
library(data.table)

# Loading data
load("Data/HMD_Data.RData")
HMDL <- HMDL[!(PopName %in% c('KOR', 'HRV', 'CHL','FRACNP')), ]
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


nms       <- unique(Results$PopName)
sex       <- unique(Results$Sex)
measures  <- c('h','g','v')

# Work with dummy variables (From Soren) -----------------------------------------------
Coin.data    <- NULL
vecm.data    <- NULL
autocor.data <- NULL

# SPANISH FLU
Results$dumvar1 <- ifelse(Results$Year==1918,1,0) 
# WW1
Results$dumvar2 <- ifelse(Results$Year==1914 |Results$Year==1915|Results$Year==1916|Results$Year==1917|Results$Year==1918|Results$Year==1919,1,0) 
# SPANISH CIVIL WAR
Results$dumvar3 <- ifelse(Results$Year==1939 |Results$Year==1940|Results$Year==1941|Results$Year==1942|Results$Year==1943|Results$Year==1944|Results$Year==1945,1,0) 
# WW2
Results$dumvar4 <- ifelse(Results$Year==1940|Results$Year==1941|Results$Year==1942|Results$Year==1943|Results$Year==1944|Results$Year==1945,1,0) 
# BREAK DOWN SOVIET UNION
Results$dumvar5 <- ifelse(Results$Year==1990|Results$Year==1991|Results$Year==1992|Results$Year==1993|Results$Year==1994|Results$Year==1995,1,0)
# WAR IN FINLAND AND SOVIET UNION
 Results$dumvar5 <- ifelse(Results$Year==1992|Results$Year==1993|Results$Year==1994|Results$Year==1995,1,0)
# CIVIL WAR FOR SPAIN
 Results$dumvar5 <- ifelse(Results$Year==1992|Results$Year==1993|Results$Year==1994|Results$Year==1995,1,0)


lags <- NULL

for (k in measures){
  for (i in nms){
    for (j in sex){
      
      col.ind <- ifelse(k == 'h',5,ifelse(k == 'v',6,7))
      # subset the data
      model.data1    <- subset(Results, PopName==i & Sex==j,select = c(col.ind,8))
      
      # get the optimal lag using AIC
      l              <- VARselect(model.data1, lag.max = 5, type = "both")$selection[1]
     
      if (l < 2) l <- 2
      
      
      optimal.lag <- cbind(measure=k,name=i,sex=j,lag=l)  
      
      lags <- rbind(lags,optimal.lag)
      
      dum.data <- subset(Results, PopName==i & Sex==j, select =c(10:14))
        
      dum.m <- NULL
      #create dummy variables according to the country.
      
      if (i =='CHE'|i =='SWE'|i =='NZL_NM') {dum.m <- dum.data$dumvar1}
      
      if (i =='GBR_NP'|i =='GBR_NIR'|i =='BEL' ){dum.m <- dum.data$dumvar4}
      
      if (i =='DEUTE'|i =='EST'|i =='HUN'|i =='LTU'|i =='LVA'|i =='POL'|i =='RUS'|i =='UKR' | i == 'BLR'){dum.m <- dum.data$dumvar5}
      
      if (i =='FIN'|i =='NLD'|i =='NOR'|i =='DNK'){dum.m <- cbind(dum.data$dumvar1,dum.data$dumvar4)}
      
      if (i =='FRATNP'|i =='ITA'|i =='GBRTENW'| i =='GBR_SCO'){dum.m <- cbind(dum.data$dumvar2,dum.data$dumvar4)}
      
      if (i =='ESP'){dum.m <- cbind(dum.data$dumvar1,dum.data$dumvar3)}
      
      if (i =='DEUTW'){dum.m <- cbind(dum.data$dumvar6)}
      
        
      # calculate the trace and max-eigen statistics and their critical values
      H1.trace      <- ca.jo(model.data1, type="trace", K=l, spec="longrun",season = NULL,ecdet = "const",dumvar = dum.m)
      H1.eigen      <- ca.jo(model.data1, type="eigen", K=l, spec="longrun",season = NULL,ecdet = "const",dumvar = dum.m )
    
      # deviance from 95% critical values
      dist.0.trace  <- H1.trace@teststat[2] - H1.trace@cval[2,2]
      dist.0.eigen  <- H1.eigen@teststat[2] - H1.eigen@cval[2,2]
      
      # Estimate de VECM with r=1
      vecm <-     cajorls(H1.trace,r=1) 
      
      #significance of coefficients THESE ARE THE SPEEDS OF ADJUSTMENTS
      sig.coef <- summary(vecm$rlm)
      sig1<-sig.coef[[1]]$coefficients[1,4]
      sig2<-sig.coef[[2]]$coefficients[1,4]
      
      #error correction term coefficient
      ect.coefficient <- vecm$rlm$coefficients[1,1:2]
      #ect.coefficient <- c(vecm$rlm$coefficients[1,1],NA)
      
      #cointegration vector
      coint.vector    <- vecm$beta[1:2]
      
      #Residuals, test for autocorrelation
      h <- dwtest(vecm$rlm$residuals[,1] ~ vecm$rlm$residuals[,2])
      
      #Store data
      f <- cbind.data.frame(Name=rep(i,1),Sex=rep(j,1),Measure = c(k),Test.stat.DW=h$statistic,
                            P.value=h$p.value,stringsAsFactors=FALSE)
      d <- cbind.data.frame(Name=rep(i,2),Sex=rep(j,2),Measure = rep(k,2),Test=c("Trace","Max Eigen"),
                            Statistic=c(dist.0.trace,dist.0.eigen),stringsAsFactors=FALSE)
      e <- cbind.data.frame(Name=rep(i,2),Sex=rep(j,2),Measure = rep(k,2),coint.vector,ect.coefficient,
                            sig.ect=c(sig2,NA),stringsAsFactors=FALSE,ind=c('v','e0'))
      row.names(e) <- NULL
      row.names(d) <- NULL
      row.names(f) <- NULL
      Coin.data <- rbind.data.frame(Coin.data,d)
      vecm.data <- rbind.data.frame(vecm.data,e)
      autocor.data <- rbind.data.frame(autocor.data,f)
      
    }
  }
}

# Plot results ------------------------------------------------------------
Coin.data <- data.table(Coin.data)
Coin.data <- Coin.data[order(Name,Sex,Measure,Test),]

NamesAbv        <- unique(Coin.data$Name)

Country.names  <- c("Australia","Austria","Belgium", "Bulgaria","Belarus","Canada","Switzerland","Czech Republic",
                    "East Germany","Germany", "West Germany", "Denmark","Spain","Estonia","Finland", "France","England and Wles (Civilian)",
                    "England and Wles (non-Civilian)",
                    "Northern Ireland","U.K.","Scotland","Greece","Hungary","Ireland","Iceland",
                    "Israel","Italy","Japan","Lithuania","Luxembourg","Latvia","Netherlands","Norway",
                    "New Zealand (Maori)","New Zealand (Non Maori)","New Zealand",
                    "Poland","Portugal","Russia","Slovakia","Slovenia","Sweden","Taiwan","Ukraine","U.S.A.")

length(NamesAbv)


txt.legend1 <- c("Trace","Max. Eigen")
my.pch1     <- c(19,17,16)  
my.fill1    <- c("blue","red","green")  
my.settings <- list(  
  strip.background=list(col="darkgrey"),
  strip.border=list(col="black"),
  layout.heights=list(panel = 1)
)

Coin.data[, Ind.test := ifelse(Statistic >=0,1,0),]

x <- table(Coin.data$Measure,Coin.data$Ind.test)
x[,2]/rowSums(x)*100

h.results <- Coin.data[Measure == 'h' & Test == 'Max Eigen',]
length(h.results[Statistic > 0,]$Statistic)/dim(h.results)[1]*100

#Rename countries

Coin.data$Country <- "l"
Coin.data$Sex[Coin.data$Sex == "f"] <- 1
Coin.data$Sex[Coin.data$Sex == "m"] <- 2
Coin.data$Sex <- as.factor(Coin.data$Sex)
levels(Coin.data$Sex) <- c("Females","Males")

for ( i in 1:length(Country.names)){
  Coin.data$Country[Coin.data$Name == NamesAbv[i]] <- Country.names[i]
}


text2 <- c(expression(g),expression(h),expression(v))

F2 <- useOuterStrips(dotplot(Country ~ Statistic|Test+Sex,aspect = c(1.2),ylab= list("Country",cex=1.3),
                             xlab=list("Distance from 95% critical value",cex=1.3),main=FALSE,
                             xlim=c(-20,100),between = list(y=1.5),origin=0,type=c("p","h"),
                             data=Coin.data, groups=Measure,pch=my.pch1, col=my.fill1,col.line='transparent',
                             lin=line,par.settings=my.settings,
                             scales=list(alternating=3,x=list(cex=1,at=seq(-20,80,20), labels=as.character(seq(-20,80,20)))
                                         ,y=list(relation="free",cex=.7,tck=0.6)),
                             key=list(x=.32,y=.62,background="white",title="Indicator",text=list(text2),points=list(pch=my.pch1,col=my.fill1,cex=1)),                       
                             prepanel = function(x, y) {
                               list(ylim = levels(reorder(y, x)))
                             },
                             panel = function(x, y,col.line='transparent', ...) {
                               panel.abline(v=seq(-20,100,20),lwd=1,lty=3,col="darkgrey")
                               panel.abline(v=c(0),lwd=1,lty=1,col="black")
                               #panel.dotplot(x,y, col.line = "transparent", ...)
                               panel.dotplot(x, reorder(y, x),col.line = "transparent", ...)
                             }),strip.left=T)
F2

