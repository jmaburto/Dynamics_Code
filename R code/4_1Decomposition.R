############# Written by JMA
############# on a continous time model (Horiuchi et al 2008)
rm(list=ls(all=TRUE))
library(data.table)
library(reshape)
library(reshape2)
library(foreach)
library(doParallel)
library(DemoDecomp)

#load data
load("Data/HMD_Data.RData")

# Be careful with the missing years of Belgium
Data  <- HMDL[HMDL$Year >= 1900 & HMDL$PopName !="BEL",]
# Get data for belgium in consecutive years
Bel   <- HMDL[HMDL$Year >= 1919 & HMDL$PopName=="BEL",]
Data  <- data.table(rbind(Data,Bel))
Data[, Sex1:= ifelse(Sex == 'f', 'Females', 'Males')]

#Data <- Data[Data$Year <= 1931 & Data$PopName == 'SWE',]
gdata::keep(Data,sure = T)

# source useful functions
source("Functions_1.R")
#DT <-  Data[Sex == 'f',]

# create a function to decompose by age every yearly change
DT.decomp.function <- function(DT = .SD, func2 = my.cv.frommx){
  mat        <- acast(DT, Age~Year, value.var="mx")  
  registerDoParallel(4)
  decomp     <- foreach(i=1:(dim(mat)[2]-1),.packages = 'DemoDecomp',.export = c('AKm02a0')) %dopar% {horiuchi(func=func2, pars1=mat[,i] , pars2=mat[,i+1], N=60,sex=DT$Sex[1])}
  stopImplicitCluster()
  
  y          <- unlist(decomp)
  y}

strt<-Sys.time()

Results.decomp <- Data[, list(decomp1 = DT.decomp.function(.SD,func2 = LifeExpectancy),
                            decomp2 = DT.decomp.function(.SD,func2 = h.frommx),
                            decomp3 = DT.decomp.function(.SD,func2 = log.G.frommx),
                            decomp4 = DT.decomp.function(.SD,func2 = my.cv.frommx)),by = list(Sex,PopName)]

names(Results.decomp) <- c('Sex', 'PopName','eo.decomp','h.decomp','g.decomp','v.decomp')

year.age <- Data[,list(Year = Year[112:length(Year)],
                          Age = Age[112:length(Year)]) ,
                          by = list(Sex,PopName)]

Results.decomp <- cbind(Results.decomp,year.age[,3:4])

save(Results.decomp, file = "Data/Decomp_results.Rdata")

print(Sys.time()-strt)

#Time difference of 6.606153 hours, need a new compuer :)
#check results:
