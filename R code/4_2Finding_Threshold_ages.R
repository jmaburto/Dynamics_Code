############# Written by JMA
############# on a continous time model (Horiuchi et al 2008)
rm(list=ls(all=TRUE))
library(data.table)
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
Data$PopName2 <- Data$PopName

gdata::keep(Data,sure = T)

# source useful functions
source("Functions_1.R")

# create a function to get the threshold age for every year 

threshold.decomp.function <- function(DT = .SD, func2 = h.frommx, Age = 20:100){
  mat        <- acast(DT, Age~Year, value.var="mx")  
  count      <- DT$PopName2[1]
  Sex        <- DT$Sex1[1]
  
  registerDoParallel(4)
  threshold.ages     <- unlist(foreach(i=1:dim(mat)[2],.packages = 'DemoDecomp',.export = 
                                         c('AKm02a0')) %dopar% {
                                          if ( as.integer(colnames(mat)[i]) >= 1960){Age <- 45:100}
                                           if (count == 'ISL' & Sex == 'Females' & as.integer(colnames(mat)[i]) %in% c(1981,1982,1991,2016)){Age <- 70:100}
                                           perturbation <- horiuchi(func=func2, pars1=mat[,i] , pars2=mat[,i]*.99, N=35,sex=DT$Sex[1])
                                           f <- approxfun(Age,perturbation[(Age+1)],method = "linear",rule=2 )
                                           tryCatch({
                                             a <- uniroot(function(x) f(x)-0,interval = c(Age[1],Age[length(Age)]))$root
                                             a},
                                             error=function(x){
                                               a <- -1
                                               a})})
  stopImplicitCluster()
  y          <- do.call(rbind, lapply(threshold.ages, data.frame, stringsAsFactors=FALSE))
  y
  }

#try with datatable
strt<-Sys.time()

Threshold.ages <- Data[, c(a.h = threshold.decomp.function(.SD,func2 = h.frommx),
                           a.g = threshold.decomp.function(.SD,func2 = log.G.frommx),
                           a.v = threshold.decomp.function(.SD,func2 = my.cv.frommx)),by = list(Sex,PopName)]

names(Threshold.ages) <- c('Sex', 'PopName','a.h','a.g','a.v')

year.label <- Data[,list(Year = unique(Year)) ,
                 by = list(Sex,PopName)]

Threshold.ages <- cbind(Threshold.ages,year.label[,3])


save(Threshold.ages, file = "Data/Threshold.ages.Rdata")

print(Sys.time()-strt)

#Time difference of 3.537534 hours



