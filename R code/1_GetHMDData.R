####### Program for getting most recent data from HMD
############# Written by JMA, thanks to Tim Riffe
############# 10/12/2019
library(HMDHFDplus)
library(data.table)

# get all countries in HMD
XYZ <- getHMDcountries()
# set your username for HMD
us <- "user"
# set your password
pw <- "password"

# get all the lifetables available from HMD
HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Males        <- readHMDweb(x,"mltper_1x1",username=us,password=pw)
  Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
  Males$Sex    <- "m"
  Females$Sex  <- "f"
  CTRY         <- rbind(Females, Males)
  CTRY$PopName <- x
  CTRY    
}, us = us, pw = pw))

# convert to data.table
HMDL <- data.table(HMDL)

# save the data
save(HMDL,file="Data/HMD_Data.RData")

#number of populations in the study
length(unique(HMDL$PopName))

#number of lifetbales
length(HMDL[Age== 0,]$ex)

#period spanning 
range(HMDL$Year)

# get 1-1 lifetables for Swedish females
LT.SWE.1 <- do.call(rbind,lapply(XYZ[41], function(x, us, pw){
  cat(x,"\n")
  Females         <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
  Females$Sex     <- "f"
  Females$PopName <- x
  Females 
}, us = us, pw = pw))
LT.SWE.1  <-  data.table(LT.SWE.1)

# get 1-10 lifetables for Swedish females
LT.SWE.10 <- do.call(rbind,lapply(XYZ[41], function(x, us, pw){
  cat(x,"\n")
  Females          <- readHMDweb(x,"fltper_1x10",username=us,password=pw)
  Females$Sex      <- "f"
  Females$PopName  <-  x
  Females
}, us = us, pw = pw))
LT.SWE.10  <-  data.table(LT.SWE.10)

# get deaths and exposures
SWE.DxNx <- do.call(rbind,lapply(XYZ[41], function(x, us, pw){
  cat(x,"\n")
  Deaths          <- readHMDweb(x,"Deaths_1x1",username=us,password=pw)
  Exposures       <- readHMDweb(x,"Exposures_1x1",username=us,password=pw)
  Deaths$Type     <- 'Deaths'
  Exposures$Type  <- 'Exposures'
  CTRY         <- rbind(Deaths, Exposures)
  CTRY$PopName <- x
  CTRY
}, us = us, pw = pw))
SWE.DxNx  <-  data.table(SWE.DxNx)

LT.SWE.1[,6:9]  <- LT.SWE.1[,6:9]/100000
LT.SWE.10[,6:9] <- LT.SWE.10[,6:9]/100000

save(LT.SWE.1,LT.SWE.10,SWE.DxNx ,file="Data/Sweden_HMD.RData")


