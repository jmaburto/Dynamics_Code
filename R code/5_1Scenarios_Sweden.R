rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(ggplot2)

# Loading data
load("Data/Sweden_HMD.RData")

#source some useful functions
source('Functions_1.R')

# Original: get the observed life expectancy and lifespan equality
SWE.ind <- LT.SWE.1[,list(h= h.frommx(mx,'f'), ex=ex[1]), by = list(Year,Sex)]

# Constant scenario: get every point in life expectancy from constant changes

#a function to minimize the difference ot two life expectancies by a constant factor
ex.optim <- function(par,mx1,ex2){
  (ex2 - LifeExpectancy(mx1*(par)))^2
}

#all years
years2 <- unique(LT.SWE.1$Year)

#initial vector of rates
mx.init <- LT.SWE.1[LT.SWE.1$Year == min(years2),]$mx
ex0     <- LifeExpectancy(mx.init)

# a data,table
Results.dt <- cbind(min(years2),1,LifeExpectancy(mx.init),h.frommx(mx.init,'f'),
                    LifeExpectancy(mx.init),h.frommx(mx.init,'f'))

# for to update mx.init, could also be done with lapply sugin <<-
for(i in years2[-1]){
  
  mx      <- LT.SWE.1[LT.SWE.1$Year == i,]$mx
  ex      <- LifeExpectancy(mx)
  h       <- h.frommx(mx,'f')
  x       <- optimize(f = ex.optim,interval = c(0,100),tol = .00001,mx1=mx.init,ex2=ex)
  fact    <- x$minimum
  ex.new  <- LifeExpectancy(mx.init*fact,'f')
  h.new <- h.frommx(mx.init*fact,'f')
  
  b          <- cbind(i,fact,ex,h,ex.new,h.new)
  Results.dt <- rbind(Results.dt,b)
  
  #update mx
  mx.init <- mx.init*fact
}

Results.dt <- data.table(Results.dt)
names(Results.dt) <- c('Year','Factor','eo.original','h.original','eo.new.constant','h.new.constant')

# Optimal scenario: get every point in life expectancy from the bottom of the age-at-death distribution

#get factor
ex.optim.2 <- function(par,mx1,ex2,j){
  mx2 <- mx1
  mx2[j] <- mx1[j]*par
  (ex2 - LifeExpectancy(mx2))^2
}

#all years
years3 <- years2

#initial vector of rates
mx.init      <- LT.SWE.1[LT.SWE.1$Year == min(years3),]$mx
ex0          <- LifeExpectancy(mx.init)
Results.dt.2 <- cbind(min(years3),1,LifeExpectancy(mx.init),h.frommx(mx.init,'f'),
                      LifeExpectancy(mx.init),h.frommx(mx.init,'f'))

j <- 1
for(i in years3[-1]){
  
  #i <- 1888
  #for(i in 1887:1885){
  
  mx <- LT.SWE.1[LT.SWE.1$Year == i,]$mx
  ex <- LifeExpectancy(mx)
  h <- h.frommx(mx,'f')
  
  objective <- .0011
  
  while(objective > .001){
    
    x        <- optimize(f = ex.optim.2,interval = c(0,100),tol = .00001,mx1=mx.init,ex2=ex,j)
    objective<- x$objective
    fact     <- x$minimum
    
    if (objective <= .001){
      mx2      <- mx.init
      mx2[j]   <- mx2[j]*fact
      ex.new   <- LifeExpectancy(mx2,'f')
      h.new  <- h.frommx(mx2,'f')
      b <- cbind(i,fact,ex,h,ex.new,h.new)
      Results.dt.2 <- rbind(Results.dt.2,b)
      #update mx
      mx.init <- mx2
    }
    
    if (objective > .001){
      #print(c(i,j,fact,objective))
      mx.init[j]   <- 0
      j <- j+1
    }
  } 
}

Results.dt.2 <- data.table(Results.dt.2)
names(Results.dt.2) <- c('Year','Factor','eo.original','h.original','ex.new.optimal','h.new.optimal')


Scenarios.data <-   cbind(Results.dt[,c('Year','eo.original','h.original','h.new.constant')],Results.dt.2[,c('h.new.optimal')])

save(Scenarios.data, file = 'Data/ScenariosData.RData')
