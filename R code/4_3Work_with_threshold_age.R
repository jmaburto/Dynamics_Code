rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(ggplot2)
library(zoo)

#load decomposition data
load('Data/Decomp_results.Rdata')
#load threshold ages data
load('Data/Threshold.ages.Rdata')

#merge results.decomp with threshold ages
Results.decomp <- merge(Results.decomp,Threshold.ages,by = c('PopName','Sex','Year'), all.x = T)
Results.decomp <- Results.decomp[order(PopName,Sex,Year,Age),]
Results.decomp <- Results.decomp[,c("PopName","Sex","Year","Age","eo.decomp","h.decomp","g.decomp","v.decomp","a.h","a.g","a.v")]
Results.decomp <- Results.decomp[Age <= 110,]

gdata::keep(Results.decomp, sure=T)

source('Functions_1.R')

#get contributions for all countries
#Results.decomp <- Results.decomp[PopName =='SWE' & Sex == 'f' & Year %in% 1970:2015,]

Threshold.contrib <- Results.decomp[,list(ab.eo.h  = get.contrib.function(contrib = eo.decomp, age.x = a.h[1]),
                                       ab.eo.g  = get.contrib.function(contrib = eo.decomp, age.x = a.g[1]),
                                       ab.eo.v  = get.contrib.function(contrib = eo.decomp, age.x = a.v[1]),
                                       ab.a.h = get.contrib.function(contrib = h.decomp, age.x = a.h[1]),
                                       ab.a.g = get.contrib.function(contrib = g.decomp, age.x = a.g[1]),
                                       ab.a.v = get.contrib.function(contrib = v.decomp, age.x = a.v[1]),
                                       ind.age = c('Below','Above')), by = list(PopName,Sex,Year)]


## Now get contributions to rolling ten years
Teny_contributions <- Threshold.contrib[, list(ab.eo.h.10  = rollapply(ab.eo.h,10,sum),
                                               ab.eo.g.10  = rollapply(ab.eo.g,10,sum),
                                               ab.eo.v.10  = rollapply(ab.eo.v,10,sum),
                                               ab.a.h.10 = rollapply(ab.a.h,10,sum),
                                               ab.a.g.10 = rollapply(ab.a.g,10,sum),
                                               ab.a.v.10 = rollapply(ab.a.v,10,sum),
                                               Year = rollapply(Year,10,min)), 
                                        by = list(PopName,Sex,ind.age)]
Teny_contributions <- Teny_contributions[Teny_contributions$ab.eo.h.10 >= -5,]

save(Threshold.contrib,Teny_contributions, file = 'Data/Threshold_contrib_Results.Rdata')


### for MS

load('Data/Threshold_contrib_Results.Rdata')

Teny_contributions[,decade := cut(Year,breaks = seq(1900,2020,10),include.lowest = T),]

y <- Teny_contributions[,list(abs.change.below.eo = abs(ab.eo.h.10[1])/sum(abs(ab.eo.h.10)),
                         abs.change.below.h = abs(ab.a.h.10[1])/sum(abs(ab.a.h.10))) 
                           ,by = list(PopName,Sex,Year)]

mean(y$abs.change.below.eo)
mean(y$abs.change.below.h)




