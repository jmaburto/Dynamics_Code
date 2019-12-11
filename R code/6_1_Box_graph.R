# Box graph
rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(ggplot2)
library(MortalitySmooth)
library(RColorBrewer)
library(gridExtra)
library(zoo)


# Loading data for Sweden
load("Data/Sweden_HMD.RData")

# need threshold age
load('Data/Threshold.ages.Rdata')

SWE.a.g          <- Threshold.ages[PopName == 'SWE' & Sex == 'f',]
SWE.LT           <- LT.SWE.1[Year >= 1900 & Sex == 'f']
SWE.LT.10        <- LT.SWE.10[Year >= 1900 & Sex == 'f']
SWE.DxNx         <- SWE.DxNx[Year >= 1900,]
Deaths           <- SWE.DxNx[Type == 'Deaths', c('PopName','Year','Age','Female')]
names(Deaths)    <-  c('PopName','Year','Age','Deaths')
Exposures        <- SWE.DxNx[Type == 'Exposures', c('PopName','Year','Age','Female')]
names(Exposures) <-  c('PopName','Year','Age','Exposures')
SWE.DxEx         <- cbind(Deaths,Exposures[,c('Exposures')])

gdata::keep(SWE.a.g,SWE.LT,SWE.LT.10,SWE.DxEx, sure= T)
#source some useful functions
source('Functions_1.R')

#get the modal age at death
SWE.m <- SWE.DxEx[,list(mode = modal.age.function(Dx = Deaths,Ex = Exposures)), by = list(PopName,Year)]

#get life expectancy
SWE.eo <- SWE.LT[Age == 0,]

col.eo  <- brewer.pal(9,name = 'Blues')[c(5)]
col.a.g <-'slateblue'
col.m   <- 'magenta'
  
# Box figure
Box.figA <- ggplot() +  
  ggtitle(bquote( atop (paste("A) Life expectancy (",e[o],"), threshold age  (",a^h,")"), "and modal age at death (M)")))+
  geom_line(data = SWE.eo, aes(x = Year, y = ex),colour=col.eo,size=1.3, lty=1)+
  geom_line(data = SWE.a.g,aes(x = Year, y = a.h),colour=col.a.g,size=1, lty=2)+
  geom_line(data = SWE.m,aes(x = Year, y = mode),colour=col.m,size=.8, lty=3)+
  scale_x_continuous(expression(" "))+
  scale_y_continuous(expression("Years"))+
  annotate("text", x = 1900 , y = 58,  label = expression(e[o]), col = col.eo, hjust = 0,size = 5)+
  annotate("text", x = 1900 , y = 73 , label = expression(a^h), col = col.a.g, hjust = 0,size = 5)+
  annotate("text", x = 1900 , y = 80,  label = "M", col = col.m, hjust = 0,size = 5)+
  
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,strip.text = element_text(color='black')
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5),
        strip.placement = 'outside',
        strip.background =  element_rect(colour="white", fill="white"))
  
previous_theme <- theme_set(theme_bw())

Box.figA



#### A second graph for the box
# We need the percentage of deaths below the threshold age, mode and life expectancy
Year <- years <- unique(SWE.LT$Year)

Deaths.below.threshold <- unlist(lapply(years,function(x){
  LT  <- SWE.LT[SWE.LT$Year == x, ]
  age <- SWE.a.g[SWE.a.g$Year == x,]$a.h
  y <- get.contrib.function(contrib = LT$dx,age.x = age)[1]*100
}))

Deaths.below.ex <- unlist(lapply(years,function(x){
  LT  <- SWE.LT[SWE.LT$Year == x, ]
  age <- SWE.eo[SWE.eo$Year == x,]$ex
  y <- get.contrib.function(contrib = LT$dx,age.x = age)[1]*100
}))

Deaths.below.mode <- unlist(lapply(years,function(x){
  LT  <- SWE.LT[SWE.LT$Year == x, ]
  age <- SWE.m[SWE.m$Year == x,]$mode
   y <- get.contrib.function(contrib = LT$dx,age.x = age)[1]*100
}))

Deaths.below <- data.table(cbind(Year,Deaths.below.threshold,Deaths.below.ex,Deaths.below.mode))


Box.figB <- ggplot() +  
  ggtitle(bquote( atop (paste("B) Proportion of deaths below "),paste(e[o],", ",a^h," and M"))))+
  geom_line(data = Deaths.below, aes(x = Year, y = Deaths.below.ex),colour=col.eo,size=1.3, lty=1)+
  geom_line(data = Deaths.below,aes(x = Year, y = Deaths.below.threshold),colour=col.a.g,size=1, lty=2)+
  geom_line(data = Deaths.below,aes(x = Year, y = Deaths.below.mode),colour=col.m,size=.8, lty=3)+
  scale_x_continuous(expression(" "))+
  scale_y_continuous(expression("Proportion"))+
  annotate("text", x = 1950 , y = 38,  label = expression(paste('% of deaths below ',e[o])), col = col.eo, hjust = 0,size = 5)+
  annotate("text", x = 1950 , y = 47 , label = expression(paste('% of deaths below ',a^h)), col = col.a.g, hjust = 0,size = 5)+
  annotate("text", x = 1950 , y = 68.5,  label = "% of deaths below M", col = col.m, hjust = 0,size = 5)+
  
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,strip.text = element_text(color='black')
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5),
        strip.placement = 'outside',
        strip.background =  element_rect(colour="white", fill="white"))
previous_theme <- theme_set(theme_bw())

Box.figB


# a third graph with the percentage of the change in life expectancy due to change in death rates below some ages
load('Data/Decomp_results.Rdata')

SWE.Decomp <- Results.decomp[PopName == 'SWE' & Sex == 'f' & Age %in% 0:105,]

#### Improvements below threshold age
#x <- 1901
Changes.below.threshold <- lapply(years[-1],function(x){
  D1  <- SWE.Decomp[SWE.Decomp$Year == x, ]$h.decomp # for lifespan equality
  D2  <- SWE.Decomp[SWE.Decomp$Year == x, ]$eo.decomp       # for life expectancy
  age <- SWE.a.g[SWE.a.g$Year == x,]$a.g
  
  early.h <- abs(sum(D1[1:(trunc(age)+1)])+ (age-trunc(age))*D1[ceiling(age)+1])
  late.h  <- abs((1-(age-trunc(age)))*D1[ceiling(age)+1] + sum(D1[(trunc(age)+2):106]))
  h       <- early.h/(late.h + early.h)*100
  
  early.eo <- abs(sum(D2[1:(trunc(age)+1)])+ (age-trunc(age))*D2[ceiling(age)+1])
  late.eo  <- abs((1-(age-trunc(age)))*D2[ceiling(age)+1] + sum(D2[(trunc(age)+2):106]))
  eo       <- early.eo/(early.eo + late.eo)*100
 
  cbind(eo1 =eo ,h1 = h)
})

Changes.below.threshold      <- data.table(do.call(rbind,Changes.below.threshold))
Changes.below.threshold.10      <- Changes.below.threshold[,list(h.10 = rollapply(h1,10,mean),
                                                                 e0.10 = rollapply(eo1,10,mean)),]
Changes.below.threshold.10$Year <- 1900:(dim(Changes.below.threshold.10)[1]+1899)

#### Improvements below life expectancy
#x <- 1901
Changes.below.eo <- lapply(years[-1],function(x){
  D1  <- SWE.Decomp[SWE.Decomp$Year == x, ]$h.decomp # for lifespan equality
  D2  <- SWE.Decomp[SWE.Decomp$Year == x, ]$eo.decomp       # for life expectancy
  age <- SWE.eo[SWE.eo$Year == x,]$ex
  
  early.h <- abs(sum(D1[1:(trunc(age)+1)])+ (age-trunc(age))*D1[ceiling(age)+1])
  late.h  <- abs((1-(age-trunc(age)))*D1[ceiling(age)+1] + sum(D1[(trunc(age)+2):106]))
  h       <- early.h/(late.h + early.h)*100
  
  early.eo <- abs(sum(D2[1:(trunc(age)+1)])+ (age-trunc(age))*D2[ceiling(age)+1])
  late.eo  <- abs((1-(age-trunc(age)))*D2[ceiling(age)+1] + sum(D2[(trunc(age)+2):106]))
  eo       <- early.eo/(early.eo + late.eo)*100
  
  cbind(eo1 =eo ,h1 = h)
})

Changes.below.eo      <- data.table(do.call(rbind,Changes.below.eo))
Changes.below.eo.10      <- Changes.below.eo[,list(h.10 = rollapply(h1,10,mean),
                                                                 e0.10 = rollapply(eo1,10,mean)),]
Changes.below.eo.10$Year <- 1900:(dim(Changes.below.eo.10)[1]+1899)



#### Improvements below the mode
#x <- 1901
Changes.below.m <- lapply(years[-1],function(x){
  D1  <- SWE.Decomp[SWE.Decomp$Year == x, ]$h.decomp # for lifespan equality
  D2  <- SWE.Decomp[SWE.Decomp$Year == x, ]$eo.decomp       # for life expectancy
  age <- SWE.m[SWE.m$Year == x,]$mode
  
  early.h <- abs(sum(D1[1:(trunc(age)+1)])+ (age-trunc(age))*D1[ceiling(age)+1])
  late.h  <- abs((1-(age-trunc(age)))*D1[ceiling(age)+1] + sum(D1[(trunc(age)+2):106]))
  h       <- early.h/(late.h + early.h)*100
  
  early.eo <- abs(sum(D2[1:(trunc(age)+1)])+ (age-trunc(age))*D2[ceiling(age)+1])
  late.eo  <- abs((1-(age-trunc(age)))*D2[ceiling(age)+1] + sum(D2[(trunc(age)+2):106]))
  eo       <- early.eo/(early.eo + late.eo)*100
  
  cbind(eo1 =eo ,h1 = h)
})

Changes.below.m      <- data.table(do.call(rbind,Changes.below.m))
Changes.below.m.10      <- Changes.below.m[,list(h.10 = rollapply(h1,10,mean),
                                                   e0.10 = rollapply(eo1,10,mean)),]
Changes.below.m.10$Year <- 1900:(dim(Changes.below.m.10)[1]+1899)

#### create figures

y.i <- 93

Box.figC <- ggplot() +  
  ggtitle(expression(paste("C) Percentage of change in lifespan equality (h)")))+
  geom_line(data = Changes.below.eo.10, aes(x = Year, y = h.10),colour=col.eo,size=1.3, lty=1)+
  geom_line(data = Changes.below.threshold.10,aes(x = Year, y = h.10),colour=col.a.g,size=1, lty=2)+
  geom_line(data = Changes.below.m.10,aes(x = Year, y = h.10),colour=col.m,size=.8, lty=3)+
  scale_x_continuous(limits = c(1900,2017),expression("Year"))+
  scale_y_continuous(limits = c(30,95), expression("% of change"))+
  annotate("text", x = 1950 , y = y.i,  label = 'Lines relate to % of changes in', col = 'black', hjust = 0,size = 5)+
  annotate("text", x = 1950 , y = y.i-2.7,  label = expression(paste('h (panel C) and ',e[o],  ' (panel D) due to changes in')), col = 'black', hjust = 0,size = 5)+
  annotate("text", x = 1950 , y = y.i-5.2,  label = 'death rates below:', col = 'black', hjust = 0,size = 5)+
  annotate("text", x = 1950 , y = y.i-7.7,  label = expression(paste('1. ',e[o])), col = col.eo, hjust = 0,size = 5)+
  annotate("text", x = 1950 , y = y.i-10.5 , label = expression(paste('2. ',a^h)), col = col.a.g, hjust = 0,size = 5)+
  annotate("text", x = 1950 , y = y.i-13.3,  label = "3. M", col = col.m, hjust = 0,size = 5)+
  
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,strip.text = element_text(color='black')
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5),
        strip.placement = 'outside',
        strip.background =  element_rect(colour="white", fill="white"))
previous_theme <- theme_set(theme_bw())

Box.figC

Box.figD <-ggplot() +  
  ggtitle(expression(paste("D) Percentage of change in life expectancy (",e[o],")")))+
  geom_line(data = Changes.below.eo.10, aes(x = Year, y = e0.10),colour=col.eo,size=1.3, lty=1)+
  geom_line(data = Changes.below.threshold.10,aes(x = Year, y = e0.10),colour=col.a.g,size=1, lty=2)+
  geom_line(data = Changes.below.m.10,aes(x = Year, y = e0.10),colour=col.m,size=.8, lty=3)+
  scale_x_continuous(limits = c(1900,2017),expression("Year"))+
  scale_y_continuous(limits = c(30,95),expression("%"))+
  # annotate("text", x = 1900 , y = 69,  label = 'Lines relate to % of changes in', col = 'black', hjust = 0,size = 5)+
  # annotate("text", x = 1900 , y = 67.5,  label = 'lifespan equality due to changes', col = 'black', hjust = 0,size = 5)+
  # annotate("text", x = 1900 , y = 66,  label = 'in death rates below:', col = 'black', hjust = 0,size = 5)+
  # annotate("text", x = 1900 , y = 64,  label = expression(paste('1. ',e[o])), col = col.eo, hjust = 0,size = 5)+
  # annotate("text", x = 1900 , y = 62 , label = expression(paste('2. ',a^h)), col = col.a.g, hjust = 0,size = 5)+
  # annotate("text", x = 1900 , y = 59,  label = "3. M", col = col.m, hjust = 0,size = 5)+
  
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,strip.text = element_text(color='black')
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5),
        strip.placement = 'outside',
        strip.background =  element_rect(colour="white", fill="white"))

previous_theme <- theme_set(theme_bw())

Box.figD

