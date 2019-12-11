############# Written by JMA
############# 10/12/2019
rm(list=ls(all=TRUE))
library(data.table)
library(reshape)
library(ggplot2)
library(viridis)
library(gridExtra)

# Loading data
load("Data/HMD_Data.RData")

# Be careful with the missing years of Belgium
Data  <- HMDL[HMDL$Year >= 1900 & HMDL$PopName !="BEL",]
# Get data for belgium in consecutive years
Bel   <- HMDL[HMDL$Year >= 1919 & HMDL$PopName=="BEL",]
Data  <- data.table(rbind(Data,Bel))
Data[, Sex1:= ifelse(Sex == 'f', 'Females', 'Males')]

gdata::keep(Data,sure = T)

#number of populations (contries and regions)
length(unique(Data$PopName))

#max life expectancy in the dataset
Data[which.max(Data$ex),]

#latest year for sweden
Data[PopName == 'SWE' & Year == 2017 & Age == 0,]

#load useful functions
source("Functions_1.R")

############## Calculate lifespan equality measures
Results           <- Data[,list(h=h.frommx(mx = mx,sex = Sex[1]), 
                                v=my.cv.frommx(mx = mx,sex = Sex[1]),
                                g=log.G.frommx(mx = mx,sex = Sex[1]),
                                eo = ex[1],
                                Period = cut(Year,breaks = c(1900,1920,1959,Inf),labels = c("1900-1921","1921-1959","1960 onwards"),include.lowest = T)),
                          by = list(PopName,Sex,Sex1,Year)]

#regresion for Swedesh females
SWE  <- Results[PopName == 'SWE' & Sex == 'f',]
mod1 <- lm(SWE$h ~ SWE$eo)
summary(mod1)
mod1$coefficients[2]
1/mod1$coefficients[2]

#from Colchero et al
1/.037

# get correlation coefficient for the manuscript
p.c.h         <- cor(Results$eo,Results$h,use="pairwise.complete.obs")
p.c.g         <- cor(Results$eo, Results$g,use="pairwise.complete.obs")
p.c.v         <- cor(Results$eo, Results$v,use="pairwise.complete.obs")

print(paste0(paste0('h = ',round(p.c.h,3)),paste0(', g = ',round(p.c.g,3)),paste0(', v = ',round(p.c.v,3))))

# Construct first graph of the manuscript;
##################### Plot both together
Results[is.na(Results$h),]
### Now create the plot
Fig1 <- ggplot(data = Results, aes(x = eo, y = h,color=Period)) +  
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  geom_smooth(data=Results,aes(x = eo, y = h), method = "lm", se=FALSE,col="black",size=1,lty=2,na.rm = T) + # if I want a linear one
  scale_x_continuous(expression(e[o]), limits=c(23,90))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  scale_y_continuous(expression(h), limits=c(-.01,2.20))+
  
  theme(legend.key.height=unit(2,"line"))+  
  #theme(legend.position = c(0.15, 0.85))+
  ggtitle(expression(paste("Life expectancy (", e[o],") vs lifespan equality (",h,')')))+
  theme(text = element_text(size = 15))+
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,legend.key = element_blank()
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.line.y = element_line(color="black", size = .5))
 previous_theme <- theme_set(theme_bw())

Fig1

#### Plots for Supplementary material

Fig1.S1.A.g <- ggplot(data = Results, aes(x = eo, y = g,color=Period)) +  
  geom_smooth(data=Results,aes(x = eo, y = g), method = "lm", se=FALSE,col="black",size=1,lty=2,na.rm = T) + # if I want a linear one
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous(expression(e[o]), limits=c(23,90))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  scale_y_continuous(expression(g), limits=c(.6,2.65))+
  theme(legend.key.height=unit(2,"line"))+  
  #theme(legend.position = c(0.15, 0.85))+
  ggtitle(expression(paste("A) Life expectancy (", e[o],") vs lifespan equality (",g,')')))+
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5))

previous_theme <- theme_set(theme_bw())

Fig1.S1.A.g

Fig1.S1.A.v<- ggplot(data = Results, aes(x = eo, y = v,color=Period)) +  
  geom_smooth(data=Results,aes(x = eo, y = v), method = "lm", se=FALSE,col="black",size=1,lty=2,na.rm = T) + # if I want a linear one
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous(expression(e[o]), limits=c(23,90))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  scale_y_continuous(expression(v), limits=c(-.012,1.96))+
  theme(legend.key.height=unit(2,"line"))+  
  #theme(legend.position = c(0.15, 0.85))+
  ggtitle(expression(paste("A) Life expectancy (", e[o],") vs lifespan equality (",v,')')))+
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5))

previous_theme <- theme_set(theme_bw())

Fig1.S1.A.v


# First differences Analysis ----------------------------------------------
#Calculate differences on life expectancy and lifespan equality indicators
Dif.data           <- Results[,list(dif.h = diff(h),
                                    dif.g = diff(g),
                                    dif.v = diff(v),
                                    dif.eo = diff(eo),
                                    dif.year= Y(Year,lag.2 = 1), 
                                    Period = cut(Year[-1],breaks = c(1900,1920,1959,Inf),labels = c("1900-1921","1921-1959","1960 onwards"),include.lowest = T)),
                              by = list(PopName,Sex)]

# Now make figure 1.B in the paper
Fig2.A <- ggplot(data = Dif.data, aes(y = dif.h, x = dif.eo,color=Period)) + 
  ggtitle('A) One-year changes')+
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous(expression(paste("Change in ", e[o])))+
  scale_y_continuous(expression(paste("Change in ", h)))+
  coord_cartesian(xlim =c(-10,10), ylim  = c(-.31,.31))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = NULL)+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=1,col="black",lty=2,se=F)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())+
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())
Fig2.A

Fig1.S1.B.g <- ggplot(data = Dif.data, aes(y = dif.g, x = dif.eo,color=Period)) + 
  ggtitle('B) One-year changes')+
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous(expression(paste("Change in ", e[o])))+
  scale_y_continuous(expression(paste("Change in ", g)))+
  coord_cartesian(xlim =c(-10,10), ylim  = c(-.22,.22))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = NULL)+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=1,col="black",lty=2,se=F)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())+
  theme(plot.background = element_blank())+
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())

Fig1.S1.B.g


Fig1.S1.B.v <- ggplot(data = Dif.data, aes(y = dif.v, x = dif.eo,color=Period)) + 
  ggtitle('B) One-year changes')+
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous(expression(paste("Change in ", e[o])))+
  scale_y_continuous(expression(paste("Change in ", v)))+
  coord_cartesian(xlim =c(-10,10), ylim  = c(-.2,.2))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = NULL)+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=1,col="black",lty=2,se=F)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())+
  theme(plot.background = element_blank())+
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())

Fig1.S1.B.v


# 10 year differences Analysis ----------------------------------------------
Dif.data.10           <- Results[,list(dif.h = h - shift(h,n = 10),
                                       dif.g = g - shift(g,n = 10),
                                       dif.v = v - shift(v,n = 10),
                                       dif.eo = eo - shift(eo,n = 10),
                                       Period = cut(Year,breaks = c(1900,1920,1959,Inf),labels = c("1900-1921","1921-1959","1960 onwards"),include.lowest = T)),
                                 by = list(PopName,Sex)]

Dif.data.10 <- Dif.data.10[!(is.na(Dif.data.10$dif.eo)),]
Dif.data.10[order(dif.eo)]

Fig2.B <- ggplot(data = Dif.data.10, aes(y = dif.h, x = dif.eo,color=Period)) + 
  ggtitle('B) Ten-years changes')+
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous(expression(paste("Change in ", e[o])))+
  scale_y_continuous(' ')+
  coord_cartesian(xlim =c(-28.5,28.5), ylim  = c(-1.05,1.05))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = NULL)+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=1,col="black",lty=2,se=F)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())+
  theme(plot.background = element_blank())+
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())

Fig2.B

Dif.data.10[is.na(Dif.data.10$dif.eo),]

Fig1.S1.C.g <- ggplot(data = Dif.data.10, aes(y = dif.g, x = dif.eo,color=Period)) + 
  ggtitle('C) Ten-years changes')+
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous(expression(paste("Change in ", e[o])))+
  scale_y_continuous(' ')+
  coord_cartesian(xlim =c(-28.5,28.5), ylim  = c(-1.05,1.05))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = NULL)+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=1,col="black",lty=2,se=F)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())+
  theme(plot.background = element_blank())+
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())

Fig1.S1.C.g


Fig1.S1.C.v <- ggplot(data = Dif.data.10, aes(y = dif.v, x = dif.eo,color=Period)) + 
  ggtitle('C) Ten-years changes')+
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous(expression(paste("Change in ", e[o])))+
  scale_y_continuous(' ')+
  coord_cartesian(xlim =c(-28.5,28.5), ylim  = c(-1.05,1.05))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = NULL)+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=1,col="black",lty=2,se=F)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())+
  theme(plot.background = element_blank())+
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())

Fig1.S1.C.v





################# Quantify proportions of the quadrants by decade
################# First code all decades
label.decade    <- paste0(as.character(seq(1900,2010,by = 10)),'-',as.character(seq(1900,2010,by = 10)+9))
Dif.data$decade <- cut(Dif.data$dif.year,breaks = seq(1899,2019,by = 10),labels = label.decade)

Dif.data[, c('quadrant.h','quadrant.g','quadrant.v') := list(ifelse((dif.eo < 0 & dif.h < 0), 1 , 
                                     ifelse((dif.eo > 0 & dif.h < 0), 2 ,
                                            ifelse((dif.eo > 0 & dif.h > 0), 3 ,4))),
                                     ifelse((dif.eo < 0 & dif.g < 0), 1 , 
                                            ifelse((dif.eo > 0 & dif.g < 0), 2 ,
                                                   ifelse((dif.eo > 0 & dif.g > 0), 3 ,4))),
                                     ifelse((dif.eo < 0 & dif.v < 0), 1 , 
                                            ifelse((dif.eo > 0 & dif.v < 0), 2 ,
                                                   ifelse((dif.eo > 0 & dif.v > 0), 3 ,4))))]

table.h <- table(Dif.data$decade,Dif.data$quadrant.h)/rowSums(table(Dif.data$decade,Dif.data$quadrant.h))*100
table.g <- table(Dif.data$decade,Dif.data$quadrant.g)/rowSums(table(Dif.data$decade,Dif.data$quadrant.g))*100
table.v <- table(Dif.data$decade,Dif.data$quadrant.v)/rowSums(table(Dif.data$decade,Dif.data$quadrant.v))*100

# percetnage in the desirable direction
#h
table.h[,1]+table.h[,3]

#g
table.g[,1]+table.g[,3]

#v
table.v[,1]+table.v[,3]

# percentage of changin eo and h in opposite directions
# between 1900-1959
100 - sum(table.h[1:5,c(1,3)])/sum(table.h[1:5,])*100
# in the 1960s
100 - sum(table.h[7,c(1,3)])/sum(table.h[7,])*100
# since 1970
100 - sum(table.h[8:12,c(1,3)])/sum(table.h[8:12,])*100



