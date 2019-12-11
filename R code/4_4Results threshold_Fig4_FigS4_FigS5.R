rm(list=ls(all=TRUE))
library(data.table)
library(reshape)
library(ggplot2)
library(viridis)
library(gridExtra)

load("Data/Threshold_contrib_Results.Rdata")

#############
#code periods
Teny_contributions[,Period := cut(Year,breaks = c(1900,1920,1959,Inf),labels = c("1900-1921","1921-1959","1960 onwards"),include.lowest = T),]

####### first differences plots Fig 3 Manuscript
Fig4.A <- ggplot(data = Teny_contributions[ind.age == 'Below',], aes(y = ab.a.h.10, x = ab.eo.h.10,color=Period)) + 
  ggtitle('A) Changes below the threshold age')+
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous(expression(paste("Change in ", e[o])))+
  scale_y_continuous(expression(paste("Change in ", h)))+
  coord_cartesian(xlim =c(-28.5,28.5), ylim  = c(-1.05,1.05))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(.2,.85))+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=1,col="black",lty=2,se=F)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())+
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())

Fig4.A

Fig4.B <- ggplot(data = Teny_contributions[ind.age == 'Above',], aes(y = ab.a.h.10, x = ab.eo.h.10,color=Period)) + 
  ggtitle('B) Changes above the threshold age')+
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
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())

Fig4.B

####### first differences plots Fig S4 Supplementary material
FigS4.A <- ggplot(data = Teny_contributions[ind.age == 'Below',], aes(y = ab.a.g.10, x = ab.eo.h.10,color=Period)) + 
  ggtitle('A) Changes below the threshold age with g')+
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous('')+
  scale_y_continuous(expression(paste("Change in ", g)))+
  coord_cartesian(xlim =c(-28.5,28.5), ylim  = c(-1.05,1.05))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(.2,.85))+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=1,col="black",lty=2,se=F)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())+
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())

FigS4.A

FigS4.B <- ggplot(data = Teny_contributions[ind.age == 'Above',], aes(y = ab.a.g.10, x = ab.eo.h.10,color=Period)) + 
  ggtitle('B) Changes above the threshold age with g')+
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
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())

FigS4.B

FigS4.C <- ggplot(data = Teny_contributions[ind.age == 'Below',], aes(y = ab.a.v.10, x = ab.eo.h.10,color=Period)) + 
  ggtitle('C) Changes below the threshold age with v')+
  geom_point(alpha=I(1/5),shape=16,show.legend = F)+
  scale_x_continuous('')+
  scale_y_continuous(expression(paste("Change in ", v)))+
  coord_cartesian(xlim =c(-28.5,28.5), ylim  = c(-1.05,1.05))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(.2,.85))+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=1,col="black",lty=2,se=F)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())+
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())

FigS4.C

FigS4.D <- ggplot(data = Teny_contributions[ind.age == 'Above',], aes(y = ab.a.v.10, x = ab.eo.h.10,color=Period)) + 
  ggtitle('D) Changes above the threshold age with v')+
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
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())

FigS4.D

####### Threshold ages plots Fig S5 Supplementary material
load('Data/Threshold.ages.Rdata')

FigS5.A <- ggplot(data = Threshold.ages, aes(x = Year,y = a.h,group = interaction(PopName,Sex), color=Sex)) + 
  ggtitle('Threshold age for h')+
  geom_line(alpha=I(1/4),show.legend = T)+
  scale_y_continuous(' ')+
  scale_x_continuous(' ')+
  coord_cartesian(ylim  = c(15,90))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(.1,.2))+
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank())
previous_theme <- theme_set(theme_bw())

FigS5.A


FigS5.B <- ggplot(data = Threshold.ages, aes(x = Year,y = a.g,group = interaction(PopName,Sex), color=Sex)) + 
  ggtitle('Threshold age for g')+
  geom_line(alpha=I(1/4),show.legend = F)+
  scale_y_continuous('Threshold age')+
  scale_x_continuous(' ')+
  coord_cartesian(ylim  = c(15,90))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(.2,.85))+
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank())
previous_theme <- theme_set(theme_bw())

FigS5.B


FigS5.C <- ggplot(data = Threshold.ages, aes(x = Year,y = a.v,group = interaction(PopName,Sex), color=Sex)) + 
  ggtitle('Threshold age for v')+
  geom_line(alpha=I(1/4),show.legend = F)+
  scale_y_continuous(' ')+
  scale_x_continuous('Year')+
  coord_cartesian(ylim  = c(15,90))+
  scale_color_viridis(discrete=TRUE,option = 'D') +
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(.2,.85))+
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank())
previous_theme <- theme_set(theme_bw())

FigS5.C

