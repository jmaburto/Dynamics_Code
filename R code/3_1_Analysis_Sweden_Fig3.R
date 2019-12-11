############# Written by JMA
rm(list=ls(all=TRUE))
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

# Loading data
load("Data/Sweden_HMD.RData")

#source some useful functions
source('Functions_1.R')

# start analysis
# get lifespan equality indicators
SWE.Ind <- LT.SWE.1[,list(h=h.frommx(mx = mx,sex = Sex[1]), 
                          v=my.cv.frommx(mx = mx,sex = Sex[1]),
                          g=log.G.frommx(mx = mx,sex = Sex[1]),
                          eo = ex[1]),
                    by = list(Sex,Sex,Year)]


#Get the weights for life expectancy and  lifespan equality by decade.
Weights.data <- LT.SWE.10[,list(Weights_h = W_h(ex,ax,dx,lx),
                       Weights_g = W_g(ex,ax,Age,dx,lx),
                       Weights_v = W_v(ex,ax,Age,dx,lx),
                       weights_eo = c(dx[-length(ax)]* (ex[-length(ax)] + ax[-length(ax)]*(ex[-1]-ex[-length(ax)])),ex[length(ax)]),
                       Age = Age),by=list(Year)]

Weights.data[, c('wW_h','wW_g','wW_v'):= list(weights_eo*Weights_h,weights_eo*Weights_g,weights_eo*Weights_v)]


#From Ms . By 2010, the effect of reducing mortality by 1% at birth was the same as reducing mortality by 1% at age 
Weights.2010 <- Weights.data[Year == 2010,]

cols.1 <- brewer.pal(9,name = 'Blues')[c(3,5,7,9)]

scaleFUN <- function(x) sprintf("%.2f", x)

Fig3.A <- ggplot(data = Weights.data[ Age<105 & Year %in% c(1751, 1900, 1950, 2010),])+
  ggtitle(expression(paste("A) Weights for life expectancy (", e[o],"), w(x)")))+
  geom_line(aes(x=Age,y = weights_eo,colour = factor(Year),group=Year), size = 2)+
  scale_x_continuous(" ")+
  scale_colour_manual('Period',
                      values = cols.1,
                      labels = c('1751-1759','1900-1909','1950-1959','2010-2017'),
                      aesthetics = c("colour", "fill"))+
  scale_y_continuous("w(x)",labels=scaleFUN)+
  theme(legend.key.height=unit(2,"line"))+  
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5),
        strip.placement = 'outside',
        strip.background =  element_rect(colour="white", fill="white"))+
  annotate("text", x =5 , y = 1.5, label = paste0("1751 goes up to ",as.character(round(Weights.data[ Age==0 & Year %in% c(1751, 1900),]$weights_eo[1],1))), col = 'black', hjust = 0,size=5)+
  annotate("text", x =5 , y = 1.35, label = paste0("1900 goes up to ",as.character(round(Weights.data[ Age==0 & Year %in% c(1751, 1900),]$weights_eo[2],1))), col = 'black', hjust = 0,size=5)+
  coord_cartesian(ylim=c(0, 1.5))+
  theme(legend.position = c(.75,.88))+
  geom_hline(yintercept = 0, lty = 2, lwd = 1.2)
  previous_theme <- theme_set(theme_bw())
  Fig3.A


  Fig3.B <- ggplot(data = Weights.data[ Age>=5 & Age<105 & Year %in% c(1751, 1900, 1950, 2010),])+
  ggtitle('B) Weights from age 5')+
  geom_line(aes(x=Age,y = weights_eo,colour = factor(Year),group=Year), size = 2,show.legend = F)+
  scale_x_continuous(" ",breaks = c(5,25,50,75,100))+
  scale_colour_manual('Period',
                      values = cols.1,
                      labels = c('1751-1759','1900-1909','1950-1959','2010-2017'),
                      aesthetics = c("colour", "fill"))+
  scale_y_continuous("",labels=scaleFUN)+
  theme(legend.key.height=unit(2,"line"))+  
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5),
        strip.placement = 'outside',
        strip.background =  element_rect(colour="white", fill="white"))+
  coord_cartesian(ylim=c(0, .4))+
  theme(legend.position = NULL)+
    geom_hline(yintercept = 0, lty = 2, lwd = 1.2)
  previous_theme <- theme_set(theme_bw())

  Fig3.B


cols.2 <- brewer.pal(9,name = 'YlOrRd')[c(3,5,7,9)]

Fig3.C <- ggplot(data = Weights.data[ Age<105 & Year %in% c(1751, 1900, 1950, 2010),])+
  ggtitle(expression(paste("C) Weights for lifespan equality h, ", w(x), W[h](x))))+
  geom_line(aes(x=Age,y = wW_h,colour = factor(Year),group=Year), size = 2)+
  geom_vline(xintercept = c(72.5, 78.5, 81.5, 89.2), lty = 2,col = cols.2, size = 1.5)+
  scale_x_continuous("Age")+
  scale_colour_manual('Period',
                      values = cols.2,
                      labels = c('1751-1759','1900-1909','1950-1959','2010-2017'),
                      aesthetics = c("colour", "fill"))+
  scale_y_continuous(expression(paste("", w(x), W[h](x))),labels=scaleFUN)+
  theme(legend.key.height=unit(2,"line"))+  
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5),
        strip.placement = 'outside',
        strip.background =  element_rect(colour="white", fill="white"))+
  annotate("text", x =5 , y = .112, label = paste0("1751 goes up to ",as.character(round(Weights.data[Age==0 & Year %in% c(1751, 1900),]$wW_h[1],1))), col = 'black', hjust = 0,size=5)+
  annotate("text", x =5 , y = .101, label = paste0("1900 goes up to ",as.character(round(Weights.data[Age==0 & Year %in% c(1751, 1900),]$wW_h[2],1))), col = 'black', hjust = 0,size=5)+
  coord_cartesian(ylim=c(-.01, .11))+
  theme(legend.position = c(.75,.88))+
  geom_hline(yintercept = 0, lty = 2, lwd = 1.2)
previous_theme <- theme_set(theme_bw())

Fig3.C

Fig3.D <- ggplot(data = Weights.data[Age>=5 & Age<105 & Year %in% c(1751, 1900, 1950, 2010),])+
  ggtitle("D) Weights from age 5")+
  geom_line(aes(x=Age,y = wW_h,colour = factor(Year),group=Year), size = 2,show.legend = F)+
  geom_vline(xintercept = c(72.5, 78.5, 81.5, 89.2), lty = 2,col = cols.2, size =1.5)+
  scale_x_continuous("Age",breaks = c(5,25,50,75,100))+
  scale_colour_manual('Period',
                      values = cols.2,
                      labels = c('1751-1759','1900-1909','1950-1959','2010-2017'),
                      aesthetics = c("colour", "fill"))+
  scale_y_continuous(' ',labels=scaleFUN)+
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5),
        strip.placement = 'outside',
        strip.background =  element_rect(colour="white", fill="white"))+
  coord_cartesian(ylim=c(-.01, .04))+
  theme(legend.position = NULL)+
  geom_hline(yintercept = 0, lty = 2, lwd = 1.2)
previous_theme <- theme_set(theme_bw())

Fig3.D


