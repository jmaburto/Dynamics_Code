############# Written by JMA
rm(list=ls(all=TRUE))
library(gridExtra)
library(data.table)
library(ggplot2)
library(RColorBrewer)

# load data 
load('Data/ScenariosData.RData')

### Some figures

years4          <- c(1773,1751,1850,1900,1950,2017)
fig.label.x     <- Scenarios.data[which.max(Scenarios.data$eo.original),]$eo.original+.8
ex.original.col <-brewer.pal(9,name = 'Blues')[c(7)]
ex.const.col    <- brewer.pal(9,name = 'YlOrRd')[c(7)]
ex.optimal.col  <- 'slateblue'


Fig.5 <- ggplot()+
  
  geom_point(data = Scenarios.data,aes(x=eo.original,y = h.original),show.legend = F,shape = 16, col= ex.original.col,size= 2,alpha=I(1/4))+
  geom_smooth(data = Scenarios.data,aes(x = eo.original, y = h.original), method = "lm", se=F,col="gray29",size=1.5,lty=2,show.legend = F) + # if I want a linear one
  geom_point(data = Scenarios.data,  aes(x=eo.original,y = h.new.constant),show.legend = F,shape = 18, col= ex.const.col,size= 3,alpha=I(1/4))+
  geom_point(data = Scenarios.data,  aes(x=eo.original,y = h.new.optimal),show.legend = F,shape = 15, col=ex.optimal.col ,size= 3,alpha=I(1/4))+
  
  geom_text(data = Scenarios.data[Year %in% years4,],aes(x=eo.original,y = 3,label = Year),show.legend = F,col = 'darkgrey',angle = 0)+
  geom_segment(data = Scenarios.data[Year %in% years4,],aes(x = eo.original, y = h.original, xend = eo.original, yend = 2.9),show.legend = F, col = 'grey')+
  
  scale_x_continuous(expression(paste("Life expectancy (", e[o],")")), breaks = seq(20,90,10))+
  coord_cartesian(xlim=c(18, 95))+
  scale_y_continuous(expression(paste("Lifespan equality (", h,")")))+
  
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.new.optimal),]$h.new.optimal, label = "Youngest", col = ex.optimal.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.new.optimal),]$h.new.optimal-.08, label = "equality", col = ex.optimal.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.original),]$h.original, label = "Observed", col = ex.original.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.original),]$h.original-.08, label = "points", col = ex.original.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.new.constant),]$h.new.constant, label = "Constant", col = ex.const.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.new.constant),]$h.new.constant-.08, label = "change", col = ex.const.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.new.constant),]$h.new.constant-.08*2, label = "over age", col = ex.const.col, hjust = 0)+
  
  
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

Fig.5

lay.Fig1 <- rbind(c(1,2),
                  c(3,4))

pdf(file="Figures/Figure4.pdf",width=12,height=12,pointsize=4)
grid.arrange(Fig.5, layout_matrix = lay.Fig1)
dev.off()

# for the paper
mod1 <- lm(data = Scenarios.data,h.original ~ eo.original)
summary(mod1)
1/mod1$coefficients[2]

mod2 <- lm(data = Scenarios.data,h.new.optimal ~ eo.original)
summary(mod2)

mod3 <- lm(data = Scenarios.data,h.new.optimal.wW ~ ex.new.optimal.wW)
summary(mod3)