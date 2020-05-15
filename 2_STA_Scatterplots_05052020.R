##to empty workspace. Recommended for each code start
rm(list=ls())

##Load libraries needed to execute code
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
library(ggeffects)
library(gridExtra)
library(grid)
library(ggpubr)
library(Cairo)
library(ggpmisc)


#Set Working directory
setwd("/Users/Lukas/Documents/R/CHEPs_Ageing/2_Chep_Ageing/")



## dat<- read.csv ("Single_Trial_Analysis.csv", header=T, sep=',')

dat2 <- read.csv("Single_Trial_Analysis2.csv", header=T, sep=',')


### split up data ### 

NB <- dat2[which (dat2$Stim_Protocol=="Normal Baseline"),]

CV_NB <- NB[which (NB$Analysis=="CV"),]
STA_NB <- NB[which (NB$Analysis=="STA"),]

CV_NB_C4 <- CV_NB[which (CV_NB$Dermatome_new=="C4"),]
CV_NB_C6 <- CV_NB[which (CV_NB$Dermatome_new=="C6"),]
CV_NB_C8 <- CV_NB[which (CV_NB$Dermatome_new=="C8"),]


STA_NB_C4 <- STA_NB[which (STA_NB$Dermatome_new=="C4"),]
STA_NB_C6 <- STA_NB[which (STA_NB$Dermatome_new=="C6"),]
STA_NB_C8 <- STA_NB[which (STA_NB$Dermatome_new=="C8"),]



legend_title <-"Dermatome"
DermL <- c("C4","C6","C8")
my.formula <- y~x

#### N2P2 NRS ### 
#################

f1 <- ggplot(data = CV_NB_C4, aes(x=NRS, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Pain Rating [0-10]",limits=c(0,10))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f1


f2 <- ggplot(data = STA_NB_C4, aes(x=NRS, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Pain Rating [0-10]",limits=c(0,10))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f2



f3 <- ggplot(data = CV_NB_C6, aes(x=NRS, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Pain Rating [0-10]",limits=c(0,10))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f3


f4 <- ggplot(data = STA_NB_C6, aes(x=NRS, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Pain Rating [0-10]",limits=c(0,10))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f4


f5 <- ggplot(data = CV_NB_C8, aes(x=NRS, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Pain Rating [0-10]",limits=c(0,10))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f5


f6 <- ggplot(data = STA_NB_C8, aes(x=NRS, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Pain Rating [0-10]",limits=c(0,10))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f6


### Arrange and save ### 

fn1 <- ggarrange(f1,f2,
                ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn2 <- ggarrange(f3,f4,
                 ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn3 <- ggarrange(f5,f6,
                 ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn <- ggarrange(fn1,fn2,fn3,
                ncol=1, nrow=3)


ggsave(fn, filename = "NRS_N2P2_Scatters.pdf", device=cairo_pdf, width=6,height=8,units="in")





###### N2P2 Age ##### 

f1 <- ggplot(data = CV_NB_C4, aes(x=Age, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f1


f2 <- ggplot(data = STA_NB_C4, aes(x=Age, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f2


f3 <- ggplot(data = CV_NB_C6, aes(x=Age, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f3


f4 <- ggplot(data = STA_NB_C6, aes(x=Age, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f4


f5 <- ggplot(data = CV_NB_C8, aes(x=Age, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f5


f6 <- ggplot(data = STA_NB_C8, aes(x=Age, y=N2P2_Amplitude, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,90), breaks=c(0,25,50,75))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f6


### Arrange and save ### 

fn1 <- ggarrange(f1,f2,
                 ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn2 <- ggarrange(f3,f4,
                 ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn3 <- ggarrange(f5,f6,
                 ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn <- ggarrange(fn1,fn2,fn3,
                ncol=1, nrow=3)


ggsave(fn, filename = "Age_N2P2_Scatters.pdf", device=cairo_pdf, width=6,height=8,units="in")



#### N2 Age #### 
f1 <- ggplot(data = CV_NB_C4, aes(x=Age, y=N2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2 Latency [ms]", limits=c(250,600), breaks=c(300,400,500,600))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f1


f2 <- ggplot(data = STA_NB_C4, aes(x=Age, y=N2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2 Latency [ms]", limits=c(250,600), breaks=c(300,400,500,600))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f2


f3 <- ggplot(data = CV_NB_C6, aes(x=Age, y=N2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2 Latency [ms]", limits=c(250,600), breaks=c(300,400,500,600))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f3


f4 <- ggplot(data = STA_NB_C6, aes(x=Age, y=N2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2 Latency [ms]", limits=c(250,600), breaks=c(300,400,500,600))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f4


f5 <- ggplot(data = CV_NB_C8, aes(x=Age, y=N2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2 Latency [ms]", limits=c(250,600), breaks=c(300,400,500,600))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f5


f6 <- ggplot(data = STA_NB_C8, aes(x=Age, y=N2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="N2 Latency [ms]", limits=c(250,600), breaks=c(300,400,500,600))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f6


### Arrange and save ### 

fn1 <- ggarrange(f1,f2,
                 ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn2 <- ggarrange(f3,f4,
                 ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn3 <- ggarrange(f5,f6,
                 ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn <- ggarrange(fn1,fn2,fn3,
                ncol=1, nrow=3)


ggsave(fn, filename = "Age_N2_Scatters.pdf", device=cairo_pdf, width=6,height=8,units="in")



#### P2 Latency Age ### 
f1 <- ggplot(data = CV_NB_C4, aes(x=Age, y=P2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="P2 Latency [ms]", limits=c(350,800), breaks=c(400,500,600,700,800))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f1


f2 <- ggplot(data = STA_NB_C4, aes(x=Age, y=P2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="P2 Latency [ms]", limits=c(350,800), breaks=c(400,500,600,700,800))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f2


f3 <- ggplot(data = CV_NB_C6, aes(x=Age, y=P2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="P2 Latency [ms]", limits=c(350,800), breaks=c(400,500,600,700,800))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f3


f4 <- ggplot(data = STA_NB_C6, aes(x=Age, y=P2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="P2 Latency [ms]", limits=c(350,800), breaks=c(400,500,600,700,800))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f4


f5 <- ggplot(data = CV_NB_C8, aes(x=Age, y=P2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="P2 Latency [ms]", limits=c(350,800), breaks=c(400,500,600,700,800))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Conventional Averaging")


f5


f6 <- ggplot(data = STA_NB_C8, aes(x=Age, y=P2_Latency, shape=Dermatome_new,color=Dermatome_new))+
  geom_point(aes(fill=Dermatome_new),color="#333333")+
  scale_shape_manual(legend_title,values=c(21,24,22))+
  scale_color_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  scale_fill_manual(legend_title,values=c("C4"="#999999","C6"="#E69F00","C8"="#56B4E9"))+
  geom_smooth(method = "lm", show.legend=FALSE, se=F)+
  theme_bw()+scale_y_continuous(name="P2 Latency [ms]", limits=c(350,800), breaks=c(400,500,600,700,800))+
  scale_x_continuous(name ="Age [years]",limits=c(10,90),breaks=c(20,40,60,80))+
  stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(y)~`=`~",
               aes(label=paste(..eq.label..,..rr.label..,sep="~~")),
               parse=T, size=3)+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(family = "Times", size = 12), 
        axis.text.y = element_text(family = "Times", size = 12), 
        axis.title.y = element_text(family = "Times", size = 12), 
        axis.title.x = element_text(family = "Times", size = 12),
        legend.text = element_text(family = "Times", size = 12), 
        legend.title= element_text(family = "Times", size = 12))+
  ggtitle("Single Trial Averaging")


f6


### Arrange and save ### 

fn1 <- ggarrange(f1,f2,
                 ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn2 <- ggarrange(f3,f4,
                 ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn3 <- ggarrange(f5,f6,
                 ncol = 2, nrow = 1, common.legend = TRUE, legend="right")

fn <- ggarrange(fn1,fn2,fn3,
                ncol=1, nrow=3)


ggsave(fn, filename = "Age_P2_Scatters.pdf", device=cairo_pdf, width=6,height=8,units="in")
