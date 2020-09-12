## Load Libraries 
library(lme4)
library(lmerTest)
library(emmeans)
library(pbkrtest)
library(ggplot2)
library(sjPlot)
library(sjstats)
library(psych)
library(rcompanion)
library(ggpubr)
library(Cairo)
library(reghelper)

#Clean working space
rm(list=ls())

##### Differences in stim protocol and analysis
#Set Working directory
setwd("/Users/Lukas/Documents/R/CHEPs_Ageing/2_Chep_Ageing/")

#Load data
dat2 <- read.csv("Single_Trial_Analysis2.csv", header=T, sep=',')

NB <- dat2[which (dat2$Stim_Protocol=="Normal Baseline"),]

CV <- dat2[which (dat2$Analysis=="CV"),]
STA <- dat2[which (dat2$Analysis=="STA"),]

CV_NB <- NB[which (NB$Analysis=="CV"),]
STA_NB <- NB[which (NB$Analysis=="STA"),]



##############
#### N2P2 ##### 
## Linear mixed models - Base model, add covariates, explore interactions ### 
N2P2_NB_Base <- lmer(N2P2_Amplitude ~ Analysis*Dermatome_new + (1|ID), data=NB)
anova(N2P2_NB_Base)
N2P2_NB_Int <- lmer(N2P2_Amplitude ~ Analysis*Dermatome_new+Age+Sex+Height+NRS + (1|ID), data=NB)
anova(N2P2_NB_Int)
N2P2_NB_H <- lmer(N2P2_Amplitude ~ Analysis+Dermatome_new+Age+Sex+Height+NRS+(Height*Analysis)+ (1|ID), data=NB)
anova(N2P2_NB_H)
N2P2_NB_A <- lmer(N2P2_Amplitude ~ Analysis+Dermatome_new+Age+Sex+Height+NRS+(Age*Analysis)+ (1|ID), data=NB)
anova(N2P2_NB_A)
N2P2_NB_S <- lmer(N2P2_Amplitude ~ Analysis+Dermatome_new+Age+Sex+Height+NRS+(Sex*Analysis)+ (1|ID), data=NB)
anova(N2P2_NB_S)
N2P2_NB_N <- lmer(N2P2_Amplitude ~ Analysis+Dermatome_new+Age+Sex+Height+NRS+(NRS*Analysis)+ (1|ID), data=NB)
anova(N2P2_NB_N)
# N2P2_NB_FULL <- lmer(N2P2_Amplitude ~ Analysis*Dermatome_new+Age+Sex+Height+NRS+(Age*Analysis)+(Sex*Analysis)+(Height*Analysis)+(NRS*Analysis)+ (1|ID), data=NB)
# anova(N2P2_NB_FULL)


### Follow up ## 
N2P2_NB_CV1 <- lmer(N2P2_Amplitude ~ Dermatome_new*Age +(1|ID), data=CV_NB)
anova(N2P2_NB_CV1)
summary(N2P2_NB_CV1)
tab_model(N2P2_NB_CV1)

N2P2_NB_STA1 <- lmer(N2P2_Amplitude ~ Dermatome_new*Age +(1|ID), data=STA_NB)
anova(N2P2_NB_STA1)
summary(N2P2_NB_STA1)

N2P2_NB_CV2 <- lmer(N2P2_Amplitude ~ Dermatome_new*NRS+(1|ID), data=CV_NB)
anova(N2P2_NB_CV2)
summary(N2P2_NB_CV2)
tab_model(N2P2_NB_CV2)

N2P2_NB_STA2 <- lmer(N2P2_Amplitude ~ Dermatome_new*NRS +(1|ID), data=STA_NB)
anova(N2P2_NB_STA2)
summary(N2P2_NB_STA2)

### Explore Pairwise Comparisons ### 
N2P2_NB_em1 <- emmeans(N2P2_NB_Int, pairwise ~ Analysis|Dermatome_new, adjust="bonf")
N2P2_NB_em2 <- emmeans(N2P2_NB_Int, pairwise ~ Dermatome_new|Analysis, adjust="bonf")
N2P2_NB_em3 <- emmeans(N2P2_NB_Int, pairwise ~ Dermatome_new, adjust="bonf")
N2P2_NB_em1
N2P2_NB_em2
N2P2_NB_em3

#########################
####### N2 Latency ###### 
N2_NB_Base <- lmer(N2_Latency ~ Analysis*Dermatome_new + (1|ID), data=NB)
anova(N2_NB_Base)
N2_NB_Int <- lmer(N2_Latency ~ Analysis*Dermatome_new+Age+Sex+Height+NRS + (1|ID), data=NB)
anova(N2_NB_Int)
N2_NB_H <- lmer(N2_Latency ~ Analysis+Dermatome_new+Age+Sex+Height+(Height*Analysis)+ (1|ID), data=NB)
anova(N2_NB_H)
N2_NB_A <- lmer(N2_Latency ~ Analysis+Dermatome_new+Age+Sex+Height+(Age*Analysis)+ (1|ID), data=NB)
anova(N2_NB_A)
N2_NB_S <- lmer(N2_Latency ~ Analysis+Dermatome_new+Age+Sex+Height+(Sex*Analysis)+ (1|ID), data=NB)
anova(N2_NB_S)
N2_NB_N <- lmer(N2_Latency ~ Analysis+Dermatome_new+Age+Sex+Height+NRS+(NRS*Analysis)+ (1|ID), data=NB)
anova(N2_NB_N)
# N2_NB_FULL <- lmer(N2_Latency ~ Analysis*Dermatome_new+Age+Sex+Height+(Age*Analysis)+(Sex*Analysis)+(Height*Analysis)+ (1|ID), data=NB)
# anova(N2_NB_FULL)

### Follow up ### 
N2_NB_CV1 <- lmer(N2_Latency ~ Dermatome_new+Age+Sex+Height+ (1|ID), data=CV_NB)
anova(N2_NB_CV1)
summary(N2_NB_CV1)

N2_NB_CV2 <- lmer(N2_Latency ~ relevel(Dermatome_new,"C8")+Age+Sex+Height+ (1|ID), data=CV_NB)
anova(N2_NB_CV2)
summary(N2_NB_CV2)

N2_NB_STA1 <- lmer(N2_Latency ~ Dermatome_new+Age+Sex+Height+ (1|ID), data=STA_NB)
anova(N2_NB_STA1)
summary(N2_NB_STA1)

N2_NB_STA2 <- lmer(N2_Latency ~ relevel(Dermatome_new,"C8")+Age+Sex+Height+ (1|ID), data=STA_NB)
anova(N2_NB_STA2)
summary(N2_NB_STA2)

### Explore pairwise comparisons ### 
N2_NB_em1 <- emmeans(N2_NB_FULL, pairwise ~ Analysis|Dermatome_new, adjust="bonf")
N2_NB_em2 <- emmeans(N2_NB_FULL, pairwise ~ Dermatome_new|Analysis, adjust="bonf")
N2_NB_em1
N2_NB_em2

#### P2 Latency ### 
P2_NB_Base <- lmer(P2_Latency ~ Analysis*Dermatome_new + (1|ID), data=NB)
anova(P2_NB_Base)
P2_NB_Int <- lmer(P2_Latency ~ Analysis*Dermatome_new+Age+Sex+Height+NRS + (1|ID), data=NB)
anova(P2_NB_Int)
P2_NB_H <- lmer(P2_Latency ~ Analysis+Dermatome_new+Age+Sex+Height+(Height*Analysis)+ (1|ID), data=NB)
anova(P2_NB_H)
P2_NB_A <- lmer(P2_Latency ~ Analysis+Dermatome_new+Age+Sex+Height+(Age*Analysis)+ (1|ID), data=NB)
anova(P2_NB_A)
P2_NB_S <- lmer(P2_Latency ~ Analysis+Dermatome_new+Age+Sex+Height+(Sex*Analysis)+ (1|ID), data=NB)
anova(P2_NB_S)
P2_NB_N <- lmer(P2_Latency ~ Analysis+Dermatome_new+Age+Sex+Height+NRS+(NRS*Analysis)+ (1|ID), data=NB)
anova(P2_NB_N)
# P2_NB_FULL <- lmer(P2_Latency ~ Analysis*Dermatome_new+Age+Sex+Height+(Age*Analysis)+(Sex*Analysis)+(Height*Analysis)+ (1|ID), data=NB)
# anova(P2_NB_FULL)

## Folow up 
P2_NB_CV1 <- lmer(P2_Latency ~ Dermatome_new*Age+Sex+Height+ (1|ID), data=CV_NB)
anova(P2_NB_CV1)
summary(P2_NB_CV1)

P2_NB_STA1 <- lmer(P2_Latency ~ Dermatome_new*Age+Sex+Height+ (1|ID), data=STA_NB)
anova(P2_NB_STA1)
summary(P2_NB_STA1)

### Explore pariwise comparsons ### 
P2_NB_em1 <- emmeans(P2_NB_Int, pairwise ~ Analysis|Dermatome_new, adjust="bonf")
P2_NB_em2 <- emmeans(P2_NB_Int, pairwise ~ Dermatome_new|Analysis, adjust="bonf")
P2_NB_em3 <- emmeans(P2_NB_Int, pairwise ~ Dermatome_new, adjust="bonf")
P2_NB_em1
P2_NB_em2
P2_NB_em3

### Cohen's D for N2 latency  #### 
library(esc)

describeBy(NB$N2_Latency, list(NB$Analysis,NB$Dermatome))

##  min time point ## 
esc_mean_sd(grp1m = 401.3, grp1sd = 31.6, grp1n = 82,
            grp2m = 420.3, grp2sd = 53.8, grp2n = 75, es.type = "d")

#### Boxplots #### 

NB$Age_Group <- factor(NB$Age_Group,levels(NB$Age_Group)[c(3,2,1)])
legend_title <-"Age:"

#### N2P2 ## 
f1 <- ggplot(data=NB, aes(x=interaction(Analysis, Dermatome_new),y=N2P2_Amplitude))+
  geom_boxplot(aes(fill=Dermatome_new),position=position_dodge(1), outlier.shape=NA)+
  geom_point(aes(shape=Age_Group, fill=Dermatome_new),color="#333333", 
             position=position_jitterdodge(jitter.width=0.4,jitter.height=0))+
  facet_grid(~Dermatome_new, switch = "x", scales = "free_x", space = "free_x")+
  scale_shape_manual(legend_title,breaks=c("Young","Middle","Elderly"),values=c(21,24,22,21,24,22))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_bw()+scale_y_continuous(name="N2P2 Amplitude [uV]", limits=c(0,100))+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family="Times", size= 18, colour = "black"),
        axis.text.x = element_text(family = "Times", size = 16, colour="black"), 
        axis.text.y = element_text(family = "Times", size = 16, colour="black"), 
        axis.title.y = element_text(family = "Times", size = 16), 
        axis.title.x = element_text(family = "Times", size = 16),
        legend.text = element_text(family = "Times", size = 16), 
        legend.title= element_text(family = "Times", size = 16),
        legend.position="bottom",panel.spacing=unit(0,"lines"),
        strip.background=element_blank(),strip.placement="outside",
        strip.text.x=element_text(family = "Times", size = 18))+
  scale_x_discrete(name="",labels=c("CV","STA","CV","STA","CV","STA"))+
  guides(fill=FALSE, shape=guide_legend(override.aes=list(size=4)))
f1

ggsave(f1, filename = "Figure1a_N2P2.pdf", device=cairo_pdf, width=6,height=5,units="in")

### N2 Latency ### 
f3 <- ggplot(data=NB, aes(x=interaction(Analysis, Dermatome_new),y=N2_Latency))+
  geom_boxplot(aes(fill=Dermatome_new),position=position_dodge(1), outlier.shape=NA)+
  geom_point(aes(shape=Age_Group, fill=Dermatome_new),color="#333333", 
             position=position_jitterdodge(jitter.width=0.4,jitter.height=0))+
  facet_grid(~Dermatome_new, switch = "x", scales = "free_x", space = "free_x")+
  scale_shape_manual(legend_title,breaks=c("Young","Middle","Elderly"),values=c(21,24,22,21,24,22))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_bw()+scale_y_continuous(name="N2 Latency [ms]", limits=c(200,500))+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family="Times", size= 18, colour = "black"),
        axis.text.x = element_text(family = "Times", size = 16, colour="black"), 
        axis.text.y = element_text(family = "Times", size = 16, colour="black"), 
        axis.title.y = element_text(family = "Times", size = 16), 
        axis.title.x = element_text(family = "Times", size = 16),
        legend.text = element_text(family = "Times", size = 16), 
        legend.title= element_text(family = "Times", size = 16),
        legend.position="bottom",panel.spacing=unit(0,"lines"),
        strip.background=element_blank(),strip.placement="outside",
        strip.text.x=element_text(family = "Times", size = 18))+
  scale_x_discrete(name="",labels=c("CV","STA","CV","STA","CV","STA"))+
  guides(fill=FALSE, shape=guide_legend(override.aes=list(size=4)))

f3
ggsave(f3, filename = "Figure1b_N2.pdf", device=cairo_pdf, width=6,height=5,units="in")

### P2 Latency ### 
f5 <- ggplot(data=NB, aes(x=interaction(Analysis, Dermatome_new),y=P2_Latency))+
  geom_boxplot(aes(fill=Dermatome_new),position=position_dodge(1), outlier.shape=NA)+
  geom_point(aes(shape=Age_Group, fill=Dermatome_new),color="#333333", 
             position=position_jitterdodge(jitter.width=0.4,jitter.height=0))+
  facet_grid(~Dermatome_new, switch = "x", scales = "free_x", space = "free_x")+
  scale_shape_manual(legend_title,breaks=c("Young","Middle","Elderly"),values=c(21,24,22,21,24,22))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_bw()+scale_y_continuous(name="P2 Latency [ms]", limits=c(300,700))+
  theme(panel.border=element_blank(), panel.grid.major=element_blank(), 
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black"),
        plot.title = element_text(family="Times", size= 18, colour = "black"),
        axis.text.x = element_text(family = "Times", size = 16, colour="black"), 
        axis.text.y = element_text(family = "Times", size = 16, colour="black"), 
        axis.title.y = element_text(family = "Times", size = 16), 
        axis.title.x = element_text(family = "Times", size = 16),
        legend.text = element_text(family = "Times", size = 16), 
        legend.title= element_text(family = "Times", size = 16),
        legend.position="bottom",panel.spacing=unit(0,"lines"),
        strip.background=element_blank(),strip.placement="outside",
        strip.text.x=element_text(family = "Times", size = 18))+
  scale_x_discrete(name="",labels=c("CV","STA","CV","STA","CV","STA"))+
  guides(fill=FALSE, shape=guide_legend(override.aes=list(size=4)))
f5

ggsave(f5, filename = "Figure1c_P2.pdf", device=cairo_pdf, width=6,height=5,units="in")


