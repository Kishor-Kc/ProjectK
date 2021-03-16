library(Rmisc)
library(httr)
library(RCurl)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(data.table)
library(tidyverse)
library(Rmisc)
library(RCurl)
library(reshape2)
library(ggpubr)
library(naniar)
library(rstatix)
library(psych)

#Complete datas

ForP4 <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/CLS_complete.csv"
ForP4 <- read.csv(text = getURL(ForP4))
ForP4$X <-NULL

#for Inteferon gamma and infection history

# forp4
IFNy.P4 <- select(ForP4, EH_ID, challenge, IFNy_CEWE, Eim_MC, delta, infection_history)
IFNy.P4 <- na.omit(IFNy.P4)
IFNy.P4 <- distinct(IFNy.P4)


CompleteP3 <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/E7andP3_complete.csv"
CompleteP3<- read.csv(text = getURL(CompleteP3))
CompleteP3$X <-NULL


#for P3 for IFNy
ForP3 <- subset(CompleteP3, CompleteP3$EXP== "P3")


IFN.P3 <- select(ForP3, EH_ID, IFNy_CEWE, Eim_MC, delta)
IFN.P3 <- na.omit(IFN.P3)
IFN.P3 <- distinct(IFN.P3)


P3_complete <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_Weight%26Oocyst_complete.csv"
P3_complete <- read.csv(text = getURL(P3_complete))

P3_complete$X <- NULL

P3_design <- select(P3_complete,EH_ID, challenge_infection, infection_history)
P3_design <- na.omit(P3_design)
P3_design <- distinct(P3_design)

colnames(P3_design)[colnames(P3_design)=="challenge_infection"] <- "challenge"


IFNy.P3 <- merge(IFN.P3 , P3_design)

CompleteI <- rbind(IFNy.P3,IFNy.P4)

# change challenge
CompleteI$challenge <-as.character(CompleteI$challenge)
CompleteI$challenge[CompleteI$challenge == "E64"] <- "E.ferrisi"
CompleteI$challenge[CompleteI$challenge == "E88"] <- "E.falciformis"
CompleteI$challenge[CompleteI$challenge == "UNI"] <- "uninfected"

CompleteI$infection_history <-as.character(CompleteI$infection_history )
CompleteI$infection_history [CompleteI$infection_history  == "E139:E64"] <- "E.ferrisi:E.ferrisi"
CompleteI$infection_history [CompleteI$infection_history  == "E139:E88"] <- "E.ferrisi:E.falciformis"
CompleteI$infection_history [CompleteI$infection_history  == "E139:UNI"] <- "uninfected"

CompleteI$infection_history [CompleteI$infection_history  == "E64:E64"] <- "E.ferrisi:E.ferrisi"
CompleteI$infection_history [CompleteI$infection_history  == "E64:E88"] <- "E.ferrisi:E.falciformis"
CompleteI$infection_history [CompleteI$infection_history  == "E64:UNI"] <- "uninfected"

CompleteI$infection_history [CompleteI$infection_history == "E88:E88"] <- "E.falciformis:E.falciformis"
CompleteI$infection_history [CompleteI$infection_history == "E88:UNI"] <- "uninfected"
CompleteI$infection_history [CompleteI$infection_history == "E88:E64"] <- "E.falciformis:E.ferrisi"

CompleteI$infection_history [CompleteI$infection_history  == "Eflab:E64"] <- "E.falciformis:E.ferrisi"
CompleteI$infection_history [CompleteI$infection_history  == "Eflab:E88"] <- "E.falciformis:E.falciformis"
CompleteI$infection_history [CompleteI$infection_history  == "Eflab:UNI"] <- "uninfected"

CompleteI$infection_history [CompleteI$infection_history  == "UNI:UNI"] <- "uninfected"
CompleteI$infection_history [CompleteI$infection_history  == "UNI:E88"] <- "E.falciformis"
CompleteI$infection_history [CompleteI$infection_history  == "UNI:E64"] <- "E.ferrisi"

#change Eim_MC
CompleteI$Eim_MC <- as.character(CompleteI$Eim_MC)
CompleteI$Eim_MC[CompleteI$Eim_MC == "pos"] <- "infected"
CompleteI$Eim_MC[CompleteI$Eim_MC == "neg"] <- "uninfected"

# change some uninfected to infected
LM_0354 <- subset(CompleteI, CompleteI$EH_ID == "LM_0354")
LM_0354$Eim_MC[LM_0354$Eim_MC == "uninfected"] <- "infected" 

LM_0334 <- subset(CompleteI, CompleteI$EH_ID == "LM_0334")
LM_0334$Eim_MC[LM_0334$Eim_MC == "infected"] <- "uninfected" 

LM_0335 <- subset(CompleteI, CompleteI$EH_ID == "LM_0335")
LM_0335$Eim_MC[LM_0335$Eim_MC == "infected"] <- "uninfected" 

LM_0337 <- subset(CompleteI, CompleteI$EH_ID == "LM_0337")
LM_0337$Eim_MC[LM_0337$Eim_MC == "infected"] <- "uninfected"

LM_0340 <- subset(CompleteI, CompleteI$EH_ID == "LM_0340")
LM_0340$Eim_MC[LM_0340$Eim_MC == "infected"] <- "uninfected" 

LM_0346 <- subset(CompleteI, CompleteI$EH_ID == "LM_0346")
LM_0346$Eim_MC[LM_0346$Eim_MC == "infected"] <- "uninfected" 

IFNyx <- rbind(CompleteI, LM_0354, LM_0334, LM_0335, LM_0337, LM_0340, LM_0346 )
IFNy <- IFNyx[-c(21),]

IFNy <- IFNy[-c(3),]
IFNy <- IFNy[-c(3),]
IFNy <- IFNy[-c(4),]
IFNy <- IFNy[-c(6),]

IFNy <- IFNy[-c(11),]

#select only infected
IFNy.inf <- subset(IFNy, IFNy$Eim_MC == "infected")
IFNy.uninf <- subset(IFNy, IFNy$Eim_MC == "uninfected")

shapiro.test(IFNy.inf$IFNy_CEWE)
shapiro.test(IFNy.uninf$IFNy_CEWE)

yfer <- subset(IFNy.inf, IFNy.inf$challenge=="E.ferrisi")
shapiro.test(yfer$IFNy_CEWE)

yfal <- subset(IFNy.inf, IFNy.inf$challenge=="E.falciformis")
shapiro.test(yfal$IFNy_CEWE)


ggscatter(IFNy.inf, x = "delta", y = "IFNy_CEWE", add = "reg.line", color = "challenge") +
  facet_grid(~challenge) +
  labs(y = "IFNγ (pg/mL)", x = "Infection Intensity", color = "E.falciformis") +
  stat_cor(method = "spearman",label.x = - 5, label.y = 450)+
  ylim(0, 910)+
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))+
  ggtitle("IFNγ distribution according to infection intensity")

#####


describeBy(IFNy.inf$IFNy_CEWE, IFNy.inf$challenge)
A <- wilcox_test(IFNy_CEWE ~ challenge, data = IFNy.inf, exact = FALSE)

uninfIFN <- subset(IFNy, IFNy$infection_history=="uninfected")
describeBy(uninfIFN$IFNy_CEWE, uninfIFN$infection_history)
B <- wilcox_test(IFNy_CEWE ~ infection_history, data = uninfIFN, exact = FALSE)

describeBy(IFNy$IFNy_CEWE, IFNy$challenge)
C <- wilcox_test(IFNy_CEWE ~ Eim_MC, data = IFNy, exact = FALSE)

D <- subset(IFNy.inf,IFNy.inf$challenge=="E.ferrisi")
E <- wilcox_test(IFNy_CEWE ~ challenge, data = D, exact = FALSE)

wilcox.test(IFNy_CEWE ~ challenge, data = IFNy, exact = FALSE)

ggscatter(IFNy, x = "delta", y = "IFNy_CEWE", add = "reg.line", color = "Eim_MC") +
  facet_grid(~Eim_MC) +
  labs(y = "IFNγ (pg/mL)", x = "Infection Intensity") +
  stat_cor(method = "spearman",label.x = - 5, label.y = 450)+
  ylim(0, 910) +
  ggtitle("IFNγ distribution according to infection intensity")



ggplot(IFNy.inf, aes(y=delta, x= challenge, color= challenge))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Eimeria species") + ylab("Infection intensity")+ 
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                               values=c("red", "blue", "darkgreen"))

ggplot(IFNy, aes(x=infection_history, y= IFNy_CEWE, color= infection_history, main = "Infected" ))+
  geom_boxplot()+
  geom_jitter() +
ylab("IFNγ(pg/mL) from cecum wall") + xlab("Infection History")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_comparisons1, method = "wilcox.test",size = 3, label.y.npc =0.95)

my_comparisons <- list(c("E.falciformis","E.falciformis:E.falciformis"),
                       c("E.falciformis:E.ferrisi", "E.ferrisi:E.falciformis"),
                       c("E.ferrisi","E.ferrisi:E.ferrisi") )

my_comparisons1 <- list(c("E.falciformis","E.falciformis:E.falciformis"),
                       c("E.falciformis", "E.falciformis:E.ferrisi"),
                       c("E.ferrisi","E.ferrisi:E.ferrisi"),
                       c("E.ferrisi","E.ferrisi:E.falciformis"),
                       c("E.falciformis:E.ferrisi","E.ferrisi:E.falciformis"))


ggplot(IFNy.inf, aes(x=infection_history, y= delta, color= infection_history, main = "Infected" ))+
  geom_boxplot()+
  geom_jitter() +
  ggtitle("Infection intensities according to infection history") +ylab("Infection Intensity") + xlab("Infection History")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_comparisons1, method = "wilcox.test",size = 3, label.y.npc =0.95)


INFfalci <- subset(IFNy, IFNy$challenge=="E.falciformis")


my_compari <- list(c("E.falciformis","E.falciformis:E.falciformis"),
                       c("E.falciformis", "E.ferrisi:E.falciformis"),
                       c("E.falciformis:E.falciformis","E.ferrisi:E.falciformis") )

ggplot(INFfalci, aes(x=infection_history, y= IFNy_CEWE, color= infection_history))+
  geom_boxplot()+
  geom_jitter() +
  ggtitle("Distribution of IFNγ for E.falciformis") +ylab("IFNγ(pg/mL)") + xlab("Infection History")+
  theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_compari, method = "wilcox.test",size = 3, label.y.npc =0.95)

ggplot(INFfalci, aes(x=infection_history, y= delta, color= infection_history))+
  geom_boxplot()+
  geom_jitter() +
  ggtitle("Infection intensity for E. falciformis according to infection history") + ylab("Infection intensity") + xlab("Infection history")+
  theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_compari, method = "wilcox.test",size = 3, label.y.npc =0.95)


FerCTmean <- subset(IFNy.inf, IFNy.inf$challenge=="E.ferrisi")
mean(FerCTmean$delta)
mean(FerCTmean$IFNy_CEWE)

FalCTmean <- subset(IFNy.inf, IFNy.inf$challenge=="E.falciformis")
mean(FalCTmean$delta)
mean(FalCTmean$IFNy_CEWE)

##

IT1<- subset(IFNy.inf, IFNy.inf$infection_history=="E.falciformis")
IT2<- subset(IFNy.inf, IFNy.inf$infection_history=="E.falciformis:E.falciformis")
IT <- rbind(IT1, IT2)

describeBy(IT$IFNy_CEWE, IT$infection_history)
wilcox_test(IFNy_CEWE ~ infection_history, data = IT, exact = FALSE)

#hetero

IxT1<- subset(IFNy.inf, IFNy.inf$infection_history=="E.falciformis")
IxT2<- subset(IFNy.inf, IFNy.inf$infection_history=="E.falciformis:E.ferrisi")
IxT <- rbind(IxT1, IxT2)

describeBy(IxT$IFNy_CEWE, IxT$infection_history)
wilcox_test(IFNy_CEWE ~ infection_history, data = IxT, exact = FALSE)




# for wloss and oocyst

Complete <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/CLS_complete.csv"
Complete<- read.csv(text = getURL(Complete))
Complete$X <-NULL

#change primary
Complete$primary <-as.character(Complete$primary)
Complete$primary[Complete$primary == "E139"] <- "E.ferrisi"
Complete$primary[Complete$primary == "E64"] <- "E.ferrisi"
Complete$primary[Complete$primary == "E88"] <- "E.falciformis"
Complete$primary[Complete$primary == "Eflab"] <- "E.falciformis"
Complete$primary[Complete$primary == "UNI"] <- "uninfected"

#change challenge
Complete$challenge <-as.character(Complete$challenge)
Complete$challenge[Complete$challenge == "E64"] <- "E.ferrisi"
Complete$challenge[Complete$challenge == "E88"] <- "E.falciformis"
Complete$challenge[Complete$challenge == "UNI"] <- "uninfected"

# for weightloss


Wloss_Primary <- select(Complete,labels, EH_ID, dpi,relative_weight, primary, infection_history)
Wloss_Primary <- na.omit(Wloss_Primary)
Wloss_Primary <- distinct(Wloss_Primary)

# for primary

Wloss_Primary$infection_history <-as.character(Wloss_Primary$infection_history )
Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "E139:E64"] <- "E.ferrisi"
Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "E139:E88"] <- "E.ferrisi"
Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "E139:UNI"] <- "E.ferrisi"

Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "E64:E64"] <- "E.ferrisi"
Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "E64:E88"] <- "E.ferrisi"
Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "E64:UNI"] <- "E.ferrisi"

Wloss_Primary$infection_history [Wloss_Primary$infection_history == "E88:E88"] <- "E.falciformis"
Wloss_Primary$infection_history [Wloss_Primary$infection_history == "E88:UNI"] <- "E.falciformis"
Wloss_Primary$infection_history [Wloss_Primary$infection_history == "E88:E64"] <- "E.falciformis"

Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "Eflab:E64"] <- "E.falciformis"
Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "Eflab:E88"] <- "E.falciformis"
Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "Eflab:UNI"] <- "E.falciformis"

Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "UNI:UNI"] <- "uninfected"
Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "UNI:E88"] <- "uninfected"
Wloss_Primary$infection_history [Wloss_Primary$infection_history  == "UNI:E64"] <- "uninfected"

Wloss_Primary <- Wloss_Primary[-c(195),]

Wloss_Challenge <- select(Complete,labels, EH_ID, dpi,relative_weight, challenge, infection_history)
Wloss_Challenge <- na.omit(Wloss_Challenge)
Wloss_Challenge <- distinct(Wloss_Challenge)

#change challenge
Wloss_Challenge$challenge <-as.character(Wloss_Challenge$challenge)
Wloss_Challenge$challenge[Wloss_Challenge$challenge == "E64"] <- "E.ferrisi"
Wloss_Challenge$challenge[Wloss_Challenge$challenge == "E88"] <- "E.falciformis"
Wloss_Challenge$challenge[Wloss_Challenge$challenge == "UNI"] <- "uninfected"

Wloss_Challenge$infection_history <-as.character(Wloss_Challenge$infection_history )
Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "E139:E64"] <- "E.ferrisi:E.ferrisi"
Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "E139:E88"] <- "E.ferrisi:E.falciformis"
Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "E139:UNI"] <- "uninfected"

Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "E64:E64"] <- "E.ferrisi:E.ferrisi"
Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "E64:E88"] <- "E.ferrisi:E.falciformis"
Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "E64:UNI"] <- "uninfected"

Wloss_Challenge$infection_history [Wloss_Challenge$infection_history == "E88:E88"] <- "E.falciformis:E.falciformis"
Wloss_Challenge$infection_history [Wloss_Challenge$infection_history == "E88:UNI"] <- "uninfected"
Wloss_Challenge$infection_history [Wloss_Challenge$infection_history == "E88:E64"] <- "E.falciformis:E.ferrisi"

Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "Eflab:E64"] <- "E.falciformis:E.ferrisi"
Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "Eflab:E88"] <- "E.falciformis:E.falciformis"
Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "Eflab:UNI"] <- "uninfected"

Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "UNI:UNI"] <- "uninfected"
Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "UNI:E88"] <- "E.falciformis"
Wloss_Challenge$infection_history [Wloss_Challenge$infection_history  == "UNI:E64"] <- "E.ferrisi"

# maximalweightloss

Wloss_MaxPr <- Wloss_Primary  %>%
  group_by(EH_ID, infection_history) %>%
  summarise(min = min(relative_weight, na.rm=TRUE))

Wloss_MaxPr <- na.omit( Wloss_MaxPr)
Wloss_MaxPr <- distinct( Wloss_MaxPr)


# calculate for secondary maximalwlosss

Wloss_MaxCh<- Wloss_Challenge  %>%
  group_by(EH_ID, infection_history) %>%
  summarise(min = min(relative_weight, na.rm=TRUE))


Wloss_MaxCh <- na.omit( Wloss_MaxCh)
Wloss_MaxCh <- distinct( Wloss_MaxCh)

Wloss_Max <- rbind( Wloss_MaxPr, Wloss_MaxCh)

# for primary wloss


#Wprfalci <- subset(Wloss_Primary, Wloss_Primary$primary=="E.falciformis")
#Wprfer <- subset(Wloss_Primary, Wloss_Primary$primary=="E.ferrisi")
#Wpruni <- subset(Wloss_Primary, Wloss_Primary$primary=="uninfected")

#shapiro.test(Wprfalci$relative_weight)
#shapiro.test(Wprfer$relative_weight)
#shapiro.test(Wpruni$relative_weight)
#shapiro.test(Wloss_Challenge$relative_weight)

ggplot(Wloss_Primary, aes(x = dpi, y = relative_weight, group = primary, col = primary)) +
  geom_smooth(se = FALSE)+
  geom_jitter()+
  theme_bw() +
  ggtitle("Weightloss during primary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))

ggplot(Wloss_Primary, aes(x = dpi, y = relative_weight, col = primary)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~primary) +
  scale_x_continuous(breaks=c(1:11), labels=c(1:11),limits=c(1,11))+
  ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))


#
WxT<- subset(Wloss_Primary, Wloss_Primary$primary=="E.ferrisi")
WxT2<- subset(Wloss_Primary, Wloss_Primary$primary=="E.falciformis")


describeBy(WxT$relative_weight, WxT$primary)

 wilcox_test(relative_weight ~ primary, data = WxT, exact = FALSE)


#test
WprT1<- subset(Wloss_Primary, Wloss_Primary$primary=="E.ferrisi")
WprT2<- subset(Wloss_Primary, Wloss_Primary$primary=="E.falciformis")
WprT <- rbind(WprT1, WprT2)

describeBy(WprT$relative_weight, WprT$primary)

WprTR <- wilcox_test(relative_weight ~ primary, data = WprT, exact = FALSE)
 
#test for sec
WscT1<- subset(Wloss_Challenge, Wloss_Challenge$challenge=="E.ferrisi")
WscT2<- subset(Wloss_Challenge, Wloss_Challenge$challenge=="E.falciformis")
WscT <- rbind(WscT1, WscT2)

describeBy(WscT$relative_weight, WscT$challenge)

WscTR <- wilcox_test(relative_weight ~ challenge, data = WscT, exact = FALSE)
WscTR$p

WsciT1<- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.ferrisi")
WsciT2<- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.ferrisi:E.ferrisi")
WsciT <- rbind(WsciT1, WsciT2)

describeBy(WsciT$relative_weight, WsciT$infection_history)

wilcox_test(relative_weight ~ infection_history, data = WsciT, exact = FALSE)
#hwtero
WscihT1<- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.ferrisi")
WscihT2<- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.ferrisi:E.falciformis")
WscihT <- rbind(WscihT1, WscihT2)

describeBy(WscihT$relative_weight, WscihT$infection_history)

wilcox_test(relative_weight ~ infection_history, data = WscihT, exact = FALSE)


WscifT1<- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.falciformis")
WscifT2<- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.falciformis:E.falciformis")
WscifT <- rbind(WscifT1, WscifT2)

describeBy(WscifT$relative_weight, WscifT$infection_history)

wilcox_test(relative_weight ~ infection_history, data = WscifT, exact = FALSE)
#hetero
WscifxT1<- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.falciformis")
WscifxT2<- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.falciformis:E.ferrisi")
WscifxT <- rbind(WscifxT1, WscifxT2)

describeBy(WscifxT$relative_weight, WscifxT$infection_history)
wilcox_test(relative_weight ~ infection_history, data = WscifxT, exact = FALSE)
#

shapiro.test(Wloss_Primary$relative_weight)
shapiro.test(Wloss_Challenge$relative_weight)
                   
# for secondary wloss

ggplot(Wloss_Challenge, aes(x = dpi, y = relative_weight, col = challenge)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~challenge) +
  scale_x_continuous(breaks=c(1:8), labels=c(1:8),limits=c(1,8))+
ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))

#try new table for secondary wloss

Wloss_sec1 <- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.falciformis:E.ferrisi")
Wloss_sec2 <- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.ferrisi:E.ferrisi")
Wloss_sec3 <- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.ferrisi:E.falciformis")
Wloss_sec4 <- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.falciformis:E.falciformis")
Wloss_sec5 <- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="uninfected")

Wloss_Sec <- rbind(Wloss_sec1, Wloss_sec2, Wloss_sec3, Wloss_sec4, Wloss_sec5)

ggplot(Wloss_Sec, aes(x = dpi, y = relative_weight, col = challenge)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~challenge) +
  scale_x_continuous(breaks=c(1:8), labels=c(1:8),limits=c(1,8))+
  ggtitle("Weightloss during secondary infection") +ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))





#
Wscfalci <- subset(Wloss_Sec, Wloss_Sec$challenge=="E.falciformis")
shapiro.test(Wscfalci$relative_weight)

Wscfer <- subset(Wloss_Sec, Wloss_Sec$challenge=="E.ferrisi")
shapiro.test(Wscfer$relative_weight)

# try new table for primary

Wloss_pr1 <- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.falciformis")
Wloss_pr2 <- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.ferrisi")


Wloss_pr3 <- rbind(Wloss_pr1, Wloss_pr2)

colnames(Wloss_pr3)[colnames(Wloss_pr3)=="challenge"] <- "primary"

Wloss_PR <-rbind(Wloss_Primary, Wloss_pr3)

#test

WpT1<- subset(Wloss_PR, Wloss_PR$primary=="E.ferrisi")
WpT2<- subset(Wloss_PR, Wloss_PR$primary=="E.falciformis")
WpT <- rbind(WpT1, WpT2)

describeBy(WpT$relative_weight, WpT$primary)

WpTR <- wilcox_test(relative_weight ~ primary, data = WpT, exact = FALSE)

ggplot(Wloss_PR, aes(x = dpi, y = relative_weight, col = primary)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~primary) +
  ggtitle("Weightloss during primary infection") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))

wprM <- subset(Wloss_PR, Wloss_PR$primary=="E.ferrisi")
mean(wprM$relative_weight)

wprMf <- subset(Wloss_PR, Wloss_PR$primary=="E.falciformis")
mean(wprMf$relative_weight)

mean(Wloss_Primary$relative_weight)

wprMx <- subset(Wloss_Primary, Wloss_Primary$primary=="E.ferrisi")
mean(wprMx$relative_weight)

wprMfx <- subset(Wloss_Primary, Wloss_Primary$primary=="E.falciformis")
mean(wprMfx$relative_weight)

mean(Wloss_Challenge$relative_weight)

wprMxx <- subset(Wloss_Ch, Wloss_PR$primary=="E.ferrisi")
mean(wprMxx$relative_weight)

wprMfxx <- subset(Wloss_PR, Wloss_PR$primary=="E.falciformis")
mean(wprMfxx$relative_weight)
# for primary and sec wloss regarding inf history

ggplot(Wloss_Challenge, aes(x = dpi, y = relative_weight, col = infection_history)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~infection_history) + 
  ylab("Weightloss in percentage") + xlab("Day post infection (dpi)")

wscM <- subset(Wloss_Challenge, Wloss_Challenge$challenge=="E.ferrisi")
mean(wscM$relative_weight)
wscMf <- subset(Wloss_Challenge, Wloss_Challenge$challenge=="E.falciformis")
mean(wscMf$relative_weight)

wscmu <- subset(Wloss_Challenge,Wloss_Challenge$infection_history=="uninfected")
mean(wscmu$relative_weight)
A6 <- subset(Wloss_PR, Wloss_PR$primary=="uninfected")
mean(A6$relative_weight)

A7 <- subset(Wloss_Challenge, Wloss_Challenge$challenge=="uninfected")
mean(A7$relative_weight)

mean(Wloss_Primary$relative_weight)
mean(Wloss_Challenge$relative_weight)

A1 <-  subset(Wloss_Challenge,Wloss_Challenge$infection_history=="E.ferrisi:E.ferrisi")
mean(A1$relative_weight)
A2 <-  subset(Wloss_Challenge,Wloss_Challenge$infection_history=="E.falciformis:E.falciformis")
mean(A2$relative_weight)
A3 <- rbind(A1,A2)

A4 <-  subset(Wloss_Challenge,Wloss_Challenge$infection_history=="E.ferrisi:E.falciformis")
mean(A4$relative_weight)

A5 <-  subset(Wloss_Challenge,Wloss_Challenge$infection_history=="E.falciformis:E.ferrisi")
mean(A5$relative_weight)

A6 <- rbind(A4,A5)

mean(A6$relative_weight)


my_comparisons <- list(c("E.falciformis","E.falciformis:E.falciformis"),
                       c("E.falciformis:E.ferrisi", "E.ferrisi:E.falciformis"),
                       c("E.ferrisi:E.ferrisi","E.ferrisi") )




ggplot(Wloss_Max, aes(x = infection_history, y = min, color = infection_history)) + 
  geom_boxplot() + 
  geom_point(size=0.1)+
  geom_jitter() +
 ylab("Maximal for weight loss") + xlab("Infection History")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_comparisons1, method = "wilcox.test",size = 3, label.y.npc =0.95)


WlossX <- select(Wloss_PR, EH_ID, primary)
WlossX <- distinct(WlossX)

WlossY <- select(Wloss_Challenge, EH_ID, challenge)
WlossY <- distinct(WlossY)

WlossXY <- merge(WlossX, WlossY)
WlossXYZ <- merge(Wloss_Max, WlossXY)

ggplot(WlossXYZ, aes(x = infection_history, y = min, color = infection_history)) + 
  geom_boxplot() + 
  geom_point(size=0.1)+
  geom_jitter() +
  ggtitle("Maximal for weight loss for primary and secondary infection") +ylab("Maximal for weight loss") + xlab("Infection History")+ 
  theme(axis.text.x = element_text(angle = 12, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_comparisons1, method = "wilcox.test",size = 3, label.y.npc =0.95)


Wihfalci <- subset(WlossXYZ, WlossXYZ$infection_history=="E.falciformis:E.falciformis")
shapiro.test(Wihfalci$min)




shapiro.test(Wloss_Max$min)
ggdensity(Wloss_Max$min, 
          main = "Density plot of tooth length",
          xlab = "Weightloss")


# for oocyst


OPG_Primary <- select(Complete,labels, EH_ID, dpi,OPG, primary, infection_history)
OPG_Primary <- na.omit(OPG_Primary)
OPG_Primary <- distinct(OPG_Primary)

# cahnge for primary

OPG_Primary$primary <-as.character(OPG_Primary$primary)
OPG_Primary$primary[OPG_Primary$primary == "E64"] <- "E.ferrisi"
OPG_Primary$primary[OPG_Primary$primary == "E139"] <- "E.ferrisi"
OPG_Primary$primary[OPG_Primary$primary == "E88"] <- "E.falciformis"
OPG_Primary$primary[OPG_Primary$primary == "Eflab"] <- "E.falciformis"
OPG_Primary$primary[OPG_Primary$primary == "UNI"] <- "uninected"

# for primary

OPG_Primary$infection_history <-as.character(OPG_Primary$infection_history )
OPG_Primary$infection_history [OPG_Primary$infection_history  == "E139:E64"] <- "E.ferrisi"
OPG_Primary$infection_history [OPG_Primary$infection_history  == "E139:E88"] <- "E.ferrisi"
OPG_Primary$infection_history [OPG_Primary$infection_history  == "E139:UNI"] <- "E.ferrisi"

OPG_Primary$infection_history [OPG_Primary$infection_history  == "E64:E64"] <- "E.ferrisi"
OPG_Primary$infection_history [OPG_Primary$infection_history  == "E64:E88"] <- "E.ferrisi"
OPG_Primary$infection_history [OPG_Primary$infection_history  == "E64:UNI"] <- "E.ferrisi"

OPG_Primary$infection_history [OPG_Primary$infection_history == "E88:E88"] <- "E.falciformis"
OPG_Primary$infection_history [OPG_Primary$infection_history == "E88:UNI"] <- "E.falciformis"
OPG_Primary$infection_history [OPG_Primary$infection_history == "E88:E64"] <- "E.falciformis"

OPG_Primary$infection_history [OPG_Primary$infection_history  == "Eflab:E64"] <- "E.falciformis"
OPG_Primary$infection_history [OPG_Primary$infection_history  == "Eflab:E88"] <- "E.falciformis"
OPG_Primary$infection_history [OPG_Primary$infection_history  == "Eflab:UNI"] <- "E.falciformis"

OPG_Primary$infection_history [OPG_Primary$infection_history  == "UNI:UNI"] <- "uninfected"
OPG_Primary$infection_history [OPG_Primary$infection_history  == "UNI:E88"] <- "uninfected"
OPG_Primary$infection_history [OPG_Primary$infection_history  == "UNI:E64"] <- "uninfected"



# for OPG challenge

OPG_Challenge <- select(Complete,labels, EH_ID, dpi,OPG, challenge, infection_history)
OPG_Challenge <- na.omit(OPG_Challenge)
OPG_Challenge <- distinct(OPG_Challenge)


Wloss_Challenge$challenge <-as.character(Wloss_Challenge$challenge)
Wloss_Challenge$challenge[Wloss_Challenge$challenge == "E64"] <- "E.ferrisi"
Wloss_Challenge$challenge[Wloss_Challenge$challenge == "E88"] <- "E.falciformis"
Wloss_Challenge$challenge[Wloss_Challenge$challenge == "UNI"] <- "uninfected"



OPG_Challenge$infection_history <-as.character(OPG_Challenge$infection_history )
OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "E139:E64"] <- "E.ferrisi:E.ferrisi"
OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "E139:E88"] <- "E.ferrisi:E.falciformis"
OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "E139:UNI"] <- "uninfected"

OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "E64:E64"] <- "E.ferrisi:E.ferrisi"
OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "E64:E88"] <- "E.ferrisi:E.falciformis"
OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "E64:UNI"] <- "uninfected"

OPG_Challenge$infection_history [OPG_Challenge$infection_history == "E88:E88"] <- "E.falciformis:E.falciformis"
OPG_Challenge$infection_history [OPG_Challenge$infection_history == "E88:UNI"] <- "uninfected"
OPG_Challenge$infection_history [OPG_Challenge$infection_history == "E88:E64"] <- "E.falciformis:E.ferrisi"

OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "Eflab:E64"] <- "E.falciformis:E.ferrisi"
OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "Eflab:E88"] <- "E.falciformis:E.falciformis"
OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "Eflab:UNI"] <- "uninfected"

OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "UNI:UNI"] <- "uninfected"
OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "UNI:E88"] <- "E.falciformis"
OPG_Challenge$infection_history [OPG_Challenge$infection_history  == "UNI:E64"] <- "E.ferrisi"



shapiro.test(OPG_Primary$OPG)
ggdensity(OPG_Primary$OPG, 
          main = "Density plot of tooth length",
          xlab = "Weightloss")

shapiro.test(OPG_Challenge$OPG)

OpT1<- subset(OPG_Primary, OPG_Primary$primary=="E.ferrisi")
OpT2<- subset(OPG_Primary, OPG_Primary$primary=="E.falciformis")
OpT <- rbind(OpT1, OpT2)

describeBy(OpT$OPG, OpT$primary)

OpTR <- wilcox_test(OPG ~ primary, data = OpT, exact = FALSE)


OsT1<- subset(OPG_Challenge, OPG_Challenge$challenge=="E.ferrisi")

OsT2<- subset(OPG_Challenge, OPG_Challenge$challenge=="E.falciformis")
OsT <- rbind(OsT1, OsT2)

describeBy(OsT$OPG, OsT$challenge)

OsTR <- wilcox_test(OPG ~ challenge, data = OsT, exact = FALSE)

#####TEST

OhoT1<- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.ferrisi")
OhoT2<- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.ferrisi:E.ferrisi")
OhoT <- rbind(OhoT1, OhoT2)

describeBy(OhoT$OPG, OhoT$infection_history)

wilcox_test(OPG ~ infection_history, data = OhoT, exact = FALSE)
#hwtero
OhoxT1<- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.ferrisi")
OhoxT2<- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.ferrisi:E.falciformis")
OhoxT <- rbind(OhoxT1, OhoxT2)

describeBy(OhoxT$OPG, OhoxT$infection_history)

wilcox_test(OPG ~ infection_history, data = OhoxT, exact = FALSE)


OheT1<- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.falciformis")
OheT2<- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.falciformis:E.falciformis")
OheT <- rbind(OheT1, OheT2)

describeBy(OheT$OPG, OheT$infection_history)

wilcox_test(OPG ~ infection_history, data = OheT, exact = FALSE)
#hetero
OhexT1<- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.falciformis")
OhexT2<- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.falciformis:E.ferrisi")
OhexT <- rbind(OhexT1, OhexT2)

describeBy(OhexT$OPG, OhexT$infection_history)

wilcox_test(OPG ~ infection_history, data = OhexT, exact = FALSE)

# maximalweightloss

OPG_MaxPr<- OPG_Primary  %>%
  group_by(EH_ID, infection_history) %>%
  summarise(sum = sum(OPG, na.rm=TRUE))


OPG_MaxPr <- na.omit(OPG_MaxPr)
OPG_MaxPr <- distinct(OPG_MaxPr)




# calculate for secondary maximalwlosss


OPG_MaxCh<- OPG_Challenge  %>%
  group_by(EH_ID, infection_history) %>%
  summarise(sum = sum(OPG, na.rm=TRUE))


OPG_MaxCh <- na.omit(OPG_MaxCh)
OPG_MaxCh <- distinct(OPG_MaxCh)

OPG_Max <- rbind(OPG_MaxPr, OPG_MaxCh) 


# for primary OPG

ggplot(OPG_Primary, aes(x = dpi, y = OPG, col = primary)) +
  geom_smooth()+
  geom_point(size=1)+
  theme_bw()+
  facet_grid(~primary) +
  ylim(0,2500000) +
  scale_x_continuous(breaks=c(1:11), labels=c(1:11),limits=c(1,11))+
  ggtitle("Oocyst shedding for primary infection ") +ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c ("E. falciformis","E. ferrisi","uninfected"),
                     values=c ("red", "blue", "darkgreen"))

# for sec OPG


ggplot(OPG_Challenge, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=1)+
  theme_bw()+
  facet_grid(~challenge) +
  ylim(0,2500000) +
  scale_x_continuous(breaks=c(1:8), labels=c(1:8),limits=c(1,8))+
  ggtitle("Oocyst shedding for secondary infection ") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)")+
  scale_color_manual(breaks = c("E. falciformis","E. ferrisi","uninfected"),
                     values=c("red", "blue", "darkgreen"))

Oprfer <- subset(OPG_Primary,OPG_Primary$primary=="E.ferrisi")
shapiro.test(Oprfer$OPG)
Oprfal <- subset(OPG_Primary,OPG_Primary$primary=="E.falciformis")
shapiro.test(Oprfal$OPG)

Oscfer <- subset(OPG_Challenge,OPG_Challenge$challenge=="E.ferrisi")
shapiro.test(Oscfer$OPG)

Oscfal <- subset(OPG_Challenge,OPG_Challenge$challenge=="E.falciformis")
shapiro.test(Oscfal$OPG)


#try new table for secondary wloss

OPG_ch1 <- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.falciformis:E.ferrisi")
OPG_ch2 <- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.ferrisi:E.ferrisi")
OPG_ch3 <- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.ferrisi:E.falciformis")
OPG_ch4 <- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.falciformis:E.falciformis")
OPG_ch5 <- subset(OPG_Challenge, OPG_Challenge$infection_history=="uninfected")

OPG_Sec <- rbind(OPG_ch1,OPG_ch2,OPG_ch3, OPG_ch4, OPG_ch5)

ggplot(OPG_Sec, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~challenge) +
  ylim(0,2500000) +
  scale_x_continuous(breaks=c(1:8), labels=c(1:8),limits=c(1,8))+
  ggtitle("Oocyst shedding during secondary infection") +ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E. falciformis","E. ferrisi","uninfected"),
                     values=c("red", "blue", "darkgreen"))


OPGsfalci <- subset(OPG_Sec, OPG_Sec$challenge=="E.falciformis")
shapiro.test(OPGsfalci$OPG)

OPGsfer <- subset(OPG_Sec, OPG_Sec$challenge=="E.ferrisi")
shapiro.test(OPGsfer$OPG)



OPGdpi3<- subset(OPG_Sec, OPG_Sec$dpi=="3")
OPGdpi4<- subset(OPG_Sec, OPG_Sec$dpi=="4")
OPGdpi5<- subset(OPG_Sec, OPG_Sec$dpi=="5")
OPGdpi6<- subset(OPG_Sec, OPG_Sec$dpi=="6")
OPGdpi7<- subset(OPG_Sec, OPG_Sec$dpi=="7")
OPGdpi8<- subset(OPG_Sec, OPG_Sec$dpi=="8")
OPG_Secall <- rbind(OPGdpi3, OPGdpi4, OPGdpi5, OPGdpi6, OPGdpi7, OPGdpi8)

ggplot(OPG_Secall, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~challenge) +
  ylim(0,3000000) +
  ggtitle("Sum of oocyst shedding during secondary infection") + ylab("Sum of OPG") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E. falciformis","E. ferrisi","uninfected"),
                     values=c("red", "blue", "darkgreen"))

# try new table for primary

OPG_pr1 <- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.falciformis")
OPG_pr2 <- subset(OPG_Challenge, OPG_Challenge$infection_history=="E.ferrisi")


OPG_pr3 <- rbind(OPG_pr1, OPG_pr2)

colnames(OPG_pr3)[colnames(OPG_pr3)=="challenge"] <- "primary"

OPG_PR <-rbind(OPG_Primary, OPG_pr3)

ggplot(OPG_PR, aes(x = dpi, y = OPG, col = primary)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~primary) +
  ylim(0,2500000) +
  scale_x_continuous(breaks=c(1:11), labels=c(1:11),limits=c(1,11))+
 ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c ("E. falciformis","E. ferrisi","uninfected"),
                     values=c ("red", "blue", "darkgreen"))

ggplot(OPG_Sec, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~challenge) +
  ylim(0,2500000) +
  scale_x_continuous(breaks=c(1:8), labels=c(1:8),limits=c(1,8))+ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c ("E. falciformis","E. ferrisi","uninfected"),
                     values=c ("red", "blue", "darkgreen"))


#for p value

my_comparisons <- list(c("E.falciformis","E.falciformis:E.falciformis"),
                       c("E.falciformis:E.ferrisi", "E.ferrisi:E.falciformis"),
                       c("E.ferrisi","E.ferrisi:E.ferrisi") )

ggplot(OPG_MaxCh, aes(x = infection_history, y = sum, color = infection_history)) + 
  geom_boxplot() + 
  geom_point(size=0.1)+
  geom_jitter() +
  ggtitle("Shedded oocysts for primary and secondary infection") + ylab("Sum of shedded oocysts (OPG)") + xlab("Infection History")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_comparisons1, method = "wilcox.test",size = 3, label.y.npc =0.95)

# for primary and sec OPG regarding inf history

ggplot(OPG_Challenge, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~infection_history)+
  ylim(0,3000000) +
  ggtitle("Oocyst shedding according to infection history") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)")


OPr <- select(OPG_Primary, EH_ID, dpi, OPG, infection_history)

O_MaxPr <- OPr  %>%
  group_by(EH_ID, infection_history) %>%
  summarise(max = max(OPG, na.rm=TRUE))

O_MaxPr <- na.omit( O_MaxPr)
O_MaxPr <- distinct( O_MaxPr)


OSc <- select(OPG_Sec, EH_ID,dpi, OPG, infection_history)

O_MaxCH <- OSc  %>%
  group_by(EH_ID, infection_history) %>%
  summarise(max = max(OPG, na.rm=TRUE))

O_MaxCH <- na.omit( O_MaxCH)
O_MaxCH <- distinct( O_MaxCH)

OPGtotal <- rbind(OPr, OSc)
OMAX <- rbind(O_MaxPr, O_MaxCH)

my_comparisons <- list(c("E.falciformis","E.falciformis:E.falciformis"),
                       c("E.falciformis:E.ferrisi", "E.ferrisi:E.falciformis"),
                       c("E.ferrisi","E.ferrisi:E.ferrisi") )

ggplot(OPGtotal, aes(x = infection_history, y = OPG, color = infection_history)) + 
  geom_boxplot() + 
  geom_point(size=0.05)+
  geom_jitter() +
  ggtitle("Shedded oocysts for primary and secondary infection") + ylab("Sum of shedded oocysts (OPG)") + xlab("Infection History")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_comparisons, method = "wilcox.test",size = 3, label.y.npc =0.95)

ggplot(OMAX, aes(x = infection_history, y = max, color = infection_history)) + 
  geom_boxplot() + 
  geom_point(size=0.1)+
  geom_jitter() +
  ylab("Maximum shedded oocysts (OPG)") + xlab("Infection History")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_comparisons1, method = "wilcox.test",size = 3, label.y.npc =0.95)



my_comparisons <- list(c("E.falciformis","E.falciformis:E.falciformis"),
                       c("E.falciformis:E.ferrisi", "E.ferrisi:E.falciformis"),
                       c("E.ferrisi","E.ferrisi:E.ferrisi") )


ggplot(OPG_Max, aes(x = infection_history, y = sum, color = infection_history)) + 
  geom_boxplot() + 
  geom_point(size=0.1)+
  geom_jitter() +
  ggtitle("Sum of shedded oocysts for primary and secondary infection") + ylab("Sum of shedded oocysts (OPG)") + xlab("Infection History")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_comparisons1, method = "wilcox.test",size = 3, label.y.npc =0.95)


#make new graphs

DeltaOPG <- merge(OPG_Max, IFNy)

InfectedOPGdelta <- subset(DeltaOPG, DeltaOPG$Eim_MC=="infected")


ggplot(InfectedOPGdelta, aes(x = delta, y = sum, col = challenge)) + 

  geom_point(size=0.1)+
  geom_jitter() +
  facet_grid(~infection_history) +
  ggtitle("Sum of oocyst shedding during infection and infection intensity") +ylab("Sum of OPG") + xlab("Infection intensity") 

InfectedOPGdelta$infection_history <-as.character(InfectedOPGdelta$infection_history )
InfectedOPGdelta$infection_history [InfectedOPGdelta$infection_history  == "E.falciformis"] <- "E.fal"
InfectedOPGdelta$infection_history [InfectedOPGdelta$infection_history  == "E.falciformis:E.falciformis"] <- "E.fal:E.fal"
InfectedOPGdelta$infection_history [InfectedOPGdelta$infection_history  == "E.falciformis:E.ferrisi"] <- "E.fal:E.fer"
InfectedOPGdelta$infection_history [InfectedOPGdelta$infection_history  == "E.ferrisi"] <- "E.fer"
InfectedOPGdelta$infection_history [InfectedOPGdelta$infection_history  == "E.ferrisi:E.ferrisi"] <- "E.fer:E.fer"
InfectedOPGdelta$infection_history [InfectedOPGdelta$infection_history  == "E.ferrisi:E.falciformis"] <- "E.fer:E.fal"

ggplot(InfectedOPGdelta, aes(x = IFNy_CEWE, y = sum, col = challenge)) + 
  geom_point(size=0.1)+
  geom_jitter() +
  facet_grid(~infection_history) +
   ylab("Sum of OPG") + xlab("IFNy") 

FalciIFN <- subset(InfectedOPGdelta, InfectedOPGdelta$challenge=="E.falciformis")

ggplot(FalciIFN, aes(x = IFNy_CEWE, y = sum, color= infection_history)) + 
  geom_boxplot() + 
  geom_point(size=0.1)+
  geom_jitter() +
  facet_grid(~infection_history) +
  ggtitle("Sum of OPG and distribution of IFNy for E.falciformis") + xlab("IFNy") + ylab("Sum of OPG") 

DeltaWloss <- merge(Wloss_Max, IFNy)

InfWlossdelta <- subset(DeltaWloss, DeltaWloss$Eim_MC=="infected")


ggplot(InfWlossdelta, aes(x = delta, y = min, col = challenge)) + 
  geom_point(size=0.1)+
  geom_jitter() +
  facet_grid(~infection_history) +
  ggtitle("Weight loss and infection intensity ") + ylab("Maximum Weightloss") + xlab("Delta") 


ggplot(InfWlossdelta, aes(x = IFNy_CEWE, y = min, col = challenge)) + 
 geom_boxplot()+
  geom_point(size=0.1)+
  geom_jitter() +
  facet_grid(~infection_history) +
   ylab("Max Wloss") + xlab("IFNy") 


ggplot(InfWlossdelta, aes(y = IFNy_CEWE, x = min, col = challenge)) + 
  geom_boxplot()+
  geom_point(size=0.1)+
  geom_jitter() +
  facet_grid(~infection_history) +
  ggtitle("Maximal weightloss and IFNy") + xlab("Max Wloss)") + ylab("IFNy") 

InfWlossdeltaFalci <- subset(InfWlossdelta, InfWlossdelta$challenge=="E.falciformis")

ggplot(InfWlossdeltaFalci, aes(y = IFNy_CEWE, x = min, col = challenge)) + 
 geom_boxplot()+
  geom_point(size=0.1)+
  geom_jitter() +
  facet_grid(~infection_history) +
  ggtitle("Relation of IFNy and Maximal weightloss") + xlab("Maximal Weightloss") + ylab("IFNy") 



#deltapos <- as.numeric(pos$delta)
#deltaneg <- as.numeric(neg$delta)

#cor.test(IFNpos, deltapos, method = "kendall")
#cor.test(IFNneg, deltaneg, method = "kendall")

MaxWlossOPG <- "https://raw.githubusercontent.com/Kishor-Kc/ProjectK/master/MaxWlossOPG.csv"
MaxWlossOPG <- read.csv(text = getURL(MaxWlossOPG))
colnames(MaxWlossOPG)[colnames(MaxWlossOPG)=="ï..dpi"] <- "dpi"


MaxWlossOPG$Eim_sp <-as.character(MaxWlossOPG$Eim_sp)
MaxWlossOPG$Eim_sp[MaxWlossOPG$Eim_sp == "E.fer"] <- "E.ferrisi"
MaxWlossOPG$Eim_sp[MaxWlossOPG$Eim_sp== "E.fal"] <- "E.falciformis"


ggplot(MaxWlossOPG, aes(y = dpi, x = variable)) + 
  geom_point()+
  facet_grid(~Eim_sp)+
  ylab("day post infection (dpi)")

ggplot(MaxWlossOPG, aes(y = dpi, x = variable)) +
  geom_smooth()+
  geom_jitter()+
  theme_bw()+
facet_grid(~Eim_sp)

ggpaired(MaxWlossOPG, x= "variable", y="dpi",
     line.color = "gray", line.size = 0.5,
         palette = "	
Eim_sp")

 ggplot(MaxWlossOPG, aes(x = variable, y = dpi)) +
  geom_line(aes(group = Eim_sp, color = factor(variable)))+
   facet_grid(~Eim_sp)

Table <- select(Complete, EH_ID, primary, challenge, relative_weight, dpi, OPG, infection_history)


WL <- merge(Wloss_Primary, OPG_Primary)
WL1 <- merge(WL, OPG_MaxPr)
WL11 <- merge(WL1, Wloss_MaxPr)
colnames(WL11)[colnames(WL11)=="sum"] <- "SumOPG"
colnames(WL11)[colnames(WL11)=="min"] <- "MaxWloss"
colnames(WL11)[colnames(WL11)=="EH_ID"] <- "MouseID"

TxPr <- select(WL11,MouseID, primary, infection_history, SumOPG, MaxWloss )
TxPr <- distinct(TxPr)

write.table(TxPr, file = "TableforPrimary.csv", row.names = F, sep = ",")


WL2 <- merge(Wloss_Challenge, OPG_Challenge)

WL3 <- merge(WL2, IFNy)
WL31 <- merge(WL3, OPG_MaxCh)
WL32 <- merge(WL31, Wloss_MaxCh)

colnames(WL32)[colnames(WL32)=="sum"] <- "SumOPG"
colnames(WL32)[colnames(WL32)=="min"] <- "MaxWloss"
colnames(WL32)[colnames(WL32)=="EH_ID"] <- "MouseID"
colnames(WL32)[colnames(WL32)=="delta"] <- "deltaCT"

TxCh<- select(WL32, MouseID, challenge, infection_history, SumOPG, MaxWloss, IFNy_CEWE, Eim_MC, deltaCT )
TxCh <- distinct(TxCh)

write.table(TxCh, file = "TableforSecondary2.csv", row.names = F, sep = ",")




                   
                   
