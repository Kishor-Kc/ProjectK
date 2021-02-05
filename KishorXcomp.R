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
#Complete datas

Complete <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/CLS_complete.csv"
Complete<- read.csv(text = getURL(Complete))
Complete$X <-NULL

#change primary
Complete$primary <-as.character(Complete$primary)
Complete$primary[Complete$primary == "E139"] <- "E.ferrisi"
Complete$primary[Complete$primary == "E64"] <- "E.ferrisi"
Complete$primary[Complete$primary == "E88"] <- "E.falciformis"
Complete$primary[Complete$primary == "Eflab"] <- "E.falciformis"
Complete$primary[Complete$primary == "UNI"] <- "uninected"

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
  ggtitle("Weightloss during primary infection") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))


# for secondary wloss

ggplot(Wloss_Challenge, aes(x = dpi, y = relative_weight, col = challenge)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~challenge) +
  ggtitle("Weightloss during secondary infection") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") + 
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
  ggtitle("Weightloss during secondary infection") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))

# try new table for primary

Wloss_pr1 <- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.falciformis")
Wloss_pr2 <- subset(Wloss_Challenge, Wloss_Challenge$infection_history=="E.ferrisi")


Wloss_pr3 <- rbind(Wloss_pr1, Wloss_pr2)

colnames(Wloss_pr3)[colnames(Wloss_pr3)=="challenge"] <- "primary"

Wloss_PR <-rbind(Wloss_Primary, Wloss_pr3)

ggplot(Wloss_PR, aes(x = dpi, y = relative_weight, col = primary)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~primary) +
  ggtitle("Weightloss during primary infection") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))

# for primary and sec wloss regarding inf history

ggplot(Wloss_Challenge, aes(x = dpi, y = relative_weight, col = challenge)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~infection_history)

ggplot(Wloss_Max, aes(x = infection_history, y = min, color = infection_history)) + 
  geom_boxplot() + 
  geom_point(size=0.1)+
  geom_jitter() +
  ggtitle("Maximal for weight loss for primary and secondary infection") + ylab("Maximal for weight loss") + xlab("Infection History")



my_comparisons <- list(c("E.falciformis","E.falciformis:E.falciformis"),
                       c("E.falciformis:E.ferrisi", "E.ferrisi:E.falciformis"),
                       c("E.ferrisi:E.ferrisi","E.ferrisi") )




ggplot(Wloss_Max, aes(x = infection_history, y = min, color = infection_history)) + 
  geom_boxplot() + 
  geom_point(size=0.1)+
  geom_jitter() +
  ggtitle("Maximal for weight loss for primary and secondary infection") + ylab("Maximal for weight loss") + xlab("Infection History")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_comparisons, method = "wilcox.test",size = 3, label.y.npc =0.95)


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
  ylim(0,3000000) +
  ggtitle("Oocyst shedding for primary infection ") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c ("E. falciformis","E. ferrisi","uninfected"),
                     values=c ("red", "blue", "darkgreen"))

# for sec OPG

ggplot(OPG_Challenge, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=1)+
  theme_bw()+
  facet_grid(~challenge) +
  ylim(0,3000000) +
  ggtitle("Oocyst shedding for secondary infection ") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)")+
  scale_color_manual(breaks = c("E. falciformis","E. ferrisi","uninfected"),
                     values=c("red", "blue", "darkgreen"))

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
  ggtitle("Oocyst shedding for primary infection ") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c ("E. falciformis","E. ferrisi","uninfected"),
                     values=c ("red", "blue", "darkgreen"))

# for primary and sec OPG regarding inf history

ggplot(OPG_Challenge, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~infection_history)


my_comparisons <- list(c("E.falciformis","E.falciformis:E.falciformis"),
                       c("E.falciformis:E.ferrisi", "E.ferrisi:E.falciformis"),
                       c("E.ferrisi","E.ferrisi:E.ferrisi") )


ggplot(OPG_Max, aes(x = infection_history, y = sum, color = infection_history)) + 
  geom_boxplot() + 
  geom_point(size=0.1)+
  geom_jitter() +
  ggtitle("Sum of shedded oocysts for primary and secondary infection") + ylab("Sum of shedded oocysts (OPG)") + xlab("Infection History")+ 
  theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=1))+
  stat_compare_means(mapping=aes(label=..p.adj..), comparisons=my_comparisons, method = "wilcox.test",size = 3, label.y.npc =0.95)




# Infecion intensity

Inf_Int <- select(Complete, EH_ID, delta, Eim_MC, infection_history, challenge)
Inf_Int <- na.omit(Inf_Int)
Inf_Int <- distinct(Inf_Int)

Inf_Int$Eim_MC <- as.character(Inf_Int$Eim_MC)

Inf_Int$Eim_MC[Inf_Int$Eim_MC == "pos"] <- "infected"
Inf_Int$Eim_MC[Inf_Int$Eim_MC == "neg"] <- "uninfected"


# change LM_0354 uninfected to infected
LM_0354 <- subset(Inf_Int, Inf_Int$EH_ID == "LM_0354")
LM_0354$Eim_MC[LM_0354$Eim_MC == "uninfected"] <- "infected" 



DeltaCT <- rbind(Inf_Int, LM_0354)
DeltaCT <- DeltaCT[-c(5),]


DeltaCT$ infection_history <-as.character(DeltaCT$ infection_history)
DeltaCT$infection_history [DeltaCT$infection_history  == "E139:E64"] <- "E.ferrisi:E.ferrisi"
DeltaCT$infection_history [DeltaCT$infection_history  == "E139:E88"] <- "E.ferrisi:E.falciformis"
DeltaCTinfection_history [DeltaCT$infection_history  == "E139:UNI"] <- "ininfected"

DeltaCT$infection_history [DeltaCT$infection_history  == "E64:E64"] <- "E.ferrisi:E.ferrisi"
DeltaCT$infection_history [DeltaCT$infection_history  == "E64:E88"] <- "E.ferrisi:E.falciformis"
DeltaCT$infection_history [DeltaCT$infection_history  == "E64:UNI"] <- "uninfected"

DeltaCT$infection_history [DeltaCT$infection_history  == "E88:E88"] <- "E.falciformis:E.falciformis"
DeltaCT$infection_history [DeltaCT$infection_history  == "E88:UNI"] <- "uninfected"
DeltaCT$infection_history [DeltaCT$infection_history  == "E88:E64"] <- "E.falciformis:E.ferrisi"

DeltaCT$infection_history [DeltaCT$infection_history  == "Eflab:E64"] <- "E.falciformis:E.ferrisi"
DeltaCT$infection_history [DeltaCT$infection_history  == "Eflab:E88"] <- "E.falciformis:E.falciformis"
DeltaCT$infection_history [DeltaCT$infection_history  == "Eflab:UNI"] <- "uninfected"

DeltaCT$infection_history [DeltaCT$infection_history  == "UNI:UNI"] <- "uninfected"
DeltaCT$infection_history [DeltaCT$infection_history  == "UNI:E88"] <- "E.falciformis"
DeltaCT$infection_history [DeltaCT$infection_history  == "UNI:E64"] <- "E.ferrisi"






ggplot(DeltaCT, aes(x = Eim_MC, y = delta, color = challenge)) +
  geom_violin()+
  geom_smooth() +
  geom_jitter()+
  theme_bw()

dCTpos <- subset(DeltaCT,DeltaCT$Eim_MC == "infected")


ggplot(dCTpos, aes(x = challenge, y = delta, color = challenge)) +
  geom_boxplot() +
  geom_jitter()+
  ggtitle("Distribution of infection intensity") + xlab("Eimeria species") + ylab("Infection intensity") 


ggplot(dCTpos, aes(x = infection_history, y = delta, color = challenge)) +
  geom_boxplot() +
  geom_jitter()+
  ggtitle("Distribution of infection intensity") + xlab("Eimeria species") + ylab("Infection intensity") 


IFN_y <- select(Complete, EH_ID, IFNy_CEWE, infection_history, Eim_MC,delta)
IFN_y <- na.omit(IFN_y)
IFN_y<- distinct(IFN_y)

#ggplot for ifny

IFN_y$ infection_history <-as.character(IFN_y$ infection_history)
IFN_y$infection_history [IFN_y$infection_history  == "E139:E64"] <- "E.ferrisi:E.ferrisi"
IFN_y$infection_history [IFN_y$infection_history  == "E139:E88"] <- "E.ferrisi:E.falciformis"
IFN_yinfection_history [IFN_y$infection_history  == "E139:UNI"] <- "ininfected"

IFN_y$infection_history [IFN_y$infection_history  == "E64:E64"] <- "E.ferrisi:E.ferrisi"
IFN_y$infection_history [IFN_y$infection_history  == "E64:E88"] <- "E.ferrisi:E.falciformis"
IFN_y$infection_history [IFN_y$infection_history  == "E64:UNI"] <- "uninfected"

IFN_y$infection_history [IFN_y$infection_history  == "E88:E88"] <- "E.falciformis:E.falciformis"
IFN_y$infection_history [IFN_y$infection_history  == "E88:UNI"] <- "uninfected"
IFN_y$infection_history [IFN_y$infection_history  == "E88:E64"] <- "E.falciformis:E.ferrisi"

IFN_y$infection_history [IFN_y$infection_history  == "Eflab:E64"] <- "E.falciformis:E.ferrisi"
IFN_y$infection_history [IFN_y$infection_history  == "Eflab:E88"] <- "E.falciformis:E.falciformis"
IFN_y$infection_history [IFN_y$infection_history  == "Eflab:UNI"] <- "uninfected"

IFN_y$infection_history [IFN_y$infection_history  == "UNI:UNI"] <- "uninfected"
IFN_y$infection_history [IFN_y$infection_history  == "UNI:E88"] <- "E.falciformis"
IFN_y$infection_history [IFN_y$infection_history  == "UNI:E64"] <- "E.ferrisi"

IFN_y$Eim_MC <- as.character(IFN_y$Eim_MC)

IFN_y$Eim_MC[IFN_y$Eim_MC == "pos"] <- "infected"
IFN_y$Eim_MC[IFN_y$Eim_MC == "neg"] <- "uninfected"


# change LM_0354 uninfected to infected
LM_0354 <- subset(IFN_y, IFN_y$EH_ID == "LM_0354")
LM_0354$Eim_MC[LM_0354$Eim_MC == "uninfected"] <- "infected" 

IFNy <- rbind(IFN_y, LM_0354)
DeltaCT <- DeltaCT[-c(5),]



ggplot(IFNy, aes(y = IFNy_CEWE, x = delta, color= Eim_MC)) +
  geom_smooth(method="lm")+
  geom_jitter()+
  theme_bw()+
  facet_wrap(~Eim_MC) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x = -10, label.y = 750) +
  ggtitle("Distribution of IFNγ") + ylab("IFNγ from cecum wall") + xlab("Infection intensity") +
  scale_color_manual(breaks = c("INF", "UNI"),
                     values=c("blue","darkgreen"))



IFNinf <- subset(IFNy, IFNy$Eim_MC == "infected")
IFNinf <- distinct(IFNinf)

IFNxx <- select(DeltaCT, EH_ID, challenge)

IFN_inf <- merge(IFNinf, IFNxx)

ggplot(IFN_inf, aes(x=infection_history, y= IFNy_CEWE, color= infection_history, main = "Infected" ))+
  geom_boxplot()+
  geom_jitter() +
  ggtitle("Distribution of IFNγ") + ylab("IFNγ from cecum wall") + xlab("Infection History")

ggplot(IFN_inf, aes(x=delta, y= IFNy_CEWE, color = challenge))+
  geom_smooth(method="lm")+
  geom_jitter() +
  facet_wrap(~challenge) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x = -10, label.y = 750) +
  ggtitle("Distribution of IFNγ") + ylab("IFNγ from cecum wall") + xlab("Infection intensity") +
  scale_color_manual(breaks = c("INF", "UNI"),
                     values=c("red","blue"))


IFNy_falci <-  IFN_inf[-c(3),]
IFNy_falcix<- IFNy_falci[-c(5),]

ggplot(IFNy_falcix, aes(x=infection_history, y= IFNy_CEWE, color= infection_history, main = "Infected" ))+
  geom_boxplot()+
  geom_jitter() +
  ggtitle("Distribution of IFNγ") + ylab("IFNγ from cecum wall") + xlab("Infection History")

ggscatter(IFNy_falcix, x = "delta", y = "IFNy_CEWE", add = "reg.line", color = "Eim_MC") +
  facet_grid(~Eim_MC) +
  labs(y = "IFNy (pg/mL)", x = "deltaCT = Mouse - Eimeria", color = "E.falciformis", fill = "E.falciformis") +
  theme(axis.text=element_text(size=12, face = "bold"),
        title = element_text(size = 16, face = "bold"),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        legend.text=element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold")) +
  ylim(0, 510) +
  ggtitle("IFNy affected by infection intensity")


ggscatter(IFNy_falcix, x = "delta", y = "IFNy_CEWE", add = "reg.line", color = "Eim_MC") +
  facet_grid(~Eim_MC) +
  labs(y = "IFNy (pg/mL)", x = "deltaCT", color = "E.falciformis", fill = "E.falciformis") +
  stat_cor(method = "spearman",label.x =0, label.y = 450)+
   theme(axis.text=element_text(size=12, face = "bold"),
        title = element_text(size = 16, face = "bold"),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        legend.text=element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold")) +
  ylim(0, 510) +
  ggtitle("IFNy affected by infection intensity")


ggscatter(IFN_inf, x = "delta", y = "IFNy_CEWE", add = "reg.line", color = "Eim_MC") +
  facet_grid(Eim_MC~challenge, scales = "free")+
  stat_cor(method = "spearman",label.x =0, label.y = 450) +
  stat_regline_equation(label.x = 0, label.y = 350) + 
  labs(y = "IFNy (pg/mL)", x = "deltaCT = mouse -eimeria") +
  theme(axis.text=element_text(size=12, face = "bold"),
        title = element_text(size = 16, face = "bold"),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        legend.text=element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold")) +
  ggtitle("IFNy affected by infection intensity")

