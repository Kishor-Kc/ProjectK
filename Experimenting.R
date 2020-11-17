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

#P3

P3_complete <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_Weight%26Oocyst_complete.csv"
P3_complete <- read.csv(text = getURL(P3_complete))

P3_complete$X <- NULL


P3_a <-select(P3_complete, EH_ID, labels, OPG, wloss, dpi, batch, primary, infHistory)
P3_a <-na.omit(P3_a)

P3_b <- select(P3_complete, EH_ID, labels, OPG, wloss, dpi, batch, challenge, infHistory)
P3_b <- na.omit(P3_b)


# for gene expression

P3_design <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experimental_design/P3_112019_Eim_design.csv"
P3_design <- read.csv(text = getURL(P3_design))

P3_gene <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_CEWE_RTqPCR.csv"
P3_gene <- read.csv(text = getURL(P3_gene))
P3_gene$X <-NULL

P3_Delta <-"https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_CEWE_qPCR.csv"
P3_Delta <- read.csv(text = getURL(P3_Delta))
P3_Delta$X <- NULL

P3_DeltaX <- merge(P3_Delta, P3_design)

P3_Comp <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_COMPLETE.csv"
P3_Comp <- read.csv(text = getURL(P3_Comp))
P3_Comp$X <- NULL

P3_geneX <- select(P3_Comp,"CXCR3", "IL.12", "IRG6", "EH_ID") 
P3_geneX <- na.omit(P3_geneX)

Comp1 <- select(P3_Comp,Eim_MC,EH_ID)
Comp1 <- na.omit(Comp1)

Comp2 <- merge(P3_DeltaX, Comp1)
Comp2 <- distinct(Comp2)




compX <- merge(P3_gene, P3_Delta, by = "EH_ID")

#boxplot for gene expression

complete <- merge(compX, Comp1)

Comp.long <- reshape(complete, 
                     direction = "long",
                     varying = list(names(complete)[2:4]),
                     v.names = "NE",
                     times = c("CXCR3", "IRG6", "IL.12"),
                     timevar = "Target",
                     idvar = "EH_ID")

P3_GE <- select(P3_Comp, EH_ID, challenge)
P3_GE <-na.omit(P3_GE)

P3_GET <- merge(P3_GE, Comp.long)


ggplot(P3_GET, aes(x = challenge, y = NE, color = challenge)) +
  geom_violin() + 
  geom_jitter() +
  facet_wrap(~Target) +
  ggtitle("P3 gene expression") + xlab("Eimeria") + ylab("normalised gene expression")


OPGx <- select(P3_Comp,"OPG","dpi","challenge", "EH_ID") 
OPGx <- na.omit(OPGx)

OPG1 <- select(P3_Comp, "OPG", "dpi", "primary", "EH_ID")
OPG1 <- na.omit(OPG1)

# fix problem with 0340,0335

LM_0340 <- subset(P3_b, P3_b$EH_ID == "LM_0340")

LM_O335 <-  subset(P3_b, P3_b$EH_ID == "LM_0335")

ggplot(P3_b, aes(x = dpi, y = OPG, group = EH_ID, col = challenge)) +
  geom_smooth()+
  geom_line()+
  theme_bw()+
  facet_grid(~challenge)


ggplot(P3_a, aes(x = dpi, y = OPG, group = EH_ID, col = primary)) +
  geom_smooth()+
  geom_line()+
  theme_bw()+
  facet_grid(~primary)

ggplot(P3_Comp, aes(x = dpi, y = OPG, group = EH_ID, col = infHistory)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~infHistory)



# for p4 experiment

P4_complete <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P4_082020_Eim_COMPLETE.csv"
P4_complete <- read.csv(text = getURL(P4_complete))

P4_1 <- select(P4_complete, label, dpi, infHistory, wloss, primary, challenge, OPG)

P4_2 <- select(P4_1, label, primary)
P4_2 <- na.omit(P4_2)

P4_3 <- select(P4_1, label, OPG)
P4_3 <- na.omit(P4_3)

P4_4 <- select(P4_1,label, dpi, wloss)

P4_5 <- merge(P4_2, P4_3)
P4_6 <- merge(P4_5, P4_4)

#plot OOcyst and  weightloss for P4a

ggplot(P4_6, aes(x = dpi, y = OPG, color = primary)) +
  geom_point()+
  theme_bw()+
  facet_grid(~primary)

ggplot(P4_6, aes(x = dpi, y = wloss, col = primary)) +
  geom_smooth()+
  geom_line()+
  theme_bw()+
  facet_grid(~primary)

# plot OOcyst and  weightloss for P4b

P4_7 <- select(P4_1, label, challenge)
P4_7 <- na.omit(P4_7)

P4_8 <- select(P4_1, label, OPG)
P4_8 <- na.omit(P4_8)

P4_9 <- select(P4_1,label, dpi, wloss)

P4_10 <- merge(P4_7, P4_8)
P4_11 <- merge(P4_10, P4_9)

ggplot(P4_11, aes(x = dpi, y = OPG, color = challenge)) +
  geom_point()+
  theme_bw()+
  facet_grid(~challenge)

ggplot(P4_11, aes(x = dpi, y = wloss, col = challenge)) +
  geom_smooth()+
  geom_line()+
  theme_bw()+
  facet_grid(~challenge)

#add infHis 

P4_12 <- select(P4_1, label, infHistory, challenge)
P4_12 <- na.omit(P4_12)

P4_13 <- merge(P4_11, P4_12)

ggplot(P4_13, aes(x = dpi, y = OPG, col = infHistory)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~infHistory)


# let make P3 + P4 together

# for primary wloss

P3_w1 <- select(P3_Comp,label, EH_ID, dpi, wloss,primary, infHistory)
P3_w1 <- na.omit(P3_w1)

P4_w1 <- select(P4_complete,label, EH_ID, dpi, wloss, primary, infHistory)
P4_w1 <- na.omit(P4_w1)

Pr_wloss<- rbind(P3_w1, P4_w1)
Pr_wloss <- Pr_wloss[-c(195),]


Pr_wloss $ infHistory <-as.character(Pr_wloss $ infHistory)
Pr_wloss$infHistory[Pr_wloss$infHistory == "E139:E64"] <- "E.fer:E.fer"
Pr_wloss$infHistory[Pr_wloss$infHistory == "E139:E88"] <- "E.fer:E.falci"
Pr_wloss$infHistory[Pr_wloss$infHistory == "E139:UNI"] <- "E.fer"

Pr_wloss$infHistory[Pr_wloss$infHistory == "E64:E64"] <- "E.fer:E.fer"
Pr_wloss$infHistory[Pr_wloss$infHistory == "E64:E88"] <- "E.fer:E.falci"
Pr_wloss$infHistory[Pr_wloss$infHistory == "E64:UNI"] <- "E.fer"

Pr_wloss$infHistory[Pr_wloss$infHistory == "E88:E88"] <- "E.falci:E.falci"
Pr_wloss$infHistory[Pr_wloss$infHistory == "E88:UNI"] <- "E.falci"
Pr_wloss$infHistory[Pr_wloss$infHistory == "E88:E64"] <- "E.falci:E.fer"

Pr_wloss$infHistory[Pr_wloss$infHistory == "Eflab:E64"] <- "E.falci:E.fer"
Pr_wloss$infHistory[Pr_wloss$infHistory == "Eflab:E88"] <- "E.falci:E.falci"
Pr_wloss$infHistory[Pr_wloss$infHistory == "Eflab:UNI"] <- "E.falci"

Pr_wloss$infHistory[Pr_wloss$infHistory == "UNI:UNI"] <- "UNI"
Pr_wloss$infHistory[Pr_wloss$infHistory == "UNI:E88"] <- "E.falci"
Pr_wloss$infHistory[Pr_wloss$infHistory == "UNI:E64"] <- "E.fer"




ggplot(Pr_wloss, aes(x = dpi, y = wloss, group = primary, col = primary)) +
  geom_smooth(se = FALSE)+
  geom_jitter()+
  theme_bw() +
  ggtitle("Weightloss during primary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E139", "E64", "E88","Eflab","UNI"),
                     values=c("red", "blue", "darkgreen","orange","violet"))

#for presentation
Pr_wloss$primary <-as.character(Pr_wloss$primary)
Pr_wloss$primary[Pr_wloss$primary == "E139"] <- "E. ferrisi"
Pr_wloss$primary[Pr_wloss$primary == "E64"] <- "E. ferrisi"
Pr_wloss$primary[Pr_wloss$primary == "E88"] <- "E. falciformis"
Pr_wloss$primary[Pr_wloss$primary == "Eflab"] <- "E. falciformis"


Pr_wloss <- distinct(Pr_wloss)

ggplot(Pr_wloss, aes(x = dpi, y = wloss, group = primary, col = primary)) +
  geom_smooth(se = FALSE)+
  geom_jitter()+
  theme_bw() +
  ggtitle("Weightloss during primary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))

ggplot(Pr_wloss, aes(x = dpi, y = wloss, col = primary)) +
  geom_smooth()+
  geom_point(size=0.1)+
  theme_bw()+
  facet_grid(~primary) +
  ggtitle("Weightloss during primary infection") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))





# sec wloss

P3_w2 <- select(P3_Comp,label, EH_ID, dpi, wloss,challenge)
P3_w2 <- na.omit(P3_w2)

P4_w2 <- select(P4_complete,label, EH_ID, dpi, wloss, challenge)
P4_w2 <- na.omit(P4_w2)

Ch_wloss <- rbind(P3_w2, P4_w2)
Ch_wloss <- distinct(Ch_wloss)

Ch_wloss$challenge <-as.character(Ch_wloss$challenge)
Ch_wloss$challenge[Ch_wloss$challenge == "E64"] <- "E. ferrisi"
Ch_wloss$challenge[Ch_wloss$challenge == "E88"] <- "E. falciformis"



ggplot(Ch_wloss, aes(x = dpi, y = wloss, group = challenge, col = challenge )) +
  geom_smooth(se = FALSE)+
  geom_jitter()+
  theme_bw() +
  ggtitle("Weightloss during secondary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))





ggplot(Ch_wloss, aes(x = dpi, y = wloss, group = challenge, col = challenge)) +
  geom_smooth()+
  geom_jitter()+
  facet_grid(~challenge)+
  ggtitle("Weightloss during secondary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))


ggplot(Ch_wloss, aes(x = dpi, y = wloss, col = challenge)) +
  geom_smooth()+
  geom_point(size=0.5)+
  theme_bw()+
  facet_grid(~challenge) +
  ggtitle("Weightloss during secondary infection") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)")+
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))





P4_d1 <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experimental_design/P3_112019_Eim_design.csv"
P4_d1 <- read.csv(text = getURL(P4_d1))

P4_d2 <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experimental_design/P4a_082020_Eim_design.csv"
P4_d2 <- read.csv(text = getURL(P4_d2))

P4_d11 <- select(P4_d1, EH_ID, primary)
P4_d11$primary <-as.character(P4_d11$primary)
P4_d11$primary[P4_d11$primary == "E139"] <- "E. ferrisi"
P4_d11$primary[P4_d11$primary == "E64"] <- "E. ferrisi"
P4_d11$primary[P4_d11$primary == "E88"] <- "E. falciformis"
P4_d11$primary[P4_d11$primary == "Eflab"] <- "E. falciformis"

P4_d21 <- select(P4_d2, EH_ID, primary)
P4_d21$primary <-as.character(P4_d21$primary)
P4_d21$primary[P4_d21$primary == "E139"] <- "E. ferrisi"
P4_d21$primary[P4_d21$primary == "E64"] <- "E. ferrisi"
P4_d21$primary[P4_d21$primary == "E88"] <- "E. falciformis"
P4_d21$primary[P4_d21$primary == "Eflab"] <- "E. falciformis"



P4_D <- rbind(P4_d11, P4_d21)


P4_WW <- merge(Ch_wloss, P4_D)
P4_WW <- distinct(P4_WW)


ggplot(P4_WW, aes(x = dpi, y = wloss, group = challenge, col = challenge)) +
  geom_smooth(se = FALSE)+
  geom_point(size=0.7)+
  geom_jitter()+
  theme_bw()+
  facet_grid(~primary) +
  ggtitle("Weightloss during secondary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)")+
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))

#add infHistory

Inf_His1x <- select(P3_Comp, EH_ID, infHistory)
Inf_His1x <- na.omit(Inf_His1x)

P3_OPGIHx <- merge(Inf_His1,Ch_wloss)
P3_OPGIHx <- distinct(P3_OPGIHx)

Inf_His2x <- select(P4_complete, EH_ID, infHistory)
Inf_His2x <- na.omit(Inf_His2x)

P4_OPGIHx <- merge(Inf_His2,Ch_wloss)
P4_OPGIHx <-distinct(P4_OPGIHx)


P_OPGIH1x <- rbind(P3_OPGIHx, P4_OPGIHx)
P_OPGIH1x <-na.omit(P_OPGIH1x)
P_OPGIH1x <-distinct(P_OPGIH1x)

P_OPGIH1x $ infHistory <-as.character(P_OPGIH1x $ infHistory)
P_OPGIH1x $infHistory[P_OPGIH1x$infHistory == "E139:E64"] <- "E.fer:E.fer"
P_OPGIH1x $infHistory[P_OPGIH1x$infHistory == "E139:E88"] <- "E.fer:E.falci"
P_OPGIH1x$infHistory[P_OPGIH1x$infHistory == "E139:UNI"] <- "UNI"

P_OPGIH1x$infHistory[P_OPGIH1x$infHistory == "E64:E64"] <- "E.fer:E.fer"
P_OPGIH1x $infHistory[P_OPGIH1x$infHistory == "E64:E88"] <- "E.fer:E.falci"
P_OPGIH1x$infHistory[P_OPGIH1x$infHistory == "E64:UNI"] <- "UNI"

P_OPGIH1x $infHistory[P_OPGIH1x$infHistory == "E88:E88"] <- "E.falci:E.falci"
P_OPGIH1x $infHistory[P_OPGIH1x$infHistory == "E88:UNI"] <- "UNI"
P_OPGIH1x$infHistory[P_OPGIH1x$infHistory == "E88:E64"] <- "E.falci:E.fer"

P_OPGIH1x$infHistory[P_OPGIH1x$infHistory == "Eflab:E64"] <- "E.falci:E.fer"
P_OPGIH1x $infHistory[P_OPGIH1x$infHistory == "Eflab:E88"] <- "E.falci:E.falci"
P_OPGIH1x $infHistory[P_OPGIH1x$infHistory == "Eflab:UNI"] <- "UNI"

P_OPGIH1x$infHistory[P_OPGIH1x$infHistory == "UNI:UNI"] <- "UNI"
P_OPGIH1x $infHistory[P_OPGIH1x$infHistory == "UNI:E88"] <- "E.falci"
P_OPGIH1x $infHistory[P_OPGIH1x$infHistory == "UNI:E64"] <- "E.fer"

P_OPGIH2x <- select(Pr_wloss, EH_ID, primary)
P_OPGIH3x <- merge(P_OPGIH1x, P_OPGIH2x)
P_OPGIH3x <- distinct(P_OPGIH3x)




ggplot(subset(P_OPGIH3x, !is.na(P_OPGIH3x$challenge) & P_OPGIH3x$wloss > 0), aes(x = infHistory, y = wloss, color = infHistory)) + 
  geom_boxplot() + 
  geom_point(size=0.1)+
  geom_jitter()+
  ggtitle("Maximal for weight loss for primary and secondary infection") + ylab("Maximal for weight loss") + xlab("Infection History")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

WlossMean <- aggregate(P_OPGIH3x, list(P_OPGIH3x$EH_ID), mean)


WlossMean1 <- select(WlossMean, Group.1, wloss)

colnames(WlossMean1)[colnames(WlossMean1)=="Group.1"] <- "EH_ID"
WlossMean2 <- select(P_OPGIH3x, EH_ID, infHistory)

WlossMeanx <- merge(WlossMean1,WlossMean2)
WlossMeanx <- na.omit(WlossMeanx)
WlossMeanx <- distinct(WlossMeanx)

my_comparisons <- list(c("E.falci:E.falci", "E.falci"),
                       c("E.fal:E.fer", "E.fer:E.falci"),
                       c("E.fer:E.fer","E.fer") )

ggplot(WlossMeanx, aes(x = infHistory, y = wloss, color = infHistory)) + 
  geom_boxplot() + 
  geom_point(size=0.1)+
  geom_jitter()+
  ggtitle("Maximal for weight loss for primary and secondary infection") + ylab("Maximal for weight loss") + xlab("Infection History")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_compare_means(comparisons = my_comparisons,
                     method = "wilcox.test", 
                     aes(label = ..p.signif..), 
                     size = 3, label.y.npc =0.95) 
  

WlossPrmean  <- aggregate(Pr_wloss, list(Pr_wloss$EH_ID), mean)
WlossPrmean1 <-select(WlossPrmean, Group.1, wloss)

colnames(WlossPrmean1)[colnames(WlossPrmean1)=="Group.1"] <- "EH_ID"
WlossPrmean2 <- select(Pr_wloss, EH_ID, infHistory)

WlossPrMeanx <- merge(WlossPrmean1,WlossPrmean2)
WlossPrMeanx <- na.omit(WlossPrMeanx)
WlossPrMeanx <- distinct(WlossPrMeanx)

Wloss_infHis <- rbind(WlossPrMeanx, WlossMeanx)

my_comparisons <- list(c("E.falci:E.falci", "E.falci"),
                       c("E.fal:E.fer", "E.fer:E.falci"),
                      c("E.fer:E.fer","E.fer") )


ggplot(Wloss_infHis, aes(x = infHistory, y = wloss, color = infHistory)) + 
  geom_boxplot() + 
  geom_jitter()+
  ggtitle("Maximal for weight loss for primary and secondary infection") + ylab("Maximal for weight loss") + xlab("Infection History") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
stat_compare_means(comparisons = my_comparisons,
                   method = "wilcox.test", 
                   aes(label = ..p.signif..), 
                   size = 3, label.y.npc =0.95) 



my_comparisons <-  list(c("E.falciformis", "E.ferrisi"), 
                        c("E.falciformis", "Uninfected"), 
                        c("E.ferrisi" , "Uninfected"))


# Ococyst plot
#for primary

P4_O1 <- select(P4_complete,OPG, dpi, primary, EH_ID, infHistory)
P4_O1 <- na.omit(P4_O1)

P4_O1$batch <- c("P4a")

P3_O1 <- select(P3_a, OPG, dpi, primary, EH_ID, infHistory)
P3_O1$batch <- c("P3a")

OPG_a <- rbind(P4_O1, P3_O1)

P3_O2 <- select(P3_b, OPG, dpi, challenge, EH_ID,infHistory)
P3_O2$batch <- c("P3b")

P4_O2 <- select(P4_complete,OPG, dpi, challenge, EH_ID, infHistory)
P4_O2 <- na.omit(P4_O2)
P4_O2$batch <- c("P4b")

Pr_OPG <- rbind(P4_O1, P3_O1)
Pr_OPG <- distinct(Pr_OPG)
Pr_OPG $primary <-as.character(Pr_OPG$primary)
Pr_OPG $primary[Pr_OPG$primary == "E139"] <- "E. ferrisi"
Pr_OPG $primary[Pr_OPG$primary == "E64"] <- "E. ferrisi"
Pr_OPG$primary[Pr_OPG$primary == "E88"] <- "E. falciformis"
Pr_OPG$primary[Pr_OPG$primary == "Eflab"] <- "E. falciformis"


Pr_OPG$ infHistory <-as.character(Pr_OPG$ infHistory)
Pr_OPG $infHistory[Pr_OPG$infHistory == "E139:E64"] <- "E.ferrisi:E.ferrisi"
Pr_OPG$infHistory[Pr_OPG$infHistory == "E139:E88"] <- "E.ferrisi:E.falciformis"
Pr_OPG$infHistory[Pr_OPG$infHistory == "E139:UNI"] <- "E.ferrisi"

Pr_OPG$infHistory[Pr_OPG$infHistory == "E64:E64"] <- "E.ferrisi:E.ferrisi"
Pr_OPG $infHistory[Pr_OPG$infHistory == "E64:E88"] <- "E.ferrisi:E.falciformis"
Pr_OPG$infHistory[Pr_OPG$infHistory == "E64:UNI"] <- "E.ferrisi"

Pr_OPG$infHistory[Pr_OPG$infHistory == "E88:E88"] <- "E.falciformis:E.falciformis"
Pr_OPG$infHistory[Pr_OPG$infHistory == "E88:UNI"] <- "E.falciformis"
Pr_OPG$infHistory[Pr_OPG$infHistory == "E88:E64"] <- "E.falciformis:E.ferrisi"

Pr_OPG$infHistory[Pr_OPG$infHistory == "Eflab:E64"] <- "E.falciformis:E.ferrisi"
Pr_OPG$infHistory[Pr_OPG$infHistory == "Eflab:E88"] <- "E.falciformis:E.falciformis"
Pr_OPG$infHistory[Pr_OPG$infHistory == "Eflab:UNI"] <- "E.falciformis"

Pr_OPG$infHistory[Pr_OPG$infHistory == "UNI:UNI"] <- "UNI"
Pr_OPG$infHistory[Pr_OPG$infHistory == "UNI:E88"] <- "E.falciformis"
Pr_OPG$infHistory[Pr_OPG$infHistory == "UNI:E64"] <- "E.ferrisi"






Ch_OPG <- rbind(P4_O2, P3_O2)
Ch_OPG <- distinct(Ch_OPG)
Ch_OPG$challenge <-as.character(Ch_OPG$challenge)
Ch_OPG$challenge[Ch_OPG$challenge == "E64"] <- "E. ferrisi"
Ch_OPG$challenge[Ch_OPG$challenge == "E88"] <- "E. falciformis"

Inf_His1 <- select(P3_Comp, EH_ID, infHistory)
Inf_His1 <- na.omit(Inf_His1)

P3_OPGIH <- merge(Inf_His1,Ch_OPG)
P3_OPGIH <- distinct(P3_OPGIH)

Inf_His2 <- select(P4_complete, EH_ID, infHistory)
Inf_His2 <- na.omit(Inf_His2)

P4_OPGIH <- merge(Inf_His2,Ch_OPG)
P4_OPGIH <-distinct(P4_OPGIH)


P_OPGIH <- rbind(P3_OPGIH, P4_OPGIH)
P_OPGIH <-na.omit(P_OPGIH)
P_OPGIH <-distinct(P_OPGIH)


P_OPGIH $ infHistory <-as.character(P_OPGIH $ infHistory)
P_OPGIH $infHistory[P_OPGIH$infHistory == "E139:E64"] <- "E.ferrisi:E.ferrisi"
P_OPGIH $infHistory[P_OPGIH$infHistory == "E139:E88"] <- "E.ferrisi:E.falciformis"
P_OPGIH$infHistory[P_OPGIH$infHistory == "E139:UNI"] <- "UNI"

P_OPGIH$infHistory[P_OPGIH$infHistory == "E64:E64"] <- "E.ferrisi:E.ferrisi"
P_OPGIH $infHistory[P_OPGIH$infHistory == "E64:E88"] <- "E.ferrisi:E.falciformis"
P_OPGIH $infHistory[P_OPGIH$infHistory == "E64:UNI"] <- "UNI"

P_OPGIH $infHistory[P_OPGIH$infHistory == "E88:E88"] <- "E.falciformis:E.falciformis"
P_OPGIH $infHistory[P_OPGIH$infHistory == "E88:UNI"] <- "UNI"
P_OPGIH$infHistory[P_OPGIH$infHistory == "E88:E64"] <- "E.falciformis:E.ferrisi"

P_OPGIH$infHistory[P_OPGIH$infHistory == "Eflab:E64"] <- "E.falciformis:E.ferrisi"
P_OPGIH $infHistory[P_OPGIH$infHistory == "Eflab:E88"] <- "E.falciformis:E.falciformis"
P_OPGIH $infHistory[P_OPGIH$infHistory == "Eflab:UNI"] <- "UNI"

P_OPGIH$infHistory[P_OPGIH$infHistory == "UNI:UNI"] <- "UNI"
P_OPGIH $infHistory[P_OPGIH$infHistory == "UNI:E88"] <- "E.falciformis"
P_OPGIH $infHistory[P_OPGIH$infHistory == "UNI:E64"] <- "E.ferrisi"

cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange","12"="pink")

ggplot(Pr_OPG, aes(x = dpi, y = OPG, col = primary)) +
  geom_smooth()+
  geom_point(size=1)+
  theme_bw()+
  facet_grid(~primary) +
  ylim(0,5000000) +
  ggtitle("Oocyst shedding for primary infection ") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))


ggplot(Ch_OPG, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=1)+
  theme_bw()+
  facet_grid(~challenge) +
  ylim(0,5000000) +
  ggtitle("Oocyst shedding for secondary infection ") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)")+
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))



OPGMean <- aggregate(P_OPGIH, list(P_OPGIH$EH_ID), mean)
OPGMean1 <- select(OPGMean, Group.1, OPG)
colnames(OPGMean1)[colnames(OPGMean1)=="Group.1"] <- "EH_ID"
OPGMean2 <- select(P_OPGIH, EH_ID, infHistory)

OPGMeanx <- merge(OPGMean1,OPGMean2)
OPGMeanx <- na.omit(OPGMeanx)
OPGMeanx <- distinct(OPGMeanx)

OPGchMean <- aggregate(Pr_OPG, list(Pr_OPG$EH_ID), mean)
OPGchMean1 <- select(OPGchMean, Group.1, OPG)
colnames(OPGchMean1)[colnames(OPGchMean1)=="Group.1"] <- "EH_ID"
OPGchMean2 <- select(Pr_OPG, EH_ID, infHistory)

OPGchMeanx <- merge(OPGchMean1,OPGchMean2)
OPGchMeanx <- na.omit(OPGchMeanx)
OPGchMeanx <- distinct(OPGchMeanx)

OPG_INFHIS <- rbind(OPGchMeanx, OPGMeanx)
OPG_INFHIS <- distinct(OPG_INFHIS)

my_comparison <- list(c("E.falciformis:E.falciformis", "E.falciformis"),
                       c("E.ferrisi:E.falciformis", "E.falciformis:E.ferrisi"),
                       c("E.ferrisi:E.ferrisi","E.ferrisi") )

ggplot(OPG_INFHIS, aes(x = infHistory, y = OPG, color = infHistory)) + 
  geom_boxplot() + 
  geom_jitter()+
  ggtitle("Sum of shedded oocysts for primary and secondary infection") + ylab("Sum of shedded oocysts (OPG)") + xlab("Infection History") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  stat_compare_means(comparisons = my_comparison,
                     method = "wilcox.test", 
                     aes(label = ..p.signif..), 
                     size = 3, label.y.npc =0.95) 





ggplot(P_OPGIH, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=0.8)+
  theme_bw()+
  facet_grid(~batch) +
  ylim(0,3000000) +
  ggtitle("Oocyst shedding for secondary infection ") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))


ggplot(P_OPGIHxx, aes(x = dpi, y = OPG, group = challenge, col = challenge)) +
  geom_smooth(se = FALSE)+
  geom_point(size=0.8)+
  geom_jitter()+
  theme_bw()+
  facet_grid(~primary) +
  ylim(0,3000000) +
  ggtitle("Oocyst shedding for secondary infection ") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))

P_OPGIH$batch <-as.character(P_OPGIH$batch)
P_OPGIH$batch[P_OPGIH$batch == "P3b"] <- "First exp"
P_OPGIH$batch[P_OPGIH$batch == "P4b"] <- "Second exp"

ggplot(P_OPGIH, aes(x = dpi, y = OPG, group = challenge, color= batch)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~infHistory) +
  ggtitle("Oocyst shedding regarding infection history") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)")

ggplot(Comp.long, aes(x = Eim_MC, y = delta, color = Eim_MC,)) +
  geom_boxplot() + 
  geom_jitter() + facet_wrap(~Target)

ggplot(P_OPGIH, aes(x = infHistory, y = OPG, color= infHistory)) +
  geom_boxplot()+
  ggtitle("Oocyst shedding regarding infection history") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)")


ggplot(subset(P_OPGIHxx, !is.na(P_OPGIHxx$challenge) & P_OPGIHxx$OPG > 0), aes(x = infHistory, y = OPG, color = infHistory)) + 
  geom_boxplot() + 
  geom_jitter()+
  ggtitle("Oocyst shedding regarding infection history") + ylab("Oocysts per gram (OPG)") + xlab("Infection History")


ggplot(P_OPGIH, aes(x = dpi, y = OPG, group = challenge, color= batch)) +
  geom_point()+
  geom_line()+
  
  
  
  
  boxplot(OPG ~ infHistory, data = P_OPGIH)

OP_1 <- select(P_OPGIH, EH_ID, OPG, infHistory)




OP_2 <- select(OPGMean1, Group.1, OPG)
names(OP_2)[names(OP_2) == "Group.1"] <- "EH_ID"

OP_3 <- select(P_OPGIH, EH_ID, infHistory, challenge)
OP_3 <- distinct(OP_3)

OPG_final <- merge(OP_2, OP_3)

OPG_xxx <- select(OPG_final, OPG, infHistory)

ggplot(subset(OPG_xxx, !is.na(OPG_xxx$infHistory) & OPG_xxx$OPG > 0), aes(x = infHistory, y = OPG, color = infHistory)) + 
  geom_boxplot() + 
  geom_jitter()

boxplot(OPG ~ infHistory, data = OPG_xxx)

ggplot(OPG_xxx, aes(x = infHistory, y = OPG)) + 
  geom_boxplot() + 
  geom_jitter()




AVG_1 <- subset(OPG_final, OPG_final$infHistory== "E.ferrisi:E.falciformis")
AVG_1x <- mean(AVG_1[,"OPG"], na.rm = TRUE)

AVG_2 <- subset(OPG_final, OPG_final$infHistory== "E.ferrisi:E.ferrisi")
AVG_2x <- mean(AVG_2[,"OPG"], na.rm = TRUE)

AVG_3 <- subset(OPG_final, OPG_final$infHistory== "E.ferrisi:UNI")
AVG_3x <- mean(AVG_3[,"OPG"], na.rm = TRUE)

AVG_4 <- subset(OPG_final, OPG_final$infHistory== "E.falciformisi:E.falciformis")
AVG_4x <- mean(AVG_4[,"OPG"], na.rm = TRUE)

AVG_5 <- subset(OPG_final, OPG_final$infHistory== "E.falciformis:E.ferrisi")
AVG_5x <- mean(AVG_5[,"OPG"], na.rm = TRUE)

AVG_6 <- subset(OPG_final, OPG_final$infHistory== "E.falciformis:UNI")
AVG_6x <- mean(AVG_6[,"OPG"], na.rm = TRUE)

AVG_7 <- subset(OPG_final, OPG_final$infHistory== "E.ferrisi")
AVG_7x <- mean(AVG_7[,"OPG"], na.rm = TRUE)

AVG_8 <- subset(OPG_final, OPG_final$infHistory== "E.falciformis")
AVG_8x <- mean(AVG_8[,"OPG"], na.rm = TRUE)



#

OPG_mean <- "https://raw.githubusercontent.com/Kishor-Kc/ProjectK/master/OPG_Finalxx.csv"
OPG_mean <- read.csv(text = getURL(OPG_mean))

ggplot(OPG_mean, aes(x = infHistory, y = OPG)) + 
  geom_point() 







ggplot(P_OPGIH, aes(x = dpi, y = wloss, group = challenge, col = challenge)) +
  geom_smooth()+
  geom_jitter()+
  facet_grid(~batch)+
  ggtitle("Weightloss during secondary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E. ferrisi","E. falciformis","UNI" ),
                     values=c("red", "blue", "darkgreen"))

# Delta Eim_MC


P4_Delta <- select( P4_complete, EH_ID, delta, Eim_MC, infHistory,challenge)
P4_Delta <- na.omit(P4_Delta)
P4_Delta <- distinct(P4_Delta)

P4_Delta$Eim_MC[P4_Delta$Eim_MC == "TRUE"] <- "infected"
P4_Delta$Eim_MC[P4_Delta$Eim_MC == "FALSE"] <- "uninfected"

P4_Delta$Eim_MC <- as.character(P4_Delta$Eim_MC)

P3_Delta <- select(P3_Comp, EH_ID, delta, Eim_MC, infHistory,challenge)
P3_Delta <- na.omit(P3_Delta)
P3_Delta$Eim_MC <- as.character(P3_Delta$Eim_MC)

P3_Delta$Eim_MC[P3_Delta$Eim_MC == "pos"] <- "infected" 
P3_Delta$Eim_MC[P3_Delta$Eim_MC == "neg"] <- "uninfected"

P_Delta <- rbind(P3_Delta, P4_Delta)
P_Delta <- distinct(P_Delta)

# change LM_0354 uninfected to infected
LM_0354 <- subset(P_Delta, P_Delta$EH_ID == "LM_0354")
LM_0354$Eim_MC[LM_0354$Eim_MC == "uninfected"] <- "infected" 

LM337_D <- subset(P_Delta, P_Delta$EH_ID == "LM_0337")
LM337_D$Eim_MC[LM337_D$Eim_MC == "infected"] <- "uninfected" 

LM346_D <- subset(P_Delta, P_Delta$EH_ID == "LM_0346")
LM346_D$Eim_MC[LM346_D$Eim_MC == "infected"] <- "uninfected" 

P_Delta <- P_Delta[-c(7),]
P_Delta1 <- P_Delta[-c(10),]
P_Delta2 <- P_Delta1[-c(16),]


P_Deltax <- rbind(P_Delta2, LM_0354, LM337_D, LM346_D)
P_Deltax$challenge <-as.character(P_Deltax$challenge)
P_Deltax$challenge[P_Deltax$challenge == "E64"] <- "E. ferrisi"
P_Deltax$challenge[P_Deltax$challenge == "E88"] <- "E. falciformis"

P_Deltax$ infHistory <-as.character(P_Deltax$ infHistory)
P_Deltax$infHistory[P_Deltax$infHistory == "E139:E64"] <- "E.ferrisi:E.ferrisi"
P_Deltax$infHistory[P_Deltax$infHistory == "E139:E88"] <- "E.ferrisi:E.falciformis"
P_Deltax$infHistory[P_Deltax$infHistory == "E139:UNI"] <- "UNI"

P_Deltax$infHistory[P_Deltax$infHistory == "E64:E64"] <- "E.ferrisi:E.ferrisi"
P_Deltax $infHistory[P_Deltax$infHistory == "E64:E88"] <- "E.ferrisi:E.falciformis"
P_Deltax $infHistory[P_Deltax$infHistory == "E64:UNI"] <- "UNI"

P_Deltax $infHistory[P_Deltax$infHistory == "E88:E88"] <- "E.falciformis:E.falciformis"
P_Deltax $infHistory[P_Deltax$infHistory == "E88:UNI"] <- "UNI"
P_Deltax$infHistory[P_Deltax$infHistory == "E88:E64"] <- "E.falciformis:E.ferrisi"

P_Deltax$infHistory[P_Deltax$infHistory == "Eflab:E64"] <- "E.falciformis:E.ferrisi"
P_Deltax$infHistory[P_Deltax$infHistory == "Eflab:E88"] <- "E.falciformis:E.falciformis"
P_Deltax$infHistory[P_Deltax$infHistory == "Eflab:UNI"] <- "UNI"

P_Deltax$infHistory[P_Deltax$infHistory == "UNI:UNI"] <- "UNI"
P_Deltax$infHistory[P_Deltax$infHistory == "UNI:E88"] <- "E.falciformis"
P_Deltax$infHistory[P_Deltax$infHistory == "UNI:E64"] <- "E.ferrisi"




ggplot(subset(P_Deltax,!is.na(P_Deltax$delta)), aes(x = infHistory, y = delta, color = Eim_MC)) +
  geom_point()+
  theme_bw()+
  facet_grid(~challenge)  

ggplot(P_Deltax, aes(x = Eim_MC, y = delta, color = challenge)) +
  geom_violin()+
  geom_smooth() +
  geom_jitter()+
  theme_bw()

P_infInt <- subset(P_Deltax, P_Deltax$Eim_MC == "infected")
P_infInt <- distinct(P_infInt)

ggplot(P_infInt, aes(x = challenge, y = delta, color = challenge)) +
  geom_boxplot() +
  geom_jitter()+
  ggtitle("Distribution of infection intensity") + xlab("Eimeria species") + ylab("Infection intensity") 
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  



#try for the average intensity

AVG <- subset(P_Deltax, P_Deltax$challenge== "E64")
AVG_64 <- mean(AVG[,"delta"], na.rm = TRUE)

AVG1 <- subset(P_Deltax, P_Deltax$challenge== "E88")
AVG1 <- distinct(AVG1)
AVG_88 <- mean(AVG1[,"delta"], na.rm = TRUE)
# INFy ELISA result

P3_IFN <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_CEWE_ELISA.csv"
P3_IFN <- read.csv(text = getURL(P3_IFN))
P3_IFN$X <- NULL


P3_E <- merge(P3_IFN,P_Deltax)
names(P3_E)[names(P3_E) == "IFNy"] <- "IFNy_CEWE"
P3_EE <- select(P3_E, EH_ID, infHistory, delta, challenge, IFNy_CEWE, Eim_MC)

P4_E <- select(P4_complete, EH_ID, IFNy_CEWE)
P4_E <- na.omit(P4_E)
P4_EE <- merge(P_Deltax, P4_E)

P_E <- rbind(P3_EE, P4_EE)
P_E <- distinct(P_E)

ggplot(P_E, aes(y = IFNy_CEWE, x = delta, color= Eim_MC)) +
  geom_smooth(method="lm")+
  geom_jitter()+
  theme_bw()+
  facet_wrap(~Eim_MC) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x = -10, label.y = 750) +
  ggtitle("Distribution of IFNγ") + ylab("IFNγ from cecum wall") + xlab("Infection intensity") +
  scale_color_manual(breaks = c("INF", "UNI"),
                     values=c("blue","darkgreen"))

ggplot(P_E, aes(y = IFNy_CEWE, x = delta, color= Eim_MC)) +
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~challenge) 

ggplot(P_E, aes(y = IFNy_CEWE, x = challenge, color= Eim_MC)) +
  geom_boxplot()+
  geom_jitter() +
  ggtitle("Distribution of IFNγ") + ylab("IFNγ from cecum wall") + xlab("Eimeria sp.")



P_Inf <- subset(P_E, P_E$Eim_MC == "infected")
P_Inf <- distinct(P_Inf)

ggplot(P_Inf, aes(x=challenge, y= IFNy_CEWE, color= challenge,main = "Infected" ))+
  geom_violin()+
  geom_jitter() 

ggplot(P_Inf, aes(x=delta, y= IFNy_CEWE, color = challenge))+
  geom_smooth(method="lm")+
  geom_jitter() +
  facet_wrap(~challenge) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x = -10, label.y = 750) +
  ggtitle("Distribution of IFNγ") + ylab("IFNγ from cecum wall") + xlab("Infection intensity") +
  scale_color_manual(breaks = c("INF", "UNI"),
                     values=c("red","blue"))


P_Inf$infHistory <-as.character(P_Inf$ infHistory)
P_Inf$infHistory[P_Inf$infHistory == "E139:E64"] <- "E.ferrisi:E.ferrisi"
P_Inf$infHistory[P_Inf$infHistory == "E139:UNI"] <- "E.ferrisi"
P_Inf$infHistory[P_Inf$infHistory == "E139:E88"] <- "E.ferrisi:E.falciformis"


P_Inf$infHistory[P_Inf$infHistory == "E64:E64"] <- "E.ferrisi:E.fererisi"
P_Inf$infHistory[P_Inf$infHistory == "E64:E88"] <- "E.ferrisi:E.falciformis"
P_Inf$infHistory[P_Inf$infHistory == "E64:UNI"] <- "UNI"


P_Inf$infHistory[P_Inf$infHistory == "E88:E88"] <- "E.falciformis:E.falciformis"
P_Inf$infHistory[P_Inf$infHistory == "E88:UNI"] <- "UNI"
P_Inf$infHistory[P_Inf$infHistory == "E88:E64"] <- "E.falciformis:E.ferrisi"

P_Inf$infHistory[P_Inf$infHistory == "Eflab:E64"] <- "E.falciformis:E.ferrisi"
P_Inf$infHistory[P_Inf$infHistory == "Eflab:E88"] <- "E.falciformis:E.falciformis"
P_Inf$infHistory[P_Inf$infHistory == "Eflab:UNI"] <- "UNI"

P_Inf$infHistory[P_Inf$infHistory == "UNI:UNI"] <- "UNI"
P_Inf$infHistory[P_Inf$infHistory == "UNI:E88"] <- "E.falciformis"
P_Inf$infHistory[P_Inf$infHistory == "UNI:E64"] <- "E.ferrisi"


P_Infx <- subset(P_Inf, P_Inf$infHistory == "E.falciformis")

P_Infxx <- subset(P_Inf, P_Inf$infHistory == "E.ferrisi:E.falciformis")

P_Infxxx <- subset(P_Inf, P_Inf$infHistory == "E.falciformis:E.falciformis")

P_Inf4x <- rbind(P_Infx,P_Infxx,P_Infxxx)

ggplot(P_Inf4x, aes(x=infHistory, y= IFNy_CEWE, color = challenge))+
  geom_boxplot()+
  geom_jitter() +
  facet_wrap(~challenge) +
  ggtitle("Distribution of IFNγ") + ylab("IFNγ from cecum wall") + xlab("Infection History") 


XX <- select(P_Inf4x, EH_ID, IFNy_CEWE, infHistory)
YY <- select(Wloss_infHis, EH_ID, wloss, infHistory)
XY <- merge(XX,YY)

ggplot(XY, aes(y=wloss, x= IFNy_CEWE, color = infHistory))+
  geom_boxplot()+
  geom_jitter() +
  ggtitle("Sum of oocyst shedding to IFNγ ") + xlab("IFNγ from cecum wall") + ylab("Sum of oocyst") 

YYY <- select(OPG_INFHIS, EH_ID, OPG, infHistory)
XX <- select(P_Inf4x, EH_ID, IFNy_CEWE, infHistory)
XYX <- merge(YYY,XX)

ggplot(XYX, aes(y=OPG, x= IFNy_CEWE, color = infHistory))+
  geom_boxplot()+
  geom_jitter() +
  ggtitle("Sum of oocyst shedding to IFNγ ") + xlab("IFNγ from cecum wall") + ylab("Sum of oocyst") 
