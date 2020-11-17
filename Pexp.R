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


P3_a <-select(P3_complete, EH_ID, labels, OPG, wloss, dpi, batch, primary)
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

P3_w1 <- select(P3_Comp,label, EH_ID, dpi, wloss,primary)
P3_w1 <- na.omit(P3_w1)

P4_w1 <- select(P4_complete,label, EH_ID, dpi, wloss, primary)
P4_w1 <- na.omit(P4_w1)

Pr_wloss<- rbind(P3_w1, P4_w1)
Pr_wloss <- Pr_wloss[-c(195),]

ggplot(Pr_wloss, aes(x = dpi, y = wloss, group = primary, col = primary)) +
  geom_smooth(se = FALSE)+
  geom_jitter()+
  theme_bw() +
  ggtitle("Weightloss during primary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E139", "E64", "E88","Eflab","UNI"),
                     values=c("red", "blue", "darkgreen","orange","violet"))


ggplot(Pr_wloss, aes(x = dpi, y = wloss, group = primary, col = primary)) +
  geom_smooth(se = FALSE)+
  geom_jitter()+
  theme_bw() +
  facet_grid(~primary) +
  ggtitle("Weightloss during primary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)")


# sec wloss

P3_w2 <- select(P3_Comp,label, EH_ID, dpi, wloss,challenge)
P3_w2 <- na.omit(P3_w2)

P4_w2 <- select(P4_complete,label, EH_ID, dpi, wloss, challenge)
P4_w2 <- na.omit(P4_w2)

Ch_wloss <- rbind(P3_w2, P4_w2)
Ch_wloss <- distinct(Ch_wloss)


ggplot(Ch_wloss, aes(x = dpi, y = wloss, group = challenge, col = challenge )) +
  geom_smooth(se = FALSE)+
  geom_jitter()+
  theme_bw() +
  ggtitle("Weightloss during secondary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E64", "E88", "UNI"),
                     values=c("blue", "darkgreen","violet"))


ggplot(Ch_wloss, aes(x = dpi, y = wloss, group = challenge, col = challenge)) +
  geom_smooth()+
  geom_jitter()+
  facet_grid(~challenge)+
  ggtitle("Weightloss during secondary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E64", "E88", "UNI"),
                     values=c("blue", "darkgreen","violet"))





P4_d1 <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experimental_design/P3_112019_Eim_design.csv"
P4_d1 <- read.csv(text = getURL(P4_d1))

P4_d2 <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experimental_design/P4a_082020_Eim_design.csv"
P4_d2 <- read.csv(text = getURL(P4_d2))

P4_d11 <- select(P4_d1, EH_ID, primary)
P4_d21 <- select(P4_d2, EH_ID, primary)

P4_D <- rbind(P4_d11, P4_d21)

P4_WW <- merge(Ch_wloss, P4_D)

ggplot(P4_WW, aes(x = dpi, y = wloss, group = EH_ID, col = primary)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_grid(~challenge)

ggplot(P4_WW, aes(x = dpi, y = wloss, group = challenge, col = challenge)) +
  geom_smooth(se = FALSE)+
  geom_jitter()+
  theme_bw()+
  facet_grid(~primary) +
  ggtitle("Weightloss during secondary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)")+
  scale_color_manual(breaks = c("E64", "E88", "UNI"),
                     values=c("blue", "darkgreen","violet"))


# Ococyst plot
#for primary

P4_O1 <- select(P4_complete,OPG, dpi, primary, EH_ID)
P4_O1 <- na.omit(P4_O1)

P4_O1$batch <- c("P4a")

P3_O1 <- select(P3_a, OPG, dpi, primary, EH_ID)
P3_O1$batch <- c("P3a")

OPG_a <- rbind(P4_O1, P3_O1)

P3_O2 <- select(P3_b, OPG, dpi, challenge, EH_ID)
P3_O2$batch <- c("P3b")

P4_O2 <- select(P4_complete,OPG, dpi, challenge, EH_ID)
P4_O2 <- na.omit(P4_O2)
P4_O2$batch <- c("P4b")

Pr_OPG <- rbind(P4_O1, P3_O1)
Ch_OPG <- rbind(P4_O2, P3_O2)

Inf_His1 <- select(P3_Comp, EH_ID, challenge, infHistory)
Inf_His1 <- na.omit(Inf_His1)
P3_OPGIH <- merge(Inf_His1,P3_O2)

Inf_His2 <- select(P4_complete, EH_ID, challenge, infHistory)
Inf_His2 <- na.omit(Inf_His2)
P4_OPGIH <- merge(Inf_His2,P4_O2)

P_OPGIH <- rbind(P3_OPGIH, P4_OPGIH)
P_OPGIH <-na.omit(P_OPGIH)

cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange","12"="pink")

ggplot(Pr_OPG, aes(x = dpi, y = OPG, col = primary)) +
  geom_smooth()+
  geom_point(size=2)+
  theme_bw()+
  facet_grid(~primary) +
  ylim(0,5000000) +
  ggtitle("Oocyst shedding for primary infection ") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)") + 
  scale_color_manual(breaks = c("E139", "E64", "E88","Eflab","UNI"),
                     values=c("red", "blue", "darkgreen","orange","violet"))


ggplot(P_OPGIH, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=2)+
  theme_bw()+
  facet_grid(~challenge) +
  ylim(0,3000000) +
  ggtitle("Oocyst shedding for secondary infection ") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)")+
  scale_color_manual(breaks = c("E64", "E88", "UNI"),
                     values=c("blue", "darkgreen","violet"))

ggplot(P_OPGIH, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=2)+
  theme_bw()+
  facet_grid(~batch) +
  ylim(0,3000000) +
  ggtitle("Oocyst shedding for secondary infection ") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E64", "E88", "UNI"),
                     values=c("blue", "darkgreen","violet"))
 


ggplot(P_OPGIH, aes(x = dpi, y = OPG, group = challenge, col = batch)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~infHistory) +
  ggtitle("Oocyst shedding regarding infection history") + ylab("Oocysts per gram (OPG)") + xlab("Day post infection (dpi)")

#wlos for sec

P_wlossCH <- merge(P_OPGIH,Ch_wloss) 

ggplot(P_wlossCH, aes(x = dpi, y = wloss, group = challenge, col = challenge)) +
  geom_smooth()+
  geom_jitter()+
  facet_grid(~batch)+
  ggtitle("Weightloss during secondary infection ") + ylab("Weightloss in percentage") + xlab("Day post infection (dpi)") +
  scale_color_manual(breaks = c("E64", "E88", "UNI"),
                     values=c("blue", "darkgreen","violet"))

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

P_Delta <- P_Delta[-c(9),]
P_Delta1 <- P_Delta[-c(13),]
P_Delta2 <- P_Delta1[-c(19),]


P_Deltax <- rbind(P_Delta2, LM_0354, LM337_D, LM346_D)

ggplot(subset(P_Deltax,!is.na(P_Deltax$delta)), aes(x = infHistory, y = delta, color = Eim_MC)) +
  geom_point()+
  theme_bw()+
  facet_grid(~challenge)  

ggplot(P_Deltax, aes(x = Eim_MC, y = delta, color = challenge)) +
  geom_violin()+
  geom_smooth() +
  geom_jitter()+
  theme_bw()

ggplot(P_Deltax, aes(x = Eim_MC, y = delta, color = challenge)) +
  geom_smooth() +
  geom_jitter()+
  ggtitle("Distribution of infection intensity") + xlab("Infection presence") + ylab("Infection intensity") +
  scale_color_manual(breaks = c("E64", "E88", "UNI"),
                     values=c("blue", "darkgreen","violet"))



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
                     values=c("blue","violet"))


P_Inf <- subset(P_E, P_E$Eim_MC == "infected")
P_Inf <- distinct(P_Inf)

ggplot(P_Inf, aes(x=challenge, y= IFNy_CEWE, color= challenge,main = "Infected" ))+
  geom_violin()+
  geom_jitter() 

ggplot(P_Inf, aes(x=delta, y= IFNy_CEWE, color = challenge))+
  geom_smooth()+
  geom_jitter() +
  facet_wrap(~challenge) +
  ggtitle("Presence of IFNγ in infected mice") + ylab("IFNγ from cecum wall") + xlab("Infection intensity")+
  scale_color_manual(breaks = c("E64", "E88"),
                     values=c("blue", "darkgreen"))


