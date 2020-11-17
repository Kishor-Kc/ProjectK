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

P3a_weight <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3a_112019_Eim_Record.csv"
P3a_weight <- read.csv(text = getURL(P3a_weight))
P3b_weight <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3b_112019_Eim_Record.csv"
P3b_weight <- read.csv(text = getURL(P3b_weight))
P3_design <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experimental_design/P3_112019_Eim_design.csv"
P3_design <- read.csv(text = getURL(P3_design))


P3_design <- P3_design[, !colnames(P3_design)%in%c("labels")]

P3a <- merge(P3a_weight,P3_design, all = TRUE)
P3b <- merge(P3b_weight, P3_design, all = TRUE)
P3b$X <- NULL

P3 <- merge(P3a, P3b, all = TRUE)

#To make ggplot

ggplot(P3a, aes(x = dpi, y = wloss, group = primary, col = primary)) +
  geom_smooth(se = FALSE)+
  geom_jitter()+
  theme_bw() 

ggplot(P3a, aes(x = dpi, y = wloss, group = EH_ID, col = primary)) +
  geom_point()+
  geom_line()+
  theme_bw() +
  facet_grid(~primary)


P3b <- P3b[, !colnames(P3b)%in%c("faeces_weight", "day_change")]

P3b <- na.omit(P3b)

#plot some graphs for weightloss

ggplot(P3b, aes(x = dpi, y = wloss, group = EH_ID, col = challenge )) +
  geom_smooth()+
  geom_line()+
  theme_bw()

ggplot(P3b, aes(x = dpi, y = weight, group = EH_ID, col = primary )) +
  geom_point()+
  geom_line()+
  theme_bw()


ggplot(P3b, aes(x = dpi, y = wloss, group = EH_ID, col = primary)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_grid(~challenge)

ggplot(P3b, aes(x = dpi, y = wloss, group = EH_ID, col = challenge)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_grid(.~primary)


#make boxplot and more

P3_gx <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_CEWE_RTqPCR.csv"
P3_gx <- read.csv(text = getURL(P3_gx))
P3_gx$X <-NULL

Inten <-"https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_CEWE_qPCR.csv"
Inten <- read.csv(text = getURL(Inten))
Inten$X <- NULL

Intern1 <- merge(Inten, P3_design)
Comp <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_COMPLETE.csv"
Comp <- read.csv(text = getURL(Comp))
Comp$X <- NULL

gene1 <- select(Comp,"CXCR3", "IL.12", "IRG6", "EH_ID") 
gene1 <- na.omit(gene1)

Comp1 <- select(Comp,Eim_MC,EH_ID)
Comp1 <- na.omit(Comp1)
Comp2 <- merge(Intern1, Comp1)
Comp2 <- distinct(Comp2)

ggplot(subset(Comp,!is.na(Comp$delta)), aes(x = infHistory, y = delta, color = infHistory)) +
  geom_point()+
  theme_bw()+
  facet_grid(~challenge)

ggplot(subset(Comp,!is.na(Comp$delta)), aes(x = Eim_MC, y = delta, color = infHistory)) +
  geom_point()+
  theme_bw()+
  facet_grid(~challenge)

complete <- merge(P3_gx, Inten, by = "EH_ID")

#boxplot for gene expression

complete <- merge(complete, Comp1)

Comp.long <- reshape(complete, 
                     direction = "long",
                     varying = list(names(complete)[2:4]),
                     v.names = "NE",
                     times = c("CXCR3", "IRG6", "IL.12"),
                     timevar = "Target",
                     idvar = "EH_ID")

GE <- select(Comp, EH_ID, challenge)
GE <-na.omit(GE)

GET <- merge(GE, Comp.long)

ggplot(Comp.long, aes(x = Eim_MC, y = NE, color = challenge)) +
  geom_violin() + 
  geom_jitter() +
  facet_wrap(~Target) +
  ggtitle("P3 gene expression") + xlab("Eim_MC") + ylab("normalised gene expression")

ggplot(GET, aes(x = challenge, y = NE, color = challenge)) +
  geom_violin() + 
  geom_jitter() +
  facet_wrap(~Target) +
  ggtitle("P3 gene expression") + xlab("Eimeria") + ylab("normalised gene expression")

ggplot(Comp.long, aes(x = Eim_MC, y = delta, color = Eim_MC,)) +
  geom_boxplot() + 
  geom_jitter() + facet_wrap(~Target)

# nothing just tried boxplot
boxplot(gene1,data=gene1, main="melting curve eimeria expression", 
        xlab="melting curve expression", ylab="normalised gene expression")

boxplot(gene1,data=gene1, main="Gene Expression", xlab="gene", ylab="delta") 

#ggplot with facetwrap

ggplot(Intern1, aes(x = challenge, y = delta, group = EH_ID, col = primary)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_grid(~primary)



OPGx <- select(Comp,"OPG","dpi","challenge", "EH_ID") 
OPGx <- na.omit(OPGx)

OPG1 <- select(Comp, "OPG", "dpi", "primary", "EH_ID")
OPG1 <- na.omit(OPG1)

# fix problem with 0340,0335

LM_0340 <- subset(OPGx, OPGx$EH_ID == "LM_0340")

LM_O335 <-  subset(OPGx, OPGx$EH_ID == "LM_0335")

ggplot(OPG, aes(x = dpi, y = OPG, group = EH_ID, col = challenge)) +
  geom_smooth()+
  geom_line()+
  theme_bw()+
  facet_grid(~challenge)


ggplot(OPG1, aes(x = dpi, y = OPG, group = EH_ID, col = primary)) +
  geom_smooth()+
  geom_line()+
  theme_bw()+
  facet_grid(~primary)

ggplot(Comp, aes(x = dpi, y = OPG, group = EH_ID, col = infHistory)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~infHistory)

# more boxplot and use facetwrap


ggplot(Comp.long, aes(x = Eim_MC, y = NE)) +
  geom_point(size = 3) +
  facet_wrap("Target", scales = "free") +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14, face = "bold"),
        legend.text=element_text(size=12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        title = element_text(size = 14, face = "bold")) +
  ggtitle("P3 gene expression")

#p4

P4 <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P4_082020_Eim_COMPLETE.csv"
P4 <- read.csv(text = getURL(P4))

P4_1 <- select(P4, label, dpi, infHistory, wloss, primary, challenge, OPG)

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


#P3 + P4 together

# primary wloss

P3_w1 <- select(Comp,label, EH_ID, dpi, wloss,primary)
P3_w1 <- na.omit(P3_w1)

P4_w1 <- select(P4,label, EH_ID, dpi, wloss, primary)
P4_w1 <- na.omit(P4_w1)

P4_w <- rbind(P3_w1, P4_w1)
P4_w <- P4_w[-c(195),]

ggplot(P4_w, aes(x = dpi, y = wloss, group = primary, col = primary)) +
  geom_smooth(se = FALSE)+
  geom_jitter()+
  theme_bw() 

ggplot(P4_w, aes(x = dpi, y = wloss, group = EH_ID, col = primary)) +
  geom_point()+
  geom_line()+
  theme_bw() +
  facet_grid(~primary)

# sec wloss

P3_w2 <- select(Comp,label, EH_ID, dpi, wloss,challenge)
P3_w2 <- na.omit(P3_w2)

P4_w2 <- select(P4,label, EH_ID, dpi, wloss, challenge)
P4_w2 <- na.omit(P4_w2)

P4_wS <- rbind(P3_w2, P4_w2)

P4_w3 <- merge(P4_wS,P4_w)

ggplot(P4_wS, aes(x = dpi, y = wloss, group = EH_ID, col = challenge )) +
  geom_smooth(se = FALSE)+
  geom_jitter()+
  theme_bw()

ggplot(P4_wS, aes(x = dpi, y = wloss, group = EH_ID, col = challenge)) +
  geom_point()+
  geom_line() +
  theme_bw() +
  facet_grid(~challenge)


P4_d1 <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experimental_design/P3_112019_Eim_design.csv"
P4_d1 <- read.csv(text = getURL(P4_d1))

P4_d2 <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experimental_design/P4a_082020_Eim_design.csv"
P4_d2 <- read.csv(text = getURL(P4_d2))

P4_d11 <- select(P4_d1, EH_ID, primary)
P4_d21 <- select(P4_d2, EH_ID, primary)

P4_D <- rbind(P4_d11, P4_d21)

P4_WW <- merge(P4_wS, P4_D)

ggplot(P4_WW, aes(x = dpi, y = wloss, group = EH_ID, col = primary)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_grid(~challenge)

ggplot(P4_WW, aes(x = dpi, y = wloss, group = EH_ID, col = challenge)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_grid(~primary)

# Ococyst plot
#for primary

P4_O1 <- select(P4,OPG, dpi, primary, EH_ID)
P4_O1 <- na.omit(P4_O1)

P4_O1$batch <- c("P4a")
OPG1$batch <- c("P3a")

OPG_a <- rbind(P4_O1, OPG1)

OPG$batch <- c("P3b")
P4_OPG1$batch <- c("P4b")



OPG_b <- rbind(OPG, P4_OPG1)

P4_OPG <- rbind(P4_O1, OPG1)


ggplot(P4_OPG, aes(x = dpi, y = OPG, col = primary)) +
  geom_smooth()+
  geom_point(size=1.5)+
  theme_bw()+
  facet_grid(~primary) +
  ylim(0,5000000)



P4_O2 <- select(P4,OPG, dpi, challenge, EH_ID)
P4_O2 <- na.omit(P4_O2)
P4_OPG1 <- rbind(P4_O2, OPG)

ggplot(P4_OPG1, aes(x = dpi, y = OPG, col = challenge)) +
  geom_smooth()+
  geom_point(size=3)+
  theme_bw()+
  facet_grid(~challenge) +
  ylim(0,3000000)

ggplot(OPG_a, aes(x = dpi, y = OPG, group = EH_ID, col = batch)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~infHistory)

# try to join P3 complete and P4 complete

P3_Comp <- select(Comp, EH_ID, dpi, OPG, infHistory)
P4_Comp <- select(P4, EH_ID, dpi, OPG, infHistory)

PP <- rbind(P3_Comp, P4_Comp)
PP <- na.omit(PP)


P <- merge(OPG_b, PP)

ggplot(P, aes(x = dpi, y = OPG, group = EH_ID, col = batch)) +
 geom_smooth()+
  geom_point()+
  theme_bw()+
  facet_wrap(~infHistory)

# Delta Eim_MC


P4_Delta <- select( P4, EH_ID, delta, Eim_MC, infHistory,challenge)
P4_Delta <- na.omit(P4_Delta)

P4_Delta$Eim_MC[P4_Delta$Eim_MC == "TRUE"] <- "infected"
P4_Delta$Eim_MC[P4_Delta$Eim_MC == "FALSE"] <- "uninfected"

P4_Delta$Eim_MC <- as.character(P4_Delta$Eim_MC)

P3_Delta <- select(Comp, EH_ID, delta, Eim_MC, infHistory,challenge)
P3_Delta <- na.omit(P3_Delta)
P3_Delta$Eim_MC <- as.character(P3_Delta$Eim_MC)

P3_Delta$Eim_MC[P3_Delta$Eim_MC == "pos"] <- "infected" 
P3_Delta$Eim_MC[P3_Delta$Eim_MC == "neg"] <- "uninfected"

P_Delta <- rbind(P3_Delta, P4_Delta)
P_Delta <- distinct(P_Delta)

# change LM_0354 uninfected to infected
LM_0354 <- subset(P_Delta, P_Delta$EH_ID == "LM_0354")
LM_0354$Eim_MC[LM_0354$Eim_MC == "uninfected"] <- "infected" 

LM335_D <- subset(P_Delta, P_Delta$EH_ID == "LM_0335")
LM335_D$Eim_MC[LM335_D$Eim_MC == "infected"] <- "uninfected" 

LM340_D <- subset(P_Delta, P_Delta$EH_ID == "LM_0340")
LM340_D$Eim_MC[LM340_D$Eim_MC == "infected"] <- "uninfected" 

P_Delta <- P_Delta[-c(21),]
P_Delta1 <- P_Delta[-c(3),]
P_Delta2 <- P_Delta1[-c(6),]


P_Deltax <- rbind(P_Delta2, LM_0354, LM335_D, LM340_D)

ggplot(subset(P_Delta,!is.na(P_Delta$delta)), aes(x = infHistory, y = delta, color = Eim_MC)) +
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
  theme_bw() +
  ggtitle("Distribution of infection intensity") + xlab("Infection presence") + ylab("Delta CT")



#try for the average intensity

AVG <- subset(P_Delta, P_Delta$challenge== "E64")
AVG_64 <- mean(AVG[,"delta"], na.rm = TRUE)

AVG1 <- subset(P_Delta, P_Delta$challenge== "E88")
AVG1 <- distinct(AVG1)
AVG_88 <- mean(AVG1[,"delta"], na.rm = TRUE)
# INFy ELISA result

P3_IFN <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_CEWE_ELISA.csv"
P3_IFN <- read.csv(text = getURL(P3_IFN))
P3_IFN$X <- NULL


P3_E <- merge(P3_IFN,P3_Delta)
names(P3_E)[names(P3_E) == "IFNy"] <- "IFNy_CEWE"
P3_EE <- select(P3_E, EH_ID, infHistory, delta, challenge, IFNy_CEWE, Eim_MC)

P4_E <- select(P4, EH_ID, IFNy_CEWE)
P4_E <- na.omit(P4_E)
P4_EE <- merge(P4_Delta, P4_E)

P_E <- rbind(P3_EE, P4_EE)
P_E <- distinct(P_E)

ggplot(P_E, aes(y = IFNy_CEWE, x = delta, color= Eim_MC)) +
  geom_smooth(method="lm")+
  geom_jitter()+
  theme_bw()+
  facet_wrap(~Eim_MC) +
stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x = -10, label.y = 750)+
  stat_regline_equation(label.x = -10, label.y = 800)


P_Inf <- subset(P_E, P_E$Eim_MC == "infected")
P_Inf <- distinct(P_Inf)

ggplot(P_Inf, aes(x=challenge, y= IFNy_CEWE, color= challenge,main = "Infected" ))+
  geom_violin()+
  geom_jitter() 

ggplot(P_Inf, aes(x=delta, y= IFNy_CEWE, color = challenge))+
  geom_violin()+
  geom_jitter() +
  facet_wrap(~challenge)


ggscatter(P_Inf, x = "challenge", y = "IFNy_CEWE",
          color = "challenge", main = "Infected Mice",
           repel = TRUE) + 
  geom_smooth(size=3) +
  geom_line() 



