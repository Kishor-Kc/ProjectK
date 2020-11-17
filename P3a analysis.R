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




P3_complete <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/Experiment_results/P3_112019_Eim_COMPLETE.csv"
P3_complete <- read.csv(text = getURL(P3_complete))

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

ggplot(Comp.long, aes(x = Eim_MC, y = NE, color = Eim_MC)) +
  geom_violin() + 
  geom_jitter() +
  facet_wrap(~Target) +
  ggtitle("P3 gene expression") + xlab("Eim_MC") + ylab("normalised gene expression")

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

OPG <- select(Comp,"OPG","dpi","challenge", "EH_ID") 
OPG <- na.omit(OPG)

OPG1 <- select(Comp, "OPG", "dpi", "primary", "EH_ID")
OPG1 <- na.omit(OPG1)

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


