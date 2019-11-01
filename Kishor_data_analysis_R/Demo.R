library(Rmisc)
library(httr)
library(RCurl)
library(dplyr)
library(Hmisc)
library(data.table)

INFa <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/2_designTables/E7a_112018_Eim_design.csv"
INFb <-"https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/2_designTables/E7a_112018_Eim_design.csv"

INFa <- read.csv(text=getURL(INFa))
INFb <- read.csv(text =getURL(INFb))

col2keep <- c("EH_ID", "HybridStatus" )
INFa<- INFa[col2keep]
INFb <- INFb[col2keep]

HIS_a <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/2_designTables/E7a_112018_Eim_infection.history.csv"
HIS_a <- read.csv(text=getURL(HIS_a))
HIS_b <- "https://raw.githubusercontent.com/derele/Eimeria_Lab/master/data/2_designTables/E7b_112018_Eim_infection.history.csv"
HIS_b <- read.csv(text=getURL(HIS_b))
INF_His <- rbind(HIS_a, HIS_b)

INF <- rbind(INFa, INFb)
INF <- merge(INF, INF_His, by = "EH_ID")


