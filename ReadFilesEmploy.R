#install.packages("plm")
library(AER)
library(plm)

library(dplyr)
library(MASS)
library(mvtnorm)
library(rddtools)
library(scales)
library(stargazer)
library(tidyr)

library(xtable)
library(foreign)
library(readxl)

library(rcompanion)
library(DescTools)

library(MASS)
library(reshape)

###########################################################################
#                          Reading data
###########################################################################

files.ready <- "/Users/wilmermartinez/Documents/RA_Jev/data/Ready_files/"
SuicidesA <- read.table(paste(files.ready,"Suicide_prop_2013_19",".txt",sep = ""), header = T)
Employ <- read.table(paste(files.ready, "Employ_rate_2010_18.txt",sep=""), header = T)
Pop <- read.table(paste(files.ready,"Population_2010_19",".txt",sep = ""), header = T)
Cognitive <- read.table(paste(files.ready,"cognitive_disability_2010_18",".txt",sep = ""), header = T)
HI <- read.table(paste(files.ready,"Health_Insurance_2010_18",".txt",sep = ""), header = T)
Ages <- read.table(paste(files.ready,"Ages_groupsSui_2010_18",".txt",sep = ""), header = T)
Unempl <- read.table(paste(files.ready,"Unemploy_rate_2010_18",".txt",sep = ""), header = T)
Gender <- read.table(paste(files.ready,"Male_10_18",".txt",sep = ""), header = T)
Income <- read.table(paste(files.ready,"IncomePC_10_18",".txt",sep = ""), header = T)
#houseType <- read.table(paste(files.ready, "UnitsH.txt",sep=""), header = T)

Business <- read.csv(paste(files.ready,"SH_Businesses_2012_2018",".csv",sep = ""), header = T)
colnames(Business)[1] <- c("GEO_ID2")
Business$GEO_ID <- substr(Business$GEO_ID2,nchar(Business$GEO_ID2[1]) - 4, nchar(Business$GEO_ID2[1]))
Business <- Business[,c(ncol(Business), 2, grep("^ratio", names(Business)))]
Business[grep("^ratio", names(Business))] <- Business[grep("^ratio", names(Business))]*100
colnames(Business)[3:ncol(Business)] <- c(paste("BLSratio_",2012:2018,sep=""))
#boxplot(Business[,grep("^ratio", names(Business))])
#houseType <- read.table(paste(files.ready, "UnitsH.txt",sep=""), header = T)

path <- "/Users/wilmermartinez/Documents/RA_Jev/data/project_Excess_mortality/"
Stratum <- read.csv(paste(path,"Stratum_munis.csv",sep = ""), header = T)
Stratum <- Stratum[order(Stratum$ResidencePlace),]
Stratum$GEO_ID <- unique(Suicides$GEO_ID)

Suicides <- Employ
Suicides <- Suicides[,-c(2)]
Suicides <- merge(Suicides, SuicidesA[,1:5],by.x = "GEO_ID",by.y = "GEO.id2")
###########################################################################
# This section is for organizing the data in format long
###########################################################################

longform <- function(data1, tvar, idvar){
  names(data1)[1] <- "GEO_ID"
  Suicides2 <- melt(data1[,c(1, grep(tvar, names(data1)))], id=c("GEO_ID"))
  name1 <- names(data1[,c(grep(tvar, names(data1)))])
  long.n <- nchar(name1[1])
  names(Suicides2)[3] <- idvar 
  Suicides2$year <- as.numeric(substr(Suicides2$variable, long.n-3,long.n))
  Suicides2$Merge <- paste(Suicides2[,1], Suicides2$year,sep = "")
  Suicides2 <- Suicides2[,-2]
  return(Suicides2)
}

#######################

Suicides2 <- longform(Suicides, "^Emp", "Employ")
#Cognitive2 <- longform(Cognitive, "^cogni", "Cognitive")
HI2 <- longform(HI, "^HI","HI")
Ages2a <- longform(Ages, "^y5_17_","y5_17")
Ages2b <- longform(Ages, "^y18_34_","y18_34")
Ages2c <- longform(Ages, "^y35_64_","y35_64")
Ages2d <- longform(Ages, "^y65_74_","y65_74")
Unempl2 <- longform(Unempl, "^Unemp","Unemp")
Gender2 <- longform(Gender, "^Male","Male")
Income2 <- longform(Income, "^I_","IncomePC")
Business2 <- longform(Business, "^BLSratio","BLSratio")

Suicides3 <- merge(Suicides2, HI2[,c(2,4)], by.x = "Merge", by.y = "Merge")
#Suicides3 <- merge(Suicides3, Cognitive2[,c(2,4)], by.x = "Merge", by.y = "Merge")
Suicides3 <- merge(Suicides3, Ages2a[,c(2,4)], by.x = "Merge", by.y = "Merge")
Suicides3 <- merge(Suicides3, Ages2b[,c(2,4)], by.x = "Merge", by.y = "Merge")
Suicides3 <- merge(Suicides3, Ages2c[,c(2,4)], by.x = "Merge", by.y = "Merge")
Suicides3 <- merge(Suicides3, Ages2d[,c(2,4)], by.x = "Merge", by.y = "Merge")
#Suicides3 <- merge(Suicides3, Unempl2[,c(2,4)], by.x = "Merge", by.y = "Merge")
Suicides3 <- merge(Suicides3, Gender2[,c(2,4)], by.x = "Merge", by.y = "Merge")
Suicides3 <- merge(Suicides3, Income2[,c(2,4)], by.x = "Merge", by.y = "Merge")
Suicides3 <- merge(Suicides3, Business2[,c(2,4)], by.x = "Merge", by.y = "Merge")
Suicides3 <- merge(Suicides3, Stratum[,c("seitert","GEO_ID")], by.x = "GEO_ID", by.y = "GEO_ID")

###########################################################################
# This section is for adding treatment variables
###########################################################################

pjump <- 6
Suicides3$Time <- rep(1:(length(unique(Suicides3$year))), length(unique(Suicides3$GEO_ID)))
Suicides3$Tint=rep(pjump,nrow(Suicides3))
Suicides3$Teval=Suicides3$Time - Suicides3$Tint
Suicides3 %>%
  mutate(D=ifelse(Time<pjump,0,1)) -> Suicides4