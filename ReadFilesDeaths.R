
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
library(nlme)
library(lattice)


###########################################################################
#                          Reading data
###########################################################################

files.ready <- "/Users/wilmermartinez/Documents/RA_Jev/data/Ready_files/"
Suicides <- read.table(paste(files.ready,"Deaths_2010_2018",".txt",sep = ""), header = T)
Pop <- read.table(paste(files.ready,"Population_2010_19",".txt",sep = ""), header = T)
Cognitive <- read.table(paste(files.ready,"cognitive_disability_2010_18",".txt",sep = ""), header = T)
HI <- read.table(paste(files.ready,"Health_Insurance_2010_18",".txt",sep = ""), header = T)
Ages <- read.table(paste(files.ready,"Ages_groupsSui_2010_18",".txt",sep = ""), header = T)
Unempl <- read.table(paste(files.ready,"Unemploy_rate_2010_18",".txt",sep = ""), header = T)
Gender <- read.table(paste(files.ready,"Male_10_18",".txt",sep = ""), header = T)
Income <- read.table(paste(files.ready,"IncomePC_10_18",".txt",sep = ""), header = T)

path <- "/Users/wilmermartinez/Documents/RA_Jev/data/project_Excess_mortality/"
Stratum <- read.csv(paste(path,"Stratum_munis.csv",sep = ""), header = T)
Stratum <- Stratum[order(Stratum$ResidencePlace),]
Stratum$GEO_ID <- unique(Suicides$GEO_ID)
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

#Suicides2 <- longform(Suicides, "^Count_", "Suicides")
Suicides2 <- Suicides
Cognitive2 <- longform(Cognitive, "^cogni", "Cognitive")
HI2 <- longform(HI, "^HI","HI")
Ages2a <- longform(Ages, "^y5_17_","y5_17")
Ages2b <- longform(Ages, "^y18_34_","y18_34")
Ages2c <- longform(Ages, "^y35_64_","y35_64")
Ages2d <- longform(Ages, "^y65_74_","y65_74")
Unempl2 <- longform(Unempl, "^Unemp","Unemp")
Gender2 <- longform(Gender, "^Male","Male")
Income2 <- longform(Income, "^I_","IncomePC")

Pop.1 <- Pop
Pop.1$GEO_ID <- Pop$GEO_ID2
Pop.1 <- Pop.1[,-ncol(Pop.1)]
Pop2 <- longform(Pop.1, "^POP","POP")
colnames(Suicides2)[1] <- "Merge"
Suicides3 <- merge(Suicides2, Pop2[,c(2,4)], by.x = "Merge", by.y = "Merge")
Suicides3$Ndeath.pr <- Suicides3$Ndeath/Suicides3$POP * 10000
Suicides3 <- merge(Suicides3, Stratum[,c("seitert","GEO_ID")], by.x = "GEO_ID", by.y = "GEO_ID")

Suicides3 <- merge(Suicides3, Cognitive2[,c(2,4)], by.x = "Merge", by.y = "Merge", all.x = T)
Suicides3 <- merge(Suicides3, HI2[,c(2,4)], by.x = "Merge", by.y = "Merge", all.x = T)
Suicides3 <- merge(Suicides3, Ages2a[,c(2,4)], by.x = "Merge", by.y = "Merge", all.x = T)
Suicides3 <- merge(Suicides3, Ages2b[,c(2,4)], by.x = "Merge", by.y = "Merge", all.x = T)
Suicides3 <- merge(Suicides3, Ages2c[,c(2,4)], by.x = "Merge", by.y = "Merge", all.x = T)
Suicides3 <- merge(Suicides3, Ages2d[,c(2,4)], by.x = "Merge", by.y = "Merge", all.x = T)
Suicides3 <- merge(Suicides3, Unempl2[,c(2,4)], by.x = "Merge", by.y = "Merge", all.x = T)
Suicides3 <- merge(Suicides3, Gender2[,c(2,4)], by.x = "Merge", by.y = "Merge", all.x = T)
Suicides3 <- merge(Suicides3, Income2[,c(2,4)], by.x = "Merge", by.y = "Merge", all.x = T)


###########################################################################
# This section is for adding treatment variables
###########################################################################

pjump <- 8
Suicides3$Time <- rep(1:(length(unique(Suicides3$year))), length(unique(Suicides3$GEO_ID)))
Suicides3$Tint=rep(pjump,nrow(Suicides3))
Suicides3$Teval=Suicides3$Time - Suicides3$Tint
Suicides3 %>%
  mutate(D=ifelse(Time<pjump,0,1)) -> Suicides4
