library(AER)
library(plm)
library(nlme)

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

###############################################################################
Ready.path <- "/Users/wilmermartinez/Documents/RA_Jev/data/Abuse_substance/Ready_files/"
#Abuse2012 <- read.table(paste(Ready.path,"AbuseAl_2012",".txt",sep = ""), header = T)

#   aos <- c(1997:2003); paste("AbuseAl_",aos,sep = "")
files1 <- c("AbuseAl_1992_1994","AbuseAl_1995_1999", "AbuseAl_2000_2004",
            "AbuseAl_2005_2009", "AbuseAl_2010_2012", paste("AbuseAl_201",3:8,sep = ""))
years <- seq(2013, 2018,1)
AbuseS5 <- NULL

for(ao in files1){
  #ao=files1[1]
  #k=k+1
  Abuse2018 <- read.table(paste(Ready.path, ao,".txt",sep = ""), header = T)
  
  AbuseS2 <- Abuse2018
  drop1 <- which(AbuseS2$CBSA == -9)
  if(length(drop1) > 0) AbuseS2 <- AbuseS2[-drop1,]
  fby1 <- factor(AbuseS2$YEAR, exclude = "")
  if(length(fby1) == 0) AbuseS2$YEAR <- as.numeric(substr(ao,nchar(ao)-3, nchar(ao)))
  AbuseS3a <- subset(AbuseS2, SUB1 >= 2)
  fby1 <- factor(AbuseS3a$YEAR, exclude = "")
  fby2 <- factor(AbuseS3a$CBSA, exclude = "")
  AbuseS3 <- aggregate(AbuseS3a$SUB1, by = list(fby1, fby2), FUN = "sum")
  
  AbuseS5 <- rbind(AbuseS5, AbuseS3)
}

###############################################################################


library("readxl")
# xlsx files
files.ready <- "/Users/wilmermartinez/Documents/RA_Jev/data/Ready_files/"
Regions <- data.frame(read_excel(paste(Ready.path,"regions",".xlsx",sep = ""), sheet = "Sheet3"))
PopPR <- data.frame(read_excel(paste(Ready.path,"populationPR_1930_2010",".xlsx",sep = ""), sheet = "Sheet1"))
pop <- read.table(paste(files.ready,"Population_2010_19",".txt",sep = ""), header = T)

#matplot(t(PopPR[,c(8:10)]), type = "l")
PopPR1 <- PopPR[,c(1,8:10)]
SuicidesA <- read.table(paste(files.ready,"Suicide_prop_2013_19",".txt",sep = ""), header = T)

#########################################################
##  Linear Interpolation
#########################################################
x <- c(1990,2000,2010)
x2 <- c(1990:2010)
PopPR2 <- NULL
for(j in 1:nrow(PopPR1)){
  y <- t(PopPR1[j,2:4])
  #op <- par(mfrow = c(2,1))
  mod1 <- lm(y~x)
  y2 <- round(mod1$coefficients[1] + mod1$coefficients[2]*x2,0)
  #plot(x2, y2, ylim = c(min(y,y2), max(y,y2)))
  #points(x,y,col = 2, pch = "*")
  PopPR2 <- rbind(PopPR2, data.frame(PopPR1[j,1], t(y2)))
}
colnames(PopPR2) <- c("Municipio", paste("POP.",x2,sep=""))
PopPR2 <- PopPR2[,-2]

### For this we used estimations of population from Excess Mort project
PopPR.A <- read.csv(paste(files.ready,"estimados_poblacionales",".csv",sep = ""), 
                   header = T, sep = "\t")
PopPR.A <- PopPR.A %>% filter(Año >=1991 & Año <=2010)
PopPR.A <- PopPR.A[order(PopPR.A$Año),]

PopPR.A <- data.frame(t(PopPR.A))

colnames(PopPR.A) <- c(paste("POP.",PopPR.A[1,],sep=""))
# remove the first line which is the year and the last which is the total PR
PopPR.A <- PopPR.A[-c(1,nrow(PopPR.A)),]
PopPR.A$Municipio2 <- rownames(PopPR.A)
PopPR.A$Municipio <- PopPR2$Municipio
#########################################################

PopPR3 <- merge(PopPR.A, SuicidesA[,c(1:2)], by.x = "Municipio", by.y = "Municipio")
PopPR3 <- PopPR3[,-which(names(PopPR3) == "Municipio2")]

popR <- merge(PopPR3, pop[,-3], by.x = "GEO.id2", by.y = "GEO_ID2")
popR <- merge(popR, Regions, by.x = "GEO.id2", by.y = "Municipio")

Re <- unique(popR$Region)
popR5 <- NULL
for(i in Re){
  #i=Re[4]
  popR2 <- subset(popR, Region == i)
  popR3 <- colSums(popR2[,grep("^POP", names(popR2))])
  popR4 <- popR2[1,]
  popR4[,grep("^POP", names(popR2))] <- popR3
  popR5 <- rbind(popR5, popR4) 
}

########################################################################
# Alcohol
colnames(AbuseS5) <- c("year", "Region", "A_substance")
data_wide1 <- spread(AbuseS5, year, A_substance)
for(i in 1:nrow(data_wide1)) data_wide1[i,which(is.na(data_wide1[i,]))] <- 0

data_wide1[1,-c(1)] <- data_wide1[1,-c(1)] + data_wide1[2,-c(1)]

data_wide2 <- merge(data_wide1, popR5, by.x = "Region", by.y = "Region")
data_wide3 <- data_wide2
data_wide3[,c(grep("^1", names(data_wide2)), grep("^2", names(data_wide2)))] <- data_wide2[,c(grep("^1", names(data_wide2)), grep("^2", names(data_wide2)))]/data_wide2[,grep("^POP", names(data_wide2))[-c(1,7:11,29)]] *10^4


###########################################################################
# Optional to run
###########################################################################
# Define Runplot1 = TRUE if want to run the next plot

if(Runplot1 == TRUE){

  color2 <- c("orange", "#00FF00", "#6495ED")
  #missing1 <- matrix(NA,nrow = 5, ncol=4)
  name_sale10 <- "Abuse_subs_regionNew"
  file.out.ex = "/Users/wilmermartinez/Documents/RA_Jev/R_program/Simu_result_ts_interv/"
  
  format = "eps"
  save=1
  #format = "png"
  
  if(save1==1 && format=="png"){
    name.out = paste(file.out.ex, name_sale10,".png", sep = "") 
    png(name.out, res=110)
  }else if(save1==1 && format=="eps"){
    setEPS()
    name.out = paste(file.out.ex, name_sale10,".eps", sep = "") 
    postscript(name.out)
  }  
  
  #quartz()
  op <- par(las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=1.2,cex.lab=1.5,cex.main=1)
  datos <- t(data_wide3[,c(grep("^2", names(data_wide2)))][,-1])
  
  matplot(2003:2018,
          datos,
          type="o", col = color2, ylab = "Substance abuse per 10000 inhabitants",
          xlab = "year",lty = rep(1,4),lwd = rep(2,4), pch=20, axes=FALSE,
          ylim = c(min(datos)-30,max(datos)))
  #abline(v=2016+0.5, col="red", lty=2,lwd=1.5)
  axis(2)
  axis(1, 2003:2018, seq(2003,2018,1))
  legend("bottom",c(as.character(data_wide3$Region)), col = color2,
         pch=20, ncol = 3,cex=1.2, box.col = "white")
  segments(2016.7,min(datos), 2016.7, max(datos), col="red",lty=2, lwd = 2)
  
  par(op)
  dev.off()
}

###########################################################################
# TS models
###########################################################################
datos <- t(data_wide3[,c(grep("^2", names(data_wide2)))][,-1])

datos <- data.frame(datos)
colnames(datos) <- paste("Re",as.character(data_wide3$Region), sep = "")
datos$year <- 2003:2018
x <- datos
xD <- c(rep(0,nrow(x)-2),1,1)
#mod0<- auto.arima(x[,3], xreg = xD)
#summary(mod0)


###########################################################################
# This section is for adding treatment variables
###########################################################################
datos1 <- data_wide3[,c(1, grep("^2", names(data_wide2))[-1])]
datos2 <- gather(datos1, "year", "cases", 2:ncol(datos1)) 

pjump <- nrow(x)-2
datos2$Time <- rep(1:(length(unique(datos2$year))), each=length(unique(datos2$Region)))
datos2$Tint <- rep(pjump,nrow(datos2))
datos2$Teval=datos2$Time - datos2$Tint
datos2 %>%
  mutate(D=ifelse(Time<=pjump,0,1)) -> Suicides4


###########################################################################
#                          Reading data
###########################################################################

files.ready <- "/Users/wilmermartinez/Documents/RA_Jev/data/Ready_files/"
#Suicides <- read.table(paste(files.ready,"Suicide_prop_2013_19",".txt",sep = ""), header = T)
Pop <- read.table(paste(files.ready,"Population_2010_19",".txt",sep = ""), header = T)
#Cognitive <- read.table(paste(files.ready,"cognitive_disability_2010_18",".txt",sep = ""), header = T)
HI <- read.table(paste(files.ready,"Health_Insurance_2010_18",".txt",sep = ""), header = T)
Ages <- read.table(paste(files.ready,"Ages_groupsSui_2010_18",".txt",sep = ""), header = T)
Unempl <- read.table(paste(files.ready,"Unemploy_rate_2010_18",".txt",sep = ""), header = T)
Gender <- read.table(paste(files.ready,"Male_10_18",".txt",sep = ""), header = T)
Income <- read.table(paste(files.ready,"IncomePC_10_18",".txt",sep = ""), header = T)

#################################
##  Income
popR <- Income
popR <- merge(popR, Regions, by.x = "GEO_ID", by.y = "Municipio")

Re <- unique(popR$Region)
popR10 <- NULL
for(i in Re){
  #i=Re[4]
  popR2 <- subset(popR, Region == i)
  popR3 <- colSums(popR2[,grep("^I_", names(popR2))])
  popR4 <- popR2[1,]
  popR4[,grep("^I_", names(popR2))] <- popR3
  popR10 <- rbind(popR10, popR4) 
}
popR11 <- popR10[,-c(1:2)]
colnames(popR11)[1:(ncol(popR11)-1)] <- 2010:2018 
datosI <- gather(popR11, "year", "Income", 1:(ncol(popR11)-1)) 

########################################################################
#################################
##  Gender
popR <- Gender
popR <- merge(popR, Pop[,-c(1:2)], by.x = "GEO_ID", by.y = "GEO_ID2")
for(i in 3:11) popR[,i] <- popR[,i]*popR[,(i+9)]/100 

popR <- merge(popR, Regions, by.x = "GEO_ID", by.y = "Municipio")

Re <- unique(popR$Region)
popR20 <- NULL
for(i in Re){
  #i=Re[1]
  popR2 <- subset(popR, Region == i)
  popR3 <- colSums(popR2[,grep("^Male_", names(popR2))])
  popR3a <- colSums(popR2[,grep("^POP.", names(popR2))])
  popR4 <- popR2[1,]
  popR4[,grep("^Male_", names(popR2))] <- popR3
  popR4[,grep("^POP.", names(popR2))] <- popR3a
  ii <- grep("^Male_", names(popR2))
  jj <- grep("^POP.", names(popR2))
  for(k in 1:length(ii)) popR4[,ii[k]] <- popR4[,ii[k]]/popR4[,jj[k]]*100 
  popR20 <- rbind(popR20, popR4) 
}
popR21 <- popR20[,c(grep("^Male_", names(popR20)),ncol(popR20))]
colnames(popR21)[1:(ncol(popR21)-1)] <- 2010:2018 
datosG <- gather(popR21, "year", "Gender", 1:(ncol(popR21)-1)) 

#################################
##  Umemployment
popR <- Unempl
popR <- merge(popR, Pop[,-c(1:2)], by.x = "GEO_ID", by.y = "GEO_ID2")
for(i in 3:11) popR[,i] <- popR[,i]*popR[,(i+9)]/100 

popR <- merge(popR, Regions, by.x = "GEO_ID", by.y = "Municipio")

Re <- unique(popR$Region)
popR25 <- NULL
for(i in Re){
  #i=Re[1]
  popR2 <- subset(popR, Region == i)
  popR3 <- colSums(popR2[,grep("^Unemp", names(popR2))])
  popR3a <- colSums(popR2[,grep("^POP.", names(popR2))])
  popR4 <- popR2[1,]
  popR4[,grep("^Unemp", names(popR2))] <- popR3
  popR4[,grep("^POP.", names(popR2))] <- popR3a
  ii <- grep("^Unemp", names(popR2))
  jj <- grep("^POP.", names(popR2))
  for(k in 1:length(ii)) popR4[,ii[k]] <- popR4[,ii[k]]/popR4[,jj[k]]*100 
  popR25 <- rbind(popR25, popR4) 
}
popR26 <- popR25[,c(grep("^Unemp", names(popR25)),ncol(popR25))]
colnames(popR26)[1:(ncol(popR26)-1)] <- 2010:2018 
datosU <- gather(popR26, "year", "Unemp", 1:(ncol(popR26)-1)) 

########################################################################
########################################################################

Suicides4$key <- paste(Suicides4$Region, Suicides4$year,sep="_")
Suicides4$Region <- as.factor(Suicides4$Region)
Suicides4$year <- as.factor(Suicides4$year)
datosI$key <- paste(datosI$Region, datosI$year, sep="_")
datosG$key <- paste(datosG$Region, datosG$year, sep="_")
datosU$key <- paste(datosU$Region, datosU$year, sep="_")

Suicides5 <- merge(Suicides4, datosI[,c("Income","key")], by.x = "key", by.y = "key", all.x = TRUE)
Suicides5 <- merge(Suicides5, datosG[,c("Gender","key")], by.x = "key", by.y = "key", all.x = TRUE)
Suicides5 <- merge(Suicides5, datosU[,c("Unemp","key")], by.x = "key", by.y = "key", all.x = TRUE)

##############################################################################
#  forecasting backward
##############################################################################
Suicides4$key <- paste(Suicides4$Region, Suicides4$year,sep="_")
Suicides4$Region <- as.factor(Suicides4$Region)
Suicides4$year <- as.factor(Suicides4$year)

###########################################
# Interpolation figures
###########################################
# This step depends on function Interp_fig locate 
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/Abuse_AddFunt.R")

# Define Runplot2 = TRUE to run this plot
if(Runplot2 == TRUE){
  #These three lines are optional if we want to run the fanchart per covariate
  # Define the path to sve the plot, save=1 means save the plot, format could be 
  #    "eps" or "png"
  file.out.ex = "/Users/wilmermartinez/Documents/RA_Jev/R_program/Simu_result_ts_interv/"
  Interp_figFC(popR11, file.out.ex, name.out ="Interp_IncomeNewB", save=1, format="eps")
  Interp_figFC(popR21, file.out.ex, name.out ="Interp_GenderNewB", save=1, format="eps")
  Interp_figFC(popR26, file.out.ex, name.out ="Interp_UnemplNewB", save=1, format="eps")
}
###########################################

datosI2 <- Interp_backF(popR11, "Income")
datosG2 <- Interp_backF(popR21, "Gender")
datosU2 <- Interp_backF(popR26, "Unemp")

if(intervals == TRUE){
  datosI2 <- Interp_backF2(popR11, "Income", indW)
  datosG2 <- Interp_backF2(popR21, "Gender", indW)
  datosU2 <- Interp_backF2(popR26, "Unemp", indW)
}


datosI2$key <- paste(datosI2$Region, datosI2$year, sep="_")
datosG2$key <- paste(datosG2$Region, datosG2$year, sep="_")
datosU2$key <- paste(datosU2$Region, datosU2$year, sep="_")

Suicides5A <- merge(Suicides4, datosI2[,c("Income","key")], by.x = "key", by.y = "key", all.x = TRUE)
Suicides5A <- merge(Suicides5A, datosG2[,c("Gender","key")], by.x = "key", by.y = "key", all.x = TRUE)
Suicides5A <- merge(Suicides5A, datosU2[,c("Unemp","key")], by.x = "key", by.y = "key", all.x = TRUE)

Suicides4 <- Suicides5A
