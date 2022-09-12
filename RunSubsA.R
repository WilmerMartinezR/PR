
######################################################################
#   This file run the data for the application Median House prices
#             (MHprices)
######################################################################

# To run the ReadFiles we need to define Runplot1 and Runplot2 TRUE or FALSE
#    if Runplot1 = TRUE run the plot the series Substances A per region
#    if Runplot2 = TRUE run the plot fanchar for the covariates defined so far
Runplot1 <- Runplot2 <- FALSE

# if intervals is true we can consider limits of the intervals in the backasting
# in the sequence seq1 = c(seq(50,99,5),99) defined in the function backforecast.
# To define an specific interval we have to define 
#  indW = 1 point estimation
#  indW = 2 liminf 50, indW = 3 limsup 50, so on... indW = length(seq1)  limsup 99                                                
intervals <- TRUE
indW = 21
# 2,3,  14,15  20,21

source("/Users/wilmermartinez/Documents/RA_Jev/R_program/ReadFilesSubstancesA.R")

library("writexl")
file.data = "/Users/wilmermartinez/Documents/RA_Jev/data/Final_data/"
nom_out2 <- paste(file.data, "SubstancesAbuse.xlsx",sep = "")
#write_xlsx(Suicides4, nom_out2)
######################################
# Global parameters
######################################
#outcome <- "log(Ndeath.pr) ~ "
#logT = TRUE
outcome <- "cases ~ "
logT = FALSE
outcome1 <- "cases"

# Since some variables only have data from 2012 we remove 2010 - 2011
Full_list_variables <- "D + Time + Gender + Income + Unemp"

Full_list_variables_wD <- "Time + Gender + Income + Unemp"

# counter effet variable name
CountFE_name = "casesH0"
##  Define Full data set
# Suicides4 <- 

# To run StepWise procedure 
runSW = TRUE

######################################################################
#   This file run the case 1, this case is describe inside Case1_PD.R
######################################################################
Suicides4 <- na.omit(Suicides4)
Suicides4$year <- as.numeric(as.character(Suicides4$year))
colnames(Suicides4)[1] <- "Merge"
colnames(Suicides4)[2] <- "GEO_ID"
Suicides4$GEO_ID <- as.character(Suicides4$GEO_ID)
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/Case1_PD_forSubsA.R")

if(intervals == TRUE){
  # indW = 1, 2,3,  14,15  20,21; see line 12 before
  CP <- Case1_output$GeneralModel$Model1_OutFE$ModelFit
  CP50Linf <- Case1_output$GeneralModel$Model1_OutFE$ModelFit
  CP50Lsup <- Case1_output$GeneralModel$Model1_OutFE$ModelFit
  
  CP80Linf <- Case1_output$GeneralModel$Model1_OutFE$ModelFit
  CP80Lsup <- Case1_output$GeneralModel$Model1_OutFE$ModelFit

  CP95Linf <- Case1_output$GeneralModel$Model1_OutFE$ModelFit
  CP95Lsup <- Case1_output$GeneralModel$Model1_OutFE$ModelFit

  CPs <- rbind(CP, CP50Linf, CP50Lsup, CP80Linf, CP80Lsup, CP95Linf, CP95Lsup) 
  name.out1 = paste(file.out.ex, "estimationsIntervalsBack_subsA",".txt", sep = "")
  write.table(CPs, name.out1, sep = "\t")
}
xtable(CPs[,-c(7:8)], digits = 4)
######################################################################
#   This file run the case 2, this case is describe inside Case2_PD.R
######################################################################
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/Case2_PD_forSubsA.R")

# Result
# Case2_output$GeneralModel$Model1_OutRE$ModelFit

######################################################################
#   This file run the case 3, this case is describe inside Case3_PD.R
#   To run this case is necesary to run first the code UnitChange.R
#    which calculat the unit significant changes
######################################################################

Start_year=2003
Last_year=2018

# This is the position of predictor in the Total data
#   usually called Suicides4
ListCovars <- c(5,9:11)
# kindComp <- "MedianC"
kindComp <- "AverageC"
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/UnitChange.R")
# Result
# outputUnitChang
# twoparts = TRUE
#source("/Users/wilmermartinez/Documents/RA_Jev/R_program/Case3_PD_2Parts.R")
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/Case3_PD.R")
# Result
# Case3_output$ForesModel$ForesQM
# Case3_output$ForesModel$ForestOutRE


######################################################################
#   This file has the base functions for this procedure
######################################################################
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/FunctionsPD.R")
# Results in
# outputUnitChang

####################################################################################
###                           Graphical results
####################################################################################


####################################################################################
### Now we compare the units (before and after) that (do not) change significantly 
####################################################################################

datFore1 <- Case1_output$DataForest$data_evalTE1
colnames(datFore1) <- c("GEO_ID", "year", outcome1, paste(outcome1, "M1",sep=""))
datFore1$merge <- rownames(datFore1)
datFore2 <- Case2_output$DataForest$data_evalTE1
colnames(datFore2) <- c("GEO_ID", "year", outcome1, paste(outcome1, "M2",sep=""))
datFore2$merge <- rownames(datFore2)
datFore3 <- Case3_output$DataForest$data_evalTE1
colnames(datFore3) <- c("GEO_ID", "year", outcome1, paste(outcome1, "M3",sep=""))
datFore3$merge <- rownames(datFore3)

datFore <- merge(datFore1, datFore2[,c(4:5)], by.x = "merge", by.y = "merge")
datFore <- merge(datFore, datFore3[,c(4:5)], by.x = "merge", by.y = "merge")
datFore$new <- rowMeans(datFore[,5:ncol(datFore)])
colnames(datFore)[ncol(datFore)] <- paste(outcome1, "Av",sep="")
datFore <- datFore[,-1]
if(logT == TRUE) datFore$Ndeath.pr <- log(datFore$Ndeath.pr)

coefB <- outputUnitChang$model_ChangU$ModelFit
# Define the number of checking the model running this
outputUnitChang$model_ChangU$ModelObj$formula
npredictor <- 3

# The file Suicides should be in format wide. Suicides.1 is define to consider
#  the Ndeath transform variable
# Suicides.1 <- Suicides3[,c("GEO_ID","Municipio","year", "Ndeath.pr")]
Suicides <- reshape(Suicides4[,2:4], idvar = c("GEO_ID"), timevar = "year", direction = "wide")

# Initials according to the outcome in data Suicides which is run in ReadFilesMHprices.R
InitialN.0<- "^cases"
# Inicials for the names of the outcome
InitialN <- paste("^",outcome1,sep="")

# To save this plot define
save1 = 1 # save, or = 0 just to print 
Name_output <- "SubstancesASig_ModNew2"
file.out.ex = "/Users/wilmermartinez/Documents/RA_Jev/R_program/Simu_result_ts_interv/"
# For now format can be .eps or .png
format = "eps"
#format = "png"

if(save1==1 && format=="png"){
  name.out = paste(file.out.ex, Name_output,".png", sep = "") 
  png(name.out, res=110)
}else if(save1==1 && format=="eps"){
  setEPS()
  name.out = paste(file.out.ex, Name_output,".eps", sep = "") 
  postscript(name.out)
}  
titles <- c("Tendency ", "Significant changes ")
op <- par(mfrow=c(1,2),las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
op <- par(mfrow=c(2,2),las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
#Suicides <- Suicides[,-c(2:3)]
for(w in 0:1){
  #w=1
  ## This remove the years 2010 and 2011
  coefC <- subset(coefB[-c(1:(npredictor+1)),], SigPval == w)
  proport <- round(nrow(coefC)/nrow(Suicides)*100,2)
  colnames(Suicides)[1] <- "GEO_ID"
  if(logT == TRUE) for(k in 3:ncol(Suicides)) Suicides[,k] <- log(Suicides[,k])
  estimator1a <- merge(coefC[,c("GEO_ID","SigPval")], Suicides, by.x = "GEO_ID", by.y = "GEO_ID")
  datos1a <- estimator1a[,grep(InitialN.0, names(estimator1a))]
  name_sale1 <- Name_output
  #quartz()
  box_summary(datos1a, paste(titles[w+1], proport," (%)",sep=""),
              xfin=16, yini=Start_year,yfin=Last_year,xint=14.7,
              save1=0,name_sale1,"eps",file.out.ex)
}
for(w in 0:1){
  coefC <- subset(coefB[-c(1:npredictor),], SigPval == w)
  datosA <- datFore[,1:4]
  datosA_w <- reshape(datosA, idvar = "GEO_ID", timevar = "year", direction = "wide")
  datosA_w <- merge(datosA_w, coefC[,c("GEO_ID","SigPval")], by.x = "GEO_ID", by.y = "GEO_ID")
  datos1a <- datosA_w[,grep(InitialN, names(datosA_w))]
  #quartz()
  box_summary3(datos1a, title="", xfin=6, yini=Start_year,yfin=Last_year,xint=4.7,
               save1=0,n.models=(ncol(datos1a)-2)/2,AoF=2017, name_sale1,format, file.out.ex)
}
par(op)
dev.off()
#############################################################
### Plotting some municipalities to check observations 
##    and counterfactual effects (in red)
#############################################################

datosT <- Suicides4

### These three lines filter the no change units if SigPval = 0
##  coefC is run below
munis <- unique(datosT$GEO_ID)
coefC <- subset(coefB[-c(1:(npredictor+1)),], SigPval == 1)
cuales1 <- which(coefC$GEO_ID %in% munis)

# To save this plot define
save1 = 1 # save, or = 0 just to print 
Name_output2 <- "EmployUnits_ModNew"
file.out.ex = "/Users/wilmermartinez/Documents/RA_Jev/R_program/Simu_result_ts_interv/"
format = "eps"
#format = "png"
if(logT == TRUE) datosT$Ndeath.pr <- log(datosT$Ndeath.pr)
plot_cfe(datosT, datFore, Nrow=5, Ncol=5, cuales=cuales1[1:21], yini=Start_year,yfin=Last_year, 
         save1,Name_output2,format, file.out.ex)


###########
###################
##################################
####################################################################################
###                           Organizing Results
####################################################################################


library(stargazer)

model1 <- Case1_output$Model0$Model0_OutRE$ModelObj
model2 <- Case1_output$GeneralModel$Model1_OutFE$ModelObj
#model3 <- Case2_output$GeneralModel$Model1_OutFE$ModelObj
#model4 <- Case3_output$GeneralModel$Model1_OutFE$ModelObj

rob_se <- list(
  sqrt(diag(vcovHC(model1, type = "HC4"))),
  sqrt(diag(vcovHC(model2, type = "HC4"))))
  #sqrt(diag(vcovHC(model3, type = "HC1"))),
  #sqrt(diag(vcovHC(model4, type = "HC1"))))

# generate the table
stargazer(model1,model2,
          #stargazer(suic_mod5.2, suic_mod5.3, suic_mod5.4,suic_mod5.5,
          digits = 3,
          header = FALSE,
          type = "latex", 
          se = rob_se,
          title = "Linear Panel Regression Models of Substances Abuse Rate",
          model.numbers = FALSE,
          column.labels = c("(0)", "(1)"))


####################################################################################
###                  This measure the Treatment Effect, based on 
###                  paired t-test
####################################################################################

datFore2 <- reshape(datFore[,1:4], idvar = c("GEO_ID"), timevar = "year", direction = "wide")
xtable(datFore2, digits = 3)

# paired t-test
TreatE <- function(datFore2, nameDif="Dif"){
  TE2 <- NULL
  for(M in 4:(ncol(datFore2))){
    #M=4
    y1 <- datFore2[,outcome1]
    y2 <- datFore2[,M]
    TE <- t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric
    TE2 <- rbind(TE2, c(Estimation = TE$estimate, Pvalue = TE$p.value, CI = TE$conf.int))
  }
  TE2 <- data.frame(TE2)
  #TE2a <- TE2*78
  #TE2a$Pvalue <- TE2$Pvalue
  colnames(TE2) <- c(nameDif, "pval","L_CI","U_CI")
  rownames(TE2) <- c(paste("M", 1:4, sep=""))
  return(TE2)
}


datFore2 <- datFore
datFore2a <- datFore %>% filter(year==2017)
datFore2b <- datFore %>% filter(year==2018)

Twoy <- TreatE(datFore2, "Dif_SubstancesA")
Y2017 <- TreatE(datFore2a, "Dif_SubstancesA")
Y2018 <- TreatE(datFore2b, "Dif_SubstancesA")

Full <- (Twoy)
Y2017.TE <- (2*Twoy - Y2018)
Y2018.TE <- (2*Twoy - Y2017)

Result.TE <- cbind(Full[,-2], rep(NA,4), Y2017.TE[,-2],rep(NA,4),Y2018.TE[,-2])
xtable(Result.TE, digits = 3)

# coefB using MedianC, but since only one region does not change we cannot run
# case3
npredictor <- 4
ToMap <- coefB[-c(1:npredictor),c("SigPval", "GEO_ID")]
Regions2 <- merge(Regions, ToMap, by.x = "Region", by.y = "GEO_ID", all.x = T)
Regions2 <- merge(pop[,c("GEO_ID","GEO_ID2")], Regions2, by.x = "GEO_ID2", by.y = "Municipio", all.x = T)
Regions2 <- Regions2[,-2]
colnames(Regions2)[1] <- "GEO_ID"
write.csv(Regions2, file = paste(file.out.ex, "SubstAbuseCases",".csv",sep=""), row.names = FALSE)
