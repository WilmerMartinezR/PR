
######################################################################
#   This file run the data for the application Median House prices
#             (MHprices)
######################################################################
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/ReadFilesEmploy.R")

library("writexl")
file.data = "/Users/wilmermartinez/Documents/RA_Jev/data/Final_data/"
nom_out2 <- paste(file.data, "Employment.xlsx",sep = "")
#write_xlsx(Suicides4, nom_out2)
######################################
# Global parameters
######################################
#outcome <- "log(Ndeath.pr) ~ "
#logT = TRUE
outcome <- "Employ ~ "
logT = FALSE
outcome1 <- "Employ"

# Since some variables only have data from 2012 we remove 2010 - 2011
Full_list_variables <- "D + Time + HI+ y5_17 + y18_34 + y35_64 + y65_74+
                            seitert + Male+ IncomePC+ BLSratio"

Full_list_variables_wD <- "Time + HI+ y5_17 + y18_34 + y35_64 + y65_74 + 
                            seitert + Male + IncomePC + BLSratio"

# counter effet variable name
CountFE_name = "EmployH0"
##  Define Full data set
# Suicides4 <- 

# To run StepWise procedure 
runSW = TRUE

######################################################################
#   This file run the case 1, this case is describe inside Case1_PD.R
######################################################################
Suicides4 <- na.omit(Suicides4)
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/Case1_PD_forSuicides.R")

# Case1_output

######################################################################
#   This file run the case 2, this case is describe inside Case2_PD.R
######################################################################
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/Case2_PD.R")

# Result
# Case2_output$GeneralModel$Model1_OutRE$ModelFit

######################################################################
#   This file run the case 3, this case is describe inside Case3_PD.R
#   To run this case is necesary to run first the code UnitChange.R
#    which calculat the unit significant changes
######################################################################

Start_year=2012
Last_year=2018

# This is the position of predictor in the Total data
#   usually called Suicides4
ListCovars <- 5:14
#kindComp <- "MedianC"
kindComp <- "AverageC"
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/UnitChange.R")
# Result
# outputUnitChang
#chang_E <- outputUnitChang$model_ChangU$ModelFit
#chang_E <- chang_E[-c(1:7),c("GEO_ID","SigPval")]

chang_E<- model_ChangU1$ModelFit # interact D
chang_MHP2 <- model_ChangU1$ModelFit # interact y5_17
chang_MHP3 <- model_ChangU1$ModelFit # interact IncomePC
chang_MHP4 <- model_ChangU1$ModelFit # interact Time

file.out.ex <- "/Users/wilmermartinez/Documents/RA_Jev/R_program/Index/"
name_sale <- "_E"

setEPS()
name.out = paste(file.out.ex, "Covar_Effect",name_sale,".eps", sep = "") 
postscript(name.out)

op <- par(mfrow=c(2,2),mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
plot(chang_MHP$SigPval[-c(1:7)], chang_MHP$Estimate[-c(1:7)], xlab="Significant",
     ylab="", main="Hurricane Effect")
abline(h=0,col="red")
plot(chang_MHP2$SigPval[-c(1:7)], chang_MHP2$Estimate[-c(1:7)], xlab="Significant",
     ylab="", main="y5_17 Effect")
abline(h=0,col="red")
plot(chang_MHP3$SigPval[-c(1:7)], chang_MHP3$Estimate[-c(1:7)], xlab="Significant",
     ylab="", main="Income Effect")
abline(h=0,col="red")
plot(chang_MHP4$SigPval[-c(1:7)], chang_MHP4$Estimate[-c(1:7)], xlab="Significant",
     ylab="", main="Time Effect")
abline(h=0,col="red")
par(op)
dev.off()

## Gathering all
chang_MHP$Covar <- 1:nrow(chang_MHP)
chang_MHP_all <- merge(chang_MHP[,c("Estimate","SigPval","GEO_ID","Covar")],
                       chang_MHP2[,c("Estimate","SigPval","GEO_ID")],by.x = "GEO_ID",by.y = "GEO_ID", all =T)

chang_MHP_all <- merge(chang_MHP_all,
                       chang_MHP3[,c("Estimate","SigPval","GEO_ID")],by.x = "GEO_ID",by.y = "GEO_ID", all =T)

chang_MHP_all <- merge(chang_MHP_all,
                       chang_MHP4[,c("Estimate","SigPval","GEO_ID")],by.x = "GEO_ID",by.y = "GEO_ID", all =T)

colnames(chang_MHP_all) <- c("GEO_ID", "Est_D","Sig_D","Covar","Est_yG","Sig_yG","Est_Income","Sig_Income","Est_Time","Sig_Time")
chang_MHP_all <- chang_MHP_all[order(chang_MHP_all$Covar),]

write.table(chang_MHP_all, paste(file.out.ex, "Effects",name_sale,".txt", sep = ""),sep = "\t")

chang_MHP_all2 <- chang_MHP_all[-c(1:7),]  

############
setEPS()
name.out = paste(file.out.ex, "Index",name_sale,".eps", sep = "") 
postscript(name.out)

op <- par(mfrow=c(2,2),mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
plot(chang_MHP_all2$Est_D, chang_MHP_all2$Est_yG, 
     col=chang_MHP_all2$Sig_D*chang_MHP_all2$Sig_yG+1, xlab=expression(delta),ylab="yG")
plot(chang_MHP_all2$Est_D, chang_MHP_all2$Est_Income,
     col=chang_MHP_all2$Sig_D*chang_MHP_all2$Sig_Income+1, xlab=expression(delta),ylab="Income")
plot(chang_MHP_all2$Est_D, chang_MHP_all2$Est_Time,
     col=chang_MHP_all2$Sig_D*chang_MHP_all2$Sig_Time+1, xlab=expression(delta),ylab="Time")
par(op)
dev.off()
### Standardizing the estimations
estard <- function(x){ 
  z=(x-mean(x,na.rm = T))/sd(x,na.rm = T)
  return(z)
}

chang_MHP_all2$Est_D <- estard(chang_MHP_all2$Est_D)
chang_MHP_all2$Est_yG <- estard(chang_MHP_all2$Est_yG)
chang_MHP_all2$Est_Income <- estard(chang_MHP_all2$Est_Income)
chang_MHP_all2$Est_Time <- estard(chang_MHP_all2$Est_Time)
chang_MHP_all2$Index_MHP <- chang_MHP_all2$Est_D +chang_MHP_all2$Est_yG +chang_MHP_all2$Est_Income+chang_MHP_all2$Est_Time

############
setEPS()
name.out = paste(file.out.ex, "IndexStand",name_sale,".eps", sep = "") 
postscript(name.out)

op <- par(mfrow=c(2,2),mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
plot(chang_MHP_all2$Est_D, chang_MHP_all2$Est_yG, 
     col=chang_MHP_all2$Sig_D*chang_MHP_all2$Sig_yG+1, xlab=expression(delta),ylab="yG")
plot(chang_MHP_all2$Est_D, chang_MHP_all2$Est_Income,
     col=chang_MHP_all2$Sig_D*chang_MHP_all2$Sig_Income+1, xlab=expression(delta),ylab="Income")
plot(chang_MHP_all2$Est_D, chang_MHP_all2$Est_Time,
     col=chang_MHP_all2$Sig_D*chang_MHP_all2$Sig_Time+1, xlab=expression(delta),ylab="Time")
plot(chang_MHP_all2$Est_D, chang_MHP_all2$Index_MHP,
     col=chang_MHP_all2$Sig_D+1, xlab=expression(delta),ylab="Index_MHP")
par(op)
dev.off()
# index plots

############
setEPS()
name.out = paste(file.out.ex, "IndexDistrib",name_sale,".eps", sep = "") 
postscript(name.out)

op <- par(mfrow=c(1,2),mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
hist(chang_MHP_all2$Index_MHP, xlab = "Index MHP", ylab = "", main="")
boxplot(chang_MHP_all2$Index_MHP ~ chang_MHP_all2$Sig_D, xlab = "Sinificant", ylab = "", main="Index MHP")
par(op)
dev.off()

chang_E_all <- chang_MHP_all2

###############################################

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
npredictor <- 7

# The file Suicides should be in format wide. Suicides.1 is define to consider
#  the Ndeath transform variable
# Suicides.1 <- Suicides3[,c("GEO_ID","Municipio","year", "Ndeath.pr")]
# Suicides <- reshape(Suicides.1, idvar = c("GEO_ID","Municipio"), timevar = "year", direction = "wide")

# Initials according to the outcome in data Suicides which is run in ReadFilesMHprices.R
InitialN.0<- "^Emp"
# Inicials for the names of the outcome
InitialN <- paste("^",outcome1,sep="")

# To save this plot define
save1 = 1 # save, or = 0 just to print 
Name_output <- "EmploySig_ModNewB"
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
titles <- c("Tendency ", "Significant changes \n decrease ", "Significant changes \n increase ")
op <- par(mfrow=c(2,3),las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
Suicides.plot <- Suicides[,-c(2:3)]
for(w in 0:2){
  # w=0
  ## This remove the years 2010 and 2011
  if(w==0) coefC <- subset(coefB[-c(1:npredictor),], SigPval == 0)
  if(w==1) coefC <- subset(coefB[-c(1:npredictor),], SigPval == 1 & Estimate < 0)
  if(w==2) coefC <- subset(coefB[-c(1:npredictor),], SigPval == 1 & Estimate > 0)
  #coefC <- subset(coefB[-c(1:npredictor),])
  proport <- round(nrow(coefC)/nrow(Suicides.plot)*100,2)
  colnames(Suicides.plot)[1] <- "GEO_ID"
  if(logT == TRUE) for(k in 3:ncol(Suicides.plot)) Suicides.plot[,k] <- log(Suicides.plot[,k])
  estimator1a <- merge(coefC[,c("GEO_ID","SigPval")], Suicides.plot, by.x = "GEO_ID", by.y = "GEO_ID")
  datos1a <- estimator1a[,grep(InitialN.0, names(estimator1a))]
  name_sale1 <- Name_output
  box_summary(datos1a, paste(titles[w+1], proport," (%)",sep=""),
              xfin=7, yini=Start_year,yfin=Last_year,xint=5.7,
              save1=0,name_sale1,"eps",file.out.ex, LI=20,LS=60)
}
for(w in 0:2){
  #coefC <- subset(coefB[-c(1:npredictor),], SigPval == w)
  if(w==0) coefC <- subset(coefB[-c(1:npredictor),], SigPval == 0)
  if(w==1) coefC <- subset(coefB[-c(1:npredictor),], SigPval == 1 & Estimate < 0)
  if(w==2) coefC <- subset(coefB[-c(1:npredictor),], SigPval == 1 & Estimate > 0)
  datosA <- datFore
  datosA_w <- reshape(datosA, idvar = "GEO_ID", timevar = "year", direction = "wide")
  datosA_w <- merge(datosA_w, coefC[,c("GEO_ID","SigPval")], by.x = "GEO_ID", by.y = "GEO_ID")
  datos1a <- datosA_w[,grep(InitialN, names(datosA_w))]
  box_summary3(datos1a, title="", xfin=6, yini=Start_year,yfin=Last_year,xint=4.7,
               save1=0,n.models=(ncol(datos1a)-2)/2,AoF=2017, name_sale1,format, file.out.ex, LI=20,LS=60)
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
coefC <- subset(coefB[-c(1:npredictor),], SigPval == 0)
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
model3 <- Case2_output$GeneralModel$Model1_OutFE$ModelObj
model4 <- Case3_output$GeneralModel$Model1_OutFE$ModelObj

rob_se <- list(
  sqrt(diag(vcovHC(model1, type = "HC1"))),
  sqrt(diag(vcovHC(model2, type = "HC1"))),
  sqrt(diag(vcovHC(model3, type = "HC1"))),
  sqrt(diag(vcovHC(model4, type = "HC1"))))

# generate the table
stargazer(model1,model2, model3, model4,
          #stargazer(suic_mod5.2, suic_mod5.3, suic_mod5.4,suic_mod5.5,
          digits = 3,
          header = FALSE,
          type = "latex", 
          se = rob_se,
          title = "Linear Panel Regression Models of Employment Rate",
          model.numbers = FALSE,
          column.labels = c("(0)", "(1)", "(2)","(3)"))


####################################################################################
###                           Optional
####################################################################################

model1 <- Case1_output$ForesModel$ForestOutRE$ModelObj
model2 <- Case2_output$ForesModel$ForestOutRE$ModelObj
model3 <- Case3_output$ForesModel$ForestOutRE$ModelObj

rob_se <- list(
  sqrt(diag(vcovHC(model1, type = "HC1"))),
  sqrt(diag(vcovHC(model2, type = "HC1"))),
  sqrt(diag(vcovHC(model3, type = "HC1"))))

# generate the table
stargazer(model1,model2, model3,
          #stargazer(suic_mod5.2, suic_mod5.3, suic_mod5.4,suic_mod5.5,
          digits = 3,
          header = FALSE,
          type = "latex", 
          se = rob_se,
          title = "Linear Panel Regression Models of Employment Rate",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)","(3)"))

####################################################################################
###                  This measure the Treatment Effect, based on 
###                  paired t-test
####################################################################################

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

w=1
if(w==0) coefC <- subset(coefB[-c(1:npredictor),], SigPval == 0)
if(w==1) coefC <- subset(coefB[-c(1:npredictor),], SigPval == 1 & Estimate < 0)
if(w==2) coefC <- subset(coefB[-c(1:npredictor),], SigPval == 1 & Estimate > 0)

datFore0 <- merge(datFore, coefC[,c("GEO_ID","SigPval")], by.x = "GEO_ID", by.y = "GEO_ID")

datFore2 <- datFore0[,-ncol(datFore0)]

datFore2 <- datFore
datFore2a <- datFore2 %>% filter(year==2017)
datFore2b <- datFore2 %>% filter(year==2018)

Twoy <- TreatE(datFore2, "Dif_Employ")
Y2017 <- TreatE(datFore2a, "Dif_Employ")
Y2018 <- TreatE(datFore2b, "Dif_Employ")

Full <- (Twoy)
Y2017.TE <- (2*Twoy - Y2018)
Y2018.TE <- (2*Twoy - Y2017)

Result.TE <- cbind(Full[,-2], rep(NA,4), Y2017.TE[,-2],rep(NA,4),Y2018.TE[,-2])
xtable(Result.TE, digits = 3)

ToMap <- coefB[-c(1:npredictor),c("SigPval", "GEO_ID")]
write.csv(ToMap, file = paste(file.out.ex, "EmploymentRateCases",".csv",sep=""), row.names = FALSE)

