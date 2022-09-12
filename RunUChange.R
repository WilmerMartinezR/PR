
######################################################################
#   This file run the data for the application Median House prices
#             (MHprices)
######################################################################
# source("/Users/wilmermartinez/Documents/RA_Jev/R_program/ReadFilesMHprices.R")
#install.packages("plm")

######################################
# Global parameters
######################################
outcome <- "yt ~ "
outcome1 <- "yt"

Full_list_variables <- "D + x1t + x2t + x3t"

Full_list_variables_wD <- "x1t + x2t + x3t"

# counter effet variable name
CountFE_name = "ytH0"
##  Define Full data set
# Suicides4 <- 

# To run StepWise procedure 
runSW = TRUE

######################################################################
#   This file run the case 1, this case is describe inside Case1_PD.R
######################################################################
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/Case1_PD.R")

AA1 <- Case1_output$DataForest$data_evalTE1
AA1 <- merge(AA1, changes, by.x="GEO_ID", by.y="sigCha",all.x=T)
mean(AA1$yt - AA1$ytH0)
mean(delta)
######################################################################
#   This file run the case 2, this case is describe inside Case2_PD.R
######################################################################
# source("/Users/wilmermartinez/Documents/RA_Jev/R_program/Case2_PD.R")

# Result
# Case2_output$GeneralModel$Model1_OutFE$ModelFit

######################################################################
#   This file run the case 3, this case is describe inside Case3_PD.R
#   To run this case is necesary to run first the code UnitChange.R
#    which calculat the unit significant changes
######################################################################

Start_year=2011
Last_year=2018

# This is the position of predictor in the Total data
#   usually called Suicides4
ListCovars <- 5:7
kindComp <- "AverageC"
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/UnitChange.R")
# Result
outputUnitChang$model_ChangU$ModelFit[,c(1:4,7)]
table(outputUnitChang$model_ChangU$ModelFit$SigPval)


b00 <- data.frame(as.matrix(fixef(ModelFE$ModelObj)))

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

coefB <- outputUnitChang$model_ChangU$ModelFit
# Define the number of checking the model running this
outputUnitChang$model_ChangU$ModelObj$formula
npredictor <- 6
# Initials according to the outcome in data Suicides which is run in ReadFilesMHprices.R
InitialN.0<- "^MH"
# Inicials for the names of the outcome
InitialN <- paste("^",outcome1,sep="")

# To save this plot define
save1 = 1 # save, or = 0 just to print 
Name_output <- "MedianHouseSig_ModNewC"
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
op <- par(mfrow=c(2,2),las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
for(w in 0:1){
  # w=1
  coefC <- subset(coefB[-c(1:npredictor),], SigPval == w)
  proport <- round(nrow(coefC)/nrow(Suicides)*100,2)
  estimator1a <- merge(coefC[,c("GEO_ID","SigPval")], Suicides, by.x = "GEO_ID", by.y = "GEO_ID")
  datos1a <- estimator1a[,grep(InitialN.0, names(estimator1a))]
  name_sale1 <- Name_output
  box_summary(datos1a[,-c(1:2)], paste(titles[w+1], proport," (%)",sep=""),
              xfin=7, yini=Start_year,yfin=Last_year,xint=5.7,
              save1=0,name_sale1,"eps",file.out.ex)
}
for(w in 0:1){
  coefC <- subset(coefB[-c(1:npredictor),], SigPval == w)
  datosA <- datFore
  datosA_w <- reshape(datosA, idvar = "GEO_ID", timevar = "year", direction = "wide")
  datosA_w <- merge(datosA_w, coefC[,c("GEO_ID","SigPval")], by.x = "GEO_ID", by.y = "GEO_ID")
  datos1a <- datosA_w[,grep(InitialN, names(datosA_w))]
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
coefC <- subset(coefB[-c(1:npredictor),], SigPval == 0)
cuales1 <- which(coefC$GEO_ID %in% munis)

# To save this plot define
save1 = 1 # save, or = 0 just to print 
Name_output2 <- "MedianHouseUnits_ModNewB"
file.out.ex = "/Users/wilmermartinez/Documents/RA_Jev/R_program/Simu_result_ts_interv/"
format = "eps"
#format = "png"

plot_cfe(datosT, datFore, Nrow=5, Ncol=5, cuales=cuales1[1:25], yini=Start_year,yfin=Last_year, 
         save1,Name_output2,format, file.out.ex)


###########
###################
##################################
####################################################################################
###                           Organizing Results
####################################################################################


library(stargazer)

model1 <- Case1_output$Model0$Model0_OutRE$ModelObj
model2 <- Case1_output$GeneralModel$Model1_OutRE$ModelObj
model3 <- Case2_output$GeneralModel$Model1_OutRE$ModelObj
model4 <- Case3_output$GeneralModel$Model1_OutRE$ModelObj

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
          title = "Linear Panel Regression Models of Median House prices",
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
          title = "Linear Panel Regression Models of Median House prices",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)","(3)"))

####################################################################################
###                  This measure the Treatment Effect, based on 
###                  paired t-test
####################################################################################

# paired t-test
TE1 <- NULL
for(M in 4:ncol(datFore)){
  y1 <- datFore[,outcome1]
  y2 <- datFore[,M]
  TE <- t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric
  TE1 <- rbind(TE1, c(Estimation = TE$estimate, Pvalue = TE$p.value, CI = TE$conf.int))
}
TE1 <- data.frame(TE1)
rownames(TE1) <- c(paste("M", 1:(ncol(datFore)-3), sep=""))

xtable(TE1, digits = 3)

ToMap <- coefB[-c(1:npredictor),c("SigPval", "GEO_ID")]
write.csv(ToMap, file = paste(file.out.ex, "HousePricesCases",".csv",sep=""), row.names = FALSE)


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


datFore2 <- datFore
datFore2a <- datFore %>% filter(year==2017)
datFore2b <- datFore %>% filter(year==2018)

Twoy <- TreatE(datFore2, "Dif_MedianHP")
Y2017 <- TreatE(datFore2a, "Dif_MedianHP")
Y2018 <- TreatE(datFore2b, "Dif_MedianHP")

Full <- (Twoy)
Y2017.TE <- (2*Twoy - Y2018)
Y2018.TE <- (2*Twoy - Y2017)

Result.TE <- cbind(Full[,-2], rep(NA,4), Y2017.TE[,-2],rep(NA,4),Y2018.TE[,-2])
xtable(Result.TE, digits = 3)
