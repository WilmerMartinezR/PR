
######################################################################
#   This file run the data for the application Median House prices
#             (MHprices)
######################################################################
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/ReadFilesDeaths.R")

######################################
# Global parameters
######################################
#outcome <- "log(Ndeath.pr) ~ "
#logT = TRUE
outcome <- "Ndeath.pr ~ "
logT = FALSE
outcome1 <- "Ndeath.pr"

# Since some variables only have data from 2012 we remove 2010 - 2011
Full_list_variables <- "D + Time + y5_17 + y18_34+y35_64+y65_74+seitert + Male"

Full_list_variables_wD <- "Time + y5_17 + y18_34+y35_64+y65_74+seitert + Male"

# counter effet variable name
CountFE_name = "DeathsH0"
##  Define Full data set
# Suicides4 <- 

# To run StepWise procedure 
runSW = FALSE

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
ListCovars <- 9:19
kindComp <- "MedianC"
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
npredictor <- 4

# The file Suicides should be in format wide. Suicides.1 is define to consider
#  the Ndeath transform variable
Suicides.1 <- Suicides3[,c("GEO_ID","Municipio","year", "Ndeath.pr")]
Suicides <- reshape(Suicides.1, idvar = c("GEO_ID","Municipio"), timevar = "year", direction = "wide")
## This remove the years 2010 and 2011
Suicides <- Suicides[,-c(3,4)]
# Initials according to the outcome in data Suicides which is run in ReadFilesMHprices.R
InitialN.0<- "^Ndeath.pr."
# Inicials for the names of the outcome
InitialN <- paste("^",outcome1,sep="")

# To save this plot define
save1 = 1 # save, or = 0 just to print 
Name_output <- "NDeathSig_ModNew_2"
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
  # w=0
  Suicides <- reshape(Suicides.1, idvar = c("GEO_ID","Municipio"), timevar = "year", direction = "wide")
  ## This remove the years 2010 and 2011
  Suicides <- Suicides[,-c(3,4)]
  coefC <- subset(coefB[-c(1:npredictor),], SigPval == w)
  proport <- round(nrow(coefC)/nrow(Suicides)*100,2)
  colnames(Suicides)[1] <- "GEO_ID"
  if(logT == TRUE) for(k in 3:ncol(Suicides)) Suicides[,k] <- log(Suicides[,k])
  estimator1a <- merge(coefC[,c("GEO_ID","SigPval")], Suicides, by.x = "GEO_ID", by.y = "GEO_ID")
  datos1a <- estimator1a[,grep(InitialN.0, names(estimator1a))]
  name_sale1 <- Name_output
  box_summary(datos1a, paste(titles[w+1], proport," (%)",sep=""),
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
Name_output2 <- "DeathUnits_ModNew_2"
file.out.ex = "/Users/wilmermartinez/Documents/RA_Jev/R_program/Simu_result_ts_interv/"
format = "eps"
#format = "png"
if(logT == TRUE) datosT$Ndeath.pr <- log(datosT$Ndeath.pr)
plot_cfe(datosT, datFore, Nrow=5, Ncol=5, cuales=cuales1[1:24], yini=Start_year,yfin=Last_year, 
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
          title = "Linear Panel Regression Models of Death Rate",
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
          title = "Linear Panel Regression Models of Death Rate",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)","(3)"))

####################################################################################
###                  This measure the Treatment Effect, based on 
###                  paired t-test
####################################################################################

# paired t-test
#for(k in 3:ncol(datFore)) datFore[, k] <- exp(datFore[, k])
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
write.csv(ToMap, file = paste(file.out.ex, "DeathCases",".csv"), row.names = FALSE)


head(Pop)
impact <- 23.39708
LimSup <- 5.205446
LimInf <- 1.1948541
colSums(Pop[,c("POP.2017", "POP.2018")])/10000 * impact
colSums(Pop[,c("POP.2017", "POP.2018")])/10000 * LimSup
colSums(Pop[,c("POP.2017", "POP.2018")])/10000 * LimInf

aa1 <- Suicides4 %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sumD = sum(Ndeath))

bb1 <- Suicides3 %>%
  filter(year >= 2017) %>%
  dplyr::select(Merge, POP)

datFore$Merge <- paste(datFore$GEO_ID, datFore$year, sep="")
datFore2 <- merge(datFore, bb1, by.x = "Merge", by.y = "Merge")

aa3 <- datFore2 %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(ObsDeath = sum(exp(Ndeath.pr)*POP/10000),
                   PredM1 = sum(exp(Ndeath.prM1)*POP/10000),
                   PredM2 = sum(exp(Ndeath.prM2)*POP/10000),
                   PredM3 = sum(exp(Ndeath.prM3)*POP/10000),
                   PredAv = sum(exp(Ndeath.prAv)*POP/10000),
                   DifM1 = ObsDeath - PredM1,
                   DifM2 = ObsDeath - PredM2,
                   DifM3 = ObsDeath - PredM3)

datFore2a <- datFore2 %>%
  #dplyr::group_by(year) %>%
  dplyr::mutate(ObsDeath = exp(Ndeath.pr)*POP/10000,
                   PredM1 = exp(Ndeath.prM1)*POP/10000,
                   PredM2 = exp(Ndeath.prM2)*POP/10000,
                   PredM3 = exp(Ndeath.prM3)*POP/10000,
                   PredAv = exp(Ndeath.prAv)*POP/10000)

datFore3 <- datFore2a %>%
  #filter(year == 2017) %>%
  mutate(difM1 = ObsDeath - PredM1)
mean(datFore3$difM1)  

datFore2 <- datFore2a %>% filter(year==2017)

TE2 <- NULL
for(M in 5:(ncol(datFore2)-1)){
  #M=5
  y1 <- exp(datFore2[,outcome1])*datFore2[,"POP"]/10000
  y2 <- exp(datFore2[,M])*datFore2[,"POP"]/10000
  TE <- t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric
  TE2 <- rbind(TE2, c(Estimation = TE$estimate, Pvalue = TE$p.value, CI = TE$conf.int))
}
TE2 <- data.frame(TE2)
rownames(TE2) <- c(paste("M", 1:(ncol(datFore)-4), sep=""))


impact <- 0.26734500
colSums(Pop[,c("POP.2017", "POP.2018")])/10000 * exp(impact)
LimSup <- 1.837 
LimInf <- 2.315
colSums(Pop[,c("POP.2017", "POP.2018")])/10000 * exp(LimSup)
colSums(Pop[,c("POP.2017", "POP.2018")])/10000 * exp(LimInf)        