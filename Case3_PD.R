source("/Users/wilmermartinez/Documents/RA_Jev/R_program/FunctionsPD.R")


######################################################################
###                            Case 3

##  This case use the selection variables (predictors) from the full
##   data by adding in the data before Hurricane an indicator variable
##     = 0 if the unit does not change signif, according to the step unitchange
##     = 1 o.w.  
######################################################################


######################################################################
#  Fitting a model with data before hurricane with the predictors
#    selection from the full data  + indicator variable
######################################################################

coefB <- outputUnitChang$model_ChangU$ModelFit
datosA <- Suicides4
datosA <- merge(datosA, coefB[,c("GEO_ID","SigPval")], by.x = "GEO_ID", by.y = "GEO_ID",all.x = T)

## This define the data before with only the units that not change
#        and after Hurricane M.
Suicides4_Be <- datosA %>%
  filter(year < 2017 & SigPval == 0)
  #filter(year < 2017)
Suicides4_Af <- datosA %>%
  filter(year >= 2017)

if(nrow(Suicides4_Be) > length(ListCovars)+1){
#selectVarsSW <- Case1_output$ResultsStepWise$SelectModel_StepWise$varsFE
#somet <- paste("D*GEO_ID+",paste(selectVarsSW[-c(1:2)],collapse="+"),sep="")
#modC1changeModel = as.formula(c(outcome, somet))  
if(runSW == TRUE){
  selectVarsSW <- Case1_output$ResultsStepWise$SelectModel_StepWise$varsFE
  somet <- paste("SigPval+",paste(selectVarsSW[-c(1:2)],collapse="+"),sep="")
  modC1changeModel = as.formula(c(outcome, somet))
}else{
  selectVarsSW <- Full_list_variables_wD
  somet <- paste("SigPval+",paste(selectVarsSW,collapse="+"),sep="")
  modC1changeModel = as.formula(c(outcome, somet))
}

model_1a <- Fit_mod(Suicides4_Be, modC1changeModel, effect = "random")
model_1b <- Fit_mod(Suicides4_Be, modC1changeModel, effect = "within")
#grun.varr <- pvcm(modC1changeModel, data=Suicides4_Be, model="random")

HausmanT.1 <- phtest(model_1a$ModelObj, model_1b$ModelObj)$p.value
QualityM1 <- c(model_1a$statsQM, model_1b$statsQM)

# QualityM1 <- c(model_1b$statsQM)

output1 <- list(Model1_OutRE = model_1a, Model1_OutFE = model_1b, 
                Model1_Htest = HausmanT.1, Model1_QM = QualityM1)
#output1 <- list(Model1_OutFE = model_1b, Model1_QM = QualityM1)

Suicides4_Be <- datosA %>%
  filter(year < 2017)
Suicides4_Af <- datosA %>%
  filter(year >= 2017)
#####################################################
## forecasting
#  Forecast based on the previous fitted model, and
#   create the data after Hurricane M. with both treatment data (observed data)
#   and the counterfactual (forecast)
#####################################################
ModelRE <- output1$Model1_OutRE
ModelFE <- output1$Model1_OutFE
Haust <- output1$Model1_Htest
#Haust <- 0.5
# this include the calculus of b0 as normal random
# fore0 <- fore_pdata(ModelRE, ModelFE, Haust, Suicides4_Af, CountFE_name)

fore0 <- fore_pdata2(ModelRE, ModelFE, Haust, datosA, Suicides4_Af, CountFE_name)

#####################################################
## Fitting a model to test treatment effect
#####################################################
datosA <- fore0$data_evalTE1
# no worries aobut this warning!!
modC1f <- as.formula(c(outcome, paste(CountFE_name,collapse="+")))
model_Fore1a <- Fit_mod(datosA, modC1f, effect = "random")
model_Fore1b <- Fit_mod(datosA, modC1f, effect = "within")

HausmanT.1f <- phtest(model_Fore1a$ModelObj, model_Fore1b$ModelObj)$p.value
QualityM1.f <- c(model_Fore1a$statsQM,
                 model_Fore1b$statsQM)

output_f <- list(ForestOutRE = model_Fore1a, ForestOutFE = model_Fore1b, 
                 ForesHtest = HausmanT.1f, ForesQM = QualityM1.f)

Case3_output <- list(GeneralModel = output1, 
                     DataForest = fore0, ForesModel = output_f)
}else{
  cat("All the units show a significant change therefore case3 = case1")
  Case3_output <- Case1_output
}