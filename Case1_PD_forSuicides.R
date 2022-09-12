
######################################################################
#   This file has the base functions for this procedure
######################################################################
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/FunctionsPD.R")

######################################################################
###                            Case 1

##  This case use the selection variables (predictors) from the full
##   data
######################################################################



######################################################################
#  This is case 0 with full data fit using only D
######################################################################

datosA <- Suicides4
mod0 = as.formula(c(outcome, paste("D",collapse="+")))
model_0a <- Fit_mod(datosA, mod0, effect = "random")
model_0b <- Fit_mod(datosA, mod0, effect = "within")

HausmanT <- phtest(model_0a$ModelObj, model_0b$ModelObj)$p.value
# the choice between fixed and random effects specifications 
# is based on Hausman-type tests, comparing the two estimators 
# under the null of no significant difference: if this is not 
# rejected, the more efficient random effects estimator is chosen 
# if(HausmanT > 0.05)
QualityM <- c(model_0a$statsQM,
              model_0b$statsQM)

output0 <- list(Model0_OutRE = model_0a, Model0_OutFE = model_0b, 
                Model0_Htest = HausmanT, Model0_QM = QualityM)
######################################################################
#  This fit stepwise using the full data
######################################################################

datosT <- Suicides4

modFull <- as.formula(paste(outcome, Full_list_variables, sep=""))
# Case1
# ModSW_F <- StepPD(datosA=datosT, modFull, RE = 0)
# modC1 = as.formula(c(outcome ,paste(ModSW_F$varsFE[-c(1:2)],collapse="+")))

modelo1 <- lme(modFull, random = ~1|GEO_ID, data = datosT, method = "ML")
step.model <- stepAIC(modelo1, direction = "both", 
                      trace = FALSE)
Stepbest <- summary(step.model)
varsFE <- names(Stepbest$coefficients$fixed)

ModSW_F <- list(BestSWM = Stepbest, varsFE = varsFE)
modC1 = as.formula(c(outcome ,paste(ModSW_F$varsFE[-c(1)],collapse="+")))

outputSW <- list(SelectModel_StepWise = ModSW_F, DefineModel = modC1)
######################################################################
#  Fitting a model with data before hurricane with the predictors
#    selection from the full data 
######################################################################

## This define the data before and after Hurricane M.
Suicides4_Be <- Suicides4 %>%
  filter(year < 2017)
Suicides4_Af <- Suicides4 %>%
  filter(year >= 2017)

datosA <- Suicides4_Be

model_1a <- Fit_mod(datosA, modC1, effect = "random")
model_1b <- Fit_mod(datosA, modC1, effect = "within")

HausmanT.1 <- phtest(model_1a$ModelObj, model_1b$ModelObj)$p.value
QualityM1 <- c(model_1a$statsQM,
               model_1b$statsQM)

output1 <- list(Model1_OutRE = model_1a, Model1_OutFE = model_1b, 
                Model1_Htest = HausmanT.1, Model1_QM = QualityM1)

#####################################################
## forecasting
#  Forecast based on the previous fitted model, and
#   create the data after Hurricane M. with both treatment data (observed data)
#   and the counterfactual (forecast)
#####################################################
ModelRE <- output1$Model1_OutRE
ModelFE <- output1$Model1_OutFE
Haust <- output1$Model1_Htest
fore0 <- fore_pdata(ModelRE, ModelFE, Haust, Suicides4_Af, CountFE_name)

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

Case1_output <- list(Model0 = output0, ResultsStepWise = outputSW, GeneralModel = output1, 
                     DataForest = fore0, ForesModel = output_f)

#Case1_output$ForesModel$ForesHtest
#Case1_output$ForesModel$ForesQM
#Case1_output$ForesModel$ForestOutRE

