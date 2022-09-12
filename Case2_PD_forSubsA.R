

source("/Users/wilmermartinez/Documents/RA_Jev/R_program/FunctionsPD.R")


######################################################################
###                            Case 2

##  This case use the selection variables (predictors) from the 
##   data before Hurricane
######################################################################


######################################################################
#  This fit stepwise using the data before Hurricane
######################################################################

## This define the data before and after Hurricane M.
Suicides4_Be <- Suicides4 %>%
  filter(year < 2017)
Suicides4_Af <- Suicides4 %>%
  filter(year >= 2017)

datosT <- Suicides4_Be
modFull <- as.formula(paste(outcome, Full_list_variables_wD, sep=""))

modelo1 <- lme(modFull, random = ~1|GEO_ID, data = datosT, method = "ML")
step.model <- stepAIC(modelo1, direction = "both", 
                      trace = FALSE)
Stepbest <- summary(step.model)
varsFE <- names(Stepbest$coefficients$fixed)

ModSW_F <- list(BestSWM = Stepbest, varsFE = varsFE)
modC1 = as.formula(c(outcome ,paste(ModSW_F$varsFE[-c(1)],collapse="+")))

######################################################################
#  Fitting a model with data before hurricane with the covariate
#    selection from the full data 
######################################################################

datosA <- Suicides4_Be

#model_1a <- Fit_mod(datosA, modC1, effect = "random")
model_1b <- Fit_mod(datosA, modC1, effect = "within")

#HausmanT.1 <- phtest(model_1a$ModelObj, model_1b$ModelObj)$p.value
QualityM1 <- c(
               model_1b$statsQM)

output1 <- list(Model1_OutFE = model_1b, 
                Model1_QM = QualityM1)

#####################################################
## forecasting
#  Forecast based on the previous fitted model, and
#   create the data after Hurricane M. with both treatment data (observed data)
#   and the counterfactual (forecast)
#####################################################
ModelRE <- output1$Model1_OutFE
ModelFE <- output1$Model1_OutFE
#Haust <- output1$Model1_Htest
Haust <- 0.01
fore0 <- fore_pdata(ModelRE, ModelFE, Haust, Suicides4_Af, CountFE_name)


Case2_output <- list(GeneralModel = output1, 
                     DataForest = fore0)


#Case2_output$ForesModel$ForesHtest
#Case2_output$ForesModel$ForesQM
#Case2_output$ForesModel$ForestOutRE
