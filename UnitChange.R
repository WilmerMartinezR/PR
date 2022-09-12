
source("/Users/wilmermartinez/Documents/RA_Jev/R_program/FunctionsPD.R")

########################################################################
### Now we identify the potential units that do not change significantly 

## The unit base is the average change before and after
# kindComp should be define
dataChangU <- units_changes(Suicides4, yini=Start_year, yfin=Last_year, kindComp = c(kindComp),
                            cualesCovars=ListCovars)

##  This is an alternative definition for the unit reference (base of comparison)
## The unit base is the average change through the time
#dataChangU_2 <- units_changes(Suicides4, yini=2012, yfin=2018, kindComp = c("AverageYear"),
#                              cualesCovars=5:13)

# Defining the set of variables to consider
if(runSW == TRUE){
  selectVarsSW <- Case1_output$ResultsStepWise$SelectModel_StepWise$varsFE
  somet <- paste("D*GEO_ID+",paste(selectVarsSW[-c(1:2)],collapse="+"),sep="")
  #somet <- paste("y5_17*GEO_ID+",paste(selectVarsSW[-c(1)],collapse="+"),sep="") #MHP
  #somet <- paste("y35_64*GEO_ID+",paste(selectVarsSW[-c(1)],collapse="+"),"+IncomePC",sep="") #EM
  #somet <- paste("IncomePC*GEO_ID+",paste(selectVarsSW[-c(1)],collapse="+"),sep="")
  #somet <- paste("Time*GEO_ID+",paste(selectVarsSW[-c(1)],collapse="+"),sep="") #MHP
  #somet <- paste("Time*GEO_ID+",paste(selectVarsSW[-c(1)],collapse="+"),"+IncomePC",sep="") #EM
  modC1changeModel = as.formula(c(outcome, somet))
}else{
  selectVarsSW <- Full_list_variables_wD
  somet <- paste("D*GEO_ID+",paste(selectVarsSW,collapse="+"),sep="")
  modC1changeModel = as.formula(c(outcome, somet))
}
# no worries about this warning!!
datosA <- dataChangU

#datosA <- Suicides4
# this is not estimable with ranfom effect
model_ChangU1 <- Fit_mod(datosA, modC1changeModel, effect = "within")

QualityM1.chang <- c(model_ChangU1$statsQM)

outputUnitChang <- list(model_ChangU = model_ChangU1, QualityM1.chang = QualityM1.chang)
