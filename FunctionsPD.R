### Important reference of plm
# https://cran.r-project.org/web/packages/plm/vignettes/plmPackage.html#fnref3

######################################################################
#  This function get a best model using Stepwise procedure
######################################################################
StepPD <- function(datosA, mod0, RE = 1){
  # random for treatment variable only
  # datosA = datosA
  if(RE == 1){
    modelo1 <- lme(mod0, random = ~D|GEO_ID, data = datosA, method = "ML")
    #summary(mod2)
  }else modelo1 <- lme(mod0, random = ~1|GEO_ID, data = datosA, method = "ML")
  
  step.model <- stepAIC(modelo1, direction = "both", 
                        trace = FALSE)
  Stepbest <- summary(step.model)
  varsFE <- names(Stepbest$coefficients$fixed)
  #varsRE <- Stepbest$coefficients$random
  
  return(list(BestSWM = Stepbest, varsFE = varsFE))
}

######################################################################
#  This function fit a panel data model using plm function
######################################################################
Fit_mod <- function(datosA, mod0, effect = "random"){
  
  suic_mod0 <- plm(mod0, 
                   data = datosA,
                   index = c("GEO_ID", "year"), 
                   model = effect)
  
  coefA0 <- summary(suic_mod0)
  
  aa<-unlist(coefA0)
  stats0 <- round(c(R2=aa$r.squared.rsq, 
                    R2_adj=aa$r.squared.adjrsq, 
                    Pval_mod = aa$fstatistic.p.value.Chisq),4) 
  
  coefA01 <- coeftest(suic_mod0, vcov. = vcovHC, type = "HC1")
  coefBF <- data.frame(coefA01[,1],coefA01[,2], coefA01[,3], coefA01[,4])
  colnames(coefBF) <- c("Estimate","std_error","tval","pval")
  intervBF <- coefci(suic_mod0, vcov. = vcovHC, type = "HC1",level = 0.95)
  coefBF <- cbind(coefBF,intervBF)
  
  coefBF$SigPval <- ifelse(coefBF$pval < 0.05,1,0)
  nlong <- nchar(rownames(coefBF))
  coefBF$GEO_ID <- substr(rownames(coefBF), nlong-4, nlong)
  
  list(ModelObj=suic_mod0, statsQM =stats0, ModelFit = coefBF)
}

######################################################################
#  This function forecast based on the model fitted before Hurricane
######################################################################
# thi function use a normal random variable to estimate b0
fore_pdata <- function(ModelRE, ModelFE, Haust, Suicides4_Af, CountFE_name){
  #ModelFE  = model_1b
  
  if(Haust < 0.05){
    # FE
    b0 <- data.frame(as.matrix(fixef(ModelFE$ModelObj)))
    b0$GEO_ID <- rownames(b0)
    colnames(b0)[1] <- "b0" 
    
    #summary(fixef(ModelFE$ModelObj))
    #summary(fixef(ModelFE$ModelObj, type = "dmean"))
    
    # Estimating counterfactuals y0it (t>=2017) 
    coefA0a <- data.frame(ModelFE$ModelFit)
    est1 <- coefA0a
    #est1 <- subset(est1, SigPval == 1)
    Suicides4_Af$MedianH0 <- 0
    for(i in 1:nrow(Suicides4_Af)) Suicides4_Af$MedianH0[i] <- as.matrix(Suicides4_Af[i,row.names(est1)])%*%est1[,1]
    Suicides4_Af <- merge(Suicides4_Af, b0, by.x = "GEO_ID", by.y = "GEO_ID", all.x = T) 
    if( any(is.na(Suicides4_Af$b0)) ) Suicides4_Af[which(is.na(Suicides4_Af$b0)),"b0"] <- c(rnorm(length(which(is.na(Suicides4_Af$b0))), mean(b0$b0), sd(b0$b0)))
    Suicides4_Af$MedianH0 <- Suicides4_Af$MedianH0 + Suicides4_Af$b0
  }else{
    # RE
    b0 <- c(b0 = ModelRE$ModelFit[1,1])
    
    # Estimating counterfactuals y0it (t>=2017) 
    coefA0a <- data.frame(ModelRE$ModelFit)
    est1 <- coefA0a[-1,]
    #est1 <- subset(est1, SigPval == 1)
    Suicides4_Af$MedianH0 <- 0
    for(i in 1:nrow(Suicides4_Af)) Suicides4_Af$MedianH0[i] <- as.matrix(Suicides4_Af[i,row.names(est1)])%*%est1[,1]
    Suicides4_Af$MedianH0 <- Suicides4_Af$MedianH0 + b0
  }
  
  EvalTEa <- Suicides4_Af[,c("GEO_ID","year","MedianH0")]
  colnames(EvalTEa)[3] <- outcome1
  EvalTEa$DD <- 0
  EvalTEb <- Suicides4_Af[,c("GEO_ID","year",outcome1)]
  EvalTEb$DD <- 1
  EvalTE <- rbind(EvalTEa, EvalTEb)
  
  ## To use plm we have to create this data becuase it does not allow
  ##   duplicates in subj-time
  dat1 <- pdata.frame(Suicides4_Af[,c("GEO_ID","year",outcome1, "MedianH0")], index = c("GEO_ID", "year"))
  colnames(dat1)[4] <- CountFE_name
  
  return(list(dataAf_CFE = Suicides4_Af, data_evalTE1 = dat1, data_evalTE2 = EvalTE))
}

# this function follow the panel parametric approach Hsiao and Zhou 2019
fore_pdata2 <- function(ModelRE, ModelFE, Haust, datosA, Suicides4_Af, CountFE_name){
  #ModelFE  = model_1b
  
  if(Haust < 0.05){
    # FE
    # step1
    b0 <- data.frame(as.matrix(fixef(ModelFE$ModelObj)))
    b0$GEO_ID <- rownames(b0)
    colnames(b0)[1] <- "b0" 
    
    #### New part according to parametric estimation Hisao 2019
    # step2
    Suicides4_Be <- datosA %>%
      filter(year < 2017)
    Newterm0 <- merge(Suicides4_Be, b0, by.x = "GEO_ID", by.y = "GEO_ID") 
    #Newterm0$Newterm <- Newterm0[,outcome1]*(Newterm0[,"b0"]/sum(Newterm0[,"b0"]))
    Newterm0$Newterm <- Newterm0[,outcome1]
    
    Newterm1 <- Newterm0 %>% 
      dplyr::group_by(year) %>%
      dplyr::select(year, Newterm) %>%
      dplyr::summarise(Newterm=sum(Newterm))
    
    ## Fitting the model with new term over treated units before
    Suicides4_Be2 <- datosA %>%
      filter(year < 2017 & SigPval == 1)
    Suicides4_Be2 <- merge(Suicides4_Be2, Newterm1, by.x = "year", by.y = "year", all.x = T)
    
    coefA0a <- data.frame(ModelFE$ModelFit)
    est1 <- coefA0a
    Suicides4_Be2$MedianH0 <- 0
    for(i in 1:nrow(Suicides4_Be2)) Suicides4_Be2$MedianH0[i] <- as.matrix(Suicides4_Be2[i,row.names(est1)])%*%est1[,1]
    Suicides4_Be2[,outcome1] <- Suicides4_Be2[,outcome1] - Suicides4_Be2[,CountFE_name]
    
    modC3changeModel = as.formula(c(outcome, paste("Newterm",collapse="+")))
    
    model_1c <- Fit_mod(Suicides4_Be2, modC3changeModel, effect = "within")
    gamma1 <- model_1c$ModelFit[1,1]
    
    Newtermb0 <- merge(Suicides4_Af, b0, by.x = "GEO_ID", by.y = "GEO_ID") 
    #Newtermb0$Newterm <- Newtermb0[,outcome1]*(Newtermb0[,"b0"]/sum(Newtermb0[,"b0"]))
    Newtermb0$Newterm <- Newtermb0[,outcome1]
    
    Newtermb01 <- Newtermb0 %>% 
      dplyr::group_by(year) %>%
      dplyr::select(year, Newterm) %>%
      dplyr::summarise(Newterm=sum(Newterm))
    
    #summary(fixef(ModelFE$ModelObj))
    #summary(fixef(ModelFE$ModelObj, type = "dmean"))
    
    # Estimating counterfactuals y0it (t>=2017) 
    coefA0a <- data.frame(ModelFE$ModelFit)
    est1 <- coefA0a
    #est1 <- subset(est1, SigPval == 1)
    Suicides4_Af$MedianH0 <- 0
    for(i in 1:nrow(Suicides4_Af)) Suicides4_Af$MedianH0[i] <- as.matrix(Suicides4_Af[i,row.names(est1)])%*%est1[,1]
    Suicides4_Af <- merge(Suicides4_Af, b0, by.x = "GEO_ID", by.y = "GEO_ID", all.x = T) 
    #if( any(is.na(Suicides4_Af$b0)) ) Suicides4_Af[which(is.na(Suicides4_Af$b0)),"b0"] <- c(rnorm(length(which(is.na(Suicides4_Af$b0))), mean(b0$b0), sd(b0$b0)))
    if( any(is.na(Suicides4_Af$b0)) ) Suicides4_Af[which(is.na(Suicides4_Af$b0)),"b0"] <- colMeans(Newtermb01[,2]*gamma1)
    Suicides4_Af$MedianH0 <- Suicides4_Af$MedianH0 + Suicides4_Af$b0
  }else{
    # RE
    b0 <- c(b0 = ModelRE$ModelFit[1,1])
    
    # Estimating counterfactuals y0it (t>=2017) 
    coefA0a <- data.frame(ModelRE$ModelFit)
    est1 <- coefA0a[-1,]
    #est1 <- subset(est1, SigPval == 1)
    Suicides4_Af$MedianH0 <- 0
    for(i in 1:nrow(Suicides4_Af)) Suicides4_Af$MedianH0[i] <- as.matrix(Suicides4_Af[i,row.names(est1)])%*%est1[,1]
    Suicides4_Af$MedianH0 <- Suicides4_Af$MedianH0 + b0
  }
  
  EvalTEa <- Suicides4_Af[,c("GEO_ID","year","MedianH0")]
  colnames(EvalTEa)[3] <- outcome1
  EvalTEa$DD <- 0
  EvalTEb <- Suicides4_Af[,c("GEO_ID","year",outcome1)]
  EvalTEb$DD <- 1
  EvalTE <- rbind(EvalTEa, EvalTEb)
  
  ## To use plm we have to create this data becuase it does not allow
  ##   duplicates in subj-time
  dat1 <- pdata.frame(Suicides4_Af[,c("GEO_ID","year",outcome1, "MedianH0")], index = c("GEO_ID", "year"))
  colnames(dat1)[4] <- CountFE_name
  
  return(list(dataAf_CFE = Suicides4_Af, data_evalTE1 = dat1, data_evalTE2 = EvalTE))
}

######################################################################
#  This function create a virtual unit to determine the potential
#    units that change or not
######################################################################

units_changes <- function(Suicides4, yini=2012, yfin=2018, kindComp = c("AverageC"),
                          cualesCovars=5:13){
  # could be
  # kindComp = c("AverageC","AverageYear","MedianC","MedianYear")
  # kindComp <- match.arg(kindComp)
  Suicides4AB <- Suicides4
  Suicides4AB1 <- Suicides4[1:(yfin - yini +1),]
  #Suicides4AB1$GEO_ID <- as.character(72000)
  Suicides4AB1$GEO_ID <- as.character(as.numeric(min(Suicides4$GEO_ID))-1)
  
  ########
  #This calcuate the average of MedianH for a new unit 72000 
  # before an after the treatment
  if(kindComp == "AverageC"){
    a12 <- which(as.numeric(as.character(Suicides4$year)) < 2017)
    Suicides4AB1[1:(2017-yini),outcome1] <- mean(Suicides4[a12,outcome1], na.rm = T)
    Suicides4AB1[(2017-yini+1):(yfin-yini+1),outcome1] <- mean(Suicides4[-a12,outcome1], na.rm = T)
    Suicides4AB1[,"Merge"] <- paste(Suicides4AB1$GEO_ID, Suicides4AB1$year,sep = "")
  }else if(kindComp == "AverageYear"){
    #This calcuate the average of MedianH for a new unit 72000 per year
    k=0
    for(j in as.numeric(as.character(Suicides4AB1$year))){
      a12 <- which(as.numeric(as.character(Suicides4$year)) == j)
      k=k+1
      Suicides4AB1[k,outcome1] <- mean(Suicides4[a12,outcome1], na.rm = T)
    }
    #Suicides4AB1$Merge <- paste(Suicides4AB1$GEO_ID, Suicides4AB1$year,sep = "")
  }else if(kindComp == "MedianC"){
    a12 <- which(as.numeric(as.character(Suicides4$year)) < 2017)
    Suicides4AB1[1:(2017-yini),outcome1] <- median(Suicides4[a12,outcome1], na.rm = T)
    Suicides4AB1[(2017-yini+1):(yfin-yini+1),outcome1] <- median(Suicides4[-a12,outcome1], na.rm = T)
    Suicides4AB1[,"Merge"] <- paste(Suicides4AB1$GEO_ID, Suicides4AB1$year,sep = "")
  }else if(kindComp == "MedianYear"){
    #This calcuate the average of MedianH for a new unit 72000 per year
    k=0
    for(j in as.numeric(as.character(Suicides4AB1$year))){
      a12 <- which(as.numeric(as.character(Suicides4$year)) == j)
      k=k+1
      Suicides4AB1[k,outcome1] <- median(Suicides4[a12,outcome1], na.rm = T)
    }
  }
  
  for(j in cualesCovars) Suicides4AB1[,j] <- mean(Suicides4[,j], na.rm = T)
  
  #for(j in 1:ncol(Suicides4AB1)) class(Suicides4AB1[,j]) <- class(Suicides4[,j])
  
  #cl <- cl2 <- c()
  #for(j in 1:ncol(Suicides4AB1)) cl <- c(cl,class(Suicides4AB1[,j]))
  #for(j in 1:ncol(Suicides4AB1)) cl2 <- c(cl2,class(Suicides4[,j]))
  
  Suicides4AB <- rbind(Suicides4AB1, Suicides4)
  return(Suicides4AB)
}
######################################################################
#  This function Plot we some municipalities with observed data,
#    and counter-factual in red
######################################################################

## 
plot_cfe <- function(datosT, datFore, Nrow=5, Ncol=5, cuales=1:25, yini=2012,yfin=2018, 
                     save1=0,name_sale1,format, file.out.ex){
  # datosT = Suicides4
  munis <- unique(datosT$GEO_ID)
  if(save1==1 && format=="png"){
    name.out = paste(file.out.ex, name_sale1,".png", sep = "") 
    png(name.out, res=110)
    
    #quartz()
    op <- par(mfrow=c(Nrow,Ncol),las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
    # cuales allows chose which municipalities include in the plot
    for(i in cuales){
      #i=1
      Suicides5a1 <- subset(datosT, GEO_ID == munis[i])
      Suicides5a2 <- subset(datFore, GEO_ID == munis[i])
      Min1 = min(Suicides5a1[,outcome1], min(Suicides5a2[,-c(1:2)]))
      Max1 = max(Suicides5a1[,outcome1], max(Suicides5a2[,-c(1:2)])) 
      plot(yini:yfin, c(Suicides5a1[,outcome1]) , type="o", 
           ylab="",main=munis[i], xlab = "Year",
           ylim=c(Min1, Max1))
      CountFE_name2 <- colnames(datFore)[4]
      lines(yini:yfin, c(rep(NA, 2017-yini-1), Suicides5a1[(2017-yini),outcome1],Suicides5a2[,CountFE_name2]), 
            col=c(rep("black",2017-yini),rep(rainbow(10)[1],yfin-2017+1)), type = "o",pch=20)
      # this condition is to add more models in the plots
      if(ncol(datFore) > 4){
        for(j in 5:ncol(datFore)){
          CountFE_name2 <- colnames(datFore)[j]
          lines(yini:yfin, c(rep(NA, 2017-yini-1), Suicides5a1[(2017-yini),outcome1],Suicides5a2[,CountFE_name2]), 
                col=c(rep("black",2017-yini),rep(rainbow(10)[j-3],yfin-2017+1)), type = "o",pch=20)
        }
      }
    }
    par(op)
    dev.off()
  }else if(save1==1 && format=="eps"){
    setEPS()
    name.out = paste(file.out.ex, name_sale1,".eps", sep = "") 
    postscript(name.out)
    #quartz()
    op <- par(mfrow=c(Nrow,Ncol), las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=1,cex.lab=1,cex.main=1)
    for(i in cuales){
      #i=1
      Suicides5a1 <- subset(datosT, GEO_ID == munis[i])
      Suicides5a2 <- subset(datFore, GEO_ID == munis[i])
      Min1 = min(Suicides5a1[,outcome1], min(Suicides5a2[,-c(1:2)]))
      Max1 = max(Suicides5a1[,outcome1], max(Suicides5a2[,-c(1:2)])) 
      plot(yini:yfin, c(Suicides5a1[,outcome1]) , type="o", 
           ylab="",main=munis[i], xlab = "Year",
           ylim=c(Min1, Max1))
      CountFE_name2 <- colnames(datFore)[4]
      lines(yini:yfin, c(rep(NA, 2017-yini-1), Suicides5a1[(2017-yini),outcome1],Suicides5a2[,CountFE_name2]), 
            col=c(rep("black",2017-yini),rep(rainbow(10)[1],yfin-2017+1)), type = "o",pch=20)
      # this condition is to add more models in the plots
      if(ncol(datFore) > 4){
        for(j in 5:ncol(datFore)){
          CountFE_name2 <- colnames(datFore)[j]
          lines(yini:yfin, c(rep(NA, 2017-yini-1), Suicides5a1[(2017-yini),outcome1],Suicides5a2[,CountFE_name2]), 
                col=c(rep("black",2017-yini),rep(rainbow(10)[j-3],yfin-2017+1)), type = "o",pch=20)
        }
      }
    }
    par(op)
    dev.off()
  }else{
    quartz()
    op <- par(mfrow=c(Nrow,Ncol),las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
    for(i in cuales){
      #i=1
      Suicides5a1 <- subset(datosT, GEO_ID == munis[i])
      Suicides5a2 <- subset(datFore, GEO_ID == munis[i])
      Min1 = min(Suicides5a1[,outcome1], min(Suicides5a2[,-c(1:2)]))
      Max1 = max(Suicides5a1[,outcome1], max(Suicides5a2[,-c(1:2)])) 
      plot(yini:yfin, c(Suicides5a1[,outcome1]) , type="o", 
           ylab="",main=munis[i], xlab = "Year",
           ylim=c(Min1, Max1))
      CountFE_name2 <- colnames(datFore)[4]
      lines(yini:yfin, c(rep(NA, 2017-yini-1), Suicides5a1[(2017-yini),outcome1],Suicides5a2[,CountFE_name2]), 
            col=c(rep("black",2017-yini),rep(rainbow(10)[1],yfin-2017+1)), type = "o",pch=20)
      # this condition is to add more models in the plots
      if(ncol(datFore) > 4){
        for(j in 5:ncol(datFore)){
          CountFE_name2 <- colnames(datFore)[j]
          lines(yini:yfin, c(rep(NA, 2017-yini-1), Suicides5a1[(2017-yini),outcome1],Suicides5a2[,CountFE_name2]), 
                col=c(rep("black",2017-yini),rep(rainbow(10)[j-3],yfin-2017+1)), type = "o",pch=20)
        }
      }
      
    }
    par(op)
  }
}


######################################################################
#  This function boxplot the distribution of the municipalities
######################################################################

box_summary <- function(datos1a, title, xfin=6, yini=2013,yfin=2018,xint=4.7,
                        save1=0,name_sale1,format, file.out.ex, LI=min(datos1a),LS=max(datos1a)){
  if(save1==1 && format=="png"){
    name.out = paste(file.out.ex, name_sale1,".png", sep = "") 
    png(name.out, res=110)
    op <- par(las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
    boxplot(datos1a, axes=F, main= title, ylim=c(LI,LS))
    axis(2)
    axis(1, 1:xfin, seq(yini,yfin,1))
    segments(xint,0, xint, max(datos1a)+1, col="red",lty=2,lwd=2)  
    par(op)
    dev.off()
  }else if(save1==1 && format=="eps"){
    setEPS()
    name.out = paste(file.out.ex, name_sale1,".eps", sep = "") 
    postscript(name.out)
    op <- par(las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
    boxplot(datos1a, axes=F, main= title, ylim=c(LI,LS))
    axis(2)
    axis(1, 1:xfin, seq(yini,yfin,1))
    segments(xint,0, xint, max(datos1a)+1, col="red",lty=2,lwd=2)  
    par(op)
    dev.off()
  }else{
    boxplot(datos1a, axes=F, main= title, ylim=c(LI,LS))
    axis(2)
    #axis(2,seq(0,max(datos1a),1),seq(0,max(datos1a),1))
    par(las=2)
    axis(1, 1:xfin, seq(yini,yfin,1))
    segments(xint,0, xint, max(datos1a)+1, col="red",lty=2,lwd=2)
  }
}

######################################################################
#  This function boxplot the distribution of the municipalities
#  For year to compare the true data with the forecast from different
#    models
######################################################################

box_summary2 <- function(datos1a, title, xfin=6, yini=2013,yfin=2018,xint=4.7,
                         save1=0,n.models=8,AoF=2017, name_sale1,format, file.out.ex, LI=min(datos1a),LS=max(datos1a)){
  if(save1==1 && format=="png"){
    name.out = paste(file.out.ex, name_sale1,".png", sep = "") 
    png(name.out, res=110)
    op <- par(las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
    boxplot(datos1a, axes=F, main= title,
            col=c("lightgray",rainbow(10)[1:n.models]), xlab=AoF, ylim=c(LI,LS))
    axis(2)
    axis(1, 2:(n.models), paste("M",1:(n.models-1),sep=''))
    #segments(xint,0, xint, max(datos1a)+1, col="red",lty=2,lwd=2)  
    par(op)
    dev.off()
  }else if(save1==1 && format=="eps"){
    setEPS()
    name.out = paste(file.out.ex, name_sale1,".eps", sep = "") 
    postscript(name.out)
    op <- par(las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
    boxplot(datos1a, axes=F, main= title,
            col=c("lightgray",rainbow(10)[1:n.models]), xlab=AoF, ylim=c(LI,LS))
    axis(2)
    axis(1, 2:(n.models), paste("M",1:(n.models-1),sep=''))
    #segments(xint,0, xint, max(datos1a)+1, col="red",lty=2,lwd=2)  
    par(op)
    dev.off()
  }else{
    boxplot(datos1a, axes=F, main= title,
            col=c("lightgray",rainbow(10)[1:n.models]), xlab=AoF, ylim=c(LI,LS))
    axis(2)
    axis(1, 2:(n.models), paste("M",1:(n.models-1),sep=''))
    #segments(xint,0, xint, max(datos1a)+1, col="red",lty=2,lwd=2)
  }
}

######################################################################
#  This function boxplot the distribution of the municipalities
#  For year to compare the true data with the forecast from different
#    models
######################################################################

box_summary3 <- function(datos1a, title, xfin=6, yini=2013,yfin=2018,xint=4.7,
                         save1=0,n.models=8,AoF=2017, name_sale1,format, file.out.ex, LI=min(datos1a),LS=max(datos1a)){
  if(save1==1 && format=="png"){
    name.out = paste(file.out.ex, name_sale1,".png", sep = "") 
    png(name.out, res=110)
    op <- par(las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=0.9,cex.lab=1,cex.main=1)
    boxplot(datos1a, axes=F, main= title,
            col=c(rep(c("lightgray",rainbow(10)[1:n.models]),2)), xlab="", ylim=c(LI,LS))
    axis(2)
    par(las=2)
    axis(1, 1:((n.models+1)*2), c(AoF, paste("M",1:(n.models),sep=''), 
                                  yfin, paste("M",1:(n.models),sep='')))
    #segments(xint,0, xint, max(datos1a)+1, col="red",lty=2,lwd=2)  
    par(op)
    dev.off()
  }else if(save1==1 && format=="eps"){
    setEPS()
    name.out = paste(file.out.ex, name_sale1,".eps", sep = "") 
    postscript(name.out)
    op <- par(las=1,mar=c(2.5,2.5,2,0)+.5, mgp=c(1.6,.6,0),cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
    boxplot(datos1a, axes=F, main= title,
            col=c(rep(c("lightgray",rainbow(10)[1:n.models]),2)), xlab="", ylim=c(LI,LS))
    axis(2)
    par(las=2)
    axis(1, 1:((n.models+1)*2), c(AoF, paste("M",1:(n.models),sep=''), 
                                  yfin, paste("M",1:(n.models),sep='')))
    #segments(xint,0, xint, max(datos1a)+1, col="red",lty=2,lwd=2)  
    par(op)
    dev.off()
  }else{
    boxplot(datos1a, axes=F, main= title,
            col=c(rep(c("lightgray",rainbow(10)[1:n.models]),2)), xlab="", ylim=c(LI,LS))
    axis(2)
    par(las=2)
    axis(1, 1:((n.models+1)*2), c(AoF, paste("M",1:(n.models),sep=''), 
                                  yfin, paste("M",1:(n.models),sep='')))
    #segments(xint,0, xint, max(datos1a)+1, col="red",lty=2,lwd=2)
  }
}

