
# clear the global environment
rm(list = ls())

#path to the working directory

wd <- "Z:/DEC methods/tools - R/Working_project_folders/NCL_DEC0002 Familial hypercholesterolaemia/Current2016-2017/"
#wd <- "/Users/JoyA/Documents/DEC WORK/FH/Current2016-2017"
setwd(wd)

#load in packages required
source("loadpackages.R")
loadpackages()

library("dplyr")
library("plyr")
library("pROC")
library("magrittr")
library("mice")

#load functions script
source("functions/functions.R")
source("functions/univ_regression.R")

#load script to read in data
dataset = "ahsn" # ahsn or full if complete set
SNPs = "F" # SNP analysis or not?
save_plots  = "T"  #overwrite saved plots?
imputation = "T" # exclude data which is missing or use imputation techniques?

source("functions/read_clean_update.R")
readingfiles(dataset, SNPs)

#import info on test cost for cost analysis
source("functions/inputs.R")
inputs <- inputs()

# FOR CHECKS #
sapply(mydata,class)
#View(total_study)


##### median nonHDL levels from HSE data

median_nonhdlM <- 4
median_nonhdlF <- 3.8

### GENERALISED LINEAR MODEL ##############################
# response variable - outcome(MD, NMD)
# predictors ? 
# age
# family history - 
# personal history - 
# physical exam - 
# fasting ldl - 
# nonhdl
#####
# compare with centile data
# gamlass_centiles

##############################################################

#sapply(mydata,class)

#load snp score plotting script
median_nonhdl <- median(mydata$nonhdl, na.rm = TRUE)

#mydata <- mydata[mydata$Sex!= "",] %<>% mutate(MoM = ifelse(Sex == "MALE",nonhdl/median_nonhdlM,  nonhdl/median_nonhdlF))
# two way pipe, subsets data into only those with no value for sex

mydata <- mydata %>% mutate(MoM = ifelse(Sex == "MALE",nonhdl/median_nonhdlM,  nonhdl/median_nonhdlF), ifelse(Sex == "", NA, MoM))
#visualise missing data
#

sapply(mydata,function(x) sum(is.na(x)))
missmap(mydata, main = "Missing values vs observed")
  
  ###analytical exploration: univariate analysis
  # remove Lipo as a lot of missing data
  mydata$Lipo <- NULL
 
   if(SNPs){
    mydata$SNPscore <- NULL
    mydata$SNPdecile <- NULL
    source("functions/snp_scores.R")
    snp_scores(mydata)
  }



  mydata <- subset(mydata,mydata$outcome != "")  
  n_after <- nrow(mydata)
  message(paste0(nparticipants - n_after, " participants removed as missing outcome data"))
 
  # how is R dealing with the categorical data, shows how they will be used in the model
    contrasts(mydata$Sex)
    contrasts(mydata$C_TendXan)
    contrasts(mydata$C_CornArcus)
    contrasts(mydata$outcome)
    contrasts(mydata$gamlass_centile)


# mldl <- glm(outcome~., family = "binomial", data = mydata, maxit= 100)
# summary(mldl)
# 
# anova(mldl, test="Chisq")# to see how much deviance we can explain with our model; two models can be also compared with ANOVA
# confint(mldl) # 95% CI for the coefficients
# exp(cbind(OR = coef(mldl), confint(mldl)))#odds ratio and CI
# p <- predict(mldl,mydata,type='response')

if(dataset == "ahsn"){
  figfilepath = "figures/ahsn/"
  tabfilepath = "output/ahsn/"
} else {
  figfilepath = "figures/full/"
  tabfilepath = "output/full/"
}
    
    

##exploratory analysis: plots
if(save_plots) {
  source("functions/demographic_plots.R")
  demographic_plots(mydata, figfilepath)
}
    
    

now <- format(Sys.time(), "%b%d%H%M%S")
filename <- paste0(tabfilepath,now,"demographics.csv")

#how many variables do we have?
nvari = ncol(mydata)


for (i in 1:nvari) {
  
  if (class(mydata[,i]) == "integer" | class(mydata[,i]) == "numeric" ) next
  
  vari = mydata[,i]
  vari_name = names(mydata[i])

  count <- table(mydata[i])
  
  write.table(vari_name, file= filename, sep = ",",  col.names=F, row.names=F, append=T)
  write.table(count, file= filename, sep = ",",  col.names=F, row.names=F, append=T)

}


counts0 <- table(mydata$outcome)
prev=counts0[1]/(counts0[1]+counts0[2])
prev
nrow(mydata)

# univariate analysis first

#Univariate logistic regressions were used to investigate whether the relationship 
#between outcome variable (i.e. MD or not) and the input variables (age, Sex, dutchscore,LDLC, NoRels50.risk, NoRels25.risk
#TendXan, CornArcus, TotalC, LDLC, nonhdl, Trigly, and gamlass_centiles) were linear. 
#If they were not linear, the log transformation and squared transformation were attempted. 
#If the transformation substantially significantly lowered the AIC and AUROC, 
#then the variable was transformed for use in the statistical model building analyses.  
# age

#######################################################################################


# do checks for one variable at a time
#plot(m)
#The top-left and top-right graphs are the most important one, 
#the top-left graph check for the homogeneity of the variance 
#and the linear relation, if you see no pattern in this graph 
#(ie if this graph looks like stars in the sky), then your assumptions are met. 
#The second graphs check for the normal distribution of the residuals, 
#the points should fall on a line. The bottom-left graph is similar to the top-left one, 
#the y-axis is changed, this time the residuals are square-root standardized (?rstandard) 
#making it easier to see heterogeneity of the variance. The fourth one allow 
#detecting points that have a too big impact on the regression coefficients 
#and that should be removed. 


par(mfrow=c(1,1))

filename2 <- paste0( tabfilepath,now,"univariateanalysismodels.csv")
filename3 <- paste0( tabfilepath,now,"univariateanalysiscoeffs.csv")

# to avoid a loop we should write a function for this.


if(imputation){
  mydata1.mids <- imputing_data(mydata)
  # mydata1.mids$imp$Sex
  summary(mydata1.mids)
  mydata1 <- mice::complete(mydata1.mids)
  
  sapply(mydata1, function(x) sum(is.na(x)))
  # perform checks on data
  #nonhdl
  actual_means <- sapply(mydata,function(x) {
    if(class(x) == "numeric" | class(x) == "integer") {mean(x, na.rm = TRUE)
       } else if(class(x) == "factor") {table(x)
      }
  })
  predicted_means <- sapply(mydata1,function(x) {
    if(class(x) == "numeric" | class(x) == "integer") {mean(x, na.rm = TRUE)
    } else if(class(x) == "factor") {table(x)
    }
  })
  
  actual_medians <- sapply(mydata,function(x) {
    if(class(x) == "numeric" | class(x) == "integer") {median(x, na.rm = TRUE)
    } else if(class(x) == "factor") {table(x)
    }
  })
  predicted_medians <- sapply(mydata1,function(x) {
    if(class(x) == "numeric" | class(x) == "integer") {median(x, na.rm = TRUE)
    } else if(class(x) == "factor") {table(x)
    }
  })
  
  imputated_values <- cbind(actual_means,predicted_means, actual_medians,predicted_medians)
  imputated_values <- as.data.frame(imputated_values)
  
 # write.table(as.data.frame(imputated_values), file = "imputated_value_compare.csv", quote=F,sep=",",row.names=T)
 
  mean(mydata$dutchscore,na.rm = T)
  #xyplot(modeldata2.mids,outcome ~ Famhist + PersHist + factor(TendXan) + factor(CornArcus) +  factor(LDL_level),pch=18,cex=1)
  dev.copy(png,paste0(figfilepath,'imputation_checks.png'))
  print(densityplot(mydata1.mids)) #The density of the imputed data for each imputed dataset is showed in magenta while the density of the observed data is showed in blue.
  dev.off()
   # What we would like to see is that the shape of the magenta points (imputed) matches the shape of the blue ones (observed).
  dev.copy(png,paste0(figfilepath,'imputation_checks2.png'))
  print(stripplot(mydata1.mids, pch = 20, cex = 1.2)) # function that shows the distributions of the variables as individual points
   dev.off()
}
  

for (i in 1:nvari) {
 
  if (imputation== "F"){ 
    mydata1 <- subset(mydata, mydata[i] != "Unknown")
    mydata1 <- subset(mydata1, mydata1[i] != "")
    mydata1 <- subset(mydata1, mydata1[i] != "UNKNOWN" & mydata1$outcome != "")
  }
    
  vari = mydata1[,i]
  vari_name = names(mydata1[i])
  
  if (vari_name == "NoRels50.risk" | vari_name == "NoRels25.risk" |
      vari_name == "results" | vari_name == "gamlass_centile" | 
      vari_name == "outcome" | vari_name == "prob" ) next 
  
  nrow(mydata1)
  
 
  m1 <- univ_regression(vari,vari_name, mydata1)
  
  #### square variable transform ###
  if (class(vari) != "factor"){
  vari2=vari^2
  m2 <- univ_regression(vari + vari2,vari_name, mydata1)

   } else  { 
    m2 <- m1
  }
  
 
  ### log transform age ###
  # only for coninuous variables
  if (class(vari) != "factor" ){
    m3 <- m2
    if (min(is.na(vari)) != 0 | min(is.na(vari)) > 0)   m3 <- univ_regression(log(vari),vari_name, mydata1)
  } else {m3 <- m2}
   

  # compare models
  
  modelinfo <- rbind.data.frame(m1$o1a,m2$o1a,m3$o1a)
  modelinfo$variable <- vari_name
  colnames(modelinfo) <- c("deviance", "aic", "auroc", 
                            "ciauroc", "ciauroc", "variable")
  
  # preference to linear model
  # however if other model increases the auroc >0.1 and decreases AIC by >4, choose that instead
  
  min(m2$aic[2], m3$aic[2])
  max(m2$auroc[2], m3$auroc[2])
  
  # diff in aics between linear and model 2
  diff_aic2 = m1$aic[2] - m2$aic[2]
  diff_aic3 = m1$aic[2] - m3$aic[2]
  
  # diff in aurocs between linear and model 3
  diff_auroc2 = m2$auroc[2] - m1$auroc[2] 
  diff_auroc3 = m3$auroc[2] - m1$auroc[2]
  
  #default choice is model 1
  
  modelchoice <- 1
  if(diff_aic2 > 4 & diff_auroc2 > 0.1) {
    message("For variable ", vari_name, " AIC model 2 lower AIC and higher AUC than linear")
    modelchoice <- 2
  }
  
  if(diff_aic3 > 4 & diff_auroc3 > 0.1) {
    message("For variable", vari_name, "AIC model 3 lower AIC and higher AUC than linear")
    modelchoice <- 3
    }
  
  # check between models m2 and m3
  diff_aic23 = m2$aic[2] - m3$aic[2]
  diff_auroc23 = m3$auroc[3] - m2$auroc[2] 
  if (diff_aic23 >4 & diff_auroc23 > 0.1) {
    message("For variable", vari_name, "AIC model 3 lower AIC and higher AUC than model 2")
    modelchoice <- 3
  }

  modelinfo$modelchoice <- modelchoice
  
  # write out info about model choices
  
  
  
  # write out details about the model choosen
  if(modelchoice == 1){
    df <- tidy(m1$m)
    df$mchoice <- modelchoice
    write_output(modelinfo, vari_name,filename2, i)
    write_output(m1$coeff,vari_name,filename3, i)
    #coefficient details
    
 
    if (save_plots){
      m1$g0 
      name <- paste0(figfilepath,now,"roc",vari_name, ".png")
      dev.copy(png,name)
      dev.off()
    }  
    
    }
  if(modelchoice == 2){
    df <- tidy(m2$m)
    df$mchoice <- modelchoice
    write_output(modelinfo, vari_name,filename2, i)
    write_output(m2$coeff,vari_name,filename3,i)
    

    
    if (save_plots){
      g02 
      name <- paste0(figfilepath,now,"roc",vari_name, ".png")
      dev.copy(png,name)
      dev.off()
    } 
     }
  if(modelchoice == 3){
    df <- tidy(m3$m)
    df$mchoice <- modelchoice
    write_output(modelinfo, vari_name,filename2, i)
    write_output(m3$coeff,vari_name,filename3, i)
    

#     
    if (save_plots){
      g03 
      name <- paste0(figfilepath,now,"roc",vari_name, ".png")
      dev.copy(png,name)
      dev.off()
    } 
    }
    

}
  end
#should we include any of the categorical variables as continuous?
# are the coefficients linearly increasing?
  
# values from the hypothesis
# models to test
# dutchscore or  ldlc with age  or  nonhdl/ MoM with age (model for M, model for F)
# which is better predictor?
  
modeldata <- cbind.data.frame(mydata$outcome, mydata$age, mydata$Sex, mydata$dutchscore, mydata$LDLC, mydata$nonhdl, mydata$MoM)
colnames(modeldata) <- c("outcome", "age", "Sex", "dutchscore", "LDLC", "nonhdl", "MoM")
# check correlations

with(modeldata, tapply(age, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(dutchscore, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(LDLC, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(nonhdl, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(MoM, outcome, mean,na.rm = TRUE))


################ Dutch score alone, and break it down ################
modeldata <- modeldata[which(mydata$dutchscore != ""),]

glm.out <- glm(outcome ~ dutchscore, family=binomial, data=modeldata)
summary(glm.out)
aic=extractAIC(glm.out)
anova(glm.out, test="Chisq")
modeldata$prob=predict(glm.out,type=c("response"))
predRisk <- predRisk(glm.out)
g <- roc(outcome ~ prob, data = modeldata, ci=TRUE, levels=c("NMD", "MD"))
g
auroc=ci.auc(g)
g0 <- plot.roc(modeldata$outcome, modeldata$prob, percent=TRUE, ci=TRUE, levels=c("NMD", "MD"), main ="Dutch Score alone" )
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI

# do we want to break down DLCN score?  replace fasting LDL with nonhdl.  
# then look at reclassification
modeldlcndata <- cbind.data.frame(mydata$outcome, mydata$age, mydata$Sex, mydata$DLCN, 
                                  mydata$A_max, mydata$B_max, mydata$C_max, 
                                  mydata$D_max, mydata$LDLC,  mydata$nonhdl, mydata$gamlass_centile)
colnames(modeldlcndata) <- c("outcome", "age",  "Sex", "dutchscore", "A", "B", "C", "D", "LDLC", "nonhdl", "centile")

# replace centile levels with a score, 1-8

source("functions/functions.R")

modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$outcome)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$Sex)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$LDLC)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$A)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$B)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$C)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$D)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$age)


glm.out <- glm(outcome ~ dutchscore, family=binomial, data=modeldata)
summary(glm.out)
aic=extractAIC(glm.out)
anova(glm.out, test="Chisq")
modeldata$prob=predict(glm.out,type=c("response"))
predRisk <- predRisk(glm.out)
g <- roc(outcome ~ prob, data = modeldata, ci=TRUE)
g
auroc=ci.auc(g)
g0 <- plot.roc(mydata$outcome, mydata$prob, percent=TRUE, ci=TRUE, main ="Dutch Score alone" )
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI





glm.out2 <- glm(outcome ~ LDLC + age + factor(Sex), family=binomial, data=modeldata)
glm.out2
summary(glm.out2)
aic2=extractAIC(glm.out2)
anova(glm.out2, test="Chisq")
modeldata$prob2=predict(glm.out2,type=c("response"))
predRisk <- predRisk(glm.out2)
g2 <- roc(outcome ~ prob2, data = modeldata, ci=TRUE)
g2
auroc2=ci.auc(g2)
g02 <- plot.roc(mydata$outcome, mydata$prob2, percent=TRUE, ci=TRUE, main ="LDLC + Age + Sex" )
o1a2=cbind(deviance(glm.out2),aic2[2],auroc2[2], auroc2[1], auroc2[3])#odds ratio and CI



glm.out3 <- glm(outcome ~ nonhdl+ age + factor(Sex), family=binomial, data=modeldata)
glm.out3
anova(glm.out3, test="Chisq")
summary(glm.out3)
aic3=extractAIC(glm.out3)
anova(glm.out3, test="Chisq")
modeldata$prob3=predict(glm.out3,type=c("response"))
predRisk <- predRisk(glm.out3)
g3 <- roc(outcome ~ prob3, data = modeldata, ci=TRUE)
g3
auroc3=ci.auc(g3)
g03 <- plot.roc(mydata$outcome, mydata$prob3, percent=TRUE, ci=TRUE, main ="Nonhdl + age + sex" )
o1a3=cbind(deviance(glm.out3),aic3[2],auroc3[2], auroc2[1], auroc3[3])#odds ratio and CI

#glm.out4 <- glm(outcome ~ MoM + age + factor(Sex), family=binomial, data=modeldata)
#glm.out4

####################################################################################################

# do we want to break down DLCN score?  replace fasting LDL with nonhdl.  
# then look at reclassification
modeldlcndata <- cbind.data.frame(total_study$outcome, total_study$age, total_study$Sex, total_study$DLCN, 
                                  total_study$A_max, total_study$B_max, total_study$C_max, 
                                  total_study$D_max, total_study$LDLC,  total_study$nonhdl, total_study$gamlass_centile)
colnames(modeldata) <- c("outcome", "age",  "Sex", "dutchscore", "A", "B", "C", "D", "LDLC", "nonhdl", "centile")

# replace centile levels with a score, 1-8





#### replace ducth score LDLC (D_max) with centiles

glm.out <- glm(outcome ~ A + B + C + D, family=binomial, data=modeldata)
summary(glm.out)
aic=extractAIC(glm.out)
anova(glm.out, test="Chisq")
modeldata$prob=predict(glm.out,type=c("response"))
predRisk <- predRisk(glm.out)
g <- roc(outcome ~ prob, data = modeldata, ci=TRUE)
g
auroc=ci.auc(g)
g0 <- plot.roc(mydata$outcome, mydata$prob, percent=TRUE, ci=TRUE, main ="Dutch Score alone" )
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI

glm.out <- glm(outcome ~ A + B + C + LDLC, family=binomial, data=modeldata)
summary(glm.out)
aic=extractAIC(glm.out)
anova(glm.out, test="Chisq")
modeldata$prob=predict(glm.out,type=c("response"))
predRisk <- predRisk(glm.out)
g <- roc(outcome ~ prob, data = modeldata, ci=TRUE)
g
auroc=ci.auc(g)
g0 <- plot.roc(mydata$outcome, mydata$prob, percent=TRUE, ci=TRUE, main ="Dutch Score alone" )
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI

glm.out <- glm(outcome ~ A + B + C + nonhdl, family=binomial, data=modeldata)
summary(glm.out)
aic=extractAIC(glm.out)
anova(glm.out, test="Chisq")
modeldata$prob=predict(glm.out,type=c("response"))
predRisk <- predRisk(glm.out)
g <- roc(outcome ~ prob, data = modeldata, ci=TRUE)
g
auroc=ci.auc(g)
g0 <- plot.roc(mydata$outcome, mydata$prob, percent=TRUE, ci=TRUE, main ="Dutch Score alone" )
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI

glm.out <- glm(outcome ~ A + B + C + centile, family=binomial, data=modeldata)
summary(glm.out)
aic=extractAIC(glm.out)
anova(glm.out, test="Chisq")
modeldata$prob=predict(glm.out,type=c("response"))
predRisk <- predRisk(glm.out)
g <- roc(outcome ~ prob, data = modeldata, ci=TRUE)
g
auroc=ci.auc(g)
g0 <- plot.roc(mydata$outcome, mydata$prob, percent=TRUE, ci=TRUE, main ="Dutch Score alone" )
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI


####  Look at reclassification


####################################################################################
# results show all model choice one
# non hdl better predictor than LDL
  
  
#baseline models
  
  
  
#which variables show probabl relationship with the outcome variable, p <0.1.
# these will be included in the full model logistic regression
  



#dutch score
mdutchscore <- glm(outcome~dutchscore, family = "binomial", data = mydata, maxit= 100)
summary(mdutchscore)
tidy(mdutchscore)
anova(mdutchscore, test="Chisq")# to see how much deviance we can explain with our model; two models can be also compared with ANOVA
confint(mdutchscore) # 95% CI for the coefficients
exp(cbind(OR = coef(mdutchscore), confint(mdutchscore)))#odds ratio and CI
p <- predict(mdutchscore,mydata,type='response')
pr <- prediction(p, mydata$outcome)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")



#number of relatives at 50% risk
mrel50 <- glm(outcome~NoRels50.risk , family = "binomial", data = mydata, maxit= 100)
summary(mrel50)
tidy(mrel50)

#number of relatives at 25% risk
mrel25 <- glm(outcome~NoRels25.risk , family = "binomial", data = mydata, maxit= 100)
summary(mrel25)
tidy(mrel25)

# TotalC
mTC <- glm(outcome~TotalC , family = "binomial", data = mydata, maxit= 100)
summary(mTC)
tidy(mTC)

#LDLC
mLDLC <- glm(outcome~LDLC , family = "binomial", data = mydata, maxit= 100)
summary(mLDLC)
tidy(mLDLC)

#nonhdl
mnonHDL  <- glm(outcome~nonhdl , family = "binomial", data = mydata, maxit= 100)
summary(mnonHDL)
tidy(mnonHDL)

#Trigly
mTG <- glm(outcome~Trigly , family = "binomial", data = mydata, maxit= 100)
summary(mTG)
tidy(mTG)

#LDL
mLDL <- glm(outcome~LDL , family = "binomial", data = mydata, maxit= 100)
summary(mLDL)
tidy(mLDL)

#DLCN
mDLCN <- glm(outcome~DLCN , family = "binomial", data = mydata, maxit= 100)
summary(mDLCN)
tidy(mDLCN)

#gamlass_centile
mcentile <- glm(outcome~gamlass_centile, family = "binomial", data = mydata, maxit= 100)
summary(mcentile)
tidy(mcentile)

# need to use continuous data for centile derivation.  Can we take this from the centile plot?

pairs(outcome~LDL+nonhdl+Trigly, data = mydata)

