
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
library("VIM")

#load functions script
source("functions/functions.R")
source("functions/univ_regression.R")

#load script to read in data
dataset = "full" # ahsn or full if complete set
SNPs = "T" # SNP analysis or not?
save_plots  = "T"  #overwrite saved plots?

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
    
    


modeldata <- cbind.data.frame(mydata$outcome, mydata$age, mydata$Sex, mydata$dutchscore, mydata$C_TendXan, mydata$C_CornArcus, mydata$TotalC, 
                              mydata$LDLC, mydata$nonhdl, mydata$Trigly, mydata$gamlass_centile, mydata$MoM)
colnames(modeldata) <- c("outcome", "age", "Sex", "dutchscore", "TendXan", "CornArcus", "TotalC", "LDLC", "nonhdl", "Trigly", "gamlass_centile", "MoM")
# check correlations


modeldata <- subset(modeldata, modeldata$Sex != "")


#md.pattern(modeldata)
# first line is observations with no missing data
# mice_plot <- aggr(modeldata, col=c('navyblue','yellow'),
#                   numbers=TRUE, sortVars=TRUE,
#                   labels=names(modeldata), cex.axis=.7,
#                   gap=3, ylab=c("Missing data","Pattern"))


with(modeldata, tapply(age, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(dutchscore, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(LDLC, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(nonhdl, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(MoM, outcome, mean,na.rm = TRUE))

#data.mids <- imputing_data(modeldata)
#glm.out <- glm.mids(outcome ~ dutchscore + age + factor(Sex) + TotalC + LDLC + nonhdl + Trigly + MoM, data = data.mids, family=binomial)
#summary(pool(glm.out)) ### pool averaging across all imputed dataset 
#aic=extractAIC(glm.out)

##### check for normality with chol values

modeldata$dutchscore <- replace_data(modeldata$dutchscore)
modeldata$TotalC <- replace_data(modeldata$TotalC)
modeldata$LDLC <- replace_data(modeldata$LDLC)
modeldata$nonhdl <- replace_data(modeldata$nonhdl)
modeldata$Trigly <- replace_data(modeldata$Trigly)
modeldata$MoM <- replace_data(modeldata$MoM)


################ Dutch score alone ################
#modeldata <- modeldata[which(mydata$dutchscore != ""),]

glm.out <- glm(outcome ~ dutchscore, family=binomial, data=modeldata)
summary(glm.out)
aic=extractAIC(glm.out)
modeldata <- modeldata %>% mutate(prob = predict(glm.out,type=c("response")))
anova(glm.out, test="Chisq")
predRisk <- predRisk(glm.out)

g0 <- plot.roc(modeldata$outcome, modeldata$prob, percent=TRUE, ci=TRUE, levels=c("NMD", "MD"), main ="Dutch Score alone" )
g0
auroc=ci.auc(g0)
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI


#### components of dutchscore - how many complete datasets do we have?
# the compendents are:
# A_max # family history
# B_max # personal history
# C_TendXan
# C_CornArcus
# LDLC level


modeldata2 <- cbind.data.frame(mydata$outcome, mydata$A_max, mydata$B_max, mydata$C_TendXan, 
                               mydata$C_CornArcus, mydata$LDLC, mydata$LDL)
colnames(modeldata2) <- c("outcome", "Famhist", "PersHist", "TendXan", "CornArcus", "LDLC", "LDL_level")
# check correlations


modeldata2 <- subset(modeldata2, modeldata2$LDLC != "")
modeldata2$PersHist[modeldata2$PersHist == "UNKNOWN"] <- NA

modeldata2$PersHist <- as.numeric(modeldata2$PersHist)

modeldata2$PersHist <- replace_data(modeldata2$PersHist)
modeldata2$Famhist <- replace_data(modeldata2$Famhist)



# missing too many values for Tend - 35 and Corn - 76?  
# do fit first without these variables and then try to impute.


glm.out <- glm(outcome ~ Famhist + PersHist + factor(LDL_level), family=binomial, data=modeldata2)
summary(glm.out)
aic=extractAIC(glm.out)
modeldata2 <- modeldata2 %>% mutate(prob = predict(glm.out,type=c("response")))
anova(glm.out, test="Chisq")
predRisk <- predRisk(glm.out)

g0 <- plot.roc(modeldata2$outcome, modeldata2$prob, percent=TRUE, ci=TRUE, levels=c("NMD", "MD"), main ="componements of DS with LDL levels" )
g0
auroc=ci.auc(g0)
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI

#####################
### use LDLC values rather than levels

glm.out <- glm(outcome ~ Famhist + PersHist + LDLC, family=binomial, data=modeldata2)
summary(glm.out)
aic=extractAIC(glm.out)
modeldata2 <- modeldata2 %>% mutate(prob = predict(glm.out,type=c("response")))
anova(glm.out, test="Chisq")
predRisk <- predRisk(glm.out)

g0 <- plot.roc(modeldata2$outcome, modeldata2$prob, percent=TRUE, ci=TRUE, levels=c("NMD", "MD"), main ="componements of DS with LDL" )
g0
auroc=ci.auc(g0)
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI

#####################
### try imputation for Tend and CornArcus
##https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
total_study <- total_study %>% mutate(MoM = ifelse(Sex == "MALE",nonhdl/median_nonhdlM,  nonhdl/median_nonhdlF), ifelse(Sex == "", NA, MoM))
modeldata2 <- cbind.data.frame(mydata$outcome, mydata$A_max, mydata$B_max, mydata$C_TendXan, 
                               mydata$C_CornArcus, mydata$LDLC, mydata$LDL, mydata$MoM, mydata$gamlass_centile)
colnames(modeldata2) <- c("outcome", "Famhist", "PersHist", "TendXan", "CornArcus", "LDLC", "LDL_level", "MoM", "gamlass_centile")
# check correlations


modeldata2 <- subset(modeldata2, modeldata2$LDLC != "")
modeldata2$PersHist[modeldata2$PersHist == "UNKNOWN"] <- NA

modeldata2$PersHist <- as.numeric(modeldata2$PersHist)


modeldata2.mids <- imputing_data(modeldata2)
# check values are plausible
modeldata2.mids$imp$TendXan
modeldata2.mids$imp$CornArcus
summary(modeldata2.mids)
tempData <- mice::complete(modeldata2.mids)

#xyplot(modeldata2.mids,outcome ~ Famhist + PersHist + factor(TendXan) + factor(CornArcus) +  factor(LDL_level),pch=18,cex=1)
densityplot(modeldata2.mids) #The density of the imputed data for each imputed dataset is showed in magenta while the density of the observed data is showed in blue.
# What we would like to see is that the shape of the magenta points (imputed) matches the shape of the blue ones (observed).
stripplot(modeldata2.mids, pch = 20, cex = 1.2) # function that shows the distributions of the variables as individual points


# can do fit for all models or used combined data from tempData
#glm.out <- glm.mids(outcome ~ Famhist + PersHist + factor(TendXan) + factor(CornArcus) +  factor(LDL_level) , data = modeldata2.mids, family=binomial)
#glm.out
#summary(pool(glm.out)) ### pool averaging across all imputed dataset 

glm.out <- glm(outcome ~ Famhist + PersHist + factor(TendXan) + factor(CornArcus) +  factor(LDL_level) , data = tempData, family=binomial)
glm.out
summary(glm.out) ### pool averaging across all imputed dataset 

aic=extractAIC(glm.out)

tempData <- tempData %>% mutate(prob = predict(glm.out,type=c("response")))
anova(glm.out, test="Chisq")
predRisk <- predRisk(glm.out)

g0 <- plot.roc(tempData$outcome, tempData$prob, percent=TRUE, ci=TRUE, levels=c("NMD", "MD"), main ="Predictors from Dutch S, imputated data" )
g0
auroc=ci.auc(g0)
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI

# Repeat with values of LDL
glm.out <- glm(outcome ~ Famhist + PersHist + factor(TendXan) + factor(CornArcus) +  LDLC , data = tempData, family=binomial)
glm.out
summary(glm.out) ### pool averaging across all imputed dataset 

aic=extractAIC(glm.out)

tempData <- tempData %>% mutate(prob = predict(glm.out,type=c("response")))
anova(glm.out, test="Chisq")
predRisk <- predRisk(glm.out)

g0 <- plot.roc(tempData$outcome, tempData$prob, percent=TRUE, ci=TRUE, levels=c("NMD", "MD"), main ="Predictors from Dutch S, imputated data" )
g0
auroc=ci.auc(g0)
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI


## repeat with values of MoM to replace LDL


glm.out <- glm(outcome ~ Famhist + PersHist + factor(TendXan) + factor(CornArcus) +  MoM , data = tempData, family=binomial)
glm.out
summary(glm.out) ### pool averaging across all imputed dataset 

aic=extractAIC(glm.out)

tempData <- tempData %>% mutate(prob = predict(glm.out,type=c("response")))
anova(glm.out, test="Chisq")
predRisk <- predRisk(glm.out)

g0 <- plot.roc(tempData$outcome, tempData$prob, percent=TRUE, ci=TRUE, levels=c("NMD", "MD"), main ="Predictors from Dutch S, imputated data" )
g0
auroc=ci.auc(g0)
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI

################################################ here on 21st April.####################

#replace LDL with centile scores - does it improve?


glm.out <- glm(outcome ~ Famhist + PersHist + factor(TendXan) + factor(CornArcus) +  factor(gamlass_centile) , data = tempData, family=binomial)
glm.out
summary(glm.out) ### pool averaging across all imputed dataset 

aic=extractAIC(glm.out)

tempData <- tempData %>% mutate(prob = predict(glm.out,type=c("response")))
anova(glm.out, test="Chisq")
predRisk <- predRisk(glm.out)

g0 <- plot.roc(tempData$outcome, tempData$prob, percent=TRUE, ci=TRUE, levels=c("NMD", "MD"), main ="Predictors from Dutch S, imputated data" )
g0
auroc=ci.auc(g0)
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI

#################################################################


glm.out <- glm(outcome ~ age + Sex + TotalC + LDLC + nonhdl + Trigly + MoM, family=binomial, data=modeldata)
summary(glm.out)
aic=extractAIC(glm.out)
modeldata <- modeldata %>% mutate(prob = predict(glm.out,type=c("response")))
anova(glm.out, test="Chisq")
predRisk <- predRisk(glm.out)

g0 <- plot.roc(modeldata$outcome, modeldata$prob, percent=TRUE, ci=TRUE, levels=c("NMD", "MD"), main ="Predictor Set 2" )
g0
auroc=ci.auc(g0)
o1a=cbind(deviance(glm.out),aic[2],auroc[2], auroc[1], auroc[3])#odds ratio and CI





# do we want to break down DLCN score?  replace fasting LDL with nonhdl.  
# then look at reclassification
modeldlcndata <- cbind.data.frame(total_study$outcome, total_study$age, total_study$Sex, total_study$DLCN, 
                                  total_study$A_max, total_study$B_max, total_study$C_max, 
                                  total_study$D_max, total_study$LDLC,  total_study$nonhdl, total_study$gamlass_centile)
colnames(modeldlcndata) <- c("outcome", "age",  "Sex", "dutchscore", "A", "B", "C", "D", "LDLC", "nonhdl", "centile")

# replace centile levels with a score, 1-8



modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$outcome)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$Sex)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$LDLC)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$A)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$B)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$C)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$D)
modeldlcndata <- remove_missing(modeldlcndata, modeldlcndata$age)


# not enough data to break down dutch score.  

# look at all items from univariate analysis
modeldata <- cbind.data.frame(mydata$outcome, mydata$age, mydata$Sex, mydata$dutchscore, mydata$LDLC, mydata$nonhdl, mydata$MoM)
colnames(modeldata) <- c("outcome", "age", "Sex", "dutchscore", "LDLC", "nonhdl", "MoM")
# check correlations

with(modeldata, tapply(age, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(dutchscore, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(LDLC, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(nonhdl, outcome, mean,na.rm = TRUE))
with(modeldata, tapply(MoM, outcome, mean,na.rm = TRUE))




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

