
# clear the global environment
rm(list = ls())

#path to the working directory

#wd <- "Z:/DEC methods/tools - R/Working_project_folders/NCL_DEC0002 Familial hypercholesterolaemia/Current2016-2017/"
wd <- "/Users/JoyA/Documents/DEC WORK/FH/Current2016-2017"
setwd(wd)

#load in packages required
source("loadpackages.R")
loadpackages()

#load functions script
source("functions/functions.R")

#load script to read in data
dataset = "ahsn" # ahsn or full if complete set
SNPs = "F" # SNP analysis or not?
save_plots  = "F"  #overwrite saved plots?

source("functions/read_clean_update.R")
readingfiles(dataset, SNPs)

#import info on test cost for cost analysis
source("functions/inputs.R")
inputs <- inputs()

# FOR CHECKS #
sapply(mydata,class)
#View(total_study)

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
mydata$MoM <- mydata$nonhdl/median_nonhdl

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



  mydata <- mydata[!is.na(mydata$outcome),]  
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


##exploratory analysis: plots

#age
p <- boxplot(mydata$age, xlab="age", cex.lab=2)
p

if (save_plots){
dev.copy(png,'figures/boxplotage.png')
dev.off()
}


#dutchscore
p <- boxplot(mydata$dutchscore, xlab="Dutchscore", cex.lab=2)
if (save_plots){
  dev.copy(png,'figures/boxplotDLCN.png')
  dev.off()
}


p <- ggplot(mydata, aes(outcome, dutchscore))
p <- p + geom_boxplot(aes(fill = outcome)) +  labs(x = "Mutation detected", y = "Dutch Lipid Clinic Network Score", size = 4) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
p

if (save_plots){
  dev.copy(png,'figures/boxplotDLCNvsoutcome.png')
  dev.off()
}

p <- boxplot(mydata$MoM, xlab="MoM", cex.lab=2)
p

if (save_plots){
  dev.copy(png,'figures/boxplotMoM.png')
  dev.off()
}

#MoM
p <- ggplot(mydata, aes(outcome,MoM))
p <- p + geom_boxplot(aes(fill = outcome))
p
if (save_plots){
  dev.copy(png,'figures/boxplotMoMvsoutcome.png')
  dev.off()
}


#Cholesterol
p <- ggplot(mydata, aes(outcome,TotalC))
p <- p + geom_boxplot(aes(fill = outcome))
p
if (save_plots){
  dev.copy(png,'figures/boxplotTotalCvoutome.png')
  dev.off()
}

p <- ggplot(mydata, aes(outcome,LDLC))
p <- p + geom_boxplot(aes(fill = outcome))
p
if (save_plots){
  dev.copy(png,'figures/boxplotLDLCoutcome.png')
  dev.off()
}

p <- ggplot(mydata, aes(outcome,nonhdl))
p <- p + geom_boxplot(aes(fill = outcome))
p
if (save_plots){
  dev.copy(png,'figures/boxplotnonhdloutcome.png')
  dev.off()
}


p <- ggplot(mydata, aes(outcome,Trigly))
p <- p + geom_boxplot(aes(fill = outcome))
p
if (save_plots){
  dev.copy(png,'figures/boxplotTriglyoutcome.png')
  dev.off()
}

# outlier is potentially a mis-entry for HDLC of 104 rather than 1.04.  Checked with 
#PI DN on 16th March 2017.  replace in analysis until return of results. 

p <- ggplot(mydata, aes(NoRels50.risk, fill = outcome))
p <- p + geom_histogram()
p
if (save_plots){
  dev.copy(png,'figures/rels50risk_hist.png')
  dev.off()
}

p <- ggplot(mydata, aes(NoRels25.risk, fill = outcome))
p <- p + geom_histogram()
p
if (save_plots){
  dev.copy(png,'figures/rels25risk_hist.png')
  dev.off()
}



now <- format(Sys.time(), "%b%d%H%M%S")
filename <- paste0("output/",now,"demographics.csv")


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


# 
counts0 <- table(mydata$Sex)
counts2 <- table(mydata$C_TendXan)
counts3 <- table(mydata$C_CornArcus)
counts4 <- table(mydata$LDL)
counts5 <- table(mydata$DLCN)
counts6 <- table(mydata$gamlass_centile)
counts7 <- table(mydata$outcome)
counts8 <- table(mydata$NoRels25.risk)
counts9 <- table(mydata$NoRels50.risk)




par(mfrow=c(2,3))
barplot(counts0,xlab="Sex", cex.lab=1.8, cex.axis=1.5)
barplot(counts2,xlab="Tendon Xanthomas",  cex.lab=1.8, cex.axis=1.5)
barplot(counts3, xlab="CornArcus",  cex.lab=1.8, cex.axis=1.5)
barplot(counts4, xlab="LDL", cex.lab=1.8, cex.axis=1.5)
barplot(counts5, xlab="DLCN",  cex.lab=1.8, cex.axis=1.5)
if (save_plots){
  dev.copy(png,'figures/demographics1.png')
  dev.off()
}



par(mfrow=c(2,2))
barplot(counts6, xlab="Centile interval",  cex.lab=1.8, cex.axis=1.5)
barplot(counts7, xlab="Outcome",  cex.lab=1.8, cex.axis=1.5)
barplot(counts8, xlab="NoRels at 25% risk",  cex.lab=1.8, cex.axis=1.5)
barplot(counts9, xlab="NoRels at 50% risk",  cex.lab=1.8, cex.axis=1.5)
if (save_plots){
  dev.copy(png,'figures/demographics2.png')
  dev.off()
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
i = 1

filename2 <- paste0("output/",now,"univariateanalysismodels.csv")
filename3 <- paste0("output/",now,"univariateanalysiscoeffs.csv")

for (i in 1:nvari) {
  
  mydata1 <- subset(mydata, mydata[i] != "" | mydata[i] != "UNKNOWN" | !is.na(mydata[i]))
  vari = mydata1[,i]
  vari_name = names(mydata1[i])
  
  if (vari_name == "NoRels50.risk" | vari_name == "NoRels25.risk" |
      vari_name == "results" | vari_name == "outcome" | vari_name == "prob" ) next 
  
  nrow(mydata1)
  m <- glm(outcome~vari, family = "binomial", data = mydata1, maxit= 100)
  
  summary(m) # beta, Std.Error, z value, Pr(> |z|)
  levs <- nlevels(mydata1[,i])
  tidy(m)
  
  
  if(levs == 0){
    coeff <- cbind(nrow(mydata1), round(coef(m)[2],4),paste0('(', round(confint(m)[2,1],4), ',', round(confint(m)[2,2],4),')'), 
                paste0(round(exp(cbind(OR = coef(m),confint(m)))[2,1],4), 
                       ' (', round(exp(cbind(OR = coef(m),confint(m)))[2,2],4), ',', 
                       round(exp(cbind(OR = coef(m),confint(m)))[2,3],4), ')'), round(coef(summary(m))[2,4],4))#odds ratio and CI

  }  else {
    coeff <-ldply(0:levs-2, function(j) cbind(nrow(mydata1), round(coef(m)[j+2],4),paste0('(', round(confint(m)[j+2,1],4), ',', round(confint(m)[j+2,2],4),')'), 
           paste0(round(exp(cbind(OR = coef(m),confint(m)))[j+2,1],4), 
        ' (', round(exp(cbind(OR = coef(m),confint(m)))[j+2,2],4), ',', 
           round(exp(cbind(OR = coef(m),confint(m)))[j+2,3],4), ')'), round(coef(summary(m))[j+2,4],4)))
   # does not support row names, but necessary for read into table!
  }
  
  #par(mfrow = c(2, 2))
  #plot(m)
  aic=extractAIC(m)
  mydata1$prob=predict(m,type=c("response"))
  predRisk <- predRisk(m)
  g <- roc(outcome ~ prob, data = mydata1, ci=TRUE)
  auroc=ci.auc(g)
  g0 <- plot.roc(mydata1$outcome, mydata1$prob, percent=T, ci=TRUE, main = (paste0(vari_name)))
  o1a=cbind(deviance(m),aic[2],auroc[2], auroc[1], auroc[3])
  MR=cbind(coef(m),confint(m), exp(cbind(OR = coef(m),confint(m))), coef(summary(m))[,4])#odds ratio and CI
  mdf = tidy(m)
 # m1rows = cbind(mdf, o1a)

  
  #### square variable transform ###
  if (class(vari) != "factor"){
  vari2=vari^2
  m2 <- glm(outcome~vari + vari2, family = "binomial", data = mydata1, maxit= 100)
 
  summary(m2)
  coeff_m2=cbind(nrow(mydata1), round(coef(m2)[2],4),paste0('(', round(confint(m2)[2,1],4), ',', round(confint(m2)[2,2],4),')'), 
                 paste0(round(exp(cbind(OR = coef(m2),confint(m2)))[2,1],4), 
                        ' (', round(exp(cbind(OR = coef(m2),confint(m2)))[2,2],4), ',', 
                        round(exp(cbind(OR = coef(m2),confint(m2)))[2,3],4), ')'), round(coef(summary(m2))[2,4],4))#odds ratio and CI
  
  aic2=extractAIC(m2)
  mydata1$prob2=predict(m2,type=c("response"))
  predRisk2 <- predRisk(m2)
  g2 <- roc(outcome ~ prob2, data = mydata1, ci=TRUE)
  auroc2=ci.auc(g2)
  g02 <- plot.roc(mydata1$outcome, mydata1$prob2, percent=TRUE, ci=TRUE, main = (paste0(vari_name))) 
  o1a2=cbind(deviance(m2),aic2[2],auroc2[2], auroc2[1], auroc2[3])

  m2df = tidy(m2)
  } else  { 
   m2 <- m
   o1a2 <- o1a
   aic2 <- aic
   auroc2 <- auroc
  }
  
  #m2rows = cbind(m2df, o1a2)

  ### log transform age ###
  # only for coninuous variables
  if (class(vari) != "factor"){
  

  m3 <- glm(outcome ~ log(vari), family = "binomial", data = mydata1, maxit= 100)
  
  summary(m3)
  coeff_m3=cbind(nrow(mydata1), round(coef(m3)[2],4),paste0('(', round(confint(m3)[2,1],4), ',', round(confint(m3)[2,2],4),')'), 
                 paste0(round(exp(cbind(OR = coef(m3),confint(m3)))[2,1],4), 
                        ' (', round(exp(cbind(OR = coef(m3),confint(m3)))[2,2],4), ',', 
                        round(exp(cbind(OR = coef(m3),confint(m3)))[2,3],4), ')'), round(coef(summary(m3))[2,4],4))#odds ratio and CI
  
  aic3=extractAIC(m3)
  mydata1$prob3=predict(m3,type=c("response"))
  predRisk3 <- predRisk(m3)
  g3 <- roc(outcome ~ prob3, data = mydata1, ci=TRUE)
  auroc3=ci.auc(g3)
  g03 <- plot.roc(mydata1$outcome, mydata1$prob3, percent=TRUE, ci=TRUE, main = (paste0(vari_name)))
  o1a3=cbind(deviance(m3),aic3[2],auroc3[2], auroc3[1], auroc3[3])#odds ratio and CI
  
  m3df = tidy(m3)
 # m3rows = cbind(m3df, o1a3)
  # compare aic, deviance and auroc to decide which to use for each variable in regression
  
  } else {
  
  m3 <- m2
  o1a3 <- o1a2
  aic3 <- aic2
  auroc3 <- auroc2
  }
  
  
  
  modelinfo <- rbind.data.frame(o1a,o1a2,o1a3)
  modelinfo$variable <- vari_name
  colnames(modelinfo) <- c("deviance", "aic", "auroc", 
                            "ciauroc", "ciauroc", "variable")
  
  # preference to linear model
  # however if other model increases the auroc >0.1 and decreases AIC by >4, choose that instead
  
  min(aic2[2], aic3[2])
  max(auroc2[2], auroc3[3])
  
  # diff in aics between linear and model 2
  diff_aic2 = aic[2] - aic2[2]
  diff_aic3 = aic[2] - aic3[2]
  
  # diff in aurocs between linear and model 3
  diff_auroc2 = auroc2[2] - auroc[2] 
  diff_auroc3 = auroc3[2] - auroc[2]
  
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
  diff_aic23 = aic2[2] - aic3[2]
  diff_auroc23 = auroc3[3] - auroc2[2] 
  if (diff_aic23 >4 & diff_auroc23 > 0.1) {
    message("For variable", vari_name, "AIC model 3 lower AIC and higher AUC than model 2")
    modelchoice <- 3
  }

  modelinfo$modelchoice <- modelchoice
  
  # write out info about model choices
  
  if (i == 1){
  write.table(paste0(vari_name), file= filename2, sep = ",", row.names = F, col.names=F, append=T)
  write.table(modelinfo, file=filename2, sep = ",", row.names = F, col.names=T, append=T)
  } else {
  write.table(paste0(vari_name), file=filename2, sep = ",", row.names = F, col.names=F, append=T)
  write.table(modelinfo, file=filename2, sep = ",", row.names = F, col.names=F, append=T)
  }
  
  # write out details about the model choosen
  if(modelchoice == 1){
    write_output(mdf, vari_name,filename2)
    write_output(coeff_m,vari_name,filename3)
    #coefficient details
    
    write.table(paste0(vari_name), file= filename3, sep = ",", row.names = F, col.names=F, append=T)
    write.table(coeff_m, file=filename3, sep = ",", row.names = F, col.names=F, append=T)
    g0
    if (save_plots){
      g0 
      name <- paste0("figures/",now,"roc",vari_name, ".png")
      dev.copy(png,name)
      dev.off()
    }  
    
    }
  if(modelchoice == 2){
    write.table(paste0(vari_name), file=filename2, sep = ",", row.names = F, col.names=F, append=T)
    write.table(m2df, file=filename2, sep = ",", row.names = F, col.names=F, append=T)
    
    write.table(paste0(vari_name), file=filename3, sep = ",", row.names = F, col.names=F, append=T)
    write.table(coeff_m2, file=filename3, sep = ",", row.names = F, col.names=F, append=T)
    
    if (save_plots){
      g02 
      name <- paste0("figures/",now,"roc",vari_name, ".png")
      dev.copy(png,name)
      dev.off()
    } 
     }
  if(modelchoice == 3){
    write.table(paste0(vari_name), file=filename2, sep = ",", row.names = F, col.names=F, append=T)
    write.table(m3df, file=filename2, sep = ",", row.names = F, col.names=F, append=T)
    
    write.table(paste0(vari_name), file=filename3, sep = ",", row.names = F, col.names=F, append=T)
    write.table(coeff_m3, file=filename3, sep = ",", row.names = F, col.names=F, append=T)
    
    if (save_plots){
      g03 
      name <- paste0("figures/",now,"roc",vari_name, ".png")
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

with(modeldata, tapply(age, outcome, mean))
with(modeldata, tapply(dutchscore, outcome, mean))
with(modeldata, tapply(LDLC, outcome, mean))
with(modeldata, tapply(nonhdl, outcome, mean))
with(modeldata, tapply(MoM, outcome, mean))
    
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

