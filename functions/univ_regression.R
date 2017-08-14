# function for performing and comparing univariate regressions

univ_regression <- function(vari, vari_name, mydata1){
  
  nrow(mydata1)
 
  #summary(m) # beta, Std.Error, z value, Pr(> |z|)
 
  
  
  if(class(vari) == "factor") {
    mydata1[,i] <- droplevels(mydata1[,i])
   # droplevels(mydata1[,i])
   levs <- nlevels(mydata1[,i])
  } else {
    levs <- 0
  }
  
  m <- glm(outcome~vari, family = "binomial", data = mydata1)
  
  
  
  if(levs == 0){
   coeff <- cbind(paste0(vari_name), tidy(m)[2,1], nrow(mydata1), round(coef(m)[2],4),paste0('(', round(confint(m)[2,1],4), ',', round(confint(m)[2,2],4),')'), 
                 paste0(round(exp(cbind(OR = coef(m),confint(m)))[2,1],4), 
                        ' (', round(exp(cbind(OR = coef(m),confint(m)))[2,2],4), ',', 
                        round(exp(cbind(OR = coef(m),confint(m)))[2,3],4), ')'), round(coef(summary(m))[2,4],15))#odds ratio and CI
  
  }  else {
    coeff <-ldply(1:levs, function(j)  cbind(paste0(vari_name), tidy(m)[j,1], paste0(nrow(mydata1), " total, ", count(mydata1[,i])[j,2], " for ", levels(mydata1[,i])[j] ," factor" ), 
                                             round(coef(m)[j],4),paste0('(', round(confint(m)[j,1],4), ',', round(confint(m)[j,2],4),')'), 
                                            paste0(round(exp(cbind(OR = coef(m),confint(m)))[j,1],4), 
                                                   ' (', round(exp(cbind(OR = coef(m),confint(m)))[j,2],4), ',', 
                                                   round(exp(cbind(OR = coef(m),confint(m)))[j,3],4), ')'), round(coef(summary(m))[j,4],15)))
    }


  # calculate pseudo R-squared - McFaddon
  
  nullmod <- glm(outcome~1, family="binomial", data = mydata1)
  #lrtest(nullmod, m)
  anova(m, test = "Chisq") # likelihood ratio test statistics is chi2 = 11.096 with a p value of 0.00087 therefore
  # we have relatively strong evidence in favour of rejecting Null hypo.  
  #drop1(m,test = "Chisq")
  1-logLik(m)/logLik(nullmod)
  
  aic=extractAIC(m)
  mydata1 <- mydata1 %>% mutate(prob = predict(m,type=c("response")))
  mydata1$prob=predict(m,type=c("response"))
 # predRisk <- predRisk(m)
  g <- roc(outcome~prob, data = mydata1, levels=c("NMD", "MD"), ci = TRUE)
  auroc=ci.auc(g)
  g0 <- plot.roc(outcome~ prob, data= mydata1, percent=T, ci=TRUE, levels=c("NMD", "MD"), main = (paste0(vari_name)), 
                 print.auc=TRUE ) #, print.auc.pattern=NULL,
                # print.auc.x=ifelse(x$percent, 50, .5),
               #  print.auc.y=ifelse(x$percent, 50, .5),
                # print.auc.adj=c(0,1),
                # print.auc.col=col,
                # print.auc.cex=par("cex"),)
  o1a=cbind(deviance(m),aic[2],auroc[2], auroc[1], auroc[3])
  MR=cbind(coef(m),confint(m), exp(cbind(OR = coef(m),confint(m))), coef(summary(m))[,4])#odds ratio and CI
  # m1rows = cbind(mdf, o1a)

  return(list(o1a = o1a, aic = aic, auroc = auroc, m = m, coeff = coeff, g0 = g0))
  
}