# function for performing and comparing univariate regressions

univ_regression <- function(vari, vari_name, mydata1){
  
  nrow(mydata1)
  m <- glm(outcome~vari, family = "binomial", data = mydata1, maxit= 100)

  #summary(m) # beta, Std.Error, z value, Pr(> |z|)
  droplevels(mydata1[,i])#, exclude = "Unknown")
  #factor(mydata1[,i])
  levs <- nlevels(subset(mydata1[,i], mydata1[,i] == "Unknown"))
  
  ## NEED TO MINUS 1 if a level == unknown
  #tidy(m)
  

  if(levs == 0){
   coeff <- cbind(paste0(vari_name), tidy(m)[2,1], nrow(mydata1), round(coef(m)[2],4),paste0('(', round(confint(m)[2,1],4), ',', round(confint(m)[2,2],4),')'), 
                 paste0(round(exp(cbind(OR = coef(m),confint(m)))[2,1],4), 
                        ' (', round(exp(cbind(OR = coef(m),confint(m)))[2,2],4), ',', 
                        round(exp(cbind(OR = coef(m),confint(m)))[2,3],4), ')'), round(coef(summary(m))[2,4],4))#odds ratio and CI
  
  }  else {
    coeff <-ldply(0:levs-2, function(j)  cbind(paste0(vari_name), tidy(m)[j+2,1], nrow(mydata1), round(coef(m)[j+2],4),paste0('(', round(confint(m)[j+2,1],4), ',', round(confint(m)[j+2,2],4),')'), 
                                            paste0(round(exp(cbind(OR = coef(m),confint(m)))[j+2,1],4), 
                                                   ' (', round(exp(cbind(OR = coef(m),confint(m)))[j+2,2],4), ',', 
                                                   round(exp(cbind(OR = coef(m),confint(m)))[j+2,3],4), ')'), round(coef(summary(m))[j+2,4],4)))
    }


  aic=extractAIC(m)
  mydata1 <- mydata1 %>% mutate(prob = predict(m,type=c("response")))
  mydata1$prob=predict(m,type=c("response"))
  predRisk <- predRisk(m)
  g <- roc(outcome~prob, data = mydata1, levels=c("NMD", "MD"), ci = TRUE)
  auroc=ci.auc(g)
  g0 <- plot.roc(outcome~ prob, data= mydata1, percent=T, ci=TRUE, levels=c("NMD", "MD"), main = (paste0(vari_name)))
  o1a=cbind(deviance(m),aic[2],auroc[2], auroc[1], auroc[3])
  MR=cbind(coef(m),confint(m), exp(cbind(OR = coef(m),confint(m))), coef(summary(m))[,4])#odds ratio and CI
  # m1rows = cbind(mdf, o1a)

  return(list(o1a = o1a, aic = aic, auroc = auroc, m = m, coeff = coeff, g0 = g0))
  
}