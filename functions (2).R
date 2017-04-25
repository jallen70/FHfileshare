#Useful functions

nmissing <- function(x) sum(x)

SnSp <- function(x,y) x/(y+x)

LRp <- function(x,y) x/ (1 - y) # Likelihood ratio of a positive test Sensitivity / (1 - Specificity) # Likelihood ratio of a positive test
LRn <- function(x,y) (1 - x) / y  # Likelihood ratio of a negative test, FNR / TNR, (1 - Sensitivity) / Specificity 
#DOR <- LRplus / LRneg


#confidence intervals
SnSpCIs <- function(alpha, x, y) {
  # x is either Sn or Sp
  # y is either withCondition or WithoutCondition
  #zSensitivity = qnorm(1-alpha/2)*sqrt((1-Sensitivity)*Sensitivity/WithCondition)
  #SensitivityCI <- c(Sensitivity - zSensitivity, Sensitivity + zSensitivity)
  zSnSp = qnorm(1-alpha/2)*sqrt((1-x)*x/y)
  SnSpCI = c(x - zSnSp, x + zSnSp)
return(list(lower = x - zSnSp, upper = x+ zSnSp))
}

LRCIs <- function(alpha,lr,w,w2,x,y){
 #lr
 #w = WithCondition
 #w2 = WithoutCondition
 #x = tps for LR+, fns for LR-
 #y = fps for LR+, tns for LR-
  zLR = qnorm(1-alpha/2)*sqrt(1/x - 1/w + 1/y - 1/w2)
  LRCI <- c( exp(log(lr) - zLR), exp(log(lr) + zLR))
  return(list(lower = exp(log(lr) - zLR), upper = exp(log(lr) + zLR)))
}





#function to create 2x2 table
Dx2by2fun <- function(){
  
Dx2by2 <- data.frame(c(Tp, Fn, Tp+Fn), c(Fp, Tn, Fp+Tn), c(Tp+Fp, Tn+Fn, Tp+Fp+Tn+Fn))
colnames(Dx2by2) <- c("Lab RT-PCR +ve", "  -ve", "Totals") # can also use names()
rownames(Dx2by2) <- c("Alere NPT +ve", "Alere NPT -ve", "Totals") # can also use row.names()

return(Dx2by2)
}

#function to create data frame and then x table for reading into LaTex.  Used for
# tables which count up a summarise data items.  
totalscol_table <-function(dataframe){
  row.names(dataframe) = dataframe[,1]
  dataframe$Location <- NULL
  
  #Summary data frame for creating xtables in latex document
  dataframe = data.frame(dataframe)
  Totals = colSums(Filter(is.integer,dataframe)) 
  dataframe = rbind(dataframe, Totals)
  rownames(dataframe)[6]<- "Total"  # works here as there are 4 locations
  return(dataframe)

}

col_table <-function(dataframe){
  row.names(dataframe) = dataframe[,1]
  dataframe$Location <- NULL
  
  #Summary data frame for creating xtables in latex document
  dataframe = data.frame(dataframe)
  Totals = colSums(dataframe, na.rm  = T)#colSums(Filter(is.real,dataframe)) 
  dataframe = rbind(dataframe, Totals)
  rownames(dataframe)[5]<- "Total"  # works here as there are 4 locations
  return(dataframe)
  
}


convert_glmcentiles <- function(x, y, z)  {
  x1 <- ""
  if(x != ""){
    if (x == "MALE") {
      cen <- cm
    } else {
      cen <- cw
    }
    if(y  > 16 & y < max(cen$age)) {
      #ageindex <- y - 15
      if(!is.na(z)){
        if(z >=cen[y - 15,8])   x1 <- ">99.5"
        if(z >=cen[y - 15,7] & z < cen[y - 15,8]) x1 <- "99-99.5"
        if(z >=cen[y - 15,6] & z < cen[y - 15,7]) x1 <- "97.5-99"
        if(z >=cen[y - 15,5] & z < cen[y - 15,6]) x1 <- "95-97.5"
        if(z >=cen[y - 15,4] & z < cen[y - 15,5]) x1 <- "90-95"
        if(z >=cen[y - 15,3] & z < cen[y - 15,4]) x1 <- "80-90"
        if(z >=cen[y - 15,2] & z < cen[y - 15,3]) x1 <- "75-80"
        if(z  < cen[y - 15,2]) x1 <- "<75"
      } 
    } 
  } 
  return(x1)
} 


#  function to write table of models and coeffs in 

write_output <- function(x, vari_name, filename){
  write.table(paste0(vari_name), file=filename, sep = ",", row.names = F, col.names=F, append=T)
  write.table(x, file=filename, sep = ",", row.names = F, col.names=F, append=T)
}




