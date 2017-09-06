# R script to load packages
loadpackages <- function()
  
#install the packages you require
  if (!require("Rcpp")){
  install.packages("Rcpp", repos="http://cran.rstudio.com/")
    library("Rcpp")
  }

if (!require("e1071")){
       install.packages("e1071", repos="http://cran.rstudio.com/")
       library("e1071")
}

if (!require("backports")){
  install.packages("backports", repos="http://cran.rstudio.com/")
  library("backports")
}

if (!require("tidyverse")){
  install.packages("tidyverse", repos="http://cran.rstudio.com/")
  library("tidyverse")
}

if (!require(plyr)){
    install.packages("plyr", repos="http://cran.rstudio.com/") 
    library("plyr")
}

if (!require(dplyr)){
  install.packages("dplyr", repos="http://cran.rstudio.com/") 
  library("dplyr")
}


if (!require(qdapTools)){
  install.packages("qdapTools", repos="http://cran.rstudio.com/") 
  library("qdapTools")
}


if (!require(R.huge)){
  install.packages("R.huge", repos="http://cran.rstudio.com/") 
  library("R.huge")
}

if (!require(scales)){
  install.packages("scales", repos="http://cran.rstudio.com/") 
  library("scales")
}

if (!require("gridExtra")) {
  install.packages("gridExtra", repos="http://cran.rstudio.com/") 
  library("gridExtra")
}

if (!require("ggplot2")) {
  install.packages("ggplot2", repos="http://cran.rstudio.com/", dependencies=c("Depends", "Imports", "Suggests")) 
  library("ggplot2")
}

if (!require("Amelia")) {
  install.packages("Amelia", repos="http://cran.rstudio.com/") 
  library("Amelia")
}

if (!require("VennDiagram")) {
  install.packages("VennDiagram", repos="http://cran.rstudio.com/") 
  library("VennDiagram")
}

if (!require("rmeta")) {
  install.packages("rmeta", repos="http://cran.rstudio.com/") 
  library("rmeta")
}

if (!require("stringr")) {
  install.packages("stringr", repos="http://cran.rstudio.com/") 
  library("stringr")
}

if (!require("xtable")) {
  install.packages("xtable", repos="http://cran.rstudio.com/") 
  library("xtable")
}

if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
  library("data.table")
}

if (!require("lme4")) {
  install.packages("lme4", repos = "http://cran.rstudio.com/", dependencies = TRUE)
  library("lme4") 
}

if (!require("eeptools")) {
  install.packages("eeptools", repos = "http://cran.rstudio.com/", dependencies = TRUE)
  library("eeptools") 
}

if(!require("foreign")) {
  install.packages("foreign", repos="http://cran.rstudio.com/")
   library("foreign")
}
if(!require("Hmisc")) {
  install.packages("Hmisc", repos="http://cran.rstudio.com/")
  library("Hmisc")
}

if (!require(gamlss)){
  install.packages("gamlss", repos="http://cran.rstudio.com/") 
  library("gamlss")
}

if (!require("AUC")){
  install.packages("AUC", repos="http://cran.rstudio.com/")
  library("AUC")
}

if (!require("pracma")){
  install.packages("pracma", repos="http://cran.rstudio.com/")
  library("pracma")
}

if (!require("zoo")){
  install.packages("zoo", repos="http://cran.rstudio.com/")
  library("zoo")
}

if (!require("fitdistrplus")){
  install.packages("fitdistrplus", repos="http://cran.rstudio.com/")
  library("fitdistrplus")
}

if (!require("broom")){
  install.packages("broom", repos="http://cran.rstudio.com/")
  library("broom")
}

if (!require("PerformanceAnalytics")) {
  install.packages("PerformanceAnalytics", repos="http://cran.rstudio.com/") 
  library("PerformanceAnalytics")
}

if (!require("drat")) {
  install.packages("drat", repos="http://cran.rstudio.com/") 
  library("drat")
  drat::addRepo("rcourses")
  install.packages("nclRpredictive")
  library("nclRpredictive")
}

if (!require("PredictABEL")){
  install.packages("PredictABEL", repos="http://cran.rstudio.com/")
  library("PredictABEL")
}

if (!require("pROC")){
  install.packages("pROC", repos="http://cran.rstudio.com/")
  library("pROC")
}



