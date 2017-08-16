read_data <- function(read){

if (read){
  hse_2014 <- read.spss("Raw_data/hse2014ai.sav")
  hse_2014 <- as.data.frame(hse_2014)
  hse_2014$Age90 <- as.numeric(hse_2014$Age90)
  hse_2014$Sex <-as.character(hse_2014$Sex)
  hse_2014$Cholest <- as.numeric(hse_2014$Cholest)
  hse_2014$HDLChol <- as.numeric(hse_2014$HDLChol)
  hse_2014$lipid2 <- as.character(hse_2014$lipid2)
  
  
  hse_2013 <- read.spss("Raw_data/hse2013ai.sav")
  hse_2013 <- as.data.frame(hse_2013)
  hse_2013$Age <- as.numeric(hse_2013$Age)
  hse_2013$Sex <-as.character(hse_2013$Sex)
  hse_2013$Cholest <- as.numeric(hse_2013$Cholest)
  hse_2013$HDLChol <- as.numeric(hse_2013$HDLChol)
  hse_2013$lipid2 <- as.character(hse_2013$lipid2)
  
  
  hse_2012 <- read.spss("Raw_data/hse2012ai.sav")
  hse_2012 <- as.data.frame(hse_2012)
  hse_2012$Age <- as.numeric(hse_2012$Age)
  hse_2012$Sex <-as.character(hse_2012$Sex)
  hse_2012$Cholest <- as.numeric(hse_2012$Cholest)
  hse_2012$HDLChol <- as.numeric(hse_2012$HDLChol)
  hse_2012$lipid2 <- as.character(hse_2012$lipid2)
  
  hse_2008 <- read.spss("Raw_data/hse08ai.sav")
  hse_2008 <- as.data.frame(hse_2008)
  hse_2008$age <- as.numeric(hse_2008$age)
  hse_2008$sex <-as.character(hse_2008$sex)
  hse_2008$cholest <- as.numeric(hse_2008$cholest)
  hse_2008$hdlchol <- as.numeric(hse_2008$hdlchol)
  hse_2008$lipid <- as.character(hse_2008$lipid)
  
  hse_2006 <- read.spss("Raw_data/hse06ai.sav")
  hse_2006 <- as.data.frame(hse_2006)
  hse_2006$age <- as.numeric(hse_2006$age)
  hse_2006$sex <-as.character(hse_2006$sex)
  hse_2006$cholest <- as.numeric(hse_2006$cholest)
  hse_2006$hdlchol <- as.numeric(hse_2006$hdlchol)
  hse_2006$lipid <- as.character(hse_2006$lipid)
  
  hse_2005 <- read.spss("Raw_data/hse05ai.sav")
  hse_2005 <- as.data.frame(hse_2005)
  hse_2005$age <- as.numeric(hse_2005$age)
  hse_2005$sex <-as.character(hse_2005$sex)
  hse_2005$cholest <- as.numeric(hse_2005$cholest)
  hse_2005$hdlchol <- as.numeric(hse_2005$hdlchol)
  hse_2005$lipid <- as.character(hse_2005$lipid)
  
  hse_2003 <- read.csv("Raw_data/asq.csv", stringsAsFactors = F)
  
# create a smaller data.frame with the info required
  hse2014_chol <- cbind(hse_2014$Age90, hse_2014$Sex, hse_2014$lipid2, hse_2014$Cholest, hse_2014$HDLChol)
  hse2014_chol <- as.data.frame(hse2014_chol)
  colnames(hse2014_chol) <- c("age", "sex", "lipid", "totchol", "hdlchol")
  write.csv(hse2014_chol, "Raw_data/hse2014_chol.csv")
  
  hse2013_chol <- cbind(hse_2013$Age, hse_2013$Sex, hse_2013$lipid2, hse_2013$Cholest, hse_2013$HDLChol)
  hse2013_chol <- as.data.frame(hse2013_chol)
  colnames(hse2013_chol) <- c("age", "sex","lipid", "totchol", "hdlchol")
  
  write.csv(hse2013_chol, "Raw_data/hse2013_chol.csv")
  
  hse2012_chol <- cbind(hse_2012$Age, hse_2012$Sex, hse_2012$lipid2, hse_2012$Cholest, hse_2012$HDLChol)
  hse2012_chol <- as.data.frame(hse2012_chol)
  colnames(hse2012_chol) <- c("age", "sex","lipid", "totchol", "hdlchol")
  write.csv(hse2012_chol, "Raw_data/hse2012_chol.csv")
  
  hse2008_chol <- cbind(hse_2008$age, hse_2008$sex, hse_2008$lipid, hse_2008$cholest, hse_2008$hdlchol)
  hse2008_chol <- as.data.frame(hse2008_chol)
  colnames(hse2008_chol) <- c("age", "sex","lipid", "totchol", "hdlchol")
  write.csv(hse2008_chol, "Raw_data/hse2008_chol.csv")
  
  hse2006_chol <- cbind(hse_2006$age, hse_2006$sex, hse_2006$lipid, hse_2006$cholest, hse_2006$hdlchol)
  hse2006_chol <- as.data.frame(hse2006_chol)
  colnames(hse2006_chol) <- c("age", "sex", "lipid",  "totchol", "hdlchol")
  write.csv(hse2006_chol, "Raw_data/hse2006_chol.csv")
  
  hse2005_chol <- cbind(hse_2005$age, hse_2005$sex, hse_2005$lipid, hse_2005$cholest, hse_2005$hdlchol)
  hse2005_chol <- as.data.frame(hse2005_chol)
  colnames(hse2005_chol) <- c("age", "sex", "lipid","totchol", "hdlchol")
  write.csv(hse2005_chol, "Raw_data/hse2005_chol.csv")


  hse2003_chol <- cbind(hse_2003$age, hse_2003$sex, hse_2003$lipid, hse_2003$cholest, hse_2003$hdlchol)
  hse2003_chol <- as.data.frame(hse2003_chol)
  colnames(hse2003_chol) <- c("age", "sex", "lipid", "totchol", "hdlchol")
  write.csv(hse2003_chol, "Raw_data/hse2003_chol.csv")

# hse2003_trig <- cbind(hse_2003$age, hse_2003$sex, hse_2003$ftrigl)
# colnames(hse2003_trig) <- c("age", "sex", "ftrig")
# write.csv(hse2003_trig, "Raw_data/hse2003_trig.csv")

} else {  
  hse2014_chol <- read.csv("Raw_data/hse2014_chol.csv", stringsAsFactors = F)
  hse2013_chol <- read.csv("Raw_data/hse2013_chol.csv", stringsAsFactors = F)
  hse2012_chol <- read.csv("Raw_data/hse2012_chol.csv", stringsAsFactors = F)
  hse2008_chol <- read.csv("Raw_data/hse2008_chol.csv", stringsAsFactors = F)
  hse2006_chol <- read.csv("Raw_data/hse2006_chol.csv", stringsAsFactors = F)
  hse2005_chol <- read.csv("Raw_data/hse2005_chol.csv", stringsAsFactors = F)
  hse2003_chol <- read.csv("Raw_data/hse2003_chol.csv", stringsAsFactors = F)
  hse2003_trig <- read.csv("Raw_data/hse2003_trig.csv", stringsAsFactors = F)
}
  assign("hse2014_chol", hse2014_chol, envir = .GlobalEnv)
  assign("hse2013_chol", hse2013_chol, envir = .GlobalEnv)
  assign("hse2012_chol", hse2012_chol, envir = .GlobalEnv)
  assign("hse2008_chol", hse2008_chol, envir = .GlobalEnv)
  assign("hse2006_chol", hse2006_chol, envir = .GlobalEnv)
  assign("hse2005_chol", hse2005_chol, envir = .GlobalEnv)
  assign("hse2003_chol", hse2003_chol, envir = .GlobalEnv)
  # assign("hse2003_trig", hse2003_chol, envir = .GlobalEnv)
  
  }

