# R function script to read in data and clean up by removing missing 
# or ineligible data

readingfiles <- function(dataset, SNPs){

#
# 4th Jan - n= 414 for full dataset, n= 258 for ahsn 
  if(SNPs){
    fhfull = read.csv("Raw_data/final_full_fhspreadsheet_w_snps_240417.csv", nrow = 414, stringsAsFactors=F)   
  } else {
    fhfull = read.csv("Raw_data/Update final_full_fhspreadsheet update 100417.csv", nrow = 414, stringsAsFactors=F)  
  }
  

  

total_study_full = fhfull 

total_study = total_study_full 
if (dataset == "ahsn"){
total_study = subset(total_study_full,total_study_full$Request. == "FH_original data set")  
}


nparticipants = nrow(total_study)
assign("nparticipants",nparticipants,envir = .GlobalEnv)
colnames(total_study)

## need to remove erronious 'ME' in SEX data

total_study$Sex[total_study$Sex == "ME"] <- ""
total_study$Sex[is.na(total_study$Sex)] <- ""


##### BE ABLE TO UNCOMMENT WHEN NEW PHENOTYPIC DATA #### ? 
#Date of births match?
# if (total_study$Dob[nparticipants] == total_study$p.Dob[nparticipants])  {
#   message ("yey - DOBs match so we can remove the extra column from the phenotype data")
#   total_study$p.Dob <- NULL
# } else {
#   message ("Boo - DOB in genotype and phenotype data do not match
#            so cannot remove this column until further investigation")
# }

#convert to correct classes
total_study$Sequenom.Reported <- as.Date((total_study$Sequenom.reported), "%d/%m/%Y")
#total_study$Dutch.score <- as.integer(total_study$Dutch.score)
total_study$Data.sent.to.NGS <-as.Date((total_study$Data.sent.to.NGS), "%d/%m/%Y")
total_study$MiSeq.reported.by.NGS <- as.Date((total_study$MiSeq.reported.by.NGS), "%d/%m/%Y")
#total_study$PHENOTYPE <- NULL
#total_study$Dutch.score.1 <- as.integer(total_study$Dutch.score.1)
#total_study$Family.No <-as.character(total_study$Family.No)
#total_study$p.Family.No <-as.character(total_study$p.Family.No)
#total_study$A_NoRels50.risk <- as.integer(total_study$A_NoRels50.risk)
#total_study$A_NoRels25.risk <- as.integer(total_study$A_NoRels25.risk)

# #Family number match?
#match(total_study$Family.No,total_study$Family.No.y)
#tmp<-rep(1, nrow(total_study))
#tmp[match(total_study$Family.No,total_study$Family.No.y)]<-0
#sum(tmp)
 
# total_study$test <- 0
# total_study$test[(total_study$Family.No != (total_study$Family.No.y))] <- 1  #NAs will be ignored
# sum(total_study$test) 
# test <- subset(total_study,total_study$Family.No != (total_study$Family.No.y))
# test2 <- cbind(test$Family.No, test$Family.No.y)


# #do the Dutch scores match?
#total_study$test <- 0
#total_study$test[(total_study$Dutch.score != (total_study$Dutch.score.1))] <- 1  #NAs will be ignored
#sum(total_study$test) 

#remove empty columns
total_study$X.1 <- NULL
total_study$X.2 <- NULL
total_study$X.3 <- NULL
total_study$X.4 <- NULL
total_study$X.5 <- NULL
total_study$X.6 <-NULL
total_study$test <- NULL



total_study$A_NoRels50.risk <- as.integer(total_study$A_NoRels50.risk) # replace unknowns with NA
total_study$A_NoRels25.risk <- as.integer(total_study$A_NoRels25.risk)
total_study$A_max <- as.integer(total_study$A_max)
total_study$B_max <- as.integer(total_study$B_max)
total_study$C_max <- as.integer(total_study$C_max)
total_study$D_max <- as.integer(total_study$D_max)

## data entry problems with A_max, B_max and C_max.  Need to re-compute these
total_study$A_max <- 0
total_study$A_max[total_study$A_Famhist_1 == "YES" | total_study$A_Famhist_2 == "YES"] <- 1
total_study$A_max[total_study$A_Famhist_3 == "YES" | total_study$A_Famhist_4 == "YES"] <- 2

total_study$B_max <- 0
total_study$B_max[total_study$B_Pershist_2 == "YES" ] <- 1
total_study$B_max[total_study$B_Pershist_1 == "YES"] <- 2

total_study$C_max <- 0
total_study$C_max[total_study$C_CornArcus == "YES" ] <- 4
total_study$C_max[total_study$C_TendXan == "YES"] <- 6

total_study$D_max <- 0
total_study$D_max[total_study$D_LDL_4 == "YES" ] <- 1
total_study$D_max[total_study$D_LDL_3 == "YES"] <- 3
total_study$D_max[total_study$D_LDL_2 == "YES" ] <- 5
total_study$D_max[total_study$D_LDL_1 == "YES"] <- 8

# 
# remove erroneous "6" value in TenXan column
colno <- which(sapply(total_study$C_TendXan, function(x) any(x == "6")))
total_study$C_TendXan[colno] = "YES" 


total_study$C_TendXan[total_study$C_TendXan == "0"| 
                          total_study$C_TendXan == "UNKNOWN" | total_study$C_TendXan == "UKNOWN"] <- ""

total_study$C_TendXan <- as.factor(total_study$C_TendXan)


total_study$B_Pershist_1[total_study$B_Pershist_1 == "0"| 
                        total_study$B_Pershist_1 == "UNKNOWN" | total_study$B_Pershist_1 == "UKNOWN"] <- ""

total_study$B_Pershist_1 <- as.factor(total_study$B_Pershist_1)
# 

total_study$B_Pershist_2[total_study$B_Pershist_2 == "0"| 
                           total_study$B_Pershist_2 == "UNKNOWN" | total_study$B_Pershist_2 == "UKNOWN"] <- ""

total_study$B_Pershist_2 <- as.factor(total_study$B_Pershist_2)

total_study$C_CornArcus[total_study$C_CornArcus == "0"| 
total_study$C_CornArcus == "UNKNOWN" | total_study$C_CornArcus == "UKNOWN"] <- ""
total_study$C_CornArcus <- as.factor(total_study$C_CornArcus)

# no dates of LDL have been added so remove this column
total_study$Date.of.LDL <- NULL
# 
# Clean LDL intervals column
total_study$D_LDL_1 <- as.factor(total_study$D_LDL_1)
total_study$D_LDL_1[total_study$D_LDL_1 == 8] <- "YES" 
total_study$D_LDL_1[total_study$D_LDL_1 == "UNKNOWN" |
                      total_study$D_LDL_1 == "UKNOWN" | total_study$D_LDL_1 == 0 ] <- "NO" 
total_study$D_LDL_1 <- factor(total_study$D_LDL_1)

total_study$D_LDL_2 <- as.factor(total_study$D_LDL_2)
total_study$D_LDL_2[total_study$D_LDL_2 == 5] <- "YES" 
total_study$D_LDL_2[total_study$D_LDL_2 == "UNKNOWN" |
                      total_study$D_LDL_2 == "UKNOWN" | total_study$D_LDL_2 == 0 ] <- "NO" 
total_study$D_LDL_2 <- factor(total_study$D_LDL_2)

total_study$D_LDL_3 <- as.factor(total_study$D_LDL_3)
total_study$D_LDL_3[total_study$D_LDL_3 == 3] <- "YES" 
total_study$D_LDL_3[total_study$D_LDL_3 == "UNKNOWN" | 
                      total_study$D_LDL_3 == "UKNOWN" | total_study$D_LDL_3 == "UNKOWN"
                    | total_study$D_LDL_3 == "U" | total_study$D_LDL_3 == 0] <- "NO" 
total_study$D_LDL_3 <- factor(total_study$D_LDL_3)

total_study$D_LDL_4 <- as.factor(total_study$D_LDL_4)
total_study$D_LDL_4[total_study$D_LDL_4 == "UNKNOWN" | 
                      total_study$D_LDL_4 == "UKNOWN" | total_study$D_LDL_4 == "UNKOWN"
                    | total_study$D_LDL_4 == "U" | total_study$D_LDL_4 == 0] <- "NO" 
total_study$D_LDL_4 <- factor(total_study$D_LDL_4)

total_study$LDL <- "Unknown"

total_study$LDL[total_study$D_LDL_1 == "YES"] <- "> 8.5"
total_study$LDL[total_study$D_LDL_2 == "YES"] <- "6.5 - 8.4"
total_study$LDL[total_study$D_LDL_3 == "YES"] <- "5.0 - 6.4"
total_study$LDL[total_study$D_LDL_4 == "YES"] <- "4.0 - 4.9"

total_study$LDL <- factor(total_study$LDL, levels= c("4.0 - 4.9", "5.0 - 6.4", "6.5 - 8.4", "> 8.5", "Unknown"))

total_study$LDLC <- as.numeric(total_study$LDLC)
total_study$HDLC <- as.numeric(total_study$HDLC)
total_study$TotalC <- as.numeric(total_study$TotalC)
total_study$Lipo <- as.numeric(total_study$Lipo)
total_study$Trigly <- as.numeric(total_study$Trigly)
# look to fasting lipid profile to help characterise LDL levels
# LDLC


  total_study$LDL[total_study$LDL == "Unknown" & total_study$LDLC == 0 ] <-  "Unknown"
  total_study$LDL[total_study$LDL == "Unknown" & total_study$LDLC >= 4.0 & total_study$LDLC <= 4.9 ] <- "4.0 - 4.9"
  total_study$LDL[total_study$LDL == "Unknown" & total_study$LDLC >= 5.0 & total_study$LDLC <= 6.4] <- "5.0 - 6.4"
  total_study$LDL[total_study$LDL == "Unknown" & total_study$LDLC >= 6.5 & total_study$LDLC <= 8.4 ] <- "6.5 - 8.4"
  total_study$LDL[total_study$LDL == "Unknown" & total_study$LDLC >= 8.5] <- "> 8.5"
   



#create a new column with intervals for Dutch Score.
total_study$DLCN <- 0
# 
total_study$DLCN <- "Unknown"
total_study$DLCN[total_study$DLCN == "NA" | total_study$DLCN == ""] <- "Unknown"
#total_study$DLCN[total_study$Dutch.score < 2] <- "< 2"
total_study$DLCN[total_study$Dutch.score >= 2 & total_study$Dutch.score <= 6] <- "2 - 6"
total_study$DLCN[total_study$Dutch.score >=7 & total_study$Dutch.score <= 9] <- "7 - 9"
total_study$DLCN[total_study$Dutch.score >= 10 & total_study$Dutch.score <= 12] <- "10 - 12"
total_study$DLCN[total_study$Dutch.score >= 13 & total_study$Dutch.score <= 16] <- "13 - 16"
#total_study$DLCN <-as.factor(total_study$DLCN) #, levels = c("", "< 2", "2 - 6", "7 - 9", "10 - 12", "13 - 16"))
total_study$DLCN <-factor(total_study$DLCN, levels = c( "2 - 6", "7 - 9", "10 - 12", "13 - 16", "Unknown"))



#remove erronenous "APOB" mutation in Sequenom Result column
colno <- which(sapply(total_study$Sequenom.Result, function(x) any(x == "APOB c.10580G>A p.(Arg3527Gln)")))
total_study$Sequenom.Result[colno] = "ABOB c.10580G>A p.(Arg3527Gln)"

# WHERE IS THE MISSING DATA WHICH ANN CAN HELP WITH?
total_study$Sequenom.Result <- as.factor(total_study$Sequenom.Result)
levels(total_study$Sequenom.Result)
missing_seq_result  = subset(total_study,total_study$Sequenom.Result == "NOT DONE")

total_study$MiSeq.Result[(total_study$MiSeq.Result == "nmd")] <- "NMD"
total_study$MiSeq.Result <- as.factor(total_study$MiSeq.Result)
levels(total_study$MiSeq.Result)



total_study$MiSeq.required <- as.factor(total_study$MiSeq.required)
levels(total_study$MiSeq.required)

missing_Miseq_result = subset(total_study, total_study$MiSeq.required != "NO" &
                              total_study$MiSeq.Result == "")

####### WRITE OUT CSV OF MISSING DATA ######
missing_data = rbind(missing_seq_result, missing_Miseq_result)
write.csv(missing_data, "output/missing_data.csv")
### ADDITIONAL COLUMN TO ALLOW FOR ALL POSSIBILITIES OF TESTING ####

total_study$Overall <- ""
total_study$Overall[(total_study$Sequenom.Result != "" & total_study$Sequenom.Result != "NMD" &
                       total_study$Sequenom.Result != "NOT DONE")] <- "Seq MD"
total_study$Overall[(total_study$Sequenom.Result == "NOT DONE" & total_study$MiSeq.Result == "NMD" )] <- "Seq MD"
total_study$Overall[total_study$Sequenom.Result == "NMD" & total_study$MiSeq.required == "NO"] <- "Seq NMD and no MiSeq"
total_study$Overall[total_study$Sequenom.Result == "NMD" & total_study$MiSeq.required != "NO"] <- "Seq NMD and MDT referred"
total_study$Overall[total_study$Sequenom.Result == "NMD" &
                      total_study$MiSeq.Result != "NMD" & total_study$MiSeq.Result != ""] <- "Seq NMD and MiSeq MD"
total_study$Overall[total_study$Sequenom.Result == "NMD" & total_study$MiSeq.Result == "NMD"] <- "Seq NMD and MiSeq NMD"
total_study$Overall[total_study$Sequenom.Result == "" & total_study$MiSeq.required == "done" & 
                       total_study$NGS.basis.of.referral.decision == "Ethnicity" & total_study$MiSeq.Result == "NMD" ] <- "No Seq and MiSeq NMD"
total_study$Overall[total_study$Sequenom.Result == "" & total_study$MiSeq.required == "done" & 
                       total_study$NGS.basis.of.referral.decision == "Ethnicity" & total_study$MiSeq.Result != "NMD" ] <- "No Seq and MiSeq MD"
total_study$Overall <- as.factor(total_study$Overall)
## arrange levels in order wanted graphically ##

total_study$Overall <- factor(total_study$Overall, levels = c("", "Seq MD", "Seq NMD and no MiSeq", "Seq NMD and MDT referred",
                                                              "Seq NMD and MiSeq MD", "Seq NMD and MiSeq NMD", "No Seq and MiSeq NMD","No Seq and MiSeq MD"))
missing_results = subset(total_study, total_study$Overall == "")

####### CALCULATE AGE ###########################
total_study$Dob <- as.Date((total_study$Dob), "%m/%d/%Y")
total_study <- total_study %>%  dplyr::rowwise() %>% dplyr::mutate(age = age_calc(Dob, enddate = Sys.Date(), units = "years"))

total_study$Comment...Further.info[(total_study$Comment...Further.info == "POS Miseq" | 
                                     total_study$Comment...Further.info == "POS MiSeQ"| 
                                      total_study$Comment...Further.info == "POS MiSEQ")] <- "POS MiSeq"
total_study$Comment...Further.info[(total_study$Comment...Further.info == "POS sequenom")] <- "POS Sequenom"
total_study$Comment...Further.info[(total_study$Comment...Further.info == "POS Sequenom & MiSeQ")] <- "POS Sequenom & MiSeq"


total_study$Comment...Further.info <- as.factor(total_study$Comment...Further.info)
levels(total_study$Comment...Further.info)


#ageldl <- ggplot(total_study, aes(x = age, y = LDLC, colour = ))
#plot(total_study$age, total_study$LDLC)


#calculate nonhdl level

total_study$nonhdl <- total_study$TotalC - total_study$HDLC
total_study$nonhdl[total_study$nonhdl < 0 ]<- NA
total_study$nonhdl <- as.numeric(total_study$nonhdl)

###### assign nonhdl centile ###############################
source("functions/functions.R")
cm <- read.csv("HSE_data/GAMLASS centiles/malecentiles.csv")
cw <- read.csv("HSE_data/GAMLASS centiles/femalecentiles.csv")
#as.data.frame(cm)
cm$X <- NULL
cw$X <- NULL

assign("cm",cm,envir = .GlobalEnv)
assign("cw", cw, envir=.GlobalEnv)

total_study <- total_study %>%  dplyr::rowwise() %>% dplyr::mutate(gamlass_centile = convert_glmcentiles(Sex, age, nonhdl))

total_study$gamlass_centile[total_study$gamlass_centile == "NA"] <- "Unknown" 
total_study$gamlass_centile[total_study$gamlass_centile == ""] <- "Unknown"
total_study$gamlass_centile <- as.factor(total_study$gamlass_centile)
total_study$gamlass_centile <- factor(total_study$gamlass_centile, 
                                      levels = c("Unknown",  "<75", "75-80", "80-90","90-95","95-97.5", "97.5-99", "99-99.5", ">99.5" ), exclude = NULL)
###########################################################

### binary response variable #############################
total_study$outcome <- ""
total_study$outcome[total_study$Overall == "Seq MD" | total_study$Overall =="Seq NMD and MiSeq MD"] <- "MD"
# keep extrat case in for demographics but they will be removed from analysis
total_study$outcome[total_study$Overall == "Seq NMD and MiSeq NMD" | total_study$Overall == "Seq NMD and no MiSeq"] <- "NMD"

################tidy data frame for analysis###############
if (SNPs){
  mydata = data.frame(total_study$age,total_study$Sex, 
                      total_study$Dutch.score.1,total_study$A_NoRels50.risk, total_study$A_NoRels25.risk,
                      total_study$A_max, total_study$B_max, total_study$C_max,total_study$D_max,
                      total_study$C_TendXan, total_study$C_CornArcus,
                      total_study$TotalC, total_study$Lipo, total_study$LDLC, total_study$nonhdl, total_study$Trigly,
                      total_study$LDL, total_study$DLCN, total_study$gamlass_centile,total_study$SNP.score, total_study$SNP.decile,
                      total_study$Overall, total_study$outcome)
  
  colnames(mydata) <- c("age", "Sex", "dutchscore", "NoRels50.risk", "NoRels25.risk", "A_max", "B_max", "C_max", "D_max",
                        "C_TendXan", "C_CornArcus", "TotalC", "Lipo", "LDLC", "nonhdl", "Trigly",
                        "LDL", "DLCN", "gamlass_centile", "SNPscore", "SNPdecile", "results", "outcome")
} else {
  mydata = data.frame(total_study$age,total_study$Sex, 
                      total_study$Dutch.score.1,total_study$A_NoRels50.risk, total_study$A_NoRels25.risk, total_study$A_max, total_study$B_max,
                      total_study$C_max,  total_study$D_max,
                      total_study$C_TendXan, total_study$C_CornArcus,
                      total_study$TotalC, total_study$Lipo, total_study$LDLC, total_study$nonhdl, total_study$Trigly,
                      total_study$LDL, total_study$DLCN,total_study$gamlass_centile, total_study$Overall, total_study$outcome)
  
  colnames(mydata) <- c("age", "Sex", "dutchscore", "NoRels50.risk", "NoRels25.risk", "A_max", "B_max", "C_max", "D_max",
                        "C_TendXan", "C_CornArcus", "TotalC", "Lipo", "LDLC", "nonhdl", "Trigly",
                        "LDL", "DLCN", "gamlass_centile", "results", "outcome")
}

rownames(mydata) <- NULL
mydata$LDL[mydata$LDL == "Unknown"] <- NA

#################################################################################
total_study_pos = subset(total_study, (total_study$Comment...Further.info != "NMD both" &
                                         total_study$Comment...Further.info != "NMD Sequenom" &
                                         total_study$Comment...Further.info != ""))
###################################
### for shiny app ##

#snp_data <- cbind.data.frame(total_study$Sex, total_study$age, total_study$nonhdl, total_study$SNP.score)
#colnames(snp_data) <- c("Sex", "age", "nonhdl", "SNPscore")
#write.csv(snp_data, "output/mypatientsdatawsnps.csv")


########################################################################
assign("total_study",total_study,envir = .GlobalEnv)
assign("total_study_pos", total_study_pos, envir=.GlobalEnv)
assign("mydata",mydata,envir = .GlobalEnv)

}
