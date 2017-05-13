# R function script to read in data and clean up by removing missing 
# or ineligible data

readingfiles <- function(){

#
  
fhfull = read.csv("Raw_data/finalfhspreadsheet_12thAug2016.csv", nrow = 258, stringsAsFactors=F)  
total_study_full = fhfull  
  
nparticipants = nrow(total_study_full)
assign("nparticipants",nparticipants,envir = .GlobalEnv)
colnames(total_study_full)

# check phenotype data match genotype data and then remove extra columns
total_study = total_study_full 

#Date of births match?
if (total_study$Dob[nparticipants] == total_study$p.Dob[nparticipants])  {
  message ("yey - DOBs match so we can remove the extra column from the phenotype data")
  total_study$p.Dob <- NULL
} else {
  message ("Boo - DOB in genotype and phenotype data do not match 
           so cannot remove this column until further investigation")
}  

#convert to correct classes
total_study$Sequenom.Reported <- as.Date((total_study$Sequenom.Result), "%d/%m/%Y")
total_study$Dutch.score <- as.integer(total_study$Dutch.score)
total_study$Data.sent.to.NGS <-as.Date((total_study$Data.sent.to.NGS), "%d/%m/%Y")
total_study$MiSeq.reported.by.NGS <- as.Date((total_study$MiSeq.reported.by.NGS), "%d/%m/%Y")
#total_study$PHENOTYPE <- NULL
total_study$Dutch.score.1 <- as.integer(total_study$Dutch.score.1)
total_study$Family.No <-as.character(total_study$Family.No)
total_study$p.Family.No <-as.character(total_study$p.Family.No)
total_study$A_NoRels50.risk <- as.integer(total_study$A_NoRels50.risk)
total_study$A_NoRels25.risk <- as.integer(total_study$A_NoRels25.risk)

#Family number match?
if (total_study$Family.No[nparticipants] == total_study$p.Family.No[nparticipants])  {
  message ("yey - Family numbers match so we can remove the extra column from the phenotype data")
  total_study$p.Family.No <- NULL
} else {
  message ("Boo - Family numbers in genotype and phenotype data do not match 
           so cannot remove this column until further investigation")
}


#do the Dutch scores match?
if(total_study$Dutch.score[nparticipants] == total_study$Dutch.score.1[nparticipants] ) {
  message ("yey - Dutch scores match so we can remove the extra column from the phenotype data") 
  total_study$Dutch.score.1 <- NULL
} else {
  message ("Boo - Dutch scores in genotype and phenotype data do not match 
           so cannot remove this column until further investigation")
}

#remove empty columns
total_study$a1 <- NULL
total_study$a1.1 <- NULL
total_study$a3 <- NULL
total_study$a4 <- NULL
total_study$a5 <- NULL
total_study$a5.1 <-NULL
total_study$Famhist_4 <-NULL

total_study$A_NoRels50.risk <- as.integer(total_study$A_NoRels50.risk) 
total_study$A_NoRels25.risk <- as.integer(total_study$A_NoRels25.risk)
total_study$A_max <- as.integer(total_study$A_max)
total_study$A_max <- as.integer(total_study$B_max)
total_study$A_max <- as.integer(total_study$C_max)
total_study$A_max <- as.integer(total_study$D_max)
# 
# remove erroneous "6" value in TenXan column
colno <- which(sapply(total_study$C_TendXan, function(x) any(x == "6")))
total_study$C_TendXan[colno] = "YES" 
total_study$C_TendXan <- as.factor(total_study$C_TendXan)
# 
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

# look to fasting lipid profile to help characterise LDL levels
# LDLC

if(total_study$LDL == "Unknown"){
  total_study$LDL[total_study$LDLC == 0 ] <- "Unknown"
  total_study$LDL[total_study$LDLC >= 4.0 & total_study$LDLC <= 4.9 ] <- "4.0 - 4.9"
  total_study$LDL[total_study$LDLC >= 5.0 & total_study$LDLC <= 6.4] <- "5.0 - 6.4"
    total_study$LDL[total_study$LDLC >= 6.5 & total_study$LDLC <= 8.4 ] <- "6.5 - 8.4"
  total_study$LDL[total_study$LDLC >= 8.5] <- "> 8.5"
  }
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



# #total_study$TOTAL..DUTCH.SCORE. <- as.integer(total_study$TOTAL..DUTCH.SCORE)
# 
# #Ducth score check 
# #A_max + B_max + C_max + D_max
total_study$HDLC <- as.numeric(total_study$HDLC)
total_study$TotalC <- as.numeric(total_study$TotalC)
total_study$Lipo <- as.numeric(total_study$Lipo)
total_study$Trigly <- as.numeric(total_study$Trigly)
total_study$LDLC <- as.numeric(total_study$LDLC)

#remove erronenous "APOB" mutation in Sequenom Result column
colno <- which(sapply(total_study$Sequenom.Result, function(x) any(x == "APOB c.10580G>A p.(Arg3527Gln)")))
total_study$Sequenom.Result[colno] = "ABOB c.10580G>A p.(Arg3527Gln)"

# WHERE IS THE MISSING DATA WHICH ANN CAN HELP WITH?
total_study$Sequenom.Result <- as.factor(total_study$Sequenom.Result)
levels(total_study$Sequenom.Result)
missing_seq_result  = subset(total_study,total_study$Sequenom.Result == "NOT DONE")


total_study$MiSeq.Result <- as.factor(total_study$MiSeq.Result)
levels(total_study$MiSeq.Result)

total_study$MiSeq.Result[(total_study$MiSeq.Result == "nmd")] <- "NMD"

total_study$MiSeq.required <- as.factor(total_study$MiSeq.required)
levels(total_study$MiSeq.required)

missing_Miseq_result = subset(total_study, total_study$MiSeq.required != "N" &
                              total_study$MiSeq.Result == "")

####### WRITE OUT CSV OF MISSING DATA ######
missing_data = rbind(missing_seq_result, missing_Miseq_result)
write.csv(missing_data, "output/missing_data.csv")
### ADDITIONAL COLUMN TO ALLOW FOR ALL POSSIBILITIES OF TESTING ####

total_study$Overall <- ""
total_study$Overall[(total_study$Sequenom.Result != "" & total_study$Sequenom.Result != "NMD" &
                       total_study$Sequenom.Result != "NOT DONE")] <- "Seq MD"
total_study$Overall[total_study$Sequenom.Result == "NMD" & total_study$MiSeq.required == "N"] <- "Seq NMD and no MiSeq"
total_study$Overall[total_study$Sequenom.Result == "NMD" & total_study$MiSeq.required != "N"] <- "Seq NMD and MDT referred"
total_study$Overall[total_study$Sequenom.Result == "NMD" &
                      total_study$MiSeq.Result != "NMD" & total_study$MiSeq.Result != ""] <- "Seq NMD and MiSeq MD"
total_study$Overall[ total_study$Sequenom.Result == "NMD" & total_study$MiSeq.Result == "NMD"] <- "Seq NMD and MiSeq NMD"
total_study$Overall <- as.factor(total_study$Overall)
## arrange levels in order wanted graphically ##

total_study$Overall <- factor(total_study$Overall, levels = c("", "Seq MD", "Seq NMD and no MiSeq", "Seq NMD and MDT referred",
                                                              "Seq NMD and MiSeq MD", "Seq NMD and MiSeq NMD"))


####### CALCULATE AGE ###########################
total_study$Dob <- as.Date((total_study$Dob), "%m/%d/%Y")
total_study$age <- age_calc(total_study$Dob, enddate = Sys.Date(), units = "years")

total_study_pos = subset(total_study, (total_study$Sequenom.Result != "NOT DONE" &
                           total_study$Sequenom.Result != "" & total_study$Sequenom.Result != "NMD") |
                           (total_study$MiSeq.Result != "NMD" & total_study$MiSeq.required == "done"))

assign("total_study_pos", total_study_pos, envir=.GlobalEnv)
nrow(total_study_pos)
#ageldl <- ggplot(total_study, aes(x = age, y = LDLC, colour = ))
#plot(total_study$age, total_study$LDLC)

########################################################################
assign("total_study",total_study,envir = .GlobalEnv)

}