# script to convert nonhdl data from FH study into centiles

convert2centiles_hseintervals <- function(){
  
  total_study$nonhdl <- total_study$TotalC - total_study$HDLC
  
  total_study$age_group <- 0
  total_study$age_group <- with(total_study, ifelse(age <= 16, 1, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 16 & age <= 24, 2, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 25 & age <= 34, 3, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 35 & age <= 44, 4, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 45 & age <= 54, 5, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 55 & age <= 64, 6, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 65 & age <= 74, 7, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 75, 8, age_group))
  
  cm <- read.csv("HSE_data/output_rmlipids/centilesm.csv")
  cw <- read.csv("HSE_data/output_rmlipids/centilesw.csv")
  # which centiles?
  
  View(cm)
  total_study$centile <-""
  for (i in 1:8)
  {
  total_study$centile <- with(total_study, ifelse(Sex == "MALE"& 
                                          age_group == i &
                     nonhdl >= cm[i,9], 100, centile))
  total_study$centile <- with(total_study, ifelse(Sex == "MALE"& 
                          age_group == i & nonhdl >= cm[i,8]& nonhdl < cm[i,9], "99-100", centile))
  total_study$centile <- with(total_study, ifelse(Sex == "MALE"& 
                         age_group == i & nonhdl >= cm[i,7]& nonhdl < cm[i,8], "95-99", centile))
  total_study$centile <- with(total_study, ifelse(Sex == "MALE"& 
                         age_group == i & nonhdl >= cm[i,6]& nonhdl < cm[i,7], "90-95", centile))
  total_study$centile <- with(total_study, ifelse(Sex == "MALE"& 
                        age_group == i & nonhdl >= cm[i,5]& nonhdl < cm[i,6], "80-90", centile))
  total_study$centile <- with(total_study, ifelse(Sex == "MALE"& 
                         age_group == i & nonhdl >= cm[i,4]& nonhdl < cm[i,5], "75-80", centile))
  total_study$centile <- with(total_study, ifelse(Sex == "MALE"& 
                         age_group == i & nonhdl >= cm[i,3]& nonhdl < cm[i,4], "50-75", centile))
  total_study$centile <- with(total_study, ifelse(Sex == "MALE"& 
                         age_group == i & nonhdl >= cm[i,2]& nonhdl < cm[i,3], "25-50", centile))
  total_study$centile <- with(total_study, ifelse(Sex == "MALE"& 
                         age_group == i & nonhdl < cm[i,2] , "<25", centile))
  }
  
  #repeat for female
  for (i in 1:8)
  {
    total_study$centile <- with(total_study, ifelse(Sex == "FEMALE"& 
                                                      age_group == i &
                                                      nonhdl >= cm[i,9], 100, centile))
    total_study$centile <- with(total_study, ifelse(Sex == "FEMALE"& 
                                                      age_group == i & nonhdl >= cm[i,8]& nonhdl < cm[i,9], "99-100", centile))
    total_study$centile <- with(total_study, ifelse(Sex == "FEMALE"& 
                                                      age_group == i & nonhdl >= cm[i,7]& nonhdl < cm[i,8], "95-99", centile))
    total_study$centile <- with(total_study, ifelse(Sex == "FEMALE"& 
                                                      age_group == i & nonhdl >= cm[i,6]& nonhdl < cm[i,7], "90-95", centile))
    total_study$centile <- with(total_study, ifelse(Sex == "FEMALE"& 
                                                      age_group == i & nonhdl >= cm[i,5]& nonhdl < cm[i,6], "80-90", centile))
    total_study$centile <- with(total_study, ifelse(Sex == "FEMALE"& 
                                                      age_group == i & nonhdl >= cm[i,4]& nonhdl < cm[i,5], "75-80", centile))
    total_study$centile <- with(total_study, ifelse(Sex == "FEMALE"& 
                                                      age_group == i & nonhdl >= cm[i,3]& nonhdl < cm[i,4], "50-75", centile))
    total_study$centile <- with(total_study, ifelse(Sex == "FEMALE"& 
                                                      age_group == i & nonhdl >= cm[i,2]& nonhdl < cm[i,3], "25-50", centile))
    total_study$centile <- with(total_study, ifelse(Sex == "FEMALE"& 
                                                      age_group == i & nonhdl < cm[i,2] , "<25", centile))
  }
  

 as.data.frame(count(total_study, vars= "centile"))
      # number positive for each centiles
  total_study_pos = subset(total_study, (total_study$Sequenom.Result != "NOT DONE" &
                                           total_study$Sequenom.Result != "" & total_study$Sequenom.Result != "NMD") |
                             (total_study$MiSeq.Result != "NMD" & total_study$MiSeq.required == "done"))
  as.data.frame(count(total_study_pos, vars= "centile"))
  assign("total_study",total_study,envir = .GlobalEnv)
}


convert2gamlass_centiles <- function(){
 #use GAMLSS centiles 
 # total_study$nonhdl <- total_study$TotalC - total_study$HDLC
  median(total_study$nonhdl)
  total_study$age <- round(total_study$age,0)
  
  cm <- read.csv("HSE_data/GAMLASS centiles/malecentiles.csv")
  cw <- read.csv("HSE_data/GAMLASS centiles/femalecentiles.csv")
  as.data.frame(cm)
  cm$X <- NULL
  rownames(cm) <- cm$age - 15
  cw$X <- NULL
  rownames(cw) <- cw$age - 15
  
  total_study$ageindex <- total_study$age - 15
  total_study$ageindex[total_study$ageindex < 0] <- 0
  total_study$gamlass_centile <- ""
  
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "MALE" & ageindex >0 & nonhdl >= cm[ageindex,8], ">99.5", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "MALE"& ageindex >0 &
                                    nonhdl >= cm[ageindex,7]& nonhdl < cm[ageindex,8], "99-99.5", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "MALE"& ageindex >0 &
                                     nonhdl >= cm[ageindex ,6]& nonhdl < cm[ageindex,7], "97.5-99", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "MALE"& ageindex >0 &
                                     nonhdl >= cm[ageindex,5]& nonhdl < cm[ageindex,6], "95-97.5", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "MALE"& ageindex >0 &
                                    nonhdl >= cm[ageindex,4] & nonhdl < cm[ageindex,5], "90-95", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "MALE"& ageindex >0 &
                                    nonhdl >= cm[ageindex,3]& nonhdl < cm[ageindex,4], "80-90", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "MALE"& ageindex >0 &
                                    nonhdl >= cm[ageindex,2]& nonhdl < cm[ageindex,3], "75-80", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "MALE"& ageindex >0 &
                                                            nonhdl < cm[ageindex,2], "<75", gamlass_centile))
  
  
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "FEMALE"& ageindex >0 &  nonhdl >= cw[ageindex,8], ">99.5", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "FEMALE"& 
                                                            nonhdl >= cw[ageindex,7]& nonhdl < cw[ageindex,8], "99-99.5", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "FEMALE"& ageindex >0 & 
                                                            nonhdl >= cw[ageindex,6]& nonhdl < cw[ageindex,7], "97.5-99", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "FEMALE"& ageindex >0 & 
                                                            nonhdl >= cw[ageindex,5]& nonhdl < cw[ageindex,6], "95-97.5", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "FEMALE"& ageindex >0 & 
                                                            nonhdl >= cw[ageindex,4] & nonhdl < cw[ageindex,5], "90-95", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "FEMALE"& ageindex >0 & 
                                                            nonhdl >= cw[ageindex,3]& nonhdl < cw[ageindex,4], "80-90", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "FEMALE"& ageindex >0 & 
                                                            nonhdl >= cw[ageindex,2]& nonhdl < cw[ageindex,3], "75-80", gamlass_centile))
  total_study$gamlass_centile <- with(total_study, ifelse(Sex == "FEMALE"& ageindex >0 & 
                                                            nonhdl < cw[ageindex,2], "<75", gamlass_centile))
  as.data.frame(count(total_study, vars= "gamlass_centile"))
  # number positive for each centiles
  total_study$ageindex <- NULL
  
  total_study_pos = subset(total_study, (total_study$Sequenom.Result != "NOT DONE" &
                                           total_study$Sequenom.Result != "" & total_study$Sequenom.Result != "NMD") |
                             (total_study$MiSeq.Result != "NMD" & total_study$MiSeq.required == "done"))
  as.data.frame(count(total_study_pos, vars= "gamlass_centile"))
  assign("total_study",total_study,envir = .GlobalEnv)
}


# estimate continuous values of centiles

