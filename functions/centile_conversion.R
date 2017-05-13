# script to convert nonhdl data from FH study into centiles

convert2centiles <- function(){
  
  total_study$nonhdl <- total_study$TotalC - total_study$HDLC
  
 
  total_study$age_group <- 0
  total_study$age_group <- with(total_study, ifelse(age <= 17, 1, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 18 & age <= 29, 2, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 30 & age <= 44, 3, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 45 & age <= 59, 4, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 60 & age <= 74, 5, age_group))
  total_study$age_group <- with(total_study, ifelse(age >= 75, 6, age_group))
  
  cm <- read.csv("HSE_data/output/centilesm_old.csv")
  cw <- read.csv("HSE_data/output/centilesw_old.csv")
  # which centiles?
  
  View(cm)
  total_study$centile <-""
  for (i in 1:6)
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
  for (i in 1:6)
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
