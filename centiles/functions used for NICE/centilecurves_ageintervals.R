# centile calculation for HEART UK conference.

#  start with 2003 data, use the age intervals given in Table 9.2
rm(list = ls())

#script to open the HSE data and calculated LDL centiles for use in 
# FH evaluation
# clear the global environment

#do we need to read in the files or have they been read in before?
read <- F
rm_lipids <- T

wd <- "Z:/DEC methods/tools - R/Working_project_folders/NCL_DEC0002 Familial hypercholesterolaemia/Current2016-2017/HSE_data"
#wd <- "~/Documents/DEC WORK/FH/Current2016-2017/HSE_data/"

setwd(wd)



#load in packages required
source("../loadpackages.R")
loadpackages()

source("functions/functions.R")

source("functions/read_data_rmlipid.R")
read_data(read)

  
# add factor in for year
hse2014_chol$year <- 2014
hse2013_chol$year <- 2013
hse2012_chol$year <- 2012
hse2008_chol$year <- 2008
hse2006_chol$year <- 2006
hse2005_chol$year <- 2005
hse2003_chol$year <- 2003

hse2012_chol$sex[hse2012_chol$sex == "Male"] <- "Men"
hse2012_chol$sex[hse2012_chol$sex == "Female"] <- "Women"

hse2013_chol$sex[hse2013_chol$sex == "1"] <- "Men"
hse2013_chol$sex[hse2013_chol$sex == "2"] <- "Women"

hse2014_chol$sex[hse2014_chol$sex == "Male"] <- "Men"
hse2014_chol$sex[hse2014_chol$sex == "Female"] <- "Women"

hse2003_chol$sex[hse2013_chol$sex == "1"] <- "Men"
hse2003_chol$sex[hse2013_chol$sex == "2"] <- "Women"

hsedata = rbind.data.frame(hse2003_chol, hse2006_chol,hse2008_chol,
                           hse2012_chol, hse2013_chol, hse2014_chol)

hsedata$sex[hsedata$sex == "Male"] <- "Men"
hsedata$sex[hsedata$sex == "Female"] <- "Women"

hsedata$age <- as.integer(hsedata$age)
hsedata$totchol <- as.numeric(hsedata$totchol)
hsedata$hdlchol <- as.numeric(hsedata$hdlchol)
hsedata$lipid <- as.factor(hsedata$lipid)
hsedata$sex <- as.factor(hsedata$sex)


############ CALCULATION OF NONHDL CHOLESTEROL #####################
if (rm_lipids){
hse_nonhdl = subset(hsedata, hsedata$lipid != "Taking drug" & hsedata$totchol != "NA" & hsedata$hdlchol != "NA"  &
                        hsedata$totchol > 0 & hsedata$hdlchol > 0 )
}else {
hse_nonhdl = subset(hsedata, hsedata$totchol != "NA" & hsedata$hdlchol != "NA"  &
                      hsedata$totchol > 0 & hsedata$hdlchol > 0 )
}

# cholesterol samples analysed between 12th April 2010 and 16th June 2016 
# were tested on a 

tot_num = nrow(hse_nonhdl)

hse_nonhdl$nonhdl <- 0

hse_nonhdl$nonhdl <- hse_nonhdl$totchol - hse_nonhdl$hdlchol 



# new age groups for TC cutoffs for NICE, group men and women
  hse_nonhdl$age_group1 <- ""

  hse_nonhdl$age_group1 <- ifelse(hse_nonhdl$age <= 16, "<16",hse_nonhdl$age_group1)
  hse_nonhdl$age_group1 <- ifelse(hse_nonhdl$age >= 16 & hse_nonhdl$age <= 24, "16-24", hse_nonhdl$age_group1)
  hse_nonhdl$age_group1 <- ifelse(hse_nonhdl$age >= 25, ">25", hse_nonhdl$age_group1)

  hse_nonhdl$age_group1 <- factor(hse_nonhdl$age_group1, levels = c("<16", "16-24", ">25"), order = TRUE)
  
  name <- paste0("For NICE/TCdist1.png")
  dev.copy(png,name)
  
  TCdist1 <- ggplot(hse_nonhdl, aes(x = totchol, y = ..density.., colour = age_group1, fill = age_group1)) 
  TCdist1 <- TCdist1 + geom_density(alpha = 0.1) + xlim(0,14) + xlab("Total chol (mmol/L)") +     ylab("Population density") +
    ggtitle("Total Cholesterol distributions from the HSE data") +
    theme(axis.text=element_text(size=16),    axis.title=element_text(size=16,face="bold")) 
  TCdist1
  print(TCdist1)
  dev.off()
  
  TCcentiles1 <- ddply(hse_nonhdl, .(age_group1), function(hse_nonhdl.sub) quantile(hse_nonhdl.sub$totchol, c(.75,.80,.90, .95, .975, .99, .9925, .995, .9975)))

  n1 <- count(hse_nonhdl$age_group1)
  TCcentiles1<- cbind(TCcentiles1, n1$freq)
  colnames(TCcentiles1)[11] <- "n"

  # new age groups for TC cutoffs for NICE, group men and women
  hse_nonhdl$age_group2 <- ""
  
  hse_nonhdl$age_group2 <- ifelse(hse_nonhdl$age <= 16, "<16",hse_nonhdl$age_group2)
  hse_nonhdl$age_group2 <- ifelse(hse_nonhdl$age >= 16 & hse_nonhdl$age <= 29, "16-29", hse_nonhdl$age_group2)
  hse_nonhdl$age_group2 <- ifelse(hse_nonhdl$age >= 30, ">30", hse_nonhdl$age_group2)
  
  hse_nonhdl$age_group2 <- factor(hse_nonhdl$age_group2, levels = c("<16", "16-29", ">30"), order = TRUE)
  
  name <- paste0("For NICE/TCdist2.png")
  dev.copy(png,name)
  
  TCdist2 <- ggplot(hse_nonhdl, aes(x = totchol, y = ..density.., colour = age_group2, fill = age_group2)) 
  TCdist2 <- TCdist2 + geom_density(alpha = 0.1) + xlim(0,14) + xlab("Total chol (mmol/L)") +     ylab("Population density") +
    ggtitle("Total Cholesterol distributions from the HSE data") +
    theme(axis.text=element_text(size=16),    axis.title=element_text(size=16,face="bold")) 
  TCdist2
  
  print(TCdist2)
  dev.off()
  
  
  TCcentiles2 <- ddply(hse_nonhdl, .(age_group2), function(hse_nonhdl.sub) quantile(hse_nonhdl.sub$totchol, c(.75,.80,.90, .95, .975, .99, .9925, .995, .9975)))
  
  n2 <- count(hse_nonhdl$age_group2)
  TCcentiles2<- cbind(TCcentiles2, n2$freq)
  colnames(TCcentiles2)[11] <- "n"
  
  
  # new age groups for TC cutoffs for NICE, group men and women
  hse_nonhdl$age_group3 <- ""
  
  hse_nonhdl$age_group3 <- ifelse(hse_nonhdl$age <= 16, "<16",hse_nonhdl$age_group3)
  hse_nonhdl$age_group3 <- ifelse(hse_nonhdl$age >= 16 & hse_nonhdl$age <= 24, "16-24", hse_nonhdl$age_group3)
  hse_nonhdl$age_group3 <- ifelse(hse_nonhdl$age >= 25 & hse_nonhdl$age <= 34, "25-34", hse_nonhdl$age_group3)
  hse_nonhdl$age_group3 <- ifelse(hse_nonhdl$age >= 35, ">35", hse_nonhdl$age_group3)
  
  hse_nonhdl$age_group3 <- factor(hse_nonhdl$age_group3, levels = c("<16", "16-24", "25-34", ">35"), order = TRUE)
  
  name <- paste0("For NICE/TCdist3.png")
  dev.copy(png,name)
  
  TCdist3 <- ggplot(hse_nonhdl, aes(x = totchol, y = ..density.., colour = age_group3, fill = age_group3)) 
  TCdist3 <- TCdist3 + geom_density(alpha = 0.1) + xlim(0,14) + xlab("Total chol (mmol/L)") +     ylab("Population density") +
    ggtitle("Total Cholesterol distributions from the HSE data") +
    theme(axis.text=element_text(size=16),    axis.title=element_text(size=16,face="bold")) 
  TCdist3
  
  print(TCdist3)
  dev.off()
  
  TCcentiles3 <- ddply(hse_nonhdl, .(age_group3), function(hse_nonhdl.sub) quantile(hse_nonhdl.sub$totchol, c(.75,.80,.90, .95, .975, .99, .9925, .995, .9975)))
  
  n3 <- count(hse_nonhdl$age_group3)
  TCcentiles3<- cbind(TCcentiles3, n3$freq)
  colnames(TCcentiles3)[11] <- "n"
  
  
  
  
  write.csv(TCcentiles1, "For NICE/TCcentiles_agegroup1.csv")
  write.csv(TCcentiles2, "For NICE/TCcentiles_agegroup2.csv")
  write.csv(TCcentiles3, "For NICE/TCcentiles_agegroup3.csv")
  
  #update on 16th August - redo age group 3 split between men and women
  # and create an extra age grouping sytem.
  
  hse_nonhdl$age_group4 <- ""
  
  hse_nonhdl$age_group4 <- ifelse(hse_nonhdl$age <= 34, "<34",hse_nonhdl$age_group4)
  hse_nonhdl$age_group4 <- ifelse(hse_nonhdl$age >= 35 & hse_nonhdl$age <= 54, "35-54", hse_nonhdl$age_group4)
  hse_nonhdl$age_group4 <- ifelse(hse_nonhdl$age >= 55, ">55", hse_nonhdl$age_group4)
 
  
  nrow(hse_nonhdl)
  
  hse_men <- subset(hse_nonhdl,hse_nonhdl$sex == "Men")
  hse_women <- subset(hse_nonhdl,hse_nonhdl$sex == "Women")
  
  nrow(hse_men)
  nrow(hse_women)
  
  TCcentiles3_m <- ddply(hse_men, .(age_group3), function(hse_nonhdl.sub) quantile(hse_nonhdl.sub$totchol, c(.75,.80,.90, .95, .975, .99, .9925, .995, .9975)))
  TCcentiles3_w <- ddply(hse_women, .(age_group3), function(hse_nonhdl.sub) quantile(hse_nonhdl.sub$totchol, c(.75,.80,.90, .95, .975, .99, .9925, .995, .9975)))
  
  n3m <- count(hse_men$age_group3)
  TCcentiles3_m<- cbind(TCcentiles3_m, n3m$freq)
  colnames(TCcentiles3_m)[11] <- "n"

  n3w <- count(hse_women$age_group3)
  TCcentiles3_w<- cbind(TCcentiles3_w, n3w$freq)
  colnames(TCcentiles3_w)[11] <- "n"
  
  write.csv(TCcentiles3_m, "For NICE/TCcentiles_agegroup3m.csv")
  write.csv(TCcentiles3_w, "For NICE/TCcentiles_agegroup3w.csv")
  
  # group 4 split by sex
  
  TCcentiles4_m <- ddply(hse_men, .(age_group4), function(hse_nonhdl.sub) quantile(hse_nonhdl.sub$totchol, c(.75,.80,.90, .95, .975, .99, .9925, .995, .9975)))
  TCcentiles4_w <- ddply(hse_women, .(age_group4), function(hse_nonhdl.sub) quantile(hse_nonhdl.sub$totchol, c(.75,.80,.90, .95, .975, .99, .9925, .995, .9975)))
  
  n4m <- count(hse_men$age_group4)
  TCcentiles4_m<- cbind(TCcentiles4_m, n4m$freq)
  colnames(TCcentiles4_m)[11] <- "n"
  
  n4w <- count(hse_women$age_group4)
  TCcentiles4_w<- cbind(TCcentiles4_w, n4w$freq)
  colnames(TCcentiles4_w)[11] <- "n"
  
  write.csv(TCcentiles4_m, "For NICE/TCcentiles_agegroup4m.csv")
  write.csv(TCcentiles4_w, "For NICE/TCcentiles_agegroup4w.csv")
  
  
  # not split by sex
  
  TCcentiles4 <- ddply(hse_nonhdl, .(age_group4), function(hse_nonhdl.sub) quantile(hse_nonhdl.sub$totchol, c(.75,.80,.90, .95, .975, .99, .9925, .995, .9975)))
  
  n4 <- count(hse_nonhdl$age_group4)
  TCcentiles4 <- cbind(TCcentiles4, n4$freq)
  colnames(TCcentiles4)[11] <- "n"
  
  write.csv(TCcentiles4, "For NICE/TCcentiles_agegroup4.csv")
