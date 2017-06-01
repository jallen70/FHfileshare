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


hsedata$age <- as.integer(hsedata$age)
hsedata$totchol <- as.numeric(hsedata$totchol)
hsedata$hdlchol <- as.numeric(hsedata$hdlchol)
hsedata$lipid <- as.factor(hsedata$lipid)
hsedata$sex <- as.factor(hsedata$sex)

hsedata$sex[hsedata$sex == "Male"] <- "Men"
hsedata$sex[hsedata$sex == "Female"] <- "Women"

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

hse_men <- subset(hse_nonhdl,hse_nonhdl$sex == "Men")
hse_women <- subset(hse_nonhdl,hse_nonhdl$sex == "Women")
# 

##### calculate MoM nonhdl #####
median_nonhdlM <- median(hse_men$nonhdl, na.rm = TRUE)
median_nonhdlF <- median(hse_women$nonhdl, na.rm = TRUE)


 centilecurvem <- lms(totchol, age, data = hse_men)
 centilecurvew <- lms(totchol, age, data = hse_women)
# 
# mypath <- file.path(paste("GAMLASS centiles/centilecurves_sex", ".pdf", sep=""))
# pdf(mypath, paper = "a4r", width = 11, height = 8)
# grid.arrange(centilecurvem, centilecurvew, ncol=2)
# dev.off()
# 
# plot(centilecurvem$xvar, qnorm(.9, fitted(centilecurvem), centilecurvem$sigma.fv))
# plot(centilecurvem$xvar, qnorm(.99, fitted(centilecurvem), centilecurvem$sigma.fv))
# plot(centilecurvem$xvar, qnorm(.1, fitted(centilecurvem), centilecurvem$sigma.fv))
# plot(centilecurvem$xvar, qnorm(.99, fitted(centilecurvem), centilecurvem$sigma.fv))
# plot(centilecurvem$xvar, qnorm(.99, fitted(centilecurvem), centilecurvem$sigma.fv))


mcentilesdf <- data.frame(centilecurvem$xvar, 
           qnorm(.75, fitted(centilecurvem), centilecurvem$mu.lp), 
           qnorm(.80, fitted(centilecurvem), centilecurvem$mu.lp),
           qnorm(.90, fitted(centilecurvem), centilecurvem$mu.lp),
           qnorm(.95, fitted(centilecurvem), centilecurvem$mu.lp),
           qnorm(.975, fitted(centilecurvem), centilecurvem$mu.lp),
           qnorm(.99, fitted(centilecurvem), centilecurvem$mu.lp),
           qnorm(.995, fitted(centilecurvem), centilecurvem$mu.lp)
           )
colnames(mcentilesdf) <- c("age", "75%", "80%", "90%", "95%", "97.5%", "99%", "99.5%")
mcentilesdf <- mcentilesdf[order(mcentilesdf$age),] 
mcentilesdf <- mcentilesdf[!duplicated(mcentilesdf$age),]

wcentilesdf <- data.frame(centilecurvew$xvar, 
                          qnorm(.75, fitted(centilecurvew), centilecurvew$mu.lp), 
                          qnorm(.80, fitted(centilecurvew), centilecurvew$mu.lp),
                          qnorm(.90, fitted(centilecurvew), centilecurvew$mu.lp),
                          qnorm(.95, fitted(centilecurvew), centilecurvew$mu.lp),
                          qnorm(.975, fitted(centilecurvew), centilecurvew$mu.lp),
                          qnorm(.99, fitted(centilecurvew), centilecurvew$mu.lp),
                          qnorm(.995, fitted(centilecurvew), centilecurvew$mu.lp)
)
colnames(wcentilesdf) <- c("age", "75%", "80%", "90%", "95%", "97.5%", "99%", "99.5%")
wcentilesdf <- wcentilesdf[order(wcentilesdf$age),] 
wcentilesdf <- wcentilesdf[!duplicated(wcentilesdf$age),]

write.csv(mcentilesdf, "GAMLASS centiles/malecentiles_TC.csv")
write.csv(wcentilesdf, "GAMLASS centiles/femalecentiles_TC.csv")

# need to consolidate these 
