# centile calculation for HEART UK conference.

#  start with 2003 data, use the age intervals given in Table 9.2
rm(list = ls())

#script to open the HSE data and calculated LDL centiles for use in 
# FH evaluation
# clear the global environment

#do we need to read in the files or have they been read in before?
read <- F
rm_lipids <- T

#wd <- "Z:/DEC methods/tools - R/Working_project_folders/NCL_DEC0002 Familial hypercholesterolaemia/AnalysisAug2016/HSE_data"
wd <- "~/Documents/DEC WORK/FH/Current2016-2017/HSE_data/"

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

hse_nonhdl$age_group <- ""

hse_nonhdl$age_group <- ifelse(hse_nonhdl$age <= 16, "<16",hse_nonhdl$age_group)
hse_nonhdl$age_group <- ifelse(hse_nonhdl$age >= 16 & hse_nonhdl$age <= 24, "16-24", hse_nonhdl$age_group)
hse_nonhdl$age_group <- ifelse(hse_nonhdl$age >= 25 & hse_nonhdl$age <= 34, "25-34", hse_nonhdl$age_group)
hse_nonhdl$age_group <- ifelse(hse_nonhdl$age >= 35 & hse_nonhdl$age <= 44, "35-44", hse_nonhdl$age_group)
hse_nonhdl$age_group <- ifelse(hse_nonhdl$age >= 45 & hse_nonhdl$age <= 54, "45-54", hse_nonhdl$age_group)
hse_nonhdl$age_group <- ifelse(hse_nonhdl$age >= 55 & hse_nonhdl$age <= 64, "55-64", hse_nonhdl$age_group)
hse_nonhdl$age_group <- ifelse(hse_nonhdl$age >= 65 & hse_nonhdl$age <= 74, "65-74", hse_nonhdl$age_group)
hse_nonhdl$age_group <- ifelse(hse_nonhdl$age >= 75, ">75", hse_nonhdl$age_group)

hse_nonhdl$age_group <- factor(hse_nonhdl$age_group, levels = c("<16", "16-24", "25-34", "35-44", 
                                                          "45-54",  "55-64", "65-74", ">75" ), order = TRUE)



hse_men <- subset(hse_nonhdl,hse_nonhdl$sex == "Men")
hse_women <- subset(hse_nonhdl,hse_nonhdl$sex == "Women")
# 

#tables for paper
count_men <-count(hse_men$age)
count_women <- count(hse_women$age)

freq <- merge(count_men, count_women, by = "x", all = T)

freq[is.na(freq)] <- 0
row.names(freq) <- NULL
colnames(freq) <-c("age", "male", "female")

write.csv(freq, "output/age_freq_forcentiles.csv")

count_men2 <-count(hse_men$age_group)
count_women2 <- count(hse_women$age_group)

freq2 <- merge(count_men2, count_women2, by = "x", all = T)

row.names(freq2) <- NULL
colnames(freq2) <-c("age interval", "male", "female")

write.csv(freq2, "output/ageinterval_freq_forcentiles.csv")

# plots of distributions

TCdist_men <- ggplot(hse_men, aes(x = totchol, y = ..density.., colour = age_group, fill = age_group)) 
TCdist_men <- TCdist_men + geom_density(alpha = 0.1) + xlim(0,14) + xlab("Total chol (mmol/L)") +     ylab("Population density") +
              ggtitle("Total Cholesterol distributions from the HSE data (men)") +
              theme(axis.text=element_text(size=16),    axis.title=element_text(size=16,face="bold")) 
TCdist_men


TCdist_women <- ggplot(hse_women, aes(x = totchol, y = ..density.., colour = age_group, fill = age_group)) 
TCdist_women <- TCdist_women + geom_density(alpha = 0.1) + xlim(0,14) + xlab("Total chol (mmol/L)") +     ylab("Population density") +
                ggtitle("Total Cholesterol distributions from the HSE data (women)") +
                theme(axis.text=element_text(size=16),    axis.title=element_text(size=16,face="bold")) 
TCdist_women

nonhdldist_men <- ggplot(hse_men, aes(x = nonhdl, y = ..density.., colour = age_group, fill = age_group)) 
nonhdldist_men <- nonhdldist_men+ geom_density(alpha = 0.1) + xlim(0,11) + xlab("nonhdl chol (mmol/L)") +     ylab("Population density") +
                  ggtitle("nonHDL Cholesterol distributions from the HSE data (men)") +
                  theme(axis.text=element_text(size=16),    axis.title=element_text(size=16,face="bold")) 
nonhdldist_men

nonhdldist_women <- ggplot(hse_women, aes(x = nonhdl, y = ..density.., colour = age_group, fill = age_group)) 
nonhdldist_women <- nonhdldist_women + geom_density(alpha = 0.1) + xlim(0,11) + xlab("nonhdl chol (mmol/L)") +     ylab("Population density") +
                    ggtitle("nonHDL Cholesterol distributions from the HSE data (women)") +
                    theme(axis.text=element_text(size=16),    axis.title=element_text(size=16,face="bold")) 
nonhdldist_women

mypath <- file.path(paste("Ageinterval_distributions_TCnonHDLC", ".pdf", sep=""))
pdf(mypath, paper = "a4r", width = 11, height = 8)
grid.arrange(TCdist_men,nonhdldist_men,TCdist_women,nonhdldist_women, ncol=2)
dev.off()

TCcentile_men <- ddply(hse_men, .(age_group), function(hse_men.sub) quantile(hse_men.sub$totchol, c(.75,.80,.90, .95, .975, .99, .995)))
nonHDLcentile_men <- ddply(hse_men, .(age_group), function(hse_men.sub) quantile(hse_men.sub$nonhdl, c(.75,.80,.90, .95, .975, .99, .995)))

TCcentile_women <- ddply(hse_women, .(age_group), function(hse_women.sub) quantile(hse_women.sub$totchol, c(.75,.80,.90, .95, .975, .99, .995)))
nonHDLcentile_women <- ddply(hse_women, .(age_group), function(hse_women.sub) quantile(hse_women.sub$nonhdl, c(.75,.80,.90, .95, .975, .99, .995)))

TCcentile_all <- bind_rows(TCcentile_men, TCcentile_women)
nonHDLcentile_all <- bind_rows(nonHDLcentile_men, nonHDLcentile_women)

# check against HSE 2003 data - table 9.3 

  TCtest <- ddply(test, .(age_group), function(test.sub) quantile(test.sub$totchol, c(.10,.90)))
  HSE2003_men <- as.data.frame(TCtest)
  test<- subset(hse_women, hse_women$year == "2003")
  TCtest <- ddply(test, .(age_group), function(test.sub) quantile(test.sub$totchol, c(.10,.90)))
  HSE2003_women <- as.data.frame(TCtest)
  
  HSE2003_all <- bind_rows(HSE2003_men,HSE2003_women)

  write.csv(HSE2003_all, "Agegrouped_centiles/HSE2003_check.csv")
  write.csv(TCcentile_all, "Agegrouped_centiles/TCcentile_allyears.csv")
  write.csv(nonHDLcentile_all, "Agegrouped_centiles/nonHDLcentile_allyears.csv")
  
  
  
# centilecurvem <- lms(nonhdl, age, data = hse_men)
# centilecurvew <- lms(nonhdl, age, data = hse_women)
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

# nonhdal
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

write.csv(mcentilesdf, "GAMLASS centiles/malecentiles.csv")
write.csv(wcentilesdf, "GAMLASS centiles/femalecentiles.csv")

# need to consolidate these 
