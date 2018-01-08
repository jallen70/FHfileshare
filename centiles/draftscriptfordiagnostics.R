library("Qtools")
hse_men2 <- subset(hse_men, hse_men$age < 85)
centilecurvem <- lms(nonhdl, age, data = hse_men2)
centilecurvem2 <- lms(nonhdl, age, data = hse_men, trans.x = TRUE)
# age transformation 0.3237448

centilecurvem$family
centilecurvem2$family

round(Q.stats(centilecurvem2, xvar=hse_men$age),3)
wp(centilecurvem2, xvar=hse_men$age, n.inter=9)

# compare models
fittedPlot(centilecurvem, centilecurvem2, x=hse_men$age)


op <- par(mfrow=c(2,1))
centiles(centilecurvem,hse_men$age, legend=FALSE)
# % of cases below  0.4 centile is  0.3131796 
# % of cases below  2 centile is  1.922575 
# % of cases below  10 centile is  10.00435 
# % of cases below  25 centile is  25.33275 
# % of cases below  50 centile is  49.59548 
# % of cases below  75 centile is  74.87603 
# % of cases below  90 centile is  90.02175 
# % of cases below  98 centile is  98.16442 
# % of cases below  99.6 centile is  99.61722

centiles(centilecurvem2,hse_men$age, legend=FALSE)

res1 <- resid(centilecurvem)
# % of cases below  0.4 centile is  0.3305785 
# % of cases below  2 centile is  1.922575 
# % of cases below  10 centile is  10.03045 
# % of cases below  25 centile is  25.37625 
# % of cases below  50 centile is  49.64767 
# % of cases below  75 centile is  74.86733 
# % of cases below  90 centile is  90.11744 
# % of cases below  98 centile is  98.15572 
# % of cases below  99.6 centile is  99.58243 

#m2 <-  lms(nonhdl, age, data=hse_men, trans.x=TRUE , families=c("SHASH"))
#centiles.com(centilecurvem, m2, xvar=hse_men$age, legend=FALSE, color=FALSE)

# total cholesterol

fitTCm <- lms(totchol,age, data = hse_men, trans.x = TRUE)
#    transformation of X
0.3355134