# script to open the HSE data and calculated LDL centiles for use in 
# FH evaluation

ldl_centiles <- function(){


hse_chol <- read.csv("HSE_data/Raw_data/hse_chol.csv", stringsAsFactors = F)


# create data set where the fasting LDL has been collected.
hse_fldl = subset(hse_chol, hse_chol$fldlchol != "NA")
hse_fldl_cal = subset(hse_chol, hse_chol$totchol != "NA"& hse_chol$hdlchol != "NA" & 
                        hse_chol$ftrigl != "NA" & hse_chol$fldlchol == "NA")

max(hse_fldl$fldlchol)
message(paste(nrow(hse_fldl), "people with measured LDL values"))
message(paste(nrow(hse_fldl_cal), "people without measured LDL values but 
              we can calculated using the Friedwald formula"))


 ldl_hist_tot = ggplot(hse_fldl, aes(x=fldlchol))
 ldl_hist_tot = ldl_hist_tot + geom_histogram() + ylab("Frequency") +  
   xlab("Fasting LDL")  + theme(axis.text=element_text(size=16), 
                           axis.title=element_text(size=16,face="bold"))
 
 ldl_hist_tot
 
 mypath <- file.path(paste("hse_ldl_hist_tot", ".pdf", sep=""))
 pdf(mypath, paper = "a4r", width = 11, height = 8)
 grid.arrange(ldl_hist_tot, ncol=1)
 dev.off()
 
 #### PLOT AGES
 
 age_hist_tot = ggplot(hse_fldl, aes(x=age))
 age_hist_tot = age_hist_tot + geom_histogram() + ylab("Frequency") +  
   xlab("Age")  + theme(axis.text=element_text(size=16), 
                                axis.title=element_text(size=16,face="bold"))
 
 age_hist_tot
 
 mypath <- file.path(paste("hse_age_hist_tot", ".pdf", sep=""))
 pdf(mypath, paper = "a4r", width = 11, height = 8)
 grid.arrange(age_hist_tot, ncol=1)
 dev.off()
 
 #Choose age intervals
 #max(hse_fldl$age) = 89
 #min(hse_fldl$age) = 35
 
 ########################################################################################
 # LDL (fasting) histogram for each age and then calculation of centiles
 hse_fldl_1 = subset(hse_fldl, hse_fldl$age <= 49)
 hse_fldl_2 = subset(hse_fldl, hse_fldl$age >= 50 &  hse_fldl$age <= 69)
 hse_fldl_3 = subset(hse_fldl, hse_fldl$age >= 70)
 
 nrow(hse_fldl_1)
 nrow(hse_fldl_2)
 nrow(hse_fldl_3)
 
ldl_hist_1 = ggplot(hse_fldl_1, aes(x=fldlchol)) 
ldl_hist_1 = ldl_hist_1 + geom_histogram(colour = "cadetblue3", fill = "cadetblue3") +
             ylab("Frequency") +  
             xlab("Fasting LDL (mmol/L)")  + theme(axis.text=element_text(size=16), 
                                axis.title=element_text(size=16,face="bold")) +  
             scale_y_continuous(limits = c(0, 40) )
 #  + guides(colour=FALSE)
 
 ldl_hist_1
 
 ldl_hist_2 = ggplot(hse_fldl_2, aes(x=fldlchol))
 ldl_hist_2 = ldl_hist_2 + geom_histogram(colour = "cadetblue3", fill = "cadetblue3") + ylab("Frequency")  +
              xlab("Fasting LDL (mmol/L)")  + theme(axis.text=element_text(size=16), 
                                axis.title=element_text(size=16,face="bold")) + 
              scale_y_continuous(limits = c(0, 40)) 
 
 
 ldl_hist_2
 
 
 ldl_hist_3 = ggplot(hse_fldl_3, aes(x=fldlchol))
 ldl_hist_3 = ldl_hist_3 + geom_histogram(colour = "cadetblue3", fill = "cadetblue3") + ylab("Frequency") +  
              xlab("Fasting LDL (mmol/L)")  + theme(axis.text=element_text(size=16), 
                                axis.title=element_text(size=16,face="bold")) + 
              scale_y_continuous(limits = c(0, 40))
   
 ldl_hist_3
 
 mypath <- file.path(paste("hse_ldl_age", ".pdf", sep=""))
 pdf(mypath, paper = "a4r", width = 11, height = 8)
 grid.arrange(ldl_hist_1,ldl_hist_2, ldl_hist_3, ncol=3)
 dev.off()
 
 #### density graphs #####
 hist = rbind(hse_fldl_1,hse_fldl_2,hse_fldl_3)
 ldl_dist = ggplot(hist, aes(x=fldlchol, y=..density..))
 ldl_dist = ldl_dist + geom_density(data = hse_fldl_1, colour = "red", size = 1, fill = "red", alpha = 0.2) + 
            geom_density(data = hse_fldl_2, colour = "blue", size = 1, fill = "blue", alpha = 0.2) + 
            geom_density(data = hse_fldl_3, colour = "green", size = 1, fill = "green", alpha = 0.2) +  
            theme(axis.text=element_text(size=16), 
            axis.title=element_text(size=16,face="bold")) + ylab("Denisty") + xlab("Fasting LDL (mmol/L)")
 
 ldl_dist
 

 
 
 ###########CALCULATION OF CENTILES #####################################
 centiles_1 = quantile(hse_fldl_1$fldlchol, c(0,.25,.50, .75, .95, 1))
 centiles_2 = quantile(hse_fldl_2$fldlchol, c(0,.25,.50, .75, .95, 1))
 centiles_3 = quantile(hse_fldl_3$fldlchol, c(0,.25,.50, .75, .95, 1))
 
 return(list(centiles_1 = centiles_1, centiles_2=centiles_2, centiles_3=centiles_3))
 
 }