### function to look at SNP score


snp_scores <- function(mydata){
  
  no_plotted <- subset(mydata, mydata$nonhdl != "NA" & mydata$LDLC != "NA" & mydata$SNPscore != "NA")
  no_plotMD <- subset(mydata, mydata$nonhdl != "NA" & mydata$LDLC != "NA" & mydata$SNPscore != "NA" &
                        mydata$outcome == "MD")
  no_plotNMD <- subset(mydata, mydata$nonhdl != "NA" & mydata$LDLC != "NA" & mydata$SNPscore != "NA" &
                        mydata$outcome == "NMD")
  
  snpldl <- ggplot(mydata, aes(x = SNPscore, y = LDLC, colour = outcome, fill = outcome ))
  snpldl <- snpldl + geom_point(na.rm = TRUE) +  ylim(0,15) +theme(axis.text=element_text(size=16), 
                                                      axis.title=element_text(size=16,face="bold")) +
                                                        geom_smooth(method=lm)
  snpldl
  
  
  
  snpnonhdl <- ggplot(mydata, aes(x = SNPscore, y = nonhdl, colour = outcome, fill = outcome ))
  snpnonhdl <- snpnonhdl + geom_point(na.rm = TRUE)+ ylim(0,15)  +theme(axis.text=element_text(size=16), 
                                                   axis.title=element_text(size=16,face="bold")) +
                                                    geom_smooth(method=lm)
  snpnonhdl
  
  mypath <- file.path(paste("SNP_scores", ".pdf", sep=""))
  pdf(mypath)#, paper = "a4r", width = 11, height = 8)
  grid.arrange(snpldl,snpnonhdl, ncol=2)
  dev.off()  
  
  ## plot centiles and MoM
  
  snp_mom <- ggplot(mydata, aes(x= SNPscore, y = MoM, colour = outcome, fill = outcome ))
  snp_mom <- snp_mom + geom_point(na.rm = TRUE)+ ylim(0,2)  +theme(axis.text=element_text(size=16), 
                                        axis.title=element_text(size=16,face="bold")) +
    geom_smooth(method=lm)
    #geom_smooth(method=lm)
  snp_mom
  
  snp_mom2 <- ggplot(mydata, aes(x= MoM, y = SNPscore, colour = outcome, fill = outcome ))
  snp_mom2 <- snp_mom2 + geom_point(na.rm = TRUE)  + xlim(0,2) + theme(axis.text=element_text(size=16), 
                                                                   axis.title=element_text(size=16,face="bold")) +
              geom_smooth(method=lm)
  #geom_smooth(method=lm)
  snp_mom2
  
  snp_centile <- ggplot(mydata, aes(x= gamlass_centile, y = SNPscore, colour = outcome, fill = outcome ))
  snp_centile <- snp_centile + geom_point(na.rm = TRUE) + theme(axis.text=element_text(size=16), 
                                                                       axis.title=element_text(size=16,face="bold")) +
    scale_x_discrete(labels=c("Unknown", "<75", "\n75-80","80-90","\n90-95","95-97.5", "\n97.5-99", "99-99.5", "\n>99.5"))
    
  #geom_smooth(method=lm)
  snp_centile
  
  snp_centile <- ggplot(mydata, aes(x= SNPscore, y = gamlass_centile, colour = outcome, fill = outcome ))
  snp_centile <- snp_centile + geom_point(na.rm = TRUE) + theme(axis.text=element_text(size=16), 
                                                                axis.title=element_text(size=16,face="bold")) +
    scale_x_discrete(labels=c("Unknown", "<75", "\n75-80","80-90","\n90-95","95-97.5", "\n97.5-99", "99-99.5", "\n>99.5"))
  
  #geom_smooth(method=lm)
  snp_centile
  
  #p <- ggplot(mydata, aes(x=gamlass_centiles)) 
  #p <- p + geom_histogram(aes(weights=SNPscores, fill=gamlass_centiles))
  #p <- p + scale_fill_brewer(palette="Set3")
  #p <- p + facet_wrap( ~ weekday, ncol=1)
  #p
  
  snpldl2 <- ggplot(mydata, aes(x = LDLC, y = SNPscore, colour = outcome, fill = outcome ))
  snpldl2 <- snpldl2 + geom_point(na.rm = TRUE) +  scale_x_continuous(limits = c(0,15)) + #, breaks = c(0,5,10,15),
                    #   labels = c("0", "5", "10", "15")) +
                       geom_smooth(method=lm) + 
                       theme(axis.text=element_text(size=16), 
                       axis.title=element_text(size=16,face="bold"))
  snpldl2
  
  
  
  snpnonhdl2 <- ggplot(mydata, aes(x = nonhdl, y = SNPscore, colour = outcome, fill = outcome ))
  snpnonhdl2 <- snpnonhdl2 + geom_point(na.rm = TRUE)+ xlim(0,15)  + 
                theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold")) +
                geom_smooth(method=lm)
  snpnonhdl2
  
  mypath <- file.path(paste("SNP_scores2", ".pdf", sep=""))
  pdf(mypath, paper = "a4r", width = 11, height = 8)
  grid.arrange(snpldl2,snpnonhdl2, ncol=2)
  dev.off() 
  
  # histogram of SNP deciles and MD
  
  snp_decile <- ggplot(mydata, aes( x = SNPdecile, fill = outcome, colour = outcome))
  snp_decile <- snp_decile + geom_histogram(position="dodge", alpha = 0.75) +  ylab("Number of probands") +  
     xlab("SNP decile")  + theme(axis.text=element_text(size=16), 
                                              axis.title=element_text(size=16,face="bold"))
   
   snp_decile
   
   
   snp_density <- ggplot(mydata, aes( x = SNPscore,y=..scaled.., fill = outcome, colour = outcome))
   snp_density <- snp_density + geom_density(position="dodge", alpha = 0.75) +  ylab("Number of probands") +  
     xlab("SNP score")  + theme(axis.text=element_text(size=16), 
                                 axis.title=element_text(size=16,face="bold"))
   
   snp_density
  
   mypath <- file.path(paste("SNP_histograms", ".pdf", sep=""))
   pdf(mypath, paper = "a4r", width = 11, height = 8)
   grid.arrange(snp_decile,snp_density, ncol=2)
   dev.off()
  
 
}