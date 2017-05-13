# function to plot demographic data



demographic_plots <- function(mydata, figfilepath){
 
  #age
  dev.copy(png,paste0(figfilepath,'boxplotage.png'))
  p <- boxplot(mydata$age, xlab="age", cex.lab=2)
  print(p)
  dev.off()
  

  #dutchscore
  dev.copy(png,paste0(figfilepath,'boxplotDLCN.png'))
  p2 <- boxplot(mydata$dutchscore, xlab="Dutchscore", cex.lab=2)
  print(p2)
  dev.off()

  
  dev.copy(png,paste0(figfilepath,'boxplotDLCNvsoutcome.png'))
  p2 <- ggplot(mydata, aes(outcome, dutchscore))
  p2 <- p2 + geom_boxplot(aes(fill = outcome)) +  labs(x = "Mutation detected", y = "Dutch Lipid Clinic Network Score", size = 4) + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))
  print(p2)
  dev.off()
 
  
  
  
  #MoM
  
  dev.copy(png,paste0(figfilepath,'boxplotMoM.png'))
  p3 <- boxplot(mydata$MoM, xlab="MoM", cex.lab=2)
  print(p3)
  dev.off()
  
  dev.copy(png,paste0(figfilepath,'boxplotMoMvsoutcome.png'))
  p4 <- ggplot(mydata, aes(outcome,MoM))
  p4 <- p4 + geom_boxplot(aes(fill = outcome))
  print(p4)
  dev.off()
  

  #Cholesterol
  dev.copy(png,paste0(figfilepath,'boxplotTotalCvoutome.png'))
  p5 <- ggplot(mydata, aes(outcome,TotalC))
  p5 <- p5 + geom_boxplot(aes(fill = outcome))
  print(p5)
  dev.off()
  
  dev.copy(png,paste0(figfilepath,'boxplotLDLCoutcome.png'))
  p6 <- ggplot(mydata, aes(outcome,LDLC))
  p6 <- p6 + geom_boxplot(aes(fill = outcome))
  print(p6)
  dev.off()
  
  dev.copy(png,paste0(figfilepath,'boxplotnonhdloutcome.png'))
  p7 <- ggplot(mydata, aes(outcome,nonhdl))
  p7 <- p7 + geom_boxplot(aes(fill = outcome))
  print(p7)
  dev.off()
  
  dev.copy(png,paste0(figfilepath,'boxplotTriglyoutcome.png'))
  p8 <- ggplot(mydata, aes(outcome,Trigly))
  p8 <- p8 + geom_boxplot(aes(fill = outcome))
  print(p8)
  dev.off()
  
  
  # outlier is potentially a mis-entry for HDLC of 104 rather than 1.04.  Checked with 
  #PI DN on 16th March 2017.  replace in analysis until return of results. 
  dev.copy(png,paste0(figfilepath,'rels50risk_hist.png'))
  p9 <- ggplot(mydata, aes(NoRels50.risk, fill = outcome))
  p9 <- p9 + geom_histogram()
  print(p9)
  dev.off()
  
  dev.copy(png,paste0(figfilepath,'rels25risk_hist.png'))
  p10 <- ggplot(mydata, aes(NoRels25.risk, fill = outcome))
  p10 <- p10 + geom_histogram()
  print(p10)
  dev.off()
  
  # data counts
  
  counts0 <- table(mydata$Sex)
  counts2 <- table(mydata$C_TendXan)
  counts3 <- table(mydata$C_CornArcus)
  counts4 <- table(mydata$LDL)
  counts5 <- table(mydata$DLCN)
  counts6 <- table(mydata$gamlass_centile)
  counts7 <- table(mydata$outcome)
  counts8 <- table(mydata$NoRels25.risk)
  counts9 <- table(mydata$NoRels50.risk)
  
  dev.copy(png,paste0(figfilepath,'demographics1.png'))
  
  par(mfrow=c(2,3))
  barplot(counts0,xlab="Sex", cex.lab=1.8, cex.axis=1.5)
  barplot(counts2,xlab="Tendon Xanthomas",  cex.lab=1.8, cex.axis=1.5)
  barplot(counts3, xlab="CornArcus",  cex.lab=1.8, cex.axis=1.5)
  barplot(counts4, xlab="LDL", cex.lab=1.8, cex.axis=1.5)
  barplot(counts5, xlab="DLCN",  cex.lab=1.8, cex.axis=1.5)
  dev.off()
  
  dev.copy(png,paste0(figfilepath,'demographics2.png'))
  
  par(mfrow=c(2,2))
  barplot(counts6, xlab="Centile interval",  cex.lab=1.8, cex.axis=1.5)
  barplot(counts7, xlab="Outcome",  cex.lab=1.8, cex.axis=1.5)
  barplot(counts8, xlab="NoRels at 25% risk",  cex.lab=1.8, cex.axis=1.5)
  barplot(counts9, xlab="NoRels at 50% risk",  cex.lab=1.8, cex.axis=1.5)
  
  dev.off()
  
  

}
