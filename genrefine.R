# comparing dutch score with centiles for prediction of mutation status

  genrefine <- function(figfilepath,mydata){

#   dutchscore_md <- ggplot(mydata, aes(x=dutchscore, colour = outcome, fill = outcome))
#   dutchscore_md <- dutchscore_md + geom_histogram(position="dodge", alpha = 0.75) +   
#       ylab("Frequency") 
#     xlab("Dutch Lipid Clinic Network Score")  + theme(axis.text=element_text(size=16), 
#                                                   axis.title=element_text(size=16,face="bold"))
#     
#   dutchscore_md
#   
#   #use dutchscore bands
#   dutchscore_md <- ggplot(mydata, aes(x=DLCN, colour = outcome, fill = outcome))
#   dutchscore_md <- dutchscore_md + geom_histogram(position="dodge", alpha = 0.75, stat = "count") +   
#     ylab("Frequency") 
#   xlab("Dutch Lipid Clinic Network Score")  + theme(axis.text=element_text(size=16), 
#                                                     axis.title=element_text(size=16,face="bold"))
  
  name <- paste(figfilepath, "dutchscorebands", ".png", sep="")
  dev.copy(png,name)
  
  mydata <- mydata %>% mutate(DLCN = factor(DLCN, levels = c("Unknown", "2 - 6" ,  "7 - 9" ,  "10 - 12" ,"13 - 16" )))
  
 
  
  dutchbands_md <- ggplot(data = mydata, aes(x=DLCN,colour = outcome, fill = outcome)) 
  dutchbands_md  <- dutchbands_md  +geom_bar(position="dodge", alpha = 1) + 
    ylab("Number of probands") +  ylim(0,80) + 
    xlab("Dutch Score") +  
    theme(axis.text=element_text(size=16, angle = 45), 
          axis.title=element_text(size=16,face="bold"))  #+  scale_fill_discrete(labels = "MD", "")
  
  print(dutchbands_md)
  dev.off()
  
  name <- paste(figfilepath, "centilecorebands", ".png", sep="")
  dev.copy(png,name)
  
  
  centiles_md <- ggplot(data = mydata, aes(x=gamlass_centile, colour = outcome, fill = outcome)) 
  centiles_md  <- centiles_md  +geom_bar(position="dodge", alpha = 1) + 
    ylab("Number of probands")+  ylim(0,80) + 
    xlab("Percentile range") +  
    theme(axis.text=element_text(size=16,angle = 45), 
          axis.title=element_text(size=16,face="bold"))  #+  scale_fill_discrete(labels = "MD", "")
  
  
  print(centiles_md)
  dev.off()
  
  
  ## AM working on this graph 
  dutch_centile <- ggplot(mydata, aes(x = gamlass_centile, y = dutchscore, colour = outcome, fill = outcome ))
  ageldl_pos <- ageldl_pos + geom_point(na.rm = TRUE) + geom_smooth(method = lm) + 
    theme(axis.text=element_text(size=16), 
          axis.title=element_text(size=16,face="bold"),
          axis.text.x=element_blank())
  

}