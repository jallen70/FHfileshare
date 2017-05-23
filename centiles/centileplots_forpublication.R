# centile plots for publication
wd <- "~/Documents/DEC WORK/FH/Current2016-2017/FHfileshare/centiles"
setwd(wd)
gcm <- read.csv("gamlassmalecentiles.csv")
gcw <-read.csv("gamlassfemalecentiles.csv")

gcm <- as.data.frame(gcm)
gcm <- gcm[with(gcm,order(age)), ]

distm <- ggplot(gcm,  aes(x = age, y = X75., color = "red"))
distm <- distm + geom_line(colour = "red", na.rm = TRUE)   + geom_line(colour = "blue",aes(x = age, y = X80., color = "blue")) +
  geom_line(colour = "seagreen3", aes(x = age, y = X90., color = "seagreen3")) + geom_line(colour = "orange", aes(x = age, y = X95., color = "orange"))+ 
  geom_line(colour = "purple",aes(x = age, y = X97.5., color = "purple"))+
  geom_line(colour = "turquoise4",aes(x = age, y = X99., color = "turquoise4")) + geom_line(colour = "brown",aes(x = age, y = X99.5., color = "brown")) + 
#  scale_color_manual(name = "Percentile", values = c("75"= "red", "80" = "blue", "90"= "green4", "95" = "orange", "97.5" = "purple", "99" = "turquoise4", "99.5" = "brown")) +
  xlim(0,124)+ ylim(3.1, 8.5) + ylab("non-HDL-C (mmol/L)") +     xlab("Age")  + 
  geom_hline(yintercept = 5.9) + 
  annotate("text", label = "75%",colour = "red", size = 6, x = 115, y = 3.75) +
  annotate("text", label = "80%",colour = "blue", size = 6,  x = 115, y = 4.25) +
  annotate("text", label = "90%",colour = "seagreen3", size = 6,  x = 115, y = 4.75) +
  annotate("text", label = "95%",colour = "orange", size = 6, x = 115, y = 5.25) +
  annotate("text", label = "97.5%",colour = "purple", size = 6,  x = 115, y = 5.6) + 
  annotate("text", label = "99%",colour = "turquoise4", size = 6,  x = 115, y = 6.15) +
  annotate("text", label = "99.5%",colour = "brown", size = 6,  x = 115, y = 6.75) +
  annotate("text", label = "Current cut-off", size = 4, x = 10, y = 6.1) + 
  ggtitle("Male non-HDL-C percentile plots")
distm

gcw <- as.data.frame(gcw)
gcw <- gcw[with(gcw,order(age)), ]

distw <- ggplot(gcw,  aes(x = age, y = X75., color = "red"))
distw <- distw + geom_line(colour = "red", na.rm = TRUE)   + geom_line(colour = "blue",aes(x = age, y = X80., color = "blue")) +
  geom_line(colour = "seagreen3", aes(x = age, y = X90., color = "seagreen3")) + geom_line(colour = "orange", aes(x = age, y = X95., color = "orange"))+ 
  geom_line(colour = "purple",aes(x = age, y = X97.5., color = "purple"))+
  geom_line(colour = "turquoise4",aes(x = age, y = X99., color = "turquoise4")) + geom_line(colour = "brown",aes(x = age, y = X99.5., color = "brown")) + 
  #  scale_color_manual(name = "Percentile", values = c("75"= "red", "80" = "blue", "90"= "seagreen3", "95" = "orange", "97.5" = "purple", "99" = "turquoise4", "99.5" = "brown")) +
  xlim(0,124)+ ylim(3.1, 8.5) + ylab("non-HDL-C (mmol/L)") +     xlab("Age")  + 
  geom_hline(yintercept = 5.9) + 
  annotate("text", label = "75%",colour = "red", size = 6, x = 115, y = 4.5) +
  annotate("text", label = "80%",colour = "blue", size = 6,  x = 115, y = 4.9) +
  annotate("text", label = "90%",colour = "seagreen3", size = 6,  x = 115, y = 5.3) +
  annotate("text", label = "95%",colour = "orange", size = 6, x = 115, y = 6.1) +
  annotate("text", label = "97.5%",colour = "purple", size = 6,  x = 115, y = 6.45 ) + 
  annotate("text", label = "99%",colour = "turquoise4", size = 6,  x = 115, y = 6.9) +
  annotate("text", label = "99.5%",colour = "brown", size = 6,  x = 115, y = 7.35) + 
  annotate("text", label = "Current cut-off", size = 4, x = 10, y = 6.1) + 
  ggtitle("Female non-HDL-C percentile plots")
distw
centileplots <- grid.arrange(distm, distw, ncol=2)


## do the same for total C


TCcm <- read.csv("malecentiles_TC.csv")
TCcw <-read.csv("femalecentiles_TC.csv")

TCcm <- as.data.frame(TCcm)
TCcm <- TCcm[with(TCcm,order(age)), ]

Tdistm <- ggplot(TCcm,  aes(x = age, y = X75., color = "red"))
Tdistm <- Tdistm + geom_line(colour = "red", na.rm = TRUE)   + geom_line(colour = "blue",aes(x = age, y = X80., color = "blue")) +
  geom_line(colour = "seagreen3", aes(x = age, y = X90., color = "seagreen3")) + geom_line(colour = "orange", aes(x = age, y = X95., color = "orange"))+ 
  geom_line(colour = "purple",aes(x = age, y = X97.5., color = "purple"))+
  geom_line(colour = "turquoise4",aes(x = age, y = X99., color = "turquoise4")) + geom_line(colour = "brown",aes(x = age, y = X99.5., color = "brown")) + 
  #  scale_color_manual(name = "Percentile", values = c("75"= "red", "80" = "blue", "90"= "seagreen3", "95" = "orange", "97.5" = "purple", "99" = "turquoise4", "99.5" = "brown")) +
  xlim(0,124)+ ylim(4,11.5) + ylab("non-HDL-C (mmol/L)") +     xlab("Age")  + 
  annotate("text", label = "75%",colour = "red", size = 6, x = 115, y = 5.2) +
  annotate("text", label = "80%",colour = "blue", size = 6,  x = 115, y = 5.8) +
  annotate("text", label = "90%",colour = "seagreen3", size = 6,  x = 115, y = 6.5) +
  annotate("text", label = "95%",colour = "orange", size = 6, x = 115, y = 7.0) +
  annotate("text", label = "97.5%",colour = "purple", size = 6,  x = 115, y = 7.50) + 
  annotate("text", label = "99%",colour = "turquoise4", size = 6,  x = 115, y = 8.0) +
  annotate("text", label = "99.5%",colour = "brown", size = 6,  x = 115, y = 8.5) + ggtitle("Male Total-C percentile plots")
Tdistm

TCcw <- as.data.frame(TCcw)
TCcw <- TCcw[with(TCcw,order(age)), ]

Tdistw <- ggplot(TCcw,  aes(x = age, y = X75., color = "red"))
Tdistw <- Tdistw + geom_line(colour = "red", na.rm = TRUE)   + geom_line(colour = "blue",aes(x = age, y = X80., color = "blue")) +
  geom_line(colour = "seagreen3", aes(x = age, y = X90., color = "seagreen3")) + geom_line(colour = "orange", aes(x = age, y = X95., color = "orange"))+ 
  geom_line(colour = "purple",aes(x = age, y = X97.5., color = "purple"))+
  geom_line(colour = "turquoise4",aes(x = age, y = X99., color = "turquoise4")) + geom_line(colour = "brown",aes(x = age, y = X99.5., color = "brown")) + 
  #  scale_color_manual(name = "Percentile", values = c("75"= "red", "80" = "blue", "90"= "seagreen3", "95" = "orange", "97.5" = "purple", "99" = "turquoise4", "99.5" = "brown")) +
  xlim(0,124)+ ylim(4,11.5) + ylab("non-HDL-C (mmol/L)") +     xlab("Age")  + 
  annotate("text", label = "75%",colour = "red", size = 6, x = 115, y = 6.5) +
  annotate("text", label = "80%",colour = "blue", size = 6,  x = 115, y = 7) +
  annotate("text", label = "90%",colour = "seagreen3", size = 6,  x = 115, y = 7.6) +
  annotate("text", label = "95%",colour = "orange", size = 6, x = 115, y = 8.15) +
  annotate("text", label = "97.5%",colour = "purple", size = 6,  x = 115, y = 8.9) + 
  annotate("text", label = "99%",colour = "turquoise4", size = 6,  x = 115, y = 9.5) +
  annotate("text", label = "99.5%",colour = "brown", size = 6,  x = 115, y = 9.9) + ggtitle("Female Total-C percentile plots")
Tdistw
TCcentileplots <- grid.arrange(Tdistm, Tdistw, ncol=2)

