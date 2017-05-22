 # function to plot prevalence vs savings graph for FH analyis.
 n = 256
 c_seq = 100
 c_miseq = 400
 prev <- seq(0.1, 1, by = 0.1)
 
 c_seq 
 (1-prev)*c_miseq
 
 total_cost <- (c_seq +  (1-prev)*c_miseq)
 
 
 total_cost
 
 
 # if miseq was the only test
 ctot_miseq <- n*c_miseq
 
 savings <- c_miseq - total_cost
 savings
 
 df <- cbind.data.frame(prev, total_cost, savings)
 
 dev.copy(png,'prevalenceplot.png')
 par(pty = "m")
 p <- ggplot(df,aes(100*prev, total_cost, color = "Cost"))
 p <- p +  geom_line( size = 1) + geom_line(size = 1,aes(100*prev, savings, color = "Savings")) + 
   scale_color_manual(name = "", values = c(Cost= "blue", Savings = "red")) +
   labs(x = "Prevalence of FH in index cases (%)", y = "Cost per index case (£)")  + 
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold")) 
 print(p)
 dev.off()
 