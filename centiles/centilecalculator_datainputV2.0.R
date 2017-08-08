# note shiny does not like read in files with any characters in them

library(shiny)
library(ggplot2)
library(dplyr)

# wd <- "H:/Jallens_homearea_DEC/Calculators/R/Centile calculator/"
#wd <- "~/Documents/DEC WORK/FH/Current2016-2017/FHfileshare/centiles"
#wd <-  "Z:/DEC methods/tools - R/Working_project_folders/NCL_DEC0002 Familial hypercholesterolaemia/Current2016-2017/FHfileshare/centiles"

#setwd(wd)

cm <- read.csv("malecentiles.csv")
cw <- read.csv("femalecentiles.csv")

gcm <- read.csv("gamlassmalecentiles.csv")
gcw <-read.csv("gamlassfemalecentiles.csv")

TCcm <- read.csv("malecentiles_TC.csv")
TCcw <-read.csv("femalecentiles_TC.csv")

gamlass_centiles <- TRUE

centile_script <- function(age, sex, chol, valuechol){ 
  

  
  if(!gamlass_centiles){
    
    if(sex == "Male") centiles <- cm
    if(sex == "Female") centiles <- cw
    
    age_group <- 0
    age_group <- ifelse(age <= 16, 1,age_group)
    age_group <- ifelse(age >= 16 & age <= 24, 2, age_group)
    age_group <- ifelse(age >= 25 & age <= 34, 3, age_group)
    age_group <- ifelse(age >= 35 & age <= 44, 4, age_group)
    age_group <- ifelse(age >= 45 & age <= 54, 5, age_group)
    age_group <- ifelse(age >= 55 & age <= 64, 6, age_group)
    age_group <- ifelse(age >= 65 & age <= 74, 7, age_group)
    age_group <- ifelse(age >= 75, 8, age_group)
    
    
    centile <- "NULL"
    
    for (i in 1:8)
    {
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,9], 100, centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,8] & nonhdl < centiles[i,9], "99-100", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,7]& nonhdl < centiles[i,8], "95-99", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,6]& nonhdl < centiles[i,7], "90-95", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,5]& nonhdl < centiles[i,6], "80-90", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,4]& nonhdl < centiles[i,5], "75-80", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,3]& nonhdl < centiles[i,4], "50-75", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,2]& nonhdl < centiles[i,3], "25-50", centile)
      centile <- ifelse(age_group == i & nonhdl < centiles[i,2] , "<25", centile)
    }
    
  }
  
  
  if (gamlass_centiles){
    if(sex == "Male" &  chol == "nonHDL") centiles <- gcm
    if(sex == "Female" & chol == "nonHDL") centiles <- gcw
    if(sex == "Male" &  chol == "TC") centiles <- TCcm
    if(sex == "Female" & chol == "TC") centiles <- TCcw
    
    centiles$X <- NULL
    rownames(centiles) <- centiles$age - 15
    
    ageindex <- age - 15
    ageindex[ageindex < 0] <- 0
    
    centile <- "NULL"
    
    #if(chol == "nonhdl") valuechol <- nonhdl
    #if(chol == "TC") valuechol <- totchol
    
    centile <- ifelse(ageindex >0 & valuechol >= centiles[ageindex,8], ">99.5", centile)
    centile <- ifelse(ageindex >0 & valuechol >= centiles[ageindex,7]& valuechol < centiles[ageindex,8], "99-99.5", centile)
    centile <- ifelse(ageindex >0 & valuechol >= centiles[ageindex ,6]& valuechol < centiles[ageindex,7], "97.5-99", centile)
    centile <- ifelse(ageindex >0 & valuechol >= centiles[ageindex,5]& valuechol < centiles[ageindex,6], "95-97.5", centile)
    centile <- ifelse(ageindex >0 & valuechol >= centiles[ageindex,4] & valuechol < centiles[ageindex,5], "90-95", centile)
    centile <- ifelse(ageindex >0 & valuechol >= centiles[ageindex,3]& valuechol < centiles[ageindex,4], "80-90", centile)
    centile <- ifelse(ageindex >0 & valuechol >= centiles[ageindex,2]& valuechol < centiles[ageindex,3], "75-80", centile)
    centile <- ifelse(ageindex >0 & valuechol < centiles[ageindex,2], "<75", centile)
    
  }
  
  # plots
  centiles <- as.data.frame(centiles)
  centiles <- centiles[with(centiles,order(age)), ]
  
  # dist <- ggplot(centiles, aes(x = age, y = X75., colour = "red"))
  # dist <- dist + geom_line(na.rm = TRUE)   + geom_line(aes(x = age, y = X80., colour = "blue")) +
  #   geom_line(aes(x = age, y = X90., colour = "green")) + geom_line(aes(x = age, y = X95., colour = "orange"))+ 
  #   geom_line(aes(x = age, y = X97.5., colour = "purple"))+
  #   geom_line(aes(x = age, y = X99., colour = "cyan")) + geom_line(aes(x = age, y = X99.5., colour = "brown")) + 
  #   xlim(0,120)+ ylab("chol (mmol/L)") +     xlab("Age")  
  # 
  
  
  # centile = 
  #   data.frame(
  #     Name = c("Age", "sex", "nonHDL","Centile band"),
  #     Value = c(age, sex, nonhdl,centile))
  
  
  
  return(centile)
  
}



ui<-fluidPage(
  
#  tags$style(type="text/css",
#             ".shiny-output-error { visibility: hidden; }",
#             ".shiny-output-error:before { visibility: hidden; }"
#  ),
  
  titlePanel(h4("Cholesterol percentile calculator")),
  
  sidebarLayout(
        #  Application title
     # headerPanel("Centile calculator"),
      
      # Sidebar with sliders that demonstrate various available options
      sidebarPanel(
        
        fileInput('file1', 'Choose CSV File with options',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        
        radioButtons("chol", label = h3("Total Cholesterol or nonHDL?"), 
                     choices = list("Total Cholesterol" = "TC", "nonHDL Cholesterol" = "nonHDL"),
                     selected = "TC"),
        
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','), 
        tags$hr(),
        downloadButton('downloadData', 'Download')
        
    ),
    mainPanel(
      # output the centile
        #tableOutput("view"),
      #tableOutput('contents'),
        tags$br(),
        textOutput("text1"),
        plotOutput("centilePlotm"),
       # plotOutput("snpPlotm"),
        plotOutput("centilePlotf"),
        #plotOutput("snpPlotf"),
        tags$b("Cite as:"),
        tags$br(),
        "Joy Allen, Dermot Neely",
        tags$br(),
        tags$em("A web application to calculate age and gender specific centiles for nonHDL cholesterol"),
        tags$br(),
        "NIHR Diagnostic Evidence Co-operative Newcastle. June 2017",
        tags$br(),
        verbatimTextOutput("lines")
    )
  )
)



#######################################################

#################    server     ################

server<-function(input, output) {
  #output$age <- renderPrint({ input$age })
  #output$value <- renderPrint({ input$sex })
  #output$nonhdl <- renderPrint({ input$nonhdl })
  
    formula <-reactive({
       centile_script(age, sex, chol, valuechol)
    })
    #samplesize(input$prev, input$SnI, input$CI, input$alpha, input$beta)
  
  #     output$view <- renderTable({
  #    formula()
  #  })
        
       
      myData <- reactive({
          inFile <- input$file1
          
          if (is.null(inFile))
            return(NULL)
          
          read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                   quote=input$quote)
        
          
        })
      
      outputdata <- reactive({
        
        df <- myData()
        
        if (is.null(df)) return(NULL)
        df <- as.data.frame(df)
        
        df_male <- subset(df, df$sex == "MALE")
        df_male$centile <- apply(df_male,1, function(x,y,z) centile_script(df_male$age,  "Male", "nonHDL",df_male$nonhdl))[,1]
        df_male2 <- df_male
        colnames(df_male)[colnames(df_male) == 'centile'] <- 'nonHDLcentile'
        
        df_male$centile <- apply(df_male,1, function(x,y,z) centile_script(df_male$age,  "Male", "TC",df_male$totchol))[,1]
        df_male3 <- df_male
        #df_male$TC_centile <- df_male$centile
        colnames(df_male)[colnames(df_male) == 'centile'] <- 'TCcentile'
        #cbind(df_male2, df_male3$TCcentile)
        
        df_female <- subset(df, df$sex == "FEMALE")
        df_female$centile <- apply(df_female,1, function(x,y,z) centile_script(df_female$age,  "Female", "nonHDL", df_female$nonhdl))[,1]
        colnames(df_female)[colnames(df_female) == 'centile'] <- 'nonHDLcentile'
        #df_female$nonHDL_centile <- df_female$centile
        
        df_female$centile <- apply(df_female,1, function(x,y,z) centile_script(df_female$age,  "Female", "TC", df_female$totchol))[,1]
        colnames(df_female)[colnames(df_female) == 'centile'] <- 'TCcentile'
        #df_female$TC_centile <- df_female$centile
        
        
        df2 <- rbind.data.frame(df_male, df_female)
        return(df2)
      })
        
     
        output$centilePlotm<-renderPlot({
           df <- myData()
           if (is.null(df)) return(NULL)
           
           df <- as.data.frame(df)
           df_male <- subset(df, df$sex == "MALE")
           
           df_female <- subset(df, df$sex == "FEMALE")
           
          if(!gamlass_centiles){
             mcentiles <- cm
             fcentiles <- cw
          }
          
          if (gamlass_centiles){
            if(input$chol == "nonHDL") mcentiles <- gcm
            if(input$chol == "TC") mcentiles <- TCcm
            }
          
          mcentiles <- as.data.frame(mcentiles)
          mcentiles <- mcentiles[with(mcentiles,order(age)), ]
          
          #input_age <- input$age
          #if(input$age <16 | input$age > 120) input_age <- 30
          
          if(input$chol == "TC") colnames(df_male)[colnames(df_male) == "totchol"] <- "choltype"
          if(input$chol == "nonHDL") colnames(df_male)[colnames(df_male) == "nonhdl"] <- "choltype"
          
         
          dist <- ggplot(mcentiles, aes(x = age, y = X75.), colour = "red", size =1)
          dist <- dist + geom_line(aes(x = age, y = X75.), colour = "red", size =1)   + 
            geom_line(aes(x = age, y = X80.), colour = "blue", size =1.25) +
            geom_line(aes(x = age, y = X90.), colour = "green", size = 1.25) + 
            geom_line(aes(x = age, y = X95.), colour = "orange", size = 1.25) + 
            geom_line(aes(x = age, y = X97.5.), colour = "purple", size = 1.25) +
            geom_line(aes(x = age, y = X99.), colour = "cyan", size = 1.25) + 
            geom_line(aes(x = age, y = X99.5.), colour = "brown", size = 1.25) + 
            xlim(0,120)+ ylim(2.5,12) + ylab("chol (mmol/L)") +     xlab("Age")   + 
            ggtitle(paste("Male centile plots")) + 
            geom_point(data = df_male, mapping = aes(x = df_male$age, y = df_male$choltype), size = 2) + 
              annotate("text", label = "75%",colour = "red", size = 6, x = 115, y = 3.75) +
              annotate("text", label = "80%",colour = "blue", size = 6,  x = 115, y = 4) +
              annotate("text", label = "90%",colour = "green", size = 6,  x = 115, y = 4.5) +
              annotate("text", label = "95%",colour = "orange", size = 6, x = 115, y = 5.0) +
              annotate("text", label = "97.5%",colour = "purple", size = 6,  x = 115, y = 5.50) + 
              annotate("text", label = "99%",colour = "cyan", size = 6,  x = 115, y = 6.0) +
              annotate("text", label = "99.5%",colour = "brown", size = 6,  x = 115, y = 6.5) +
             theme(plot.title = element_text(size = 14, face = "bold")) 
          
          dist
        })
        
        
        output$centilePlotf<-renderPlot({
          df <- myData()
          if (is.null(df)) return(NULL)
          
          df <- as.data.frame(df)
          
          df_female <- subset(df, df$sex == "FEMALE")
          
          if(!gamlass_centiles){
           
            fcentiles <- cw
          }
          
          if (gamlass_centiles){
            if(input$chol == "nonHDL") fcentiles <- gcw
            if(input$chol == "TC") fcentiles <- TCcw
          }
          
          fcentiles <- as.data.frame(fcentiles)
          fcentiles <- fcentiles[with(fcentiles,order(age)), ]
          
          if(input$chol == "TC") colnames(df_female)[colnames(df_female) == "totchol"] <- "choltype"
          if(input$chol == "nonHDL") colnames(df_female)[colnames(df_female) == "nonhdl"] <- "choltype"
          
          dist <- ggplot(fcentiles, aes(x = age, y = X75.), colour = "red", size =1)
          dist <- dist + geom_line(aes(x = age, y = X75.), colour = "red", size =1)   + 
            geom_line(aes(x = age, y = X80.), colour = "blue", size =1.25) +
            geom_line(aes(x = age, y = X90.), colour = "green", size = 1.25) + 
            geom_line(aes(x = age, y = X95.), colour = "orange", size = 1.25) + 
            geom_line(aes(x = age, y = X97.5.), colour = "purple", size = 1.25) +
            geom_line(aes(x = age, y = X99.), colour = "cyan", size = 1.25) + 
            geom_line(aes(x = age, y = X99.5.), colour = "brown", size = 1.25) + 
            xlim(0,120)+ ylim(2.5,12) + ylab("chol (mmol/L)") +     xlab("Age")    + 
            ggtitle(paste("Female centile plots")) + 
            geom_point(data = df_female, mapping = aes(x = df_female$age, y = df_female$choltype), size = 2) + 
            annotate("text", label = "75%",colour = "red", size = 6, x = 115, y = 3.75) +
            annotate("text", label = "80%",colour = "blue", size = 6,  x = 115, y = 4) +
            annotate("text", label = "90%",colour = "green", size = 6,  x = 115, y = 4.5) +
            annotate("text", label = "95%",colour = "orange", size = 6, x = 115, y = 5.0) +
            annotate("text", label = "97.5%",colour = "purple", size = 6,  x = 115, y = 5.50) + 
            annotate("text", label = "99%",colour = "cyan", size = 6,  x = 115, y = 6.0) +
            annotate("text", label = "99.5%",colour = "brown", size = 6,  x = 115, y = 6.5) +
            theme(plot.title = element_text(size = 14, face = "bold")) 
          
          
          
          dist
        })
        
        output$downloadData <- downloadHandler(
          filename = function() {
            paste('patientspecificcentiles-', Sys.Date(), '.csv', sep = "")
          },
          content = function(file) {
            write.csv(outputdata(), file)
          }
         )
        
        
        
      }


shinyApp(ui=ui, server=server)
