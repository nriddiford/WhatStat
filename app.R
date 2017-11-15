library(shiny)
library(datasets)

ui <- shinyUI(fluidPage(
  titlePanel("WhatStat"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Upload your WhatsApp chat log"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Select file',
                           accept='txt'
                             ),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 radioButtons('sep', 'Timezone', c('GMT', "GMT+1"),
                              ',')
               ),
               mainPanel(
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Word Cloud",
             pageWithSidebar(
               headerPanel('Group Wordcloud'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('sender', 'Select user', "")

               ),
               mainPanel(
                 plotOutput('Wordcloud')
               )
             )
    )
    
  )
)
)


parseR <- function(file=input$file1,drop="44",user="Hood"){
  
  rawData <- read.delim(file, quote = "", 
                        row.names = NULL, 
                        stringsAsFactors = FALSE,
                        header = F)
  
  
  # remove blank lines
  # rawData<-rawData[!apply(rawData == "", 1, all),]
  
  # join multi line messages into single line
  # rawData$V1<-gsub("[\r\n]", "Hello", rawData$V2)
  
  rawData$V1<-gsub("http", ' ', rawData$V1)
  # replace '/' with spaces
  rawData$V1<-gsub("/", " ", rawData$V1)
  
  sepData<-suppressWarnings(separate(rawData, V1, c("datetime", "sender", "message"), sep = ": ", extra = "merge"))
  
  # newColNames <- c("date", "time")
  # newCols <- colsplit(sepData$datetime, ", ", newColNames)
  # sepData <- cbind(sepData, newCols)
  
  sepData$message <- trimws(sepData$message)
  sepData$sender<-factor(sepData$sender)
  
  data <- sepData %>% 
    filter(!is.na(message)) %>%
    filter(!grepl(drop, sender)) %>%
    filter(sender == user) %>%
    droplevels() 
  
  # select(-datetime) %>%
  # unite(date_time, date, time, remove = TRUE)
  
  # data$date_time<-strsplit(data$date_time, '_')
  # data$datetime<-dmy_hms(data$datetime,tz=NULL)
  data$datetime<-dmy_hms(data$datetime)
  
  cleanData<-separate(data, datetime, c("date", "time"), sep = " ", remove =TRUE)
  cleanData$date<-ymd(cleanData$date)
  cleanData$time<-hms(cleanData$time)
  
  return(cleanData)
}



shinyApp(ui, server)