library(shiny)
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(RColorBrewer))
suppressMessages(library(tm))
suppressMessages(library(SnowballC))
suppressMessages(library(reshape))
suppressMessages(library(plotly))
suppressMessages(library(stringr))
suppressMessages(library(VennDiagram))
suppressMessages(library(lubridate))
suppressMessages(library("wordcloud"))

cleanTheme <- function(base_size = 12){
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    axis.text = element_text(size=15),
    # axis.title = element_text(size=30),
    panel.grid.major.y = element_line(color="grey80", size = 0.5, linetype = "dotted"),
    axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1),
    axis.title.x=element_text(size=15),
    axis.title.y=element_text(size=15),
    strip.text = element_text(size=15)
  )
}

parseR <- function(file='data/waChat.txt',drop="44",user="Hood"){
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
    droplevels() 
  
  # select(-datetime) %>%
  # unite(date_time, date, time, remove = TRUE)
  
  # data$date_time<-strsplit(data$date_time, '_')
  # data$datetime<-dmy_hms(data$datetime,tz=NULL)
  data$datetime<-dmy_hms(data$datetime, tz=NULL)
  
  cleanData<-separate(data, datetime, c("date", "time"), sep = " ", remove =TRUE)
  cleanData$date<-ymd(cleanData$date)
  cleanData$time<-hms(cleanData$time)
  
  return(cleanData)
}

senderPosts <- function(){
  data <- parseR()
  
  postCount<-as.data.frame(cbind(table(data$sender)))
  postCount <- data.frame(names = row.names(postCount), postCount)
  rownames(postCount)<-NULL
  colnames(postCount)<-c("name", "posts")
  
  postCount <- transform(postCount, name = reorder(name, -posts))
  
  # Plot bar
  p <- ggplot(postCount)
  p <- p + geom_bar(aes(name, posts),stat='identity')
  p <- p + scale_y_continuous("Number of posts", breaks=seq(0,max(postCount$posts),by=100))
  p <- p + cleanTheme()
  p
  
}

shinyServer(function(input, output) {
  output$contents <- renderTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- parseR(file=inFile$datapath)
    
  output$messageCount <-renderPlot({
    
    data <- parseR(file=inFile$datapath)
    
    postCount<-as.data.frame(cbind(table(data$sender)))
    postCount <- data.frame(names = row.names(postCount), postCount)
    rownames(postCount)<-NULL
    colnames(postCount)<-c("name", "posts")
    
    postCount <- transform(postCount, name = reorder(name, -posts))
    
    # Plot bar
    p <- ggplot(postCount)
    p <- p + geom_bar(aes(name, posts),stat='identity')
    p <- p + scale_y_continuous("Number of posts", breaks=seq(0,max(postCount$posts),by=100))
    p <- p + cleanTheme()
    p
    
  })
    
  
  })
})