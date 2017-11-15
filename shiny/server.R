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

parseR <- function(file='data/testChat.txt',drop="44"){
  rawData <- read.delim(file, quote = "", 
                        row.names = NULL, 
                        stringsAsFactors = FALSE,
                        header = F)
  
  rawData<-scan(file, what="", sep="\n")
  
  
  joinedData <- rep(NA, length(rawData))
  
  gr <- 1
  for (i in 1:length(rawData)) {
    # if starting with timestamp, save into out and move on (gr)
    find.startline <- grepl("^\\d{2}\\/\\d{2}\\/\\d{4}", rawData[i])
    if (find.startline) {
      joinedData[gr] <- rawData[i]
      gr <- gr + 1
    }
    
    if (!find.startline) {
      # if doesn't start with timestamp, append to previous (ss)
      ss <- gr - 1
      joinedData[ss] <- paste(joinedData[ss], rawData[i])
    }
  }
  
  # if there are any multiline comments, some residual NAs should be present, removed
  joinedData <- joinedData[!is.na(joinedData)]
  
  joinedData <- as.data.frame(joinedData,row.names = NULL, optional = FALSE )
  colnames(joinedData)<-'V1'
  
  
  joinedData$V1<-gsub("http", ' ', joinedData$V1)
  # replace '/' with spaces
  joinedData$V1<-gsub("/", " ", joinedData$V1)
  
  # Replace emojis with '[emoji]'
  joinedData$V1<-gsub("\\U00", "[emoji]", joinedData$V1)
  
  sepData<-suppressWarnings(separate(joinedData, V1, c("datetime", "sender", "message"), sep = ": ", extra = "merge"))
  
  sepData$message <- trimws(sepData$message)
  sepData$sender<-factor(sepData$sender)
  
  data <- sepData %>% 
    filter(!is.na(message)) %>%
    filter(!grepl(drop, sender)) %>%
    droplevels() 
  
  data$datetime<-dmy_hms(data$datetime)
  
  cleanData<-separate(data, datetime, c("date", "time"), sep = " ", remove =TRUE)
  cleanData$date<-ymd(cleanData$date)
  cleanData$time<-hms(cleanData$time)
  
  return(cleanData)
}

senderPosts <- function(){
  data <- parseR(file='data/testChat.txt')
  
  postCount<-as.data.frame(cbind(table(data$sender)))
  postCount <- data.frame(names = row.names(postCount), postCount)
  rownames(postCount)<-NULL
  colnames(postCount)<-c("name", "posts")
  
  postCount <- transform(postCount, name = reorder(name, -posts))
  
  # Plot bar
  p <- ggplot(postCount)
  p <- p + geom_bar(aes(name, posts),stat='identity')
  p <- p + scale_y_continuous("Number of posts", breaks=seq(0,max(postCount$posts),by=100))
  p <- p + cleanTheme() + 
    theme(
      axis.title.x=element_blank()
      )
  p
  
}

shinyServer(function(input, output,session) {
  
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    df <- parseR(file=inFile$datapath)

    
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    updateSelectInput(session, inputId = 'sender', label = 'Sender',
                      choices = levels(df$sender), selected = levels(df$sender))
    # updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
    #                   choices = names(df), selected = names(df)[2])
    
    return(df)
  })
  

  output$contents <- renderTable({
    head(data())
  })
    
  output$postCount <-renderPlot({
    
    data<-data()
    
    postCount<-as.data.frame(cbind(table(data$sender)))
    postCount <- data.frame(names = row.names(postCount), postCount)
    rownames(postCount)<-NULL
    colnames(postCount)<-c("name", "posts")
    
    postCount <- transform(postCount, name = reorder(name, -posts))
    
    if(max(postCount$posts) <= 100){
      division = 10
    }
    else if(max(postCount$posts) > 100 & max(postCount$posts) < 1000){
      division = 100
    }
    else{
      division = 1000
    }
    
    # Plot bar
    p <- ggplot(postCount)
    p <- p + geom_bar(aes(name, posts),stat='identity')
    p <- p + scale_y_continuous("Number of posts", breaks=seq(0,max(postCount$posts),by=division))
    p <- p + cleanTheme() + 
      theme(
        axis.title.x=element_blank()
      )
    p
    
  })
    
})