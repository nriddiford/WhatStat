# Copyright 2017 Nick Riddiford
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.



library(shiny)
suppressMessages(library(tidyverse))
suppressMessages(library(tools))
suppressMessages(library(RColorBrewer))
suppressMessages(library(tm))
suppressMessages(library(SnowballC))
suppressMessages(library(reshape))
suppressMessages(library(stringr))
suppressMessages(library(VennDiagram))
suppressMessages(library(lubridate))
suppressMessages(library(wordcloud))
library(scales)
suppressMessages(library(plotly))


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
    # panel.grid.major.x = element_line(color="grey80", size = 0.5, linetype = "dotted"),
    # axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1),
    axis.title.x=element_text(size=15),
    axis.title.y=element_text(size=15),
    strip.text = element_text(size=15)
  )
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

parseR <- function(in_file='data/testChat.txt',drop="44", user=NA){
  
  if(file_ext(in_file)=='zip'){
    # cat("Zipped file")
    suppressMessages(rawData<-unlist(read_table(in_file)))
  }

  else{
    rawData<-scan(in_file, what="", sep="\n")
  }

  joinedData <- rep(NA, length(rawData))
  
  gr <- 1
  for (i in 1:length(rawData)) {
    # if starting with timestamp, save into out and move on (gr)
    find.startline <- grepl("^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}", rawData[i])
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
  
  sepData<-suppressWarnings(separate(joinedData, V1, c("datetime", "sender", "message"), sep = ": ", extra = "merge"))
  
  sepData$message <- trimws(sepData$message)
  sepData$message<-gsub("/", " ", sepData$message)
  
  sepData$sender<-factor(sepData$sender)
  
  if(!is.na(user)){
    if(user %in% levels(sepData$sender)) {
      sepData <- filter(sepData, grepl(user, sepData$sender))
    }
  }
  
  data <- sepData %>% 
    filter(!is.na(message)) %>%
    filter(!grepl(drop, sender)) %>%
    droplevels() 
  
  user <- ifelse(data$user, user, 'NA')
    
  data$datetime<-dmy_hms(data$datetime)
  
  cleanData<-separate(data, datetime, c("date", "time"), sep = " ", remove =TRUE)
  # cleanData$date<-dmy(cleanData$date)
  # cleanData$time<-hms(cleanData$time)
  
  return(cleanData)
}

senderPosts <- function(file_in='data/testChat.txt'){
  data <- parseR(in_file=file_in)
  
  postCount<-as.data.frame(cbind(table(data$sender)))
  postCount <- data.frame(names = row.names(postCount), postCount)
  rownames(postCount)<-NULL
  colnames(postCount)<-c("name", "posts")
  
  postCount <- transform(postCount, name = reorder(name, posts))
  
  if(max(postCount$posts) <= 100){
    division = 20
  }
  else if(max(postCount$posts) > 100 & max(postCount$posts) < 200){
    division = 50
  }
  else if(max(postCount$posts) > 200 & max(postCount$posts) < 500){
    division = 100
  }
  else if(max(postCount$posts) > 500 & max(postCount$posts) < 1000){
    division = 200
  }
  else{
    division = 250
  }
  
  # Plot bar
  p <- ggplot(postCount)
  p <- p + geom_bar(aes(name, posts, fill = "deepskyblue1"),stat='identity')
  p <- p + scale_y_continuous("Number of posts", breaks=seq(0,max(postCount$posts),by=division),expand = c(0.01,0.05))
  p <- p + cleanTheme() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text = element_text(size=20),
      panel.grid.major.x = element_line(color="grey80", size = 0.5, linetype = "dotted")
    )
  p <- p + scale_fill_identity()
  p <- p + coord_flip()
  
  p
  
}


wordFreq <- function(file_in='data/testChat.txt', wordlength=3){
  data <- parseR(in_file=file_in)
  
  data$message <- gsub("(.*http.*)", "", data$message)  
  data$message <- gsub("(.+www\\S+)", "", data$message)
  
  
  docs <- Corpus(VectorSource(data$message)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower))  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, c("omitted", "image", 'video')) %>%
    tm_map(stripWhitespace)
  
  # dataframe of terms
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  all <- data.frame(word = names(v),freq=v)
  
  all <- all %>% 
    filter(nchar(as.character(word))>=wordlength) %>%
    droplevels()
  
  d <- all[1:20,]
  d  <- transform(d , word = reorder(word, freq))
  
  
  if(max(d$freq) <= 10){
    division = 2
  }
  else if(max(d$freq) > 10 & max(d$freq) < 50){
    division = 5
  }
  else if(max(d$freq) > 50 & max(d$freq) < 100){
    division = 10
  }
  else{
    division = 20
  }
  
  
  p <- ggplot(d)
  p <- p + geom_bar(aes(word, freq, fill="springgreen3"),stat='identity')
  p <- p + scale_y_continuous("Word frequency", breaks=seq(0,max(d$freq),by=division),expand=c(0.01,0))
  p <- p + scale_x_discrete("Word", expand = c(0.01,0.01))
  
  p <- p + cleanTheme() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text = element_text(size=20),
      panel.grid.major.x = element_line(color="grey80", size = 0.5, linetype = "dotted")
      
    )
    # theme(axis.text.y = element_text(angle = 90, hjust=1, vjust=0.5),
    #       axis.text = element_text(size=20),
    #       axis.title.x=element_blank()
    #       )
  p <- p + scale_fill_identity()
  p <- p + coord_flip()
  p
}


chatCloud <- function(file_in='data/testChat.txt',user=NA,wordlength=3){
  if(user=='All'){
    user=NA
  }
  data <- parseR(in_file=file_in,user=user)
  
  data$message <- gsub("(.*http.*)", "", data$message)  
  data$message <- gsub("(.+www\\S+)", "", data$message)
  
  docs <- Corpus(VectorSource(data$message)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower))  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, c("omitted", "image", 'video')) %>%
    tm_map(stripWhitespace)
  
  # dataframe of terms
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  all <- data.frame(word = names(v),freq=v)
  
  all <- all %>%
    filter(nchar(as.character(word))>=wordlength) %>%
    filter(!grepl('http', word)) %>%
    droplevels()
  
  wordcloud(words = all$word, freq = all$freq, min.freq = 1,
            max.words=80, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"),scale=c(4,.3))
  
}


senderTime <- function (file_in='data/waChat.txt', user=NA) {
  
  if(user=='All'){
    user=NA
  }
  data <- parseR(in_file=file_in,user=user)
  data$time <- hms(data$time)
  data$hour<-lubridate::hour(data$time)
  labs<-c("12am", "", "2am", "", "4am", "", "6am", "", "8am", "", "10am", "", "12pm", "", "2pm", "", "4pm", "", "6pm", "", "8pm", "", "10pm", "")
  
  
  allData <- parseR(in_file=file_in,user=NA)
  allData$time <- hms(allData$time)
  allData$hour<-lubridate::hour(allData$time)

  maxPosts<-max(table(allData$hour))
  
  p <- ggplot(data, aes(hour, fill=sender))
  p <- p + geom_area(aes(group = sender, colour = sender), stat='bin',position="stack",binwidth=1, alpha = 0.5)
  if(!is.na(user)){
    p <- p + scale_y_continuous("Number of posts", limits=c(0, maxPosts))
  }
  else{
    p <- p + scale_y_continuous("Number of posts")
  }
  p <- p + scale_x_continuous("Time", breaks=seq(0,23, by=1), labels=labs)
  
  # p <- p + scale_x_date(date_breaks="1 month", date_labels="%B")
  p <- p + cleanTheme() + 
    theme(axis.text.x = element_text(angle = 90, hjust=1),
          panel.grid.major.y = element_line(color="grey80", size = 0.5, linetype = "dotted"),
          axis.title.x=element_blank(),
          legend.position="bottom"
    )
  # p <- p + scale_fill_manual(values=cols)  
  p
  #   p <- ggplot(data)
  #   p <- p + geom_density(aes(hour, (..count..), fill = sender), stat='count', adjust = .25, alpha=0.5, show.legend=T)
  #   
  #   p <- p + scale_x_continuous("Time", breaks=seq(0,23, by=1), labels=labs)
  #   p <- p + scale_y_continuous("Number of posts")
  #   # p <- p + facet_wrap(~sender,ncol=2)
  #   p <- p + cleanTheme() +
  #     theme(axis.text.x = element_text(angle = 90, hjust=1),
  #           panel.grid.major.x = element_line(color="grey80", size = 0.5, linetype = "dotted"),
  #           axis.title.x=element_blank(),
  #           legend.position="bottom"
  #          )
  # p
  
}

senderDate <- function(file_in='data/DoolsWA.txt',user=NA,filtYear=NA){
  if(user=='All'){
    user=NA
  }
  data <- parseR(in_file=file_in,user=user)
  
  data$date <- ymd(data$date)
  data$year<-year(data$date)
  data <- filter(data, year == filtYear)
  
  data$month<-month(data$date,label = TRUE,abbr = TRUE)
  
  allData <- parseR(in_file=file_in,user=NA)
  allData$date <- ymd(allData$date)
  allData$year<-year(allData$date)
  allData$month<-month(allData$date,label = TRUE,abbr = TRUE)

  maxPosts<-max(table(week(allData$date),allData$year))
  
  labs=levels(allData$month)
  
  n<-length(levels(allData$sender))
  cols = gg_color_hue(n)
  
  months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  # p <- ggplot(data)
  
  p <- ggplot(data, aes(as.Date(date), fill=sender))
  p <- p + geom_area(aes(group = sender, colour = sender), stat='bin',position="stack",binwidth=7, alpha = 0.5)
  
  if(!is.na(user)){
    p <- p + scale_y_continuous("Number of posts", limits=c(0, maxPosts))
  }
  else{
    p <- p + scale_y_continuous("Number of posts")
  }
  # p <- p + scale_y_continuous("Number of posts", limits=c(0, maxPosts))
  # p <- p + scale_x_continuous("Date", breaks=seq(0,12, by=1), labels=levels(allData$month))
  p <- p + scale_x_date(date_breaks="1 month", date_labels="%B", expand=c(0,0))
  p <- p + cleanTheme() + 
    theme(axis.text.x = element_text(angle = 90, hjust=1,vjust = 0.5),
          panel.grid.major.y = element_line(color="grey80", size = 0.5, linetype = "dotted"),
          axis.title.x=element_blank(),
          legend.position="bottom"
          )

  # p <- p + scale_fill_manual(values=cols)  
  p
  
  
  
  # p <- p + geom_area(aes(month, (..count..), fill = sender), stat='bin',alpha=0.6)
  # # p <- p + scale_x_discrete(breaks=seq(1,length(labs), by=4), labels=months)
  # 
  # p <- p + scale_y_continuous("Number of posts", limits=c(0, maxPosts))
  # # p <- p + scale_x_date(date_breaks="1 month", date_labels="%B")
  # # ]]p <- p + scale_x_date(breaks = "1 month", minor_breaks = "1 week", date_labels = "%B")
  # p <- p + cleanTheme() + 
  #   theme(axis.text.x = element_text(angle = 90, hjust=1),
  #       panel.grid.major.y = element_line(color="grey80", size = 0.5, linetype = "dotted"),
  #       axis.title.x=element_blank(),
  #       legend.position="bottom"
  #   )
  # # p <- p + scale_fill_manual(values=cols)  
  # p
  
}

shinyServer(function(input, output, session) {
  
  data <- reactive({ 
    req(input$file1)
    
    inFile <- input$file1 
    
    df <- parseR(in_file=inFile$datapath) # Call my parser function 
    
    return(df)
  })
  
  observe({
    df = data()
    updateSelectInput(session, inputId = 'sender', label = 'Sender',
                      choices = levels(df$sender), selected = 'NA')
  })
  # Main page
  output$contents <- renderTable({
    head(data(), 25)
  })
  
  # tabPanel 1
  output$postCount <-renderPlot({
    senderPosts(file_in=input$file1$datapath)
    
  })
  
  observe({
    df = data()
    updateSelectInput(session, inputId = 'wlength', label = 'Minimum word length',
                      choices = c(3:5), selected = 3)
  })
  
  # tabPanel 2
  output$wordCount <-renderPlot({
    wordFreq(file_in=input$file1$datapath, wordlength=input$wlength)
    
  })
  
  observe({
    df = data()
    updateSelectInput(session, inputId = 'user', label = 'Sender',
                      choices = c("All", levels(df$sender)), selected = 'NA')
    updateSelectInput(session, inputId = 'cwlength', label = 'Minimum word length',
                      choices = c(3:5), selected = 3)
  })
  
  # tabPanel 3
  output$wCloud <-renderPlot({
    chatCloud(file_in=input$file1$datapath,user=input$user, wordlength=input$cwlength)
    
  })
  
  observe({
    df = data()
    updateSelectInput(session, inputId = 'Tuser', label = 'Sender',
                      choices = c("All", levels(df$sender)), selected = 'NA')
  })
  
  # tabPanel 4
  output$timePlot <-renderPlot({
    senderTime(file_in=input$file1$datapath,user=input$Tuser)
  })
  
  observe({
    df = data()
    updateSelectInput(session, inputId = 'Duser', label = 'Sender',
                      choices = c("All", levels(df$sender)), selected = 'NA')
    
    updateSelectInput(session, inputId = 'Dyear', label = 'Year',
                      choices = levels(factor(year(df$date))))
  })
  
  # tabPanel 5
  output$datePlot <-renderPlot({
    senderDate(file_in=input$file1$datapath,user=input$Duser,filtYear=input$Dyear)
  })
    
})
