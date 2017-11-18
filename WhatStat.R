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


list.of.packages <- c('RColorBrewer',
                      'tm', 'SnowballC',
                      'reshape', 'wordcloud', 'stringr', 'tidyverse', 'lubridate')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){
  cat('Installing missing packages...\n')
  install.packages(new.packages)
}
cat('Silently loading packages...')
suppressMessages(library(tidyverse))
# suppressMessages(library(dplyr))
# suppressMessages(library(plyr))
# suppressMessages(library(tidyr))
suppressMessages(library(RColorBrewer))
suppressMessages(library(tm))
suppressMessages(library(SnowballC))
suppressMessages(library(reshape))
suppressMessages(library(stringr))
suppressMessages(library(VennDiagram))
suppressMessages(library(lubridate))
suppressMessages(library("wordcloud"))

suppressMessages(library("tools"))


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


parseR <- function(in_file='data/DoolsWA.txt',drop="44", user=NA){
  
  if(file_ext(in_file)=='zip'){
    cat("Zipped file\n")
    rawData<-unlist(read_table(in_file))
  }
 
  else {
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
    if(user %in% levels(sepData$sender))
    sepData <- filter(sepData, grepl(user, sepData$sender))
  }
  
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


senderDate <- function(file_in='data/testChat.txt',user=NA){
  if(user=='All'){
    user=NA
  }
  data<-parseR()
  data$date <- ymd(data$date)
  
  data$month<-month(data$date,label = TRUE,abbr = TRUE)
  data$year<-year(data$date)
  
  # data <- filter(data, year == filtYear)
  # data$month<-lubridate::month(data$date,label = TRUE,abbr = TRUE)
  # data<-droplevels(data)
  

  # if (nrow(table(data$sender))==1){
  #   p <- ggplot(data)
  #   p <- p + geom_bar(aes(date, (..count..), fill = "deepskyblue1"),binwidth = 30, stat='count',colour='black')
  #   p <- p + scale_y_continuous("Number of posts")
  #   p <- p + scale_x_date("Date", date_breaks="months", date_labels="%b")
  #   p <- p + cleanTheme()
  #   p <- p + scale_fill_identity()
  #   
  # }
  # else {
    p <- ggplot(data)
    p <- p + geom_density(aes(as.Date(date), (..count..),fill=sender), stat='count', alpha=0.6,show.legend=F)
    p <- p + scale_y_continuous("Number of posts")
    p <- p + scale_x_date("Date", date_breaks="months", date_labels="%b")
    p <- p +cleanTheme()
    p <- p + facet_wrap(~sender,ncol = 2)
    p
  # }
  p
}


senderTime <- function (file_in='data/waChat.txt', user=NA) {
  if(user=='All'){
    user=NA
  }
  data <- parseR(in_file=file_in,user=user)
  data$hour<-lubridate::hour(data$time)
  
  if (nrow(table(data$sender))==1){
    
    p <- ggplot(data)
    # p <- p+geom_bar(aes(date, (..count..), fill = factor(year(date))),stat='count')
    # p <- p + geom_bar(aes(hour, (..count..), fill = sender),binwidth = 1, stat='count', alpha=0.9)
    p <- p + geom_bar(aes(hour, (..count..), fill = "deepskyblue1"),binwidth = 1, stat='count', alpha=0.9,colour='black')
    
    p <- p + scale_x_continuous("Time", breaks=seq(0,23, by=1))
    p <- p + scale_y_continuous("Number of posts")
    p <- p + facet_wrap(~sender)
    p <- p + cleanTheme() +
      theme(axis.text.x = element_text(angle = 0, hjust=0.5))
    p <- p + scale_fill_identity()
    
  }
  
  else {
    p <- ggplot(data)
    # p <- p+geom_bar(aes(date, (..count..), fill = factor(year(date))),stat='count')
    # p <- p + geom_bar(aes(hour, (..count..), fill = sender),binwidth = 1, stat='count', alpha=0.9)
    p <- p + geom_density(aes(hour, (..count..), fill = sender), stat='count', alpha=0.9,show.legend=F)
    
    p <- p + scale_x_continuous("Time", breaks=seq(0,23, by=1))
    p <- p + scale_y_continuous("Number of posts")
    p <- p + facet_wrap(~sender,ncol=2)
    p <- p + cleanTheme() +
      theme(axis.text.x = element_text(angle = 0, hjust=0.5,size=12))
  }  
  p

}


senderPosts <- function(file_in='data/waChat.txt', user=NA){
  data <- parseR(in_file=file_in,user=user)
  
  postCount<-as.data.frame(cbind(table(data$sender)))
  postCount <- data.frame(names = row.names(postCount), postCount)
  rownames(postCount)<-NULL
  colnames(postCount)<-c("name", "posts")
  
  postCount <- transform(postCount, name = reorder(name, posts))
  
  if(max(postCount$posts) <= 100){
    division = 10
  }
  else if(max(postCount$posts) > 100 & max(postCount$posts) < 500){
    division = 50
  }
  else if(max(postCount$posts) > 500 & max(postCount$posts) < 1000){
    division = 100
  }
  else{
    division = 200
  }
  
  # Plot bar
  p <- ggplot(postCount)
  p <- p + geom_bar(aes(name, posts, fill = "deepskyblue1"),stat='identity')
  p <- p + scale_y_continuous("Number of posts", breaks=seq(0,max(postCount$posts),by=division),expand = c(0.01,0.05))
  p <- p + cleanTheme() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text = element_text(size=20)
    )
  p <- p + scale_fill_identity()
  p <- p + coord_flip()
  
  p
  
}
 
# senderPosts <- function(){
#   data <- parseR()
#   
#   postCount<-as.data.frame(cbind(table(data$sender)))
#   postCount <- data.frame(names = row.names(postCount), postCount)
#   rownames(postCount)<-NULL
#   colnames(postCount)<-c("name", "posts")
# 
#   postCount <- transform(postCount, name = reorder(name, -posts))
#   
#   # Plot bar
#   p <- ggplot(postCount)
#   p <- p + geom_bar(aes(name, posts),stat='identity')
#   p <- p + scale_y_continuous("Number of posts", breaks=seq(0,max(postCount$posts),by=100))
#   p <- p + cleanTheme()
#   p
#   
# }

chatCloud <- function(){
  data <- parseR()
  
  docs <- Corpus(VectorSource(data$message)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower))  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, c("omitted", "image", 'https', 'video')) %>%
    tm_map(stripWhitespace)
  
  # dataframe of terms
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  all <- data.frame(word = names(v),freq=v)

  all <- all %>%
    filter(nchar(as.character(word))>3) %>%
    filter(!grepl('http', word)) %>%
    droplevels()
  
  cloudFile='chatWordcloud.png'
  cat("Plotting", cloudFile)
  # Plot word cloud
  png("chatWordcloud.png", width=12,height=8, units='in', res=300)
  set.seed(1234)
  
  wordcloud(words = all$word, freq = all$freq, min.freq = 2,
            max.words=300, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
  dev.off()
}

wordFreq <- function(length=3){
  data <- parseR(file = 'data/DoolsWA.txt')
  
  docs <- Corpus(VectorSource(data$message)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower))  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, c("omitted", "image", 'https', 'video')) %>%
    tm_map(stripWhitespace)
  
  # dataframe of terms
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  all <- data.frame(word = names(v),freq=v)
  
  all <- all %>%
    filter(nchar(as.character(word))>length) %>%
    filter(!grepl('http', word)) %>%
    droplevels()
  
  d <- all[1:20,]
  d  <- transform(d , word = reorder(word, -freq))
  
  p <- ggplot(d)
  p <- p + geom_bar(aes(word, freq),stat='identity')
  p <- p + scale_y_continuous("Word frequency", breaks=seq(0,max(d$freq),by=50))
  p <- p + scale_x_discrete("Word")
  
  p <- p + cleanTheme() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  p
}

senderWords <- function(){
  data <- parseR()

  # postCount<-as.data.frame(cbind(table(data$sender)))
  # postCount <- data.frame(names = row.names(postCount), postCount)
  # 
  data$count <- str_count(data$message, "\\S+")
  wordCount <- ddply(data, "sender", numcolwise(sum))

  wordCount<-transform(wordCount, sender = reorder(sender, -count))
  
  p <- ggplot(wordCount)
  p <- p + geom_bar(aes(sender, count),stat='identity')
  p <- p + scale_y_continuous("Total words", breaks=seq(0,max(wordCount$count),by=1000))
  p <- p + scale_x_discrete("Sender")
  p <- p + cleanTheme()
  p

}

wordAssociation <- function(){
  data <- parseR()
  
  docs <- Corpus(VectorSource(data$message)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower))  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, c("omitted", "image", 'https', 'video')) %>%
    tm_map(stripWhitespace)
  
  # dataframe of terms
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  
  head(findFreqTerms(dtm, lowfreq = 3))
  findAssocs(dtm, terms = "off", corlimit = 0.2)
  
  data <- data %>%
    get_sentiments("nrc")
  
}
