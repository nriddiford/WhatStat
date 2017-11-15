list.of.packages <- c('ggplot2', 'RColorBrewer',
                      'tm', 'SnowballC',
                      'reshape', 'wordcloud', 'stringr', 'dplyr', 'plyr', 'plotly', 'lubridate')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){
  cat('Installing missing packages...\n')
  install.packages(new.packages)
}
cat('Silently loading packages...')
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


senderDate <- function(){
  data<-parseR()
  # data$month<-lubridate::month(data$date,label = TRUE,abbr = TRUE)
  # data<-droplevels(data)
  

  if (nrow(table(data$sender))==1){
    p <- ggplot(data)
    p <- p + geom_bar(aes(date, (..count..), fill = "deepskyblue1"),binwidth = 30, stat='count',colour='black')
    p <- p + scale_y_continuous("Number of posts")
    p <- p + scale_x_date("Date", date_breaks="months", date_labels="%b")
    p <- p +cleanTheme()
    p <- p + scale_fill_identity()
    
  }
  else {
    p <- ggplot(data)
    p <- p + geom_density(aes(date, (..count..),fill=sender), stat='count', alpha=0.6,show.legend=F)
    p <- p + scale_y_continuous("Number of posts")
    p <- p + scale_x_date("Date", date_breaks="months", date_labels="%b")
    p <- p +cleanTheme()
    p <- p + facet_wrap(~sender,ncol = 2)
    
  }
  p
}


senderTime <- function () {
  data <- parseR()
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

wordFreq <- function(){
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
