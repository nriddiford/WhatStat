
suppressMessages(library(ggplot2))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(tools))
suppressMessages(library(RColorBrewer))
suppressMessages(library(tm))
suppressMessages(library(SnowballC))
suppressMessages(library(reshape))
suppressMessages(library(stringr))
suppressMessages(library(VennDiagram))
suppressMessages(library(lubridate))
suppressMessages(library(wordcloud))
suppressMessages(library(scales))
suppressMessages(library(stringi))
library(tidytext)
library(forcats)

#' parseR
#'
#' The main parsing function for extracting a dataframe from whatsApp chat log 
#' @param in_file Whats App chat log
#' @param user Select a user
#' @keywords parse
#' @import stringi, dplyr, stringr, lubridate
#' @export
#' 
parseR <- function(in_file='data/testChat.txt',drop="44", user=NA){
  
  if(file_ext(in_file)=='zip'){
    # cat("Zipped file")
    unzipped <- unzip(in_file)
    # suppressMessages(rawData<-unlist(read_table(in_file)))
    rawData<-scan(unzipped, what="", sep="\n")
    if (file.exists(unzipped)) file.remove(unzipped)
  } else{
    rawData<-scan(in_file, what="", sep="\n")
  }
  
  joinedData <- rep(NA, length(rawData))
  
  gr <- 1
  for (i in 1:length(rawData)) {
    # if starting with timestamp, save into out and move on (gr)
    find.startline <- regexpr("^\\[?\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}", rawData[i], perl = TRUE)
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
  
  
  # get rid of square brackets around datetime
  if(grepl("^\\[", joinedData$V1[5], perl = TRUE)){
    joinedData$V1 <- sub("^\\[.*?", "", joinedData$V1)
    joinedData$V1 <- sub("\\].*?", ":", joinedData$V1)
  }
  
  
  phonetype = 'iPhone'
  if(str_split(head(joinedData$V1,1), " ",simplify = TRUE)[,3] == "-"){
    phonetype = 'android'
  }
  
  if(phonetype == 'android'){
    sepData<-suppressWarnings(separate(joinedData, V1, c("datetime", "message"), sep = ": ", extra = "merge"))
    sepData<-suppressWarnings(separate(sepData, datetime, c("datetime", "sender"), sep = "- ", extra = "merge"))
  } else {
    sepData<-suppressWarnings(separate(joinedData, V1, c("datetime", "sender", "message"), sep = ": ", extra = "merge"))
  }
  
  sepData$message<- stringi::stri_trans_general(sepData$message, "latin-ascii")
  
  sepData$message <- trimws(sepData$message)
  # sepData$message<-gsub("/", " ", sepData$message)
  
  filtData <- sepData %>%
    group_by(sender) %>%
    filter(n() >= 2) %>%
    ungroup() %>%
    filter(!str_detect(sender, 'changed|left|added|created')) %>%
    filter(!grepl('\\+', sender)) %>%
    filter(!is.na(message)) %>%
    mutate(sender = as.factor(sender)) %>%
    droplevels()
  
  ## ?
  #user <- ifelse(filtData$user, user, 'NA')
  
  if(phonetype == 'android'){
    suppressWarnings(filtData$datetime<-dmy_hm(filtData$datetime))
  } else {
    suppressWarnings(filtData$datetime<-dmy_hms(filtData$datetime))
  }
  
  cleanData<-separate(filtData, datetime, c("date", "time"), sep = " ", remove =TRUE)
  # cleanData$date<-dmy(cleanData$date)
  # cleanData$time<-hms(cleanData$time)
  
  return(cleanData)
}


#' makeCorpus
#'
#' Make a word corpus using tm pacakge from a datframe 
#' @param d A dataframe containing messages
#' @param wordlength Minimum word length 
#' @keywords corpus
#' @import tm, SnowballC, dplyr
#' @export
#' 
makeCorpus <- function(d, wordlength=4){
  
  excludedWords <- c("omitted", "image", 'video', 'media')
  
  docs <- Corpus(VectorSource(d$message)) %>%
    tm_map(content_transformer(htmlStrip)) %>%  # removing email ids
    tm_map(content_transformer(RemoveEmail)) %>%  # removing email ids
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower))  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, excludedWords) %>%
    tm_map(stripWhitespace)
  
  # dataframe of terms
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  all <- data.frame(word = names(v),freq=v)
  
  all <- all %>%
    filter(nchar(as.character(word))>=wordlength) %>%
    droplevels()
  
  return(all)
  
}


#' cleanTheme
#'
#' Clean theme for ggploting 
#' @param base_size Set the base size for text
#' @keywords clean
#' @export
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
    axis.title.x=element_text(size=15),
    axis.title.y=element_text(size=15),
    strip.text = element_text(size=15)
  )
}

#' gg_colour_hue
#'
#' Emulate the defualt ggplot colour hue 
#' @param n Number of colours to generate
#' @keywords colour
#' @export
gg_colour_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#' RemoveEmail
#'
#' Strip emails addresses from string
#' @param x String
#' @keywords emails
#' @import stringr
#' @export
RemoveEmail <- function(x) {
  str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")
}


#' htmlStrip
#'
#' Strip html from string
#' @param x String
#' @keywords html
#' @export
htmlStrip <- function(y) {
  return(gsub("<.*?>", "", y))
}




senderPosts <- function(file_in='data/testChat.txt', d=NA){
  
  ifelse(is.na(d),
         data <- parseR(in_file=file_in),
         data <- d)
  
  
  postCount<-as.data.frame(cbind(table(data$sender)))
  postCount <- data.frame(names = row.names(postCount), postCount)
  rownames(postCount)<-NULL
  colnames(postCount)<-c("name", "posts")
  
  postCount <- transform(postCount, name = reorder(name, posts))
  
  division <- ceiling(max(postCount$posts)/10)
  
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


wordFreq <- function(file_in='data/testChat.txt', wordlength=3, d=NA){
  
  ifelse(is.na(d),
         data <- parseR(in_file=file_in),
         data <- d)
  
  all <- makeCorpus(d=data, wordlength = wordlength)
  
  d <- all[1:15,]
  d  <- transform(d , word = reorder(word, freq))
  
  division <- ceiling(max(d$freq)/10)
  
  if(max(d$freq) <= 10){
    division = 2
  }
  else if(max(d$freq) > 10 & max(d$freq) < 25){
    division = 5
  }
  else if(max(d$freq) > 25 & max(d$freq) < 100){
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
  p <- p + scale_fill_identity()
  p <- p + coord_flip()
  p
}


chatCloud <- function(file_in='data/testChat.txt',user=NA,wordlength=3, d=NA){
  
  ifelse(is.na(d),
         data <- parseR(in_file=file_in, user=user),
         data <- d)
  
  if(user=='All'){
    user=NA
  } 
  
  if(!is.na(user)){
    if(user %in% levels(data$sender)) {
      data <- filter(data, sender==user)
    }
  }
  
  all <- makeCorpus(d=data, wordlength = wordlength)
  
  wordcloud(words = all$word, freq = all$freq, min.freq = 1,
            max.words=80, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"),scale=c(4,.5))
}


senderTime <- function (file_in='data/testChat.txt', user=NA, d=NA) {
  
  if(user=='All'){
    user=NA
  }
  
  ifelse(is.na(d),
         data <- parseR(in_file=file_in, user=user),
         data <- d)
  
  allData <- data
  allData$time <- hms(allData$time)
  allData$hour<-lubridate::hour(allData$time)
  
  maxPosts<-max(table(allData$hour))
  
  if(!is.na(user)){
    if(user %in% levels(data$sender)) {
      data <- filter(data, sender==user)
    }
  }
  
  
  data$time <- hms(data$time)
  data$hour<-lubridate::hour(data$time)
  labs<-c("12am", "", "2am", "", "4am", "", "6am", "", "8am", "", "10am", "", "12pm", "", "2pm", "", "4pm", "", "6pm", "", "8pm", "", "10pm", "")
  
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
  p
}

senderDate <- function(file_in='data/testChat.txt',user=NA,filtYear=NA, d=NA){
  
  if(user == "All") user <- NA
  
  ifelse(is.na(d),
         data <- parseR(in_file=file_in, user=user),
         data <- d)
  
  allData <- data
  allData$date <- ymd(allData$date)
  allData$year<-year(allData$date)
  allData$month<-month(allData$date,label = TRUE,abbr = TRUE)
  
  maxPosts<-max(table(week(allData$date),allData$year))
  
  n<-length(levels(allData$sender))
  cols = gg_colour_hue(n)
  
  for(i in seq(from=1, to=n, by=1)){
    # cat(i, levels(allData$sender)[i], "\n")
  }
  
  if( !is.na(user) ){
    if(user %in% levels(data$sender)) {
      data <- filter(data, sender==user)
    }
  }
  
  data$date <- ymd(data$date)
  data$year<-year(data$date)
  data <- filter(data, year == filtYear)
  
  data$month<-month(data$date,label = TRUE,abbr = TRUE)
  
  months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  p <- ggplot(data, aes(as.Date(date), fill=sender))
  p <- p + geom_area(aes(group = sender, colour = sender), stat='bin',position="stack",binwidth=14, alpha = 0.5)
  
  if(!is.na(user)){
    p <- p + scale_y_continuous("Number of posts", limits=c(0, maxPosts))
  }
  else{
    p <- p + scale_y_continuous("Number of posts")
  }
  p <- p + scale_x_date(date_breaks="1 month", date_labels="%B", expand=c(0,0))
  p <- p + cleanTheme() +
    theme(axis.text.x = element_text(angle = 90, hjust=1,vjust = 0.5),
          panel.grid.major.y = element_line(color="grey80", size = 0.5, linetype = "dotted"),
          axis.title.x=element_blank(),
          legend.position="bottom"
    )
  
  # p <- p + scale_fill_manual(values=cols)
  p
  
}


chatSentiments <- function(file_in='data/testChat.txt', d=NA, sender = NA, top_sender = 5, method='loughran'){
  
  ifelse(is.na(d),
         data <- parseR(in_file=file_in),
         data <- d)
  
  if(is.na(sender)) {
    # Get the top 5 recipients
    topRecips <- data %>%
      group_by(sender) %>% 
      tally() %>% 
      top_n(n=top_sender)
    
    
    filtData <- data %>% 
      filter(sender %in% topRecips$sender) %>% 
      droplevels()
  } else {
    filtData <- data %>% 
      filter(sender == sender) %>% 
      droplevels()
  }
  
  tokens <- filtData %>% 
    group_by(sender) %>% 
    mutate(text = message) %>% 
    unnest_tokens(word, text) %>% 
    ungroup() %>% 
    select(sender, word) %>% 
    droplevels()
  
  excludedWords <- c("omitted", "video", "added")
  
  filtToks <- tokens %>% 
    filter(!word %in% excludedWords) %>% 
    select(sender, word) %>%
    droplevels()
  
  sentimentedTokens <- filtToks %>%
    group_by(sender) %>% 
    inner_join(get_sentiments(method)) %>%
    dplyr::count(sentiment) 
  
  senByto <- sentimentedTokens %>% 
    group_by(sender) %>% 
    as.data.frame() %>% 
    mutate(sentiment = factor(sentiment)) %>% 
    mutate(count = as.numeric(n)) %>% 
    select(sender, sentiment, count) %>% 
    droplevels()
  
  sentPerc <- senByto %>% 
    group_by(sender) %>% 
    mutate(total=sum(abs(count))) %>% 
    mutate(perc = abs(count)/total*100) %>% 
    mutate(perc = round(perc)) %>% 
    arrange(-total)
  
  p <- ggplot(sentPerc)
  p <- p + geom_bar(aes(fct_reorder(sentiment, -perc), perc, fill = sentiment), stat = "identity")
  p <- p + guides(fill = FALSE)
  p <- p + cleanTheme() +
    theme(
      panel.grid.major.y = element_line(color = "grey80", size = 0.5, linetype = "dotted"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.title.x = element_blank())
  p <- p + scale_y_continuous("Percentage of words", breaks=seq(0,100, by=10))
  p <- p + facet_wrap(~sender)
  
  p 
}
