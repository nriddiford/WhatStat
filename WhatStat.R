list.of.packages <- c('ggplot2', 'RColorBrewer',
                      'tm', 'SnowballC',
                      'reshape', 'wordcloud', 'stringr', 'plotly')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){
  cat('Installing missing packages...\n')
  install.packages(new.packages)
}
cat('Silently loading packages...')
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(RColorBrewer))
suppressMessages(library(tm))
suppressMessages(library(SnowballC))
suppressMessages(library(reshape))
suppressMessages(library(plotly))
suppressMessages(library(stringr))
suppressMessages(library(VennDiagram))
suppressMessages(library(lubridate))



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
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
  )
}

parseR <- function(file='DoolsWA.txt',drop="44"){
  rawData <- read.delim(file, quote = "", 
                  row.names = NULL, 
                  stringsAsFactors = FALSE,
                  header = F)
  
  
  # remove blank lines
  # rawData<-rawData[!apply(rawData == "", 1, all),]
  
  # join multi line messages into single line
  # rawData$V1<-gsub("[\r\n]", "Hello", rawData$V2)
  
  sepData<-separate(rawData, V1, c("datetime", "sender", "message"), sep = ": ", extra = "merge")
  
  # newColNames <- c("date", "time")
  # newCols <- colsplit(sepData$datetime, ", ", newColNames)
  # sepData <- cbind(sepData, newCols)
  
  sepData$message <- trimws(sepData$message)
  sepData$sender<-factor(sepData$sender)
  
  data<- sepData %>% 
    filter(!is.na(message)) %>%
    filter(!grepl(drop, sender)) %>%
    droplevels()
  
    # select(-datetime) %>%
    # unite(date_time, date, time, remove = TRUE)
  
  # data$date_time<-strsplit(data$date_time, '_')
  data$datetime<-dmy_hms(data$datetime,tz=NULL)
  
  cleanData<-separate(data, datetime, c("date", "time"), sep = " ", remove =TRUE)
  cleanData$date<-ymd(cleanData$date)
  cleanData$time<-hms(cleanData$time)

  return(cleanData)
}

wordFreq <- function(){
  data<-parseR()
  data$wordCount<-str_count(data$message, '\\w+')

  p <- ggplot(data)
  # p <- p+geom_bar(aes(date, (..count..), fill = factor(year(date))),stat='count')
  p <- p+geom_bar(aes(date, (..count..), fill = sender),binwidth = 30, stat='count')
  
  p <- p+scale_x_date(date_breaks="months", date_labels="%b")
  p <- p +cleanTheme()
  p
}


# require(stringr)
# nwords <- function(string, pseudo=F){
#   ifelse( pseudo, 
#           pattern <- "\\S+", 
#           pattern <- "[[:alpha:]]+" 
#   )
#   str_count(string, pattern)
# }
# 
# cleanData$words3<-nwords(cleanData$message)
