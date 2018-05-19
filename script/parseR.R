# testing for parseR

library(tools)
library(stringr)
library(tidyverse)
library(lubridate)

parseR <- function(in_file='data/DoolsWA.txt',drop="44", user=NA){
  
  rawData<-unzip(in_file, list=TRUE)
  
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
  
  if(phonetype == 'android'){
    data$datetime<-dmy_hm(data$datetime)
  } else {
    data$datetime<-dmy_hms(data$datetime)
  }
  
  cleanData<-separate(data, datetime, c("date", "time"), sep = " ", remove =TRUE)
  # cleanData$date<-dmy(cleanData$date)
  # cleanData$time<-hms(cleanData$time)
  
  return(cleanData)
}