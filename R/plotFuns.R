#' wordFreq
#'
#' Plot the word frequency in users/groups chat history
#' @param wordlength Minimum word length
#' @param corpus dataframe of words returned from \code{\link{makeCorpus}}
#' @import plyr dplyr ggplot2
#' @export
wordFreq <- function(wordlength=3, corpus){

  all <- corpus %>%
    filter(nchar(as.character(word))>=wordlength)

  d <- all[1:15,]
  d  <- transform(d , word = reorder(word, freq))

  division <- plyr::round_any(ceiling(max(d$freq)/10), 10, f = ceiling)

  if(max(d$freq)>=100){
    division <- plyr::round_any(ceiling(max(d$freq)/10), 50, f = ceiling)
  }

  if(max(d$freq)>=500){
    division <- plyr::round_any(ceiling(max(d$freq)/10), 100, f = ceiling)
  }

  p <- ggplot(d)
  p <- p + geom_bar(aes(word, freq, fill="springgreen3"),stat='identity')
  p <- p + scale_y_continuous("Word frequency", breaks=seq(0,max(d$freq),by=division),expand=c(0.01,0))
  p <- p + scale_x_discrete("Word", expand = c(0.01,0.01))

  p <- p + cleanTheme() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y = element_text(size=20),
      axis.text.x = element_text(size=15),
      panel.grid.major.x = element_line(color="grey80", size = 0.5, linetype = "dotted")

    )
  p <- p + scale_fill_identity()
  p <- p + coord_flip()
  p
}


#' chatCloud
#'
#' Plot the word frequency in users/groups chat history as a wordcloud
#' @param wordlength Minimum word length
#' @param d A dataframe containing messages created from \code{\link{parseR}}
#' @param user The user to filter for
#' @import dplyr ggplot2
#' @export
chatCloud <- function(d, user=NULL, wordlength=3){

  if(user=='All'){
    user=NULL
  }

  if(!is.null(user)){
    d <- filter(d, sender==user)
  }

  all <- makeCorpus(d)

  all <- all %>%
    filter(nchar(as.character(word))>=wordlength)

  options(warn=-1)
  wordcloud(words = all$word, freq = all$freq, min.freq = 1,
            max.words=50, random.order=FALSE,
            rot.per=0.35,
            colors=brewer.pal(6, "Paired")
  )
  options(warn=0)
}



#' senderPosts
#'
#' Plot the numer of posts per sender
#' @param file_in WhatsApp chat log (if reading from file)
#' @param d dataframe of chat returned from \code{\link{parseR}}
#' @import plyr dplyr ggplot2
#' @export
senderPosts <- function(file_in, d=NA){

  ifelse(is.na(d),
         data <- parseR(in_file=file_in),
         data <- d)

  postCount <- data %>%
    group_by(sender) %>%
    tally() %>%
    arrange(-n)

  division <- plyr::round_any(ceiling(max(postCount$n)/10), 10, f = ceiling)

  if(max(postCount$n)>=100){
    division <- plyr::round_any(ceiling(max(postCount$n)/10), 50, f = ceiling)
  }

  if(max(postCount$n)>=500){
    division <- plyr::round_any(ceiling(max(postCount$n)/10), 100, f = ceiling)
  }


  p <- ggplot(postCount)
  p <- p + geom_bar(aes(fct_reorder(sender, n), n, fill = "deepskyblue1"),stat='identity')
  p <- p + scale_y_continuous("Number of posts", breaks=seq(0,max(postCount$n),by=division),expand = c(0.01,0.05))
  p <- p + cleanTheme() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y = element_text(size=20),
      axis.text.x = element_text(size=15),
      panel.grid.major.x = element_line(color="grey80", size = 0.5, linetype = "dotted")
    )
  p <- p + scale_fill_identity()
  p <- p + coord_flip()

  p

}

#' senderTime
#'
#' Plot the time posts were sent
#' @param file_in WhatsApp chat log (if reading from file)
#' @param d dataframe of chat returned from \code{\link{parseR}}
#' @param user The user to filter for
#' @import dplyr ggplot2 lubridate
#' @export
senderTime <- function (file_in, user=NA, d=NA) {

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


#' senderDate
#'
#' Plot the date posts were sent by year
#' @param file_in WhatsApp chat log (if reading from file)
#' @param d dataframe of chat returned from \code{\link{parseR}}
#' @param user The user to filter for
#' @param filtYear The year to show
#' @import dplyr ggplot2 lubridate
#' @export
senderDate <- function(file_in, user=NA, filtYear=NA, d=NA){

  if(user == "All") user <- NA

  ifelse(is.na(d),
         data <- parseR(in_file=file_in, user=user),
         data <- d)

  allData <- data
  allData$date <- ymd(allData$date)
  allData$year <- year(allData$date)
  allData$month <- month(allData$date,label = TRUE,abbr = TRUE)

  maxPosts <- max(table(week(allData$date),allData$year))

  n<-length(levels(allData$sender))
  cols <- gg_colour_hue(n)

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
