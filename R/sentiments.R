#' chatSentiments
#'
#' Perform a per-user sentiment analysis using
#' @param file_in WhatsApp chat log (if reading from file)
#' @param d dataframe of chat returned from \code{\link{parseR}}
#' @param sender The sender to filter for
#' @param top_sender The number of senders to show (ranked by number of words)
#' @param method The sentiment analysis method to use
#' @import dplyr tidytext ggplot2 lubridate forcats
#' @export
chatSentiments <- function(file_in=NULL, chatdf=NULL, sender = NA, top_sender = 5, method='loughran'){

  if(length(file_in)>0)
    chatdf <- parseR(in_file=file_in)

  if(is.na(sender)) {
    # Get the top 5 recipients
    topRecips <- chatdf %>%
      group_by(sender) %>%
      tally() %>%
      top_n(n=top_sender)


    filtData <- chatdf %>%
      filter(sender %in% topRecips$sender) %>%
      droplevels()
  } else {
    filtData <- chatdf %>%
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
