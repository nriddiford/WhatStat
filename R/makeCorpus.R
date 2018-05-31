#' makeCorpus
#'
#' Make a word corpus using tm pacakge dataframe returned from \code{\link{parseR}}
#' @param d A dataframe containing messages created from \code{\link{parseR}}
#' @param wordlength Minimum word length
#' @keywords corpus
#' @import tm, SnowballC, dplyr
#' @export
makeCorpus <- function(d){

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

  return(all)
}
