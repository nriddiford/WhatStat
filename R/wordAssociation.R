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