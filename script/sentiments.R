library(tidytext)
library(dplyr)
library(ggplot2)
library(forcats)


plotSentiments <- function(file_in='data/WhatsApp Chat - The Doolies.zip', wordlength=3, plotly=F){
  
  data <- parseR(in_file=file_in)
  
  df2 <- data %>%
    mutate(month = month(ymd(date), label=T))
  
  tokens <- df2 %>% 
    group_by(sender, month) %>% 
    mutate(text = message) %>% 
    unnest_tokens(word, text) %>% 
    # mutate(chatter = sum(word)) %>% 
    ungroup() %>% 
    select(sender, word, month) %>% 
    droplevels()
  
  excludedWords <- c("omitted", "image", 'video', 'media')
  
  filtToks <- tokens %>% 
    filter(!word %in% excludedWords) %>% 
    droplevels()
    
  sentimentedTokens <- filtToks %>%
    group_by(sender) %>% 
    inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
    count(sentiment, sort =T) %>% 
    ungroup()
  
  senByMonth <- sentimentedTokens %>% 
    as.data.frame() %>% 
    mutate(sentiment = factor(sentiment)) %>% 
    # mutate(n = ifelse(sentiment=='negative', paste('-', n, sep=''), n)) %>% 
    mutate(count = as.numeric(n)) %>% 
    select(sender, sentiment, count) %>% 
    droplevels()
  
  senByMonth <- senByMonth %>% 
    group_by(sender) %>% 
    mutate(total=sum(abs(count))) %>% 
    mutate(perc = abs(count)/total*100) %>% 
    mutate(perc = round(perc)) %>% 
    arrange(-total)
  
  if(!plotly){
    p <- ggplot(senByMonth)
    p <- p + geom_bar(aes(fct_reorder(sender, perc), perc, fill = sentiment), stat = "identity")
    p <- p + guides(fill = FALSE)
    p <- p + cleanTheme() +
      theme(
        panel.grid.major.y = element_line(color = "grey80", size = 0.5, linetype = "dotted"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_blank())
    p <- p + facet_wrap(~sentiment)
    p <- p + scale_y_continuous("Percentage of words", breaks=seq(0,100, by=10))

  } else {
    
    p <- plot_ly(data = senByMonth,
                 x = ~fct_reorder(sender, -abs(count)),
                 y = ~count,
                 type = 'bar',
                 # showlegend = FALSE,
                 # type = 'scatter',
                 # mode = 'lines',
                 barmode = 'stack',
                 # height = 1200,
                 # width = 1000,
                 frame = ~month,
                 # text = ~paste("Feature: ", feature, "\n",
                 #               "Observed: ", observed, "\n",
                 #               "Expected: ", expected, "\n",
                 #               "P-val: ", p_val, "\n",
                 #               "Adj. P-val: ", padj, "\n",
                 #               "eScore: ", eScore, "\n"),
                 color = ~sentiment,
                 colors = "Spectral"
    ) 
  }
  p
}
