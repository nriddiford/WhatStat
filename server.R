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
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either ex press or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# options(warn=-1)
library(shiny)
source("R/whatStat.R")

shinyServer(function(input, output, session) {
   
  observe({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
  d <- parseR(inFile$datapath)
  
  output$contents <- DT::renderDataTable({
    renderDataTable(d)
  })

  
  # tabPanel 1 - Number of messages
  output$postCount <-renderPlot({
    senderPosts(d=d)
  })

  # tabPanel 2 - Top words
  observe({
    # d = data()
    updateSelectInput(session, inputId = 'wlength', label = 'Minimum word length',
                      choices = c(3:5), selected = 3)
  })

  output$wordCount <-renderPlot({
    wordFreq(d = d, wordlength=input$wlength)

  })

  # tabPanel 3 - Word cloud
  observe({
    # d = data()
    updateSelectInput(session, inputId = 'user', label = 'Sender',
                      choices = c("All", levels(d$sender)), selected = 'NA')
    updateSelectInput(session, inputId = 'cwlength', label = 'Minimum word length',
                      choices = c(3:5), selected = 3)
  })

  output$wCloud <-renderPlot({
    chatCloud(d = d ,user=input$user, wordlength=input$cwlength)

  })

  observe({
    # d = data()
    updateSelectInput(session, inputId = 'Tuser', label = 'Sender',
                      choices = c("All", levels(d$sender)), selected = 'NA')
  })

  # tabPanel 4 - Messages throughout the day
  output$timePlot <-renderPlot({
    senderTime(d = d, user=input$Tuser)
  })

  observe({
    # d = data()
    updateSelectInput(session, inputId = 'Duser', label = 'Sender',
                      choices = c("All", levels(d$sender)), selected = 'NA')

    updateSelectInput(session, inputId = 'Dyear', label = 'Year',
                      choices = levels(factor(year(d$date))))
  })

  # tabPanel 5 - Messages throughout years
  output$datePlot <-renderPlot({
    senderDate(d = d, user=input$Duser,filtYear=input$Dyear)
  })
  # session$onSessionEnded(stopApp)
  
  # tabPanel 5 - Sentiments
  observe({
    updateSelectInput(session, inputId = 'method1', label = 'Method',
                      choices = c("nrc", 'bing', 'loughran'), selected = 'loughran')
    updateSelectInput(session, inputId = 'top_sender', label = 'Top n senders',
                      choices = c(1:10), selected = 5)
  })
  
  output$sentiments <-renderPlot({
    chatSentiments(d = d, top_sender = input$top_sender, method = input$method1)
  })
})
})
