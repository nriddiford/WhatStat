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

library(shiny)
library(rsconnect)
library(lubridate)
library(WhatStat)
# source("whatStat.R")

shinyServer(function(input, output, session) {

  observe({
    inFile <- input$file

    if(is.null(inFile)) return(NULL)

    d <- parseR(in_file=inFile$datapath)

    output$contents <- renderDT(d)

    # tabPanel 1 - Number of messages
    output$postCount <-renderPlot({
      senderPosts(chatdf = d)
    })

    # tabPanel 2 - Top words
    observe({
      updateSelectInput(session, inputId = 'wlength', label = 'Minimum word length',
                        choices = c(3:5), selected = 3)
    })

    allWords <- makeCorpus(chatdf = d)

    output$wordCount <-renderPlot({
      wordFreq(corpus=allWords, wordlength=input$wlength)
    })

    # tabPanel 3 - Word cloud
    observe({
      updateSelectInput(session, inputId = 'user', label = 'Sender',
                        choices = c("All", levels(d$sender)), selected = 'All')
      updateSelectInput(session, inputId = 'cwlength', label = 'Minimum word length',
                        choices = c(3:5), selected = 3)
    })

    output$wCloud <-renderPlot({
      chatCloud(chatdf = d, wordlength=input$cwlength, user=input$user)
    })

    # tabPanel 4 - Messages throughout the day
    observe({
      updateSelectInput(session, inputId = 'Tuser', label = 'Sender',
                        choices = c("All", levels(d$sender)), selected = 'All')
    })

    output$timePlot <-renderPlot({
      senderTime(chatdf = d, user=input$Tuser)
    })

    # tabPanel 5 - Messages throughout years
    observe({
      updateSelectInput(session, inputId = 'Duser', label = 'Sender',
                        choices = c("All", levels(d$sender)), selected = 'All')

      updateSelectInput(session, inputId = 'Dyear', label = 'Year',
                        choices = c("All", levels(factor(year(d$date)))), selected = 'All')
    })

    output$datePlot <-renderPlot({
      senderDate(chatdf = d, user=input$Duser,filtYear=input$Dyear)
    })

    # tabPanel 5 - Sentiments
    observe({
      updateSelectInput(session, inputId = 'method1', label = 'Method',
                        choices = c("nrc", 'bing', 'loughran'), selected = 'loughran')
      updateSelectInput(session, inputId = 'top_sender', label = 'Top n senders',
                        choices = c(1:10), selected = 5)
    })

    output$sentiments <-renderPlot({
      chatSentiments(chatdf = d, top_sender = input$top_sender, method = input$method1)
    })
  })
  session$onSessionEnded(stopApp)

})
