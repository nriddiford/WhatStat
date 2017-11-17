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
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(shiny)

suppressMessages(library("wordcloud"))
shinyUI(fluidPage(
  titlePanel("WhatStat"),
  tabsetPanel(
    # Tab 1
    tabPanel("Upload File",
             titlePanel("Upload your WhatsApp chat log"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Select your WhatsApp chat log',
                           accept=c(".txt", ".zip")
                           ),
                 
                 
                 tags$div(class="header", checked=NA,
                          tags$p("To see instructions of how to export your WhatsApp chat log"),
                          tags$a(href="https://github.com/nriddiford/WhatStat/blob/master/README.md", "Click Here!")
                 ),
                 tags$br()
                 
               ),
               mainPanel(
                 tableOutput('contents')
               )
             )
    ),
    
    # Tab 2
    tabPanel("Post Count",
             pageWithSidebar(
               headerPanel('Number of posts per user'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('sender', 'Sender', "")
               ),
               mainPanel(
                 plotOutput('postCount')
               )
             )
    ),
    
    # Tab 3
    tabPanel("Word Frequency",
             pageWithSidebar(
               headerPanel('Most common words'),
               sidebarPanel(
                 
                 sliderInput("wlength", "Minimum word length",
                             min = 2, max = 10, "")
                 # "Empty inputs" - they will be updated after the data is uploaded
                 # selectInput('wlength', 'Word length', "")
               ),
               mainPanel(
                 plotOutput('wordCount')
               )
             )
    ),
    
    # Tab 4
    tabPanel("Word Cloud",
             pageWithSidebar(
               headerPanel('Chat cloud of most common words'),
               sidebarPanel(
                 selectInput('user', 'Sender', ""),
                 sliderInput("cwlength", "Minimum word length",
                             min = 2, max = 10, "")
               ),
               mainPanel(
                 plotOutput('wCloud')
               )
             )
    )
    
  )
)
)
