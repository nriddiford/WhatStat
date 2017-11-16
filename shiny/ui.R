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
  titlePanel("Column Plot"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Select your WhatsApp chat log',
                           accept='.txt'
                           ),
                 
                 tags$br()
                 
               ),
               mainPanel(
                 tableOutput('contents'),
                 plotOutput('messageCount')
               )
             )
    ),
    
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
    
    tabPanel("Word Frequency",
             pageWithSidebar(
               headerPanel('Most commonly used words'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('sender', 'Sender', "")
               ),
               mainPanel(
                 plotOutput('wordCount')
               )
             )
    ),
    
    tabPanel("Chat Cloud",
             pageWithSidebar(
               headerPanel('Most used words'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('sender', 'Sender', "")
               ),
               mainPanel(
                 plotOutput('chatCloud')
               )
             )
    )
    
    
  
  )
)
)
