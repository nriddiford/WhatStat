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


shinyUI(fluidPage(
  
  tags$head(HTML(
    "<script>
    (function(i,s,o,g,r,a,m){
    i['GoogleAnalyticsObject']=r;i[r]=i[r]||
    function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
    a=s.createElement(o), m=s.getElementsByTagName(o)[0];
    a.async=1;
    a.src=g;m.parentNode.insertBefore(a,m)
    })
    (window, document, 'script',
    '//www.google-analytics.com/analytics.js','ga');
    
    ga('create', 'UA-109758097-1', 'auto');
    ga('send', 'pageview');
    
    </script>"
  )),
  
  navbarPage("WhatStat"),
  
  # tabsetPanel or navlistPanel ?

  tabsetPanel(
    # Tab 1
    tabPanel("Upload File",
             titlePanel("Upload your WhatsApp chat log"),
             sidebarLayout(
               sidebarPanel(
                 # selectInput('phoneClass', 'Phone Type', ""),
                 fileInput('file1', 'Select your WhatsApp chat log',
                           accept=c(".txt", ".zip")
                           ),
                
                 tags$div(class="header", checked=NA,
                          tags$p("To see instructions on how to export your WhatsApp chat log"),
                          tags$a(href="https://github.com/nriddiford/WhatStat/blob/master/README.md", "Click Here!")
                 ),
                 tags$br()
                 
               ),
               mainPanel(
                 
                 h3("Welcome to WhatStat - an online tool to visualise your WhatsApp chats"),
                 p("To start, you need to export your WhatsApp chat log and upload it (follow link in 'Click Here!' to see further details)"),
                 p("Once the file is uploaded, click on the tabs above to see different analyses of your chat data!"),
                 p("If you're uploading a .zip file and it doesn't work, try unzipping the file first and try again"),
                 tableOutput('contents')
               )
             )
    ),
    
    # Tab 2
    tabPanel("Post Count",
             pageWithSidebar(
               headerPanel('Number of posts per user'),
               sidebarPanel(),
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
               headerPanel('Word cloud of chat'),
               sidebarPanel(
                 selectInput('user', 'Sender', ""),
                 sliderInput("cwlength", "Minimum word length",
                             min = 2, max = 10, "")
               ),
               mainPanel(
                 plotOutput('wCloud')
               )
             )
    ),
  
    # Tab 5
    tabPanel("Time of day",
             pageWithSidebar(
               headerPanel('When are messages sent?'),
               sidebarPanel(
                 selectInput('Tuser', 'Sender', "")
               ),
               mainPanel(
                 plotOutput('timePlot')
               )
             )
    ),
    
    # Tab 4
    tabPanel("Date",
             pageWithSidebar(
               headerPanel('Chat history'),
               sidebarPanel(
                 selectInput('Duser', 'Sender', ""),
                 selectInput('Dyear', 'Year', "")
               ),
               mainPanel(
                 plotOutput('datePlot')
               )
             )
    )
    
    
  )
)
)
