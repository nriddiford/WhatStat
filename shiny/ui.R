library(shiny)

suppressMessages(library("wordcloud"))
shinyUI(fluidPage(
  titlePanel("Column Plot"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
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
    )
    
    
  
  )
)
)
