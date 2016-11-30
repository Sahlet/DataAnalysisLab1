library(shiny)
#shinyUI(
  
  fluidPage(
    titlePanel("Random processes Lab1"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
#        tags$hr(),
        checkboxInput('header', 'Header', FALSE),
        radioButtons('sep', 'Separator', c( Space = '', Comma=',', Semicolon=';' ), '')

      ),
      mainPanel(
        plotOutput('correlation_field')
      )
    )
      
  )
  
#)
