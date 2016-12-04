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
        radioButtons('sep', 'Separator', c( Space = '', Comma=',', Semicolon=';' ), ''),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        sliderInput("confidence_level", "confidence level:", 
            min = 0.0001, max = 0.9999, value = 0.95, step= 0.0001)
        
      ),
      mainPanel(
        h3("Correlation field"),
        plotOutput('correlation_field'),
        br(),
        h3("Primary statistical analysis"),
        verbatimTextOutput("primary_statistical_analysis"),
        br(),
        h3("Correlation"),
        verbatimTextOutput("correlation")
      )
    )
      
  )
  
#)
