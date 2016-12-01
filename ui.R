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
        sliderInput("alpha", "significance level (alpha):", 
            min = 0.001, max = 0.999, value = 0.05, step= 0.001)
        
      ),
      mainPanel(
        plotOutput('correlation_field')
      )
    )
      
  )
  
#)
