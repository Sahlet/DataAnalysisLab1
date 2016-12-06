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
        conditionalPanel(
          condition = "output.primary_statistical_analysis != 'NULL'",
          
          br(),
          h3("Pearson correlation test"),
          verbatimTextOutput("Pearson_correlation_test"),
          br(),
          h3("Spearman correlation test"),
          verbatimTextOutput("Spearman_correlation_test"),
          br(),
          h3("Kendall correlation test"),
          verbatimTextOutput("Kendall_correlation_test"),
          br(),
          h3("Correlation index (correlation ratio) test"),
          inputPanel(
            verbatimTextOutput("min_max"),
            br(),
            checkboxInput('set_own_subranges', 'Own subranges', FALSE),
            br(),
            conditionalPanel(
              condition = "input.set_own_subranges == false",
              sliderInput("subrange_number", "subrange number", value = 5, min = 2, max = 200, step = 1)
            ),
            br(),
            conditionalPanel(
              condition = "input.set_own_subranges == true",
              textOutput("select_file_text"),
              fileInput('file2_ranges', '',
                        accept=c('text/csv', 
                                 'text/comma-separated-values,text/plain', 
                                 '.csv')
              )
            )
          ),
          verbatimTextOutput("correlation_ratio_test")
        )
      )
    )
      
  )
  
#)
