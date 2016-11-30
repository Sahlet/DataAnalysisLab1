library(shiny)

shinyServer(function(input, output) {
  
  # By declaring get_my_table as a reactive expression we ensure 
  # that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers 
  #	  (it only executes a single time)
  #
  get_my_table <- reactive({
    result <- NULL;
    if (!is.null(input$file1) && !is.null(input$file1$datapath)){
      result <- read.table(input$file1$datapath, header=input$header, sep=input$sep);
      if (!input$header) {
        colnames(result) <- c('x', 'y');
      }
    }
    return(result);
  });
   
  output$correlation_field <- renderPlot({
    my_table <- get_my_table();
    if (!is.null(my_table)){
      return(plot(my_table));
    }
    return(NULL);
  })
  
  return(NULL);
})
