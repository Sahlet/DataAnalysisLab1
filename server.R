library(shiny)
library(moments)

primary_statistical_analysis <- function(vec, confidence_level = 0.95, vec_name = "vec") {
  size_vec <- length(vec);
  t_quantile <- qt((1 + confidence_level) / 2, size_vec - 1);
  
  mean_vec <- mean(vec);
  sd_vec <- sd(vec);
  #асимметрия
  skewness_vec <- skewness(vec);
  #эксцесс
  kurtosis_vec <- kurtosis(vec);
  
  mean_deviation <- t_quantile*sd_vec/sqrt(size_vec);
  sd_deviation <- t_quantile*sd_vec/sqrt(2*size_vec);
  skewness_deviation <- t_quantile*sqrt(6*(size_vec - 2)/((size_vec + 1)*(size_vec + 3)));
  kurtosis_deviation <-
    t_quantile*sqrt(
      24*size_vec*(size_vec - 2)*(size_vec - 3)
      /
      (((size_vec + 1)^2)*(size_vec + 3)*(size_vec + 5))
    );
  
  cat(
    vec_name,":\n",
    "\tmean(", vec_name, ") = ", mean_vec,"\n",
    "\tconfidence interval = (", mean_vec - mean_deviation, ", ", mean_vec + mean_deviation, ")\n",
    "\n",
    "\tstandard deviation(", vec_name, ") = ", sd_vec,"\n",
    "\tconfidence interval = (", sd_vec - sd_deviation, ", ", sd_vec + sd_deviation, ")\n",
    "\n",
    "\tskewness(", vec_name, ") = ", skewness_vec,"\n",
    "\tconfidence interval = (", skewness_vec - skewness_deviation, ", ", skewness_vec + skewness_deviation, ")\n",
    "\n",
    "\tkurtosis(", vec_name, ") = ", kurtosis_vec,"\n",
    "\tconfidence interval = (", kurtosis_vec - kurtosis_deviation, ", ", kurtosis_vec + kurtosis_deviation, ")\n",
    "\n",
    sep = ""
  )
}


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
   
  #* **1.**	Побудову кореляційного поля
  output$correlation_field <- renderPlot({
    my_table <- get_my_table();
    if (!is.null(my_table)){
      return(plot(my_table));
      #return(hist(my_table[[2]]));
      #return(hist(log(my_table[[1]])));
      #return(plot(log(my_table[[1]]), my_table[[2]]));
      #return(plot(my_table[[1]], exp(my_table[[2]])));
    }
    return(NULL);
  })
  
  #і проведення первинного статистичного аналізу окремих ознак об’єкта (точкове та інтервальне оцінювання середнього, середньоквадратичного, коефіцієнтів асиметрії та ексцесу).
  output$primary_statistical_analysis <- renderPrint({
    my_table <- get_my_table();
    if (is.null(my_table)) return(NULL);
    cat(
      cat("sample size is", length(my_table[[1]]), "\n\n"),
      primary_statistical_analysis(my_table[[1]], input$confidence_level, colnames(my_table)[1]),
      cat("\n"),
      primary_statistical_analysis(my_table[[2]], input$confidence_level, colnames(my_table)[2]),
      sep = ""
    );
  })
  
  #* 2.1.	знаходження оцінки коефіцієнта кореляції, перевірку його значущості та призначення довірчого інтервалу (у випадку значущості);
  output$correlation <- renderPrint({
    my_table <- get_my_table();
    if (is.null(my_table)) return(NULL);
    
    cor_test <- cor.test(my_table[[1]], my_table[[2]], conf.level = input$confidence_level);
    cor_test$data.name <- paste(colnames(my_table)[1], "and", colnames(my_table)[2]);
    return(cor_test);
  })
  
  #* 2.2.	обчислення коефіцієнта кореляційного відношення та перевірку його значущості;
  
  #* 2.3.	підрахунок рангових коефіцієнтів кореляції Спірмена та Кендалла та перевірку їх значущості.
  #cor(my_table, method="spearman")
  
  return(NULL);
})
