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

correlation_ratio_test <- function(my_table, points_from_range_, confidence_level = 0.95) {
  my_table_x_range <- range(my_table[[1]]);
  points_from_range <- sort(split(points_from_range_, sapply(points_from_range_, function(arg) {
    if (arg >= my_table_x_range[1] && arg <= my_table_x_range[2]){
      return (1);
    }
    return(0);
  }))$'1');
  
  if (points_from_range[1] != my_table_x_range[1]) points_from_range <- c(my_table_x_range[1], points_from_range);
  points_from_range_length <- length(points_from_range);
  if (points_from_range[points_from_range_length] != my_table_x_range[2]) {
    points_from_range <- c(points_from_range, my_table_x_range[2]);
    points_from_range_length <- points_from_range_length + 1;
  }
  
  ranges <- matrix(nrow = points_from_range_length - 1, ncol = 2);
  
  for(n in 1:(points_from_range_length - 1)) {
    ranges[n,] <- c(points_from_range[n], points_from_range[n + 1]);
  }
  
  my_table_length <- length(my_table[[1]]);
  ranges_length <- points_from_range_length - 1;
  range_numbers <- array(dim = my_table_length);
  
  n <- 1;
  while (n <= my_table_length) {
    if (is.na(my_table[n, 1])){
      range_numbers[n] <- NA;
      n <- n + 1;
      next;
    }
    range_number <- 1;
    while(range_number <= (ranges_length - 1)){
      if (
          (ranges[range_number, 1] <= my_table[n, 1]) &&
          (my_table[n, 1] < ranges[range_number, 2])
        ){
        break;
      }
      range_number <- range_number + 1;
    }
    range_numbers[n] <- range_number;
    n <- n + 1;
  }
  
  averages <- tapply(1:my_table_length, range_numbers, function(my_table_part) {
    mean(my_table[[2]][my_table_part]);
  });
  
  ratio <- (
    sd(sapply(range_numbers, function(range_number){
      averages[as.character(range_number)];
    }))
    /
    sd(my_table[[2]])
  )^2;
  
  result <- list();
  
  result$data.description <- cat(paste("data: ", colnames(my_table)[1], "and", colnames(my_table)[2]));
  
  result$test.f <- (ratio/(ranges_length - 1))/((1 - ratio)/(my_table_length - ranges_length));
  result$test.df1 <- ranges_length - 1;
  result$test.df2 <- my_table_length - ranges_length;
  result$test.f_quantile <- qf(confidence_level, ranges_length - 1, my_table_length - ranges_length);
  result$test.p_val <- NULL;
  
  if (result$test.f <= result$test.f_quantile) {#H0
    result$hypothesis.main <- TRUE;
    result$hypothesis.alternative <- FALSE;
    result$hypothesis.description <- "main hypothesis: true ratio is equal to 0";
  } else {#H1
    result$hypothesis.main <- FALSE;
    result$hypothesis.alternative <- TRUE;
    result$hypothesis.description <- "alternative hypothesis: true ratio is not equal to 0";
  }
  
  result$conf.level <- confidence_level;
  result$conf.int <- c(qf((1 - confidence_level)/2), qf((1 + confidence_level)/2));
  
  result$conf.description <- cat(paste(result$conf.level, "percent confidence interval:", result$conf.int));
  
  result$ratio.estimate <- ratio;
  result$ratio.description <- cat(paste("ratio estimate:", result$ratio.estimate));
  
  return (ratio);
};

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
  output$Pearson_correlation_test <- renderPrint({
    my_table <- get_my_table();
    if (is.null(my_table)) return(NULL);
    
    cor_test <- cor.test(my_table[[1]], my_table[[2]], conf.level = input$confidence_level, method = "pearson");
    cor_test$data.name <- paste(colnames(my_table)[1], "and", colnames(my_table)[2]);
    return(cor_test);
  })
  
  #* 2.2.	обчислення коефіцієнта кореляційного відношення та перевірку його значущості;
  
  #* 2.3.	підрахунок рангових коефіцієнтів кореляції Спірмена та Кендалла та перевірку їх значущості.
  output$Spearman_correlation_test <- renderPrint({
    my_table <- get_my_table();
    if (is.null(my_table)) return(NULL);
    
    cor_test <- cor.test(my_table[[1]], my_table[[2]], conf.level = input$confidence_level, method = "spearman");
    cor_test$data.name <- paste(colnames(my_table)[1], "and", colnames(my_table)[2]);
    return(cor_test);
  })
  
  output$Kendall_correlation_test <- renderPrint({
    my_table <- get_my_table();
    if (is.null(my_table)) return(NULL);
    
    cor_test <- cor.test(my_table[[1]], my_table[[2]], conf.level = input$confidence_level, method = "kendall");
    cor_test$data.name <- paste(colnames(my_table)[1], "and", colnames(my_table)[2]);
    return(cor_test);
  })
  
  output$min_max <- renderPrint({
    my_table <- get_my_table();
    if (is.null(my_table)) return(NULL);
    
    return(cat(
      paste0("[min(", colnames(my_table)[1], "); max(", colnames(my_table)[1], ")]   =   [", min(my_table[[1]]), "; ", max(my_table[[1]]), "]")
    ));
  })
  
  output$select_file_text <- renderPrint({
    my_table <- get_my_table();
    if (is.null(my_table)) return(NULL);
    
    return(cat(
      paste0("Select file with vector of numbers from range [", min(my_table[[1]]), "; ", max(my_table[[1]]), "]")
    ));
  })
  
  output$correlation_ratio_test <- renderText({
    my_table <- get_my_table();
    if (is.null(my_table) || is.null(input$set_own_subranges)) return(NULL);
    
    subranges <- c();
    
    if (!input$set_own_subranges) {
      my_table_x_range <- range(my_table[[1]]);
      if (input$subrange_number <= 1) {
        subranges <- my_table_x_range;
      } else {
        range_length = (my_table_x_range[2] - my_table_x_range[1]) / input$subrange_number;
        for (n in 0:(input$subrange_number - 1)){
          subranges <- c(subranges, my_table_x_range[1] + n*range_length);
        }
        subranges <- c(subranges, my_table_x_range[2]);
      }
    } else {
      if (!is.null(input$file2_ranges) && !is.null(input$file2_ranges$datapath)){
        subranges <- read.table(input$file2_ranges$datapath, header=FALSE, sep='')[[1]];
      } else return(NULL);
    }
    
    return(correlation_ratio_test(my_table, subranges, input$confidence_level));
  })
})
