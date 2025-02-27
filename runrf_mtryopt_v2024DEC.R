#' Run Random Forest with Optimized mtry Search
#' Author: Cady Lancaster
#' Last Check: 17 December 2024
#'
#' @description Optimizes `mtry` for a Random Forest model, evaluating accuracy across test sets
#'              with parallel processing support for faster computation.
#'
#' @param data A data frame containing the predictor variables and response variable.
#' @param variable A string specifying the name of the response variable.
#' @param num_iterations Number of iterations (reseeding each time).
#' @param ntree Number of trees in each random forest iteration.
#' @param datasplit Proportion of the data for the training set (e.g., 0.7 for 70% training).
#' @param mtry_range Numeric vector specifying the range of `mtry` values to test.
#' @param balance Logical. If TRUE, balances the classes in the training set.
#'
#' @return A list containing:
#'   - `mtry_df`: A data frame of `mtry` values and accuracy metrics.
#'   - `plot`: A ggplot showing accuracy trends across `mtry` values.
#'
#' @export

runrf_mtryopt <- function(data, variable, num_iterations, ntree, datasplit, mtry_range, balance = TRUE) {
  # Load required packages
  required_pkgs <- c("caret", "randomForest", "tidyverse", "future.apply","future.apply")
  lapply(required_pkgs, function(pkg) {
    if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  })

  # Setup parallel backend
  plan(multisession)  # For parallelization across available CPU cores

  # RF helper function
  runshort_rf <- function(data, variable, num_iterations, ntree, datasplit, mtry, balance) {
    if (!is.factor(data[[variable]])) data[[variable]] <- as.factor(data[[variable]])

    variable_levels <- levels(data[[variable]])
    conf_matrix <- matrix(0, nrow = length(variable_levels), ncol = length(variable_levels))
    colnames(conf_matrix) <- rownames(conf_matrix) <- variable_levels

    for (i in 1:num_iterations) {
      set.seed(123 + i)  # Reproducibility
      train_index <- createDataPartition(y = data[[variable]], p = datasplit, list = FALSE)
      training_set <- data[train_index, ]
      testing_set <- data[-train_index, ]

      if (balance) {
        min_class_size <- min(table(training_set[[variable]]))
        training_set <- training_set %>%
          group_by(!!sym(variable)) %>%
          slice_sample(n = min_class_size) %>%
          ungroup()
      }

      fit.forest <- randomForest(as.formula(paste(variable, "~ .")),
                                 data = training_set, mtry = mtry, ntree = ntree)
      predictions <- predict(fit.forest, newdata = testing_set)
      current_conf_matrix <- table(factor(predictions, levels = variable_levels),
                                   factor(testing_set[[variable]], levels = variable_levels))
      conf_matrix <- conf_matrix + as.matrix(current_conf_matrix)
    }
    overall_accuracy <- round(sum(diag(conf_matrix)) / sum(conf_matrix) * 100, 1)
    return(overall_accuracy)
  }

  # Parallel execution over mtry_range
  #cat("Starting parallel computation...\n")


  mtry_results <- future_lapply(mtry_range, function(current_mtry) {
    #cat("Evaluating mtry =", current_mtry, "\n")
    accuracy <- runshort_rf(data, variable, num_iterations, ntree, datasplit, current_mtry, balance)
    data.frame(mtry = current_mtry, overall_accuracy = accuracy)
  }, future.seed = TRUE)  # Ensures parallel-safe random numbers

  # Combine results into a data frame
  mtry_df <- do.call(rbind, mtry_results)

  # Generate plot
  library(ggplot2)
  plot <- ggplot(mtry_df, aes(x = mtry, y = overall_accuracy)) +
    geom_line(color = "blue") +
    geom_point(size = 2) +
    labs(title = "Overall Accuracy vs mtry", x = "mtry", y = "Overall Accuracy") +
    theme_minimal()

  #cat("Optimization complete.\n")
  return(list(mtry_df = mtry_df, plot = plot))
}
