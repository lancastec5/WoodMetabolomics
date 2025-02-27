#' test_rf: Test a New Dataset with Saved Random Forest Models
#'
#' This function loads the saved random forest models and uses them to make predictions on a new dataset.
#' It also prints the number of trees and iterations that were evaluated and stores the error rates for each iteration.
#'
#' @param new_data A data frame containing the new data to be tested.
#' @param fit.list A list of trained random forest models.
#' @param variable A string specifying the target variable.
#'
#' @examples
#'
#'# Example usage
#'# Assuming `new_data` is your test data and `fit.list` is the list of trained random forest models new_data <- data.frame(
#'   # Your new data frame here
#' )
#' fit.list <- readRDS("trained_random_forest.rds")
#' result <- test_rf(new_data, fit.list, "your_target_variable")
#' print(result$conf_matrix)
#' print(result$overall_error_rate)

#' To generate a KDE of the error rates:
#' library(ggplot2)
#' ggplot(result$fit.median, aes(x = fit.median)) +
#'   geom_density(fill = "blue", alpha = 0.5) +
#'   labs(title = "Kernel Density Estimate of Error Rates",
#'        x = "Error Rate",
#'        y = "Density") +
#'   theme_minimal()
#'
#' @return A list containing predictions, a confusion matrix, overall error rate, and error rates for each iteration.
#' @export
test_rf <- function(new_data, fit.list, class_levels, unk_levels, variable) {
  # Ensure the 'variable' column is a factor in the new data
  if (!is.factor(new_data[[variable]])) {
    new_data[[variable]] <- as.factor(new_data[[variable]])
  }

  # Initialize the confusion matrix with unknown levels as rows and class levels as columns
  conf_matrix <- matrix(0, nrow = length(unk_levels), ncol = length(class_levels))
  rownames(conf_matrix) <- unk_levels
  colnames(conf_matrix) <- class_levels

  # Get the number of iterations and trees
  num_iterations <- length(fit.list)
  num_trees <- fit.list[[1]]$ntree

  # Print the number of iterations and trees
  cat("Number of iterations:", num_iterations, "\n")
  cat("Number of trees per iteration:", num_trees, "\n")

  # Initialize lists to store results
  predictions_list <- vector("list", num_iterations)
  all_predictions <- c()
  all_actuals <- new_data[[variable]]

  # Make predictions using each model and update the confusion matrix
  for (i in seq_along(fit.list)) {
    predictions <- predict(fit.list[[i]], newdata = new_data)

    # Ensure predicted values and actual values are factors with correct levels
    predictions <- factor(predictions, levels = class_levels)
    actuals <- factor(new_data[[variable]], levels = unk_levels)

    # Compute confusion matrix and accumulate
    current_conf_matrix <- table(actuals, predictions)

    # Add to cumulative confusion matrix
    for (row in rownames(current_conf_matrix)) {
      for (col in colnames(current_conf_matrix)) {
        conf_matrix[row, col] <- conf_matrix[row, col] + current_conf_matrix[row, col]
      }
    }

    predictions_list[[i]] <- predictions
    all_predictions <- c(all_predictions, predictions)
  }

  # Convert the confusion matrix to percentages (row-wise), multiply by 100 and round to 1 decimal place
  conf_matrix_perc <- sweep(conf_matrix, 1, rowSums(conf_matrix), FUN = "/") * 100
  conf_matrix_perc[is.na(conf_matrix_perc)] <- 0  # Handle cases where row sums might be zero
  conf_matrix_perc <- round(conf_matrix_perc, 1)  # Round to 1 decimal place

  # Return the predictions and aggregated confusion matrix as percentages
  return(list(
    predictions = predictions_list,
    conf_matrix = conf_matrix_perc
  ))
}
