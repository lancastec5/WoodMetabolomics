#' cmstats: Calculate and Display Confusion Matrix Statistics
#' updated 22 December 2024
#' Author: Cady A Lancaster
#' This function calculates and displays various statistics from a confusion matrix, including
#' overall accuracy, precision, recall, and F1 score. It also formats the results into a table.
#' updated 24 July 2024
#'
#' This function calculates and displays various statistics from a confusion matrix, including
#' overall accuracy, precision, recall, F1 score, and training class size. The results are formatted
#' into a table for easy viewing.
#'
#' @param confusionmatrix A confusion matrix object.
#' @param training_class_size A named numeric vector specifying the training class sizes for each level.
#' @param fit.list A list of `randomForest` model objects used for training.
#' @param variable A string specifying the class variable (e.g., "cluster").
#' @param levels A character vector specifying the levels of the factor variable (e.g., Cluster 1, Cluster 2...).
#' @return A formatted table displaying the statistics.
#' @examples
#' # Example usage:
#' cmstats(confusionmatrix, training_class_size, fit.list, "Species", levels)
#'
#' @export
cmstats <- function(confusionmatrix,
                    training_class_size,
                    fit.list,
                    variable,
                    levels) {
  conflicted::conflicts_prefer(dplyr::select)
  # Check if the confusion matrix is not empty
  if (sum(confusionmatrix) > 0) {
    # Ensure confusion matrix dimensions match provided levels
    confusionmatrix <- confusionmatrix[levels, levels, drop = FALSE]

    # Align training_class_size with levels
    training_class_size <- training_class_size[levels]
    training_class_size[is.na(training_class_size)] <- 0  # Fill any missing levels with 0

    # Compute statistics
    overall_accuracy <- sum(diag(confusionmatrix)) / sum(confusionmatrix)
    predsize <- colSums(confusionmatrix) / num_iterations
    totpred <- colSums(confusionmatrix)

    # Initialize vectors for precision, recall, and F1 scores
    precision <- numeric(ncol(confusionmatrix))
    recall <- numeric(nrow(confusionmatrix))
    f1_score <- numeric(nrow(confusionmatrix))


    for (i in 1:nrow(confusionmatrix)) {
      TP <- confusionmatrix[i, i]                  # True Positives
      FP <- sum(confusionmatrix[-i, i])            # False Positives
      FN <- sum(confusionmatrix[i, -i])            # False Negatives

      precision[i] <- TP / (TP + FP)           # Precision
      recall[i] <- TP / (TP + FN)              # Recall
      f1_score[i] <- 2 * (precision[i] * recall[i]) / (precision[i] + recall[i]) # F1
    }

    # Display results
    cat("Overall Accuracy: ", overall_accuracy, "\n")
    cat("Precision: \n")
    print(precision)
    cat("Recall: \n")
    print(recall)
    cat("F1 Score: \n")
    print(f1_score)
  } else {
    cat("Confusion matrix is empty. No predictions were made.\n")
    return(NULL)
  }

  # Combine metrics into a data frame
  metrics_df <- data.frame(
    Class = colnames(confusionmatrix),
    Precision = precision,
    Recall = recall,
    F1_Score = f1_score,
    "Training Class Size" = training_class_size,
    "Test Class Size (n)" = predsize,
    "Total Predictions (n)" = totpred
  )

  # Assign the provided levels to the row names
  #row.names(metrics_df) <- levels

  # Rename columns for better display
  metrics_df <- metrics_df %>%
    rename("Validation Class size (n)" = `Test.Class.Size..n.`) %>%
    rename("Training Class size (n)" = `Training.Class.Size.Freq`) %>%
    rename("F1 Score" = `F1_Score`) %>%
    rename("Total Predictions (n)" = `Total.Predictions..n.`)

  metrics_df <- metrics_df[, -5]
  metrics_df <- metrics_df[, -1]

  # Round all numeric values to 2 decimal places
  metrics_df <- metrics_df %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))

  # Display accuracy as a percentage
  acc <- paste(round(overall_accuracy * 100, 1), "%")

  #table <- metrics_df

  # return(metrics_df)
#
#
#   table <- knitr::kable(
#     metrics_df,
#     format = "html",  # Specify the format here
#     digits = 2,
#     row.names = TRUE,
#     align = c("c", "c", "c", "c", "c", "c")
#   ) %>%
#     kableExtra::kable_styling(latex_options = "striped") %>%
#     kableExtra::column_spec(1, italic = TRUE) %>%
#     kableExtra::footnote(general = paste(
#       "Overall prediction accuracy:", acc,
#       ". Model Information: randomForest model with ", ntree,
#       " trees reseeded over ", num_iterations,
#       " iterations with an mtry = ", mtry, "."
#     )) %>%
#     kableExtra::kable_styling("condensed")
#
#
#   # Print the table
#   print(table)

  return(list(metrics_df, acc))

}
