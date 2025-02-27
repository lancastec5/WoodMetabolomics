#' run random forest
#'
#' @description Run random forest with a new seed starting at prior to each group
#'
#' @param data the data matrix
#' @param variable response The name of the response variable (as a string).
#' @param num_iterations the number of iterations that the randomforest will reseed on
#' @param ntree are the number of trees for the randomforest to run on each iteration
#' @param datasplit the partition for the training set which subsides the test datasets
#'
#' @return random forest model as fit.forest
#' @import ggpubr
#' @examples
#' # example 1
#' graphs <- importance_graphs(importance_df, variable, slice)
#'
#' @export


importance_graphs <- function(importance_df, variable, slice) {
  # Ensure the 'variable' column is a factor
  importance_df$variable <- as.factor(importance_df$variable)

  # Aggregate variable importance scores
  aggregated_importance <- importance_df %>%
    group_by(variable) %>%
    summarize(MeanDecreaseAccuracy = mean(MeanDecreaseAccuracy, na.rm = TRUE)) %>%
    arrange(desc(MeanDecreaseAccuracy))

  # Check if aggregated_importance is empty
  if (nrow(aggregated_importance) == 0) {
    stop("Aggregated importance dataframe is empty. Check your input data.")
  }

  # Sort by MeanDecreaseAccuracy and select top variables
  top_slice_variables <- aggregated_importance %>%
    arrange(desc(MeanDecreaseAccuracy)) %>%
    slice_head(n = round(slice * nrow(aggregated_importance)))

  # Create a column to indicate top variables
  aggregated_importance <- aggregated_importance %>%
    mutate(is_top_50 = ifelse(variable %in% top_slice_variables$variable, TRUE, FALSE))

  # Create the plot for total importance distribution
  importance_total <- ggplot(aggregated_importance,
                             aes(x = reorder(variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
    geom_bar(stat = "identity", aes(fill = is_top_50)) +
    scale_fill_manual(values = c("grey", "steelblue"), guide = FALSE) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
    labs(title = "Variable Importance Distribution", x = "", y = "Mean Decrease in Accuracy")

  # Create the plot for top sliced variables
  importance_slice <- ggplot(top_slice_variables,
                             aes(x = variable, y = MeanDecreaseAccuracy)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
    labs(title = paste("Top", round(slice * 100), "% Most Important Variables"),
         x = "Variable", y = "Mean Decrease in Accuracy")

  # Arrange plots using ggarrange
  arranged_plots <- ggarrange(importance_total, importance_slice,
                              ncol = 1, nrow = 2)


  # Print the arranged plot
  print(arranged_plots)

  # Return a list containing both plots and top slice variables
  return(list(
    importance_slice = importance_slice,
    importance_total = importance_total,
    top_slice_variables = top_slice_variables
  ))
}
