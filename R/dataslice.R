#' dataslice: Subset Data Based on Variable Importance
#' Last check: 17 DEcember 2024
#' This function subsets the input data based on variable importance derived from a random forest model.
#'
#' @param data A data frame containing the original data.
#' @param datakeep A character vector specifying the names of the variables to keep.
#' @param mean_importance A data frame containing the variable importance scores.
#' @param groupingvariable A string specifying the name of the grouping variable in `mean_importance`.
#' @param slice A numeric value between 0 and 1 indicating the fraction of top important variables to keep.
#' @return A data frame containing the subset of the original data with important variables.
#' @examples
#' # Example usage:
#' data(iris)
#' mean_importance <- data.frame(variable = names(iris)[-5], MeanDecreaseAccuracy = runif(4))
#' dataslice(iris, "Species", mean_importance, "variable", 0.25)
#'
#'
#' data_balanced (dataframe)
#' datakeep <- colnames(data_balanced)[1:3]
#' mean_importance <-forest_balanced$mean_importance #importance df generated from run_rf
#' slice <- 0.1 the top portion of importance variables ranked
#'
#' @export
dataslice <- function(data, variable, datakeep, mean_importance, slice) {

  # Assuming datakeep contains column indices, convert them to column names
  datakeep_names <- colnames(data)[datakeep]


  # # Group by 'variable' and calculate the mean decrease in accuracy
  # aggregated_importance <- mean_importance %>%
  #   group_by(variable) %>%
  #   summarize(MeanDecreaseAccuracy = mean(MeanDecreaseAccuracy, na.rm = TRUE)) %>%
  #   ungroup()


  # Group by 'variable' and calculate the mean decrease in accuracy
  aggregated_importance <- mean_importance

  # Print dimensions and head of 'aggregated_importance' for debugging
  print(dim(aggregated_importance))
  #print(head(aggregated_importance))

  # Calculate the number of top variables to keep based on the 'slice' fraction
  n_top_variables <- aggregated_importance %>%
    arrange(desc(MeanDecreaseAccuracy)) %>%
    slice_head(n = round(slice * nrow(aggregated_importance))) %>%
    pull(variable)
  print(head(n_top_variables))
  # Subset the data frame
  datasliced <- data[, c(datakeep, n_top_variables), drop = FALSE]

  # Print dimensions of the filtered data for debugging
  print(dim(datasliced))

  return(datasliced)
}

