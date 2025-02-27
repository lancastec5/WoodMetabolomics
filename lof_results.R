# lof_results.R

#' Scale data and calculate LOF by group
#'
#' This function scales numeric data (excluding a specified variable), calculates the Local Outlier Factor (LOF) for each group, and combines the results.
#'
#' @param data A data frame containing the dataset.
#' @param variable A character string specifying the column name to be excluded from scaling and used for grouping.
#' @param minPts An integer specifying the number of neighbors to use for LOF calculation.
#'
#' @return A data frame with LOF scores and original data.
#' @export
#'
#' @importFrom dplyr select mutate across group_by left_join
#' @importFrom dbscan lof
#' @importFrom tidyr unnest
#' @importFrom magrittr %>%
#'
#' @examples
#' # Sample data
#' data <- tibble::tribble(
#'   ~Species, ~X107.0857, ~X109.1021, ~X119.0867, ~X121.1022,
#'   "Dalbergia odorifera", 6.9, 11.4, 5.1, 7.9,
#'   "Dalbergia odorifera", 28.6, 38.3, 23.4, 30.5,
#'   "Dalbergia odorifera", 26.2, 35.1, 21.5, 27.9,
#'   "Dalbergia odorifera", 18.3, 23, 14.4, 20.5,
#'   "Dalbergia odorifera", 11.9, 13.4, 9.2, 12.4
#' )
#'
#' # Apply the function
#' lof_results <- lof_results(data, "Species", minPts = 3)
#' print(lof_results)
#' data.lof.slice <- lof_results %>% filter(lof <1.1)%>% dplyr::select(., -c(lof)) #Filter original data based on results

# Function to scale data and calculate LOF by group
lof_results <- function(data, variable, minPts) {
  # Ensure that the variable column is a factor (to handle grouping correctly)
  data[[variable]] <- as.factor(data[[variable]])
  # Add a row identifier to ensure unique rows
  data <- data %>% mutate(row_id = row_number())

  # Scale numeric data (excluding the specified variable)
  s.data <- data %>%
    dplyr::select(., -c(all_of(variable), row_id)) %>%
    dplyr::mutate(across(everything(), scale)) %>%
    bind_cols(data %>% dplyr::select(all_of(variable), row_id))

  na_columns <- colnames(s.data)[colSums(is.na(s.data)) > 0]

  # Print columns with NA values
  print("Columns with NA values:")
  print(na_columns)

  # Remove columns with NA values
  s.data <- s.data[, !colnames(s.data) %in% na_columns]

  # Calculate LOF for each group
  lof_results <- s.data %>%
    group_by(!!sym(variable)) %>%
    do(lof = data.frame(
      row_id = .$row_id,
      lof = lof(select(., -c(all_of(
        variable
      ))), minPts = minPts)
    )) %>%
    unnest(cols = lof) %>%
    dplyr::select(., -c(all_of(variable)))

  # Plot the points sorted by increasing LOF and look for a knee.
  sorted_lof <- lof_results %>% arrange(lof) %>% pull(lof)
  plot <- ggplot(tibble(index = seq_len(length(sorted_lof)), lof = sorted_lof), aes(index, lof)) +
    geom_line() +
    geom_hline(yintercept = 1,
               color = "red",
               linetype = 2) +
    geom_hline(yintercept = 2,
               color = "red",
               linetype = 2) +
    #scale_color_manual(values = colors) +
    #scale_color_gradientn(colors = colors) +
    scale_y_continuous(breaks = seq(0, 2, by = 0.1)) +
    #scale_x_continuous(breaks = seq(0, max(sorted_lof), by = 5)) +
    ylim(0.5, 5) +# Setting the Y-axis limits
    labs(
      title = paste("Local Outlier Factor: Neighborhood Size =", minPts),
      x = "Samples",
      y = "Local Outlier Factor"
    ) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_minimal()

  print(plot)

  # Combine LOF results with the original data
  lof_combined <- data %>%
    left_join(lof_results, by = "row_id") %>%
    dplyr::select(all_of(variable), lof, everything(), -row_id)
  print("Print results")


  cat("data.lof.slice <- lof_results %>% filter(lof <1.1)%>% dplyr::select(., -c(lof)) #Filter original data based on results","\n")
 return(lof_combined)

}


