#' class_counts: Calculate and Print Class Counts
#' LAst update: 24 July 2024
#' This function calculates the counts of each unique value in a specified variable within a data frame.
#' It prints the sum of these counts and returns the counts as a table.
#'
#' @param data A data frame containing the data.
#' @param variable A string specifying the name of the variable for which class counts are to be calculated.
#' @return A table of counts for each unique value in the specified variable.
#' @examples
#' # Example usage:
#' data <- data.frame(variable = sample(letters[1:5], 100, replace = TRUE))
#' counts <- class_counts(data, "variable")
#' print(counts)
#' @export
class_counts <- function(data, variable) {
  # Create a table of counts for the specified variable
  counts <- table(data[[variable]])

  # Print the sum of counts
  cat("Sum of counts:", sum(counts), "\n")

  # Convert counts to a data frame
  counts_df <- as.data.frame(counts)
  colnames(counts_df) <- c(variable, "Count")

  # Create a formatted table using knitr and kableExtra
  formatted_table <-
    kableExtra::kable(
      counts_df,
      format = "html",
      #caption = "Class Counts",
      align = c("c", "c")
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = F
    ) %>%
    #kableExtra::add_header_above(c(" " = 1, "Class Count Table" = 1))%>%
    kableExtra::kable_styling("condensed")

  # Print the formatted table
  print(formatted_table)

  # Return the counts as a data frame
  return(counts_df)

  # Return the counts
  return(counts)
}
