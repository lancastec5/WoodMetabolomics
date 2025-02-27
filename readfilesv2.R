#' @title Read and Process Delimited Files
#'
#' @description
#' Reads a list of files, automatically detects the delimiter (space, tab, or comma),
#' and processes them into a standardized format. The function ensures data integrity
#' by handling empty rows, missing values, and normalizing the second column.
#'
#' @param fnames A character vector of file names (including paths if necessary).
#'
#' @return A list of data frames, each containing two processed columns (`V1` and `V2`).
#'
#' @details
#' - Detects and applies the correct delimiter (`space`, `tab`, or `comma`).
#' - Removes empty first rows and ensures at least two columns are present.
#' - Normalizes the second column (`V2`) to a 0–100 scale.
#' - Returns a list of processed data frames.
#'
#' @note
#' - Files without at least two columns will generate an error.
#' - If a file contains all zero values in the second column, normalization is not possible.
#'
#' @examples
#' \dontrun{
#' files <- c("data1.txt", "data2.csv", "data3.tsv")
#' results <- readfiles(files)
#' }
#'
readfiles <- function(fnames) {
  # Initialize an empty list to store processed data
  out <- vector("list", length(fnames))
  library(readr)
  # Loop through each file name
  for (i in seq_along(fnames)) {
    tryCatch({
      # Function to read a file and detect the delimiter
      readfile_auto <- function(filename) {
        first_line <- read_lines(filename, n_max = 1)  # Read only the first line
        if (grepl(" ", first_line)) {
          return(read_delim(filename, delim = " ", col_names = FALSE, show_col_types = FALSE))
        } else if (grepl("\t", first_line)) {
          return(read_tsv(filename, col_names = FALSE, show_col_types = FALSE))
        } else if (grepl(",", first_line)) {
          return(read_csv(filename, col_names = FALSE, show_col_types = FALSE))
        } else {
          stop("Unsupported delimiter in file: ", filename)
        }
      }

      # Read the data using the appropriate delimiter
      data <- readfile_auto(fnames[[i]])

      # Check if the file is empty or improperly formatted
      if (nrow(data) == 0) {
        warning(paste("File", fnames[[i]], "is empty or could not be read properly."))
        next
      }

      # Handle cases with empty first rows (e.g., in case the first row is NA or blank)
      if (is.na(data[1, 1])) {
        data <- data[-1, ]
      }

      # Ensure the file has at least two columns
      if (ncol(data) < 2) {
        stop(paste("File", fnames[[i]], "does not have the required columns."))
      }

      # Keep only the first two columns and drop any additional ones
      data <- data[, 1:2]

      # Normalize the second column to a 0–100 scale
      max_value <- max(data[[2]], na.rm = TRUE)
      if (max_value == 0) {
        stop(paste("File", fnames[[i]], "has a column with all zeroes, cannot normalize."))
      }
      data[[2]] <- (data[[2]] / max_value) * 100

      # Set column names to "V1" and "V2"
      colnames(data) <- c("V1", "V2")

      # Store the processed dataframe in the list
      out[[i]] <- data
    }, warning = function(w) {
      message("Warning in processing file ", fnames[[i]], ": ", w$message)
    }, error = function(e) {
      message("Error in processing file ", fnames[[i]], ": ", e$message)
    })
  }

  # Return the list of processed dataframes
  return(out)
}
