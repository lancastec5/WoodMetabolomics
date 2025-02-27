#' Prepare file with different mmus
#'
#' @param fnames Vector with file names
#' @param threshold Numeric, intensity threshold
#' @param bin_seq Numeric vector with intervals
#' @param save Logical, write output into CSV
#' @param cores Numeric, number of cores for parallel processing
#' @param data Dataframe, input data
#' @return A list with all the dataframes
data_prep <- function(fnames, threshold,  bin_seq, save = FALSE, cores = 1, data, min_ion = 100) {
  # Ensure necessary libraries are loaded
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("The 'tidyr' package is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("The 'dplyr' package is required.")

  library(tidyr)
  library(dplyr)

  # Initialize an empty list for output
  out <- vector("list", length(bin_seq))

  tryCatch({
    ## 1. Order rows based on ion size (V1 column)
    if (!"V1" %in% names(data) || !"V2" %in% names(data)) {
      stop("Input data must contain columns 'V1' and 'V2'.")
    }

    # Filter out rows where ion size is below the minimum threshold (min_ion)
    data <- data[data$V1 >= min_ion, ]

    ordered_data <- data[order(data$V1), ]

    ## 2. Reshape dataframe
    # Switch data format from long to wide
    short_data <- tryCatch({
      spread(data = ordered_data, key = 'V1', value = 'V2')
    }, error = function(e) stop("Error reshaping data: ", e$message))


    # Ensure files column is numeric
    if (!"files" %in% names(short_data)) {
      stop("The reshaped data must contain a 'files' column.")
    }

    short_data$files <- as.integer(short_data$files)

    short_ordered_data <- short_data[order(short_data$files), ]

    if (nrow(short_ordered_data) == length(fnames)) {
      rownames(short_ordered_data) <- seq_along(fnames)
    } else {
      warning("Mismatch in number of rows and filenames; row names not assigned.")
    }

    # Rename rows correctly
    rownames(short_ordered_data) <- seq_along(fnames)

    ## 3. Process bins
    for (i in seq_along(bin_seq)) {
      tryCatch({
        # Merge columns within one bin and keep the max intensity
        final_data <- final_merge(short_ordered_data, interval = bin_seq[i], cores = cores)

        # Add file names
        final_data[, 1] <- fnames

        # Save the dataframe to a CSV file if required
        if (save) {
          name_csv <- paste0('output/data_thresh', threshold, '_mmu', bin_seq[i]*1000, '.csv')
          write.csv(final_data, name_csv, row.names = FALSE)
          print(paste("Saved:", name_csv))
        }

        # Save the processed data in the output list
        out[[i]] <- final_data

        print(paste("Binning done for interval:", bin_seq[i]))
      }, error = function(e) {
        warning(paste("Error processing bin sequence", bin_seq[i], ":", e$message))
        out[[i]] <- NULL
      })
    }
  }, error = function(e) {
    stop("Error during data preparation: ", e$message)
  })

  # Return the output list
  return(out)
}
