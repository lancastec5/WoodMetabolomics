
#Id_bins_for_columns
#' identifies ions/columns within the interval (inclusive)
#'
#' @param data matrix in long format (rows = ions, columns = sample)
#' @param interval a number, specifies bin size. Default value = 0.001.
#'
#' @return a list with the ions to keep (highest intensity) and the start and stop rows for each bin
#'
#'
id_bins_for_columns <- function(data, interval = 0.001, min_ion = 100) {
  # Retrieve ions and filter based on the min_ion threshold
  ions <- as.numeric(rownames(data)[-1])
  ions <- ions[ions >= min_ion]  # Only keep ions above the min_ion limit

  # Set up empty vectors
  start <- c()
  stop <- c()
  skip <- c()
  pic_dist <- c(0)

  # Identify ions that fall within the interval
  for (i in seq_along(ions)) {
    # Check whether the current row is not already a duplicate
    if (!(i %in% skip)) {
      start <- c(start, i)
      j <- 1
      reps <- 0
      while (pic_dist <= interval) {
        # Calculate distance between peaks
        pic_dist <- round(ions[i + j] - ions[i], 4)
        # Increase j to include more columns
        j <- j + 1
        # Count the number of repetitions
        reps <- reps + 1
        # Escape the loop if we reach the end of the ions list
        if ((i + j) >= length(ions)) {
          break
        }
      }
      pic_dist <- c(0)
      stop <- c(stop, (i + reps - 1))
      skip <- 1:(i + reps - 1)
    }
  }

  # Vector with new column names (lowest ions from intervals)
  new_names <- rep(NA, length(start))
  for (k in 1:length(start)) {
    new_names[k] <- ions[start[k]]
  }

  return(list(ions = new_names, start = start, stop = stop))
}
