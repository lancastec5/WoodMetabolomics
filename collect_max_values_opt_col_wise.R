
#Collect_max_values_opt_col_wise
#' keeps the max value (row-wise) within the bins
#'
#' @param data matrix to be modified
#' @param start numeric, specifies start positions of bins
#' @param stop numeric, specifies stop positions of bins
#'
#' @return a short format dataframe where columns within the interval are merged and only the highest intensity is kept
#'
collect_max_values_opt_col_wise <- function (data, start, stop){
  # put the max value from each interval in the first column from interval
  for(i in 1:ncol(data)) {
    for (j in 1:length(start)) {
      from <- start[j] + 1
      to <- stop[j] + 1
      win <- data[from:to, i]
      # check wether values in interval for row i
      if(sum(win, na.rm = TRUE) != 0){
        # save max in first column of interval
        data[from, i] <- max(win, na.rm = TRUE)
      }
    }
  }

  # keep only columns of interest
  out_data <- data[c(1, start+1), ]
  return(out_data)
}
#Final_merge
#' Merge columns within one bin and keep the max intensity
#'
#' @param short_ordered_data a dataframe in the short format
#' @param interval a number, specifies bin size
#' @param cores a number, number of cores to use in parallel. Default = 1
#'
#' @return a matrix (short format)
#'
