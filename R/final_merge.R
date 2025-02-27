
#Final_merge
#' Merge columns within one bin and keep the max intensity
#'
#' @param short_ordered_data a dataframe in the short format
#' @param interval a number, specifies bin size
#' @param cores a number, number of cores to use in parallel. Default = 1
#'
#' @return a matrix (short format)
#'
final_merge<- function(short_ordered_data, interval, cores=1){
  # Set-up
  data <- t(short_ordered_data)  # easier for R to work column-wise
  # use id_bins_for_columns(.) on data
  bins <- id_bins_for_columns(data, interval)
  # divide dataframe into a list of dataframes of equal sizes for mclapply(.)
  n <- dim(data)[2]
  # split columns in same size groups
  buckets <- ntile(1:n, cores)
  # combine subsets of data into a list for mclapply(.)
  dfs <- as.list(rep(NA, cores))
  names(dfs) <- paste0('df_', 1:cores)
  for (i in 1:cores){
    start <- 1 + (i-1) * table(buckets)[[i]]
    stop <- i * table(buckets)[[i]]
    dfs[[i]] <- data[,start:stop]
  }
  ## Make the final dataframe
  # mclapply(.) is a parallelized version of lapply
  split_output <- mclapply(dfs,
                           collect_max_values_opt_col_wise, bins$start, bins$stop,
                           mc.cores = cores)
  data_bin <- do.call(cbind, split_output)
  # transpose back into short format
  final_data <-t(data_bin)

  return(final_data)
}
