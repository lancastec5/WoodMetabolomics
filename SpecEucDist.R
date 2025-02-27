#' Euclidean Distance Heatmap
#'
#' This function generates a similarity heatmap using Euclidean distance to analyze cluster similarities.
#' It allows hierarchical clustering using Ward's D2 method and visualizes the clustered distance matrix. Intensity differences capture: Euclidean distance considers differences in peak intensities, which is useful for detecting variations in signal strength across different samples.
#'Useful for absolute comparison: If spectral intensities are meaningful in absolute terms (e.g., concentration-dependent measurements), Euclidean distance is a good choice. When the data has been normalized or scaled (e.g., mean-centering, unit variance scaling), Euclidean distance can effectively highlight genuine differences between spectra rather than being biased by measurement intensity ranges.
#'
#' @param data A data frame containing the data.
#' @param cluster_col The name of the column representing clusters.
#' @param scale_data Logical, whether to scale the data before calculating distances. Default is TRUE.
#' @return A heatmap of the Euclidean distance matrix, using hierarchical clustering for visualization.
#' @examples
#' \dontrun{
#' # Example data
#' data <- data.frame(
#'   cluster = rep(1:3, each = 10),
#'   var1 = rnorm(30),
#'   var2 = rnorm(30),
#'   var3 = rnorm(30)
#' )
#'
#' # Generate Euclidean distance heatmap
#' SpecEucDist(data, "cluster")
#' }
#' @export
SpecEucDist <- function(data, cluster_col, scale_data = FALSE) {
  # Load necessary libraries within the function
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(pheatmap)

  # Scale the data if scale_data is TRUE
  if (scale_data) {
    data_scaled <- data %>%
      dplyr::select(-all_of(cluster_col)) %>%
      dplyr::mutate(across(everything(), scale)) %>%
      cbind(cluster = data[[cluster_col]])

    colnames(data_scaled)[ncol(data_scaled)] <- cluster_col
  } else {
    data_scaled <- data
  }

  num_columns <- length(data_scaled)

  na_columns <- colnames(data_scaled)[colSums(is.na(data_scaled)) > 0]

  # Print columns with NA values
  print("Columns with NA values:")
  print(na_columns)
  # Identify columns with NA values


  # Count the number of columns with NA values
  num_na_columns <- length(na_columns)
  num_columns <- length(data_scaled)

  # Print the number of columns with NA values
  print(paste("Number of columns with NA values:", num_na_columns, "out of", num_columns))

  # Remove columns with NA values
  data_scaled <- data_scaled[, !colnames(data_scaled) %in% na_columns]
  print("NA columns removed")

  # Define cosine similarity function
  cosine_similarity <- function(x, y) {
    sum(x * y) / (sqrt(sum(x * x)) * sqrt(sum(y * y)))
  }

  # Calculate pairwise Euclidean distances between all observations
  similarities <- data_scaled %>%
    dplyr::select(-all_of(cluster_col)) %>%
    as.matrix() %>%
    {
      n <- nrow(.)
      combn(1:n, 2, function(pair) {
        i <- pair[1]
        j <- pair[2]
        cluster_i <- data_scaled[[cluster_col]][i]
        cluster_j <- data_scaled[[cluster_col]][j]
        if (length(cluster_i) == 0 || length(cluster_j) == 0) {
          return(NULL)
        }
        # Compute Euclidean distance instead of cosine similarity
        dist_val <- sqrt(sum((.[i, ] - .[j, ])^2))
        data.frame(cluster_i, cluster_j, dist_val)
      }, simplify = FALSE)
    } %>%
    bind_rows() %>%
    as.data.frame()


  # Summarize similarities between clusters
  similarity_summary <- similarities %>%
    group_by(cluster_i, cluster_j) %>%
    summarize(mean_similarity = mean(dist_val, na.rm = TRUE),
              sd_similarity = sd(dist_val, na.rm = TRUE), .groups = 'drop')


  # Create a matrix to store the mean ± sd values
  clusters <- unique(c(similarity_summary$cluster_i, similarity_summary$cluster_j))
  n_clusters <- length(clusters)
  similarity_matrix <- matrix(NA, n_clusters, n_clusters, dimnames = list(clusters, clusters))
  similarity_matrix_text <- matrix(NA, n_clusters, n_clusters, dimnames = list(clusters, clusters))

  for (i in 1:n_clusters) {
    for (j in 1:n_clusters) {
      summary_values <- similarity_summary %>%
        filter((cluster_i == clusters[i] & cluster_j == clusters[j]) |
                 (cluster_i == clusters[j] & cluster_j == clusters[i]))

      if (nrow(summary_values) > 0) {
        mean_sim <- summary_values$mean_similarity[1]
        sd_sim <- summary_values$sd_similarity[1]
        similarity_matrix[i, j] <- round(mean_sim, 2)
        similarity_matrix_text[i, j] <- paste(round(mean_sim, 2), "±", round(sd_sim, 2))
      }
    }
  }

  # Perform hierarchical clustering with Ward's D2 method
  hc <- hclust(dist(similarity_matrix), method = "ward.D2")


  # Helper function to create italic labels
  italicize_labels <- function(labels) {
    as.expression(lapply(labels, function(x) bquote(italic(.(x)))))
  }

  # Calculate the minimum, midpoint, and maximum values of the similarity matrix
  min_val <- min(similarity_matrix, na.rm = TRUE)
  max_val <- max(similarity_matrix, na.rm = TRUE)
  mid_val <- (max_val + min_val) / 2  # Set midpoint for better contrast

  # Create custom breaks using quantiles for improved color contrast
  breaks <- quantile(similarity_matrix, probs = seq(0, 1, length.out = 100), na.rm = TRUE)

  # Generate the heatmap with hierarchical clustering
  pheatmap(similarity_matrix,
           cluster_rows = hc,
           cluster_cols = hc,
           labels_row = italicize_labels(rownames(similarity_matrix)),
           labels_col = italicize_labels(colnames(similarity_matrix)),
           display_numbers = similarity_matrix_text,
           number_color = "black",
           color = viridis::viridis(length(breaks) - 1, option = "plasma", direction = -1),
           #breaks = breaks,
           fontsize_number = 8,
           fontsize = 10,
           main = "Euclidean Distance Matrix Heatmap",
           angle_col = 45,  # Rotate column labels to 45 degrees
           angle_row = 45,   # Rotate row labels to 45 degrees,
           legend_breaks = c(min_val, max_val),  # Define breakpoints for labels
           legend_labels = c("Similar", "Dissimilar")  # Custom legend labels
  )

  return(invisible(NULL))
}
