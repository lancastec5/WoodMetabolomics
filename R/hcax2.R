#'Cosine Similarity
#'
#'Description
#' This function generates a similarity heatmap with cluster analysis based on the similarity score based on cosine similarity for clustered data.
#'
#' Cosine Similarity Explained
#'
#' Definition:
#'
#'   cos(𝜃) = (𝑢 ⋅ 𝑣) / (∥𝑢∥∥𝑣∥)
#'
#' where 𝑢 ⋅ 𝑣 is the dot product of vectors 𝑢 and 𝑣, ∥𝑢∥ is the magnitude (norm) of vector 𝑢, and ∥𝑣∥ is the magnitude (norm) of vector 𝑣.
#'
#' Value Range:
#'
#'   Cosine similarity ranges from -1 to 1.
#'
#' - 1 indicates that the vectors are identical (the angle between them is 0 degrees).
#' - 0 indicates that the vectors are orthogonal (the angle between them is 90 degrees), meaning they have no similarity.
#' - −1 indicates that the vectors are diametrically opposite (the angle between them is 180 degrees).
#'
#' Interpretation in the Context of Clusters:
#'
#' - High Similarity (close to 1): Indicates that the clusters have very similar average spectra.
#' - Moderate Similarity (around 0): Indicates little to no similarity between the clusters’ average spectra.
#' - Negative Similarity (close to -1): Rare in practical scenarios, especially with non-negative data like spectra, indicating that the clusters have opposite patterns in their spectra.
#'
#' Practical Significance:
#'
#' - High similarity scores can indicate redundancy in clustering.
#' - Low similarity scores validate the distinctiveness of clusters.
#' - Identifying which clusters have high similarity can help in understanding underlying patterns or features common to those clusters.
#'
#' @param data A data frame containing the data.
#' @param cluster_col The name of the column representing clusters.
#' @param scale_data Logical, whether to scale the data before calculating similarities.
#' @return A ggplot object representing the similarity heatmap.
#' @examples
#' \dontrun{
#' # Load example data
#' data <- data.frame(
#'   cluster = rep(1:3, each = 10),
#'   var1 = rnorm(30),
#'   var2 = rnorm(30),
#'   var3 = rnorm(30)
#' )
#'
#' # Generate similarity heatmap
#' hcax2(data, "cluster")
#' }
#' @export

hcax2 <- function(data, cluster_col, scale_data = TRUE) {
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

  # Calculate pairwise similarities between all observations
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
        sim <- cosine_similarity(.[i, ], .[j, ])
        data.frame(cluster_i, cluster_j, sim)
      }, simplify = FALSE)
    } %>%
    bind_rows() %>%
    as.data.frame()

  # Summarize similarities between clusters
  similarity_summary <- similarities %>%
    group_by(cluster_i, cluster_j) %>%
    summarize(mean_similarity = mean(sim, na.rm = TRUE),
              sd_similarity = sd(sim, na.rm = TRUE), .groups = 'drop')

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

  # Perform hierarchical clustering
  hc <- hclust(dist(similarity_matrix), method = "ward.D2")


  # Helper function to create italic labels
  italicize_labels <- function(labels) {
    as.expression(lapply(labels, function(x) bquote(italic(.(x)))))
  }
  # Calculate the minimum, midpoint, and maximum values of the similarity matrix
  min_val <- min(similarity_matrix, na.rm = TRUE)
  max_val <- max(similarity_matrix, na.rm = TRUE)
  mid_val <- (max_val + min_val) / 2  # Set midpoint for better contrast

  # Create custom breaks using quantiles and ensure uniqueness
  breaks <- unique(quantile(similarity_matrix, probs = seq(0, 1, length.out = 100), na.rm = TRUE))

  # Add a small jitter to avoid identical values causing errors
  if (length(breaks) < 2) {
    breaks <- seq(min_val, max_val, length.out = 100)
  } else if (anyDuplicated(breaks)) {
    breaks <- breaks + seq_along(breaks) * 1e-10  # Small increment to ensure uniqueness
  }


  # Generate the heatmap with hierarchical clustering
  heatmap <- pheatmap(similarity_matrix,
           cluster_rows = hc,
           cluster_cols = hc,
           labels_row = italicize_labels(rownames(similarity_matrix)),
           labels_col = italicize_labels(colnames(similarity_matrix)),
           display_numbers = similarity_matrix_text,
           number_color = "black",
           color = viridis::viridis(length(breaks) - 1, option = "plasma"),
           breaks = breaks,
           fontsize_number = 8,
           fontsize = 10,
           main = "Cosine Similarity Matrix Heatmap",
           angle_col = 45,  # Rotate column labels to 45 degrees
           angle_row = 45,  # Rotate row labels to 45 degrees
           legend_breaks = c(min_val, max_val),  # Define breakpoints for labels
           legend_labels = c("Dissimilar", "Similar")  # Custom legend labels
  )

  library(ggplot2)
  # Perform MDS on the similarity matrix
  mds_result <- cmdscale(dist(similarity_matrix), k = 2)

  # Convert MDS result to a data frame with proper column names
  mds_df <- data.frame(Dimension1 = mds_result[, 1], Dimension2 = mds_result[, 2],
                       Cluster = rownames(similarity_matrix))
  # Create MDS plot
  mdsplot<- library(circlize)
mdsplot<- chordDiagram(similarity_summary[, c("cluster_i", "cluster_j", "mean_similarity")],
             grid.col = rainbow(length(unique(similarity_summary$cluster_i))))

  return(list(heatmap = heatmap, mdsplot = mdsplot))
}

