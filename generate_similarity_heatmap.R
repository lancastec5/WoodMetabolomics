#' Generate Similarity Heatmap
#' Author: Cady Lancaster
#'
#' This function generates a similarity heatmap based on cosine similarity for clustered data.
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
#' generate_similarity_heatmap(data, "cluster")
#' }
#' @export
generate_similarity_heatmap <- function(data, cluster_col, scale_data = TRUE, Title, High, Low, round_digits = 2) {
  # Load necessary libraries within the function
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(viridis)

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

  # Identify columns with NA values
  na_columns <- colnames(data_scaled)[colSums(is.na(data_scaled)) > 0]

  # Print columns with NA values
  print("Columns with NA values:")
  print(na_columns)

  # Count and remove columns with NA values
  data_scaled <- data_scaled[, !colnames(data_scaled) %in% na_columns]
  print("NA columns removed")

  # Define cosine similarity function
  cosine_similarity <- function(x, y) {
    sum(x * y) / (sqrt(sum(x * x)) * sqrt(sum(y * y)))
  }

  print("Calculating cosine similarities")

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
  print("Cosine similarities calculated. Preparing for plotting")

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
        similarity_matrix[i, j] <- round(mean_sim, round_digits)
        similarity_matrix_text[i, j] <- paste0(round(mean_sim, round_digits), " ± ", round(sd_sim, round_digits))
      }
    }
  }

  # Convert the similarity matrix to a data frame for better readability
  similarity_df <- as.data.frame(similarity_matrix, stringsAsFactors = FALSE)
  similarity_df$cluster <- rownames(similarity_df)

  # Prepare similarity_matrix_text for plotting
  similarity_text_df <- as.data.frame(similarity_matrix_text, stringsAsFactors = FALSE)
  similarity_text_df$cluster <- rownames(similarity_text_df)
  similarity_text_long <- pivot_longer(similarity_text_df, cols = -cluster, names_to = "Var2", values_to = "text")

  # Reshape the data for ggplot2
  similarity_long <- pivot_longer(similarity_df, cols = -cluster, names_to = "Var2", values_to = "value") %>%
    arrange(desc(value))

  # Merge the long data frames
  similarity_long <- merge(similarity_long, similarity_text_long, by = c("cluster", "Var2"))

  # Perform hierarchical clustering
  similarity_matrix <- as.matrix(similarity_df[,-ncol(similarity_df)])
  rownames(similarity_matrix) <- similarity_df$cluster
  hc <- hclust(dist(similarity_matrix))

  # Order the factor levels based on clustering
  ordered_levels <- hc$labels[hc$order]

  # Set factor levels for proper ordering
  similarity_long$cluster <- factor(similarity_long$cluster, levels = ordered_levels)
  similarity_long$Var2 <- factor(similarity_long$Var2, levels = ordered_levels)

  # Wrap text for axis labels and make them italic
  wrapped_labels <- str_wrap(ordered_levels, width = 15)
  wrapped_labels <- str_c(wrapped_labels)

  # Adjust factor levels to ensure correct order
  similarity_long$cluster <- factor(similarity_long$cluster, levels = rev(ordered_levels), labels = rev(wrapped_labels))
  similarity_long$Var2 <- factor(similarity_long$Var2, levels = ordered_levels, labels = wrapped_labels)

  # Generate heatmap
  heatmap_plot <- ggplot(similarity_long, aes(x = Var2, y = cluster, fill = as.numeric(value))) +
    geom_tile(color = "white") +
    geom_text(aes(label = text), size = 3, color = "black") +
    scale_fill_gradientn(
      colors = viridis_pal()(9),
      limits = c(Low, High),
      na.value = "#FDE725FF",
      name = expression(cos(theta)), # Legend title
      breaks = c(Low, High),        # Define specific breaks
      labels = c("Dissimilar", "Similar") # Labels matching the breaks
    ) +
    labs(title = paste("Cosine Similarity Matrix:", Title), x = " ", y = " ") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, face = "italic"),
      axis.text.y = element_text(angle = 0, hjust = 1, face = "italic"),
      axis.title.x = element_text(),
      axis.title.y = element_text(),
      plot.title = element_text(hjust = 0.5, size = 10)
    ) +
    scale_x_discrete(position = "top")

  return(heatmap_plot)
}
