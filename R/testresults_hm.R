#' Confusion Matrix Heatmap Visualization
#'
#' This function generates a heatmap visualization of a given confusion matrix,
#' allowing for clear interpretation of classification performance.
#'
#' @param conf_matrix A confusion matrix, provided as a matrix or a data frame.
#' @param levels A vector of class labels in the desired order.
#'
#' @return A `ggplot2` object representing the heatmap visualization of the confusion matrix.
#' @export
#'
#' @import ggplot2 reshape2 scales stringr
#'
#' @examples
#' conf_matrix <- matrix(c(50, 10, 5, 100), nrow = 2, byrow = TRUE)
#' levels <- c("Class A", "Class B")
#' testresults_hm(conf_matrix, levels)
testresults_hm <- function(conf_matrix, levels) {
  library(ggplot2)
  library(reshape2)
  library(scales)
  library(stringr)

  # Convert confusion matrix to matrix if it's a data frame
  if (is.data.frame(conf_matrix)) {
    conf_matrix <- as.matrix(conf_matrix)
  }

  # Ensure levels are provided
  if (is.null(levels) || length(levels) == 0) {
    stop("Levels must be provided and cannot be NULL or empty.")
  }

  # Handle potential all-zero matrix
  if (all(conf_matrix == 0)) {
    stop("The confusion matrix contains only zeros. No meaningful visualization possible.")
  }

  # Convert the confusion matrix to long format for ggplot
  conf_matrix_long <- melt(conf_matrix)
  colnames(conf_matrix_long) <- c("Actual", "Predicted", "Value")

  # Remove rows with NA values
  conf_matrix_long <- na.omit(conf_matrix_long)

  # Ensure factor levels for ordering in the heatmap
  species_levels <- levels
  names(species_levels) <- levels

  conf_matrix_long$Actual <- factor(conf_matrix_long$Actual,
                                    levels = names(species_levels),
                                    labels = species_levels)
  conf_matrix_long$Predicted <- factor(
    conf_matrix_long$Predicted,
    levels = names(species_levels),
    labels = species_levels
  )

  # Wrap text for axis labels and make them italic
  wrapped_labels <- str_wrap(levels(conf_matrix_long$Actual), width = 15)
  wrapped_labels <- str_c(wrapped_labels)

  # Adjust factor levels to ensure correct order for heatmap visualization
  conf_matrix_long$Actual <- factor(
    conf_matrix_long$Actual,
    levels = rev(levels(conf_matrix_long$Actual)),
    labels = rev(wrapped_labels)
  )

  conf_matrix_long$Predicted <- factor(
    conf_matrix_long$Predicted,
    levels = levels(conf_matrix_long$Predicted),
    labels = wrapped_labels
  )

  # Create the heatmap plot with updated formatting
  heatmap_plot <- ggplot(conf_matrix_long, aes(x = Predicted, y = Actual, fill = Value)) +
    geom_tile(color = "black") +
    geom_text(
      aes(label = ifelse(Value > 0, sprintf("%.1f", Value), "")),
      size = 3,
      color = "black",
      show.legend = FALSE
    ) +
    scale_fill_gradient2(
      low = "white",
      high = "green",
      midpoint = mean(conf_matrix_long$Value, na.rm = TRUE),
      na.value = "white"
    ) +
    labs(title = "Confusion Matrix Heatmap", x = "Reference Class", y = "Unknown") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45,
       hjust = 0,
        face = "italic"
      ),
      axis.text.y = element_text(
        angle = 0,
        hjust = 0,
        face = "italic"
      ),
      axis.title.x = element_text(),
      axis.title.y = element_text(),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_discrete(position = "top")

  print(heatmap_plot)
  return(heatmap_plot)
}
