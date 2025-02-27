#' Plot Kernel Density Estimates of Out-of-Bag Errors
#'
#' This function generates a kernel density plot of out-of-bag (OOB) error estimates
#' for different models or datasets. It also calculates and displays mean values
#' with confidence intervals for each dataset.
#'
#'  @param oobs A list containing multiple datasets of out-of-bag errors. Each element
#'   should be a data frame or a list containing at least a ' OOB_Error' column which
#'   represents the median fit error.
#' @param variable response The name of the response variable that is being compared. This is used in the naming of the graph.

#' @return KDE plot of the oob median errors for the oob2name and oob1name data
#' @examples
#' # example 1
#' # Example OOB datasets in the specified format
#' set.seed(123)
#' oob1 <- data.frame( OOB_Error = rnorm(100, mean = 0.7, sd = 0.1))  # Simulated OOB data for the first model
#' oob2 <- data.frame( OOB_Error = rnorm(100, mean = 0.65, sd = 0.12))  # Simulated OOB data for the second model
#' oob3 <- data.frame( OOB_Error = rnorm(100, mean = 0.6, sd = 0.15))  # Simulated OOB data for the third model

# Named list of OOB datasets
#' oobs <- list(
#'   Model1 = oob1,
#'  Model2 = oob2,
#'   Model3 = oob3
#' )
#'
# Variable name
#' variable <- "Example Variable"
#'
# Call the function
#' results <- plot_kdeoob_vs(oobs, variable)

# Access the results
#' KDE_stats <- results$KDE_stats
#' KDE_plot <- results$KDEplot
#'
#'
#' @export



plot_kdeoob_vs <- function(oobs, variable,low,high) {
  # Prepare an empty data frame for combined data
  combined_data <-
    data.frame(OOB.med.err = numeric(), group = character())

  # List of packages you want to install and load
  listOfPackages <- c("ggplot2", "scales", "tidyverse")

  # Loop through the list of packages
  for (pkg in listOfPackages) {
    # Check if package is not installed, then install it
    if (!(pkg %in% installed.packages())) {
      install.packages(pkg, dependencies = TRUE)
    }

    # Load or attach the package
    require(pkg, character.only = TRUE)
  }
  # List to store individual statistics data frames
  stats_list <- list()

  # Iterate over each OOB dataset
  for (name in names(oobs)) {
    oob <- oobs[[name]]

    # # Ensure the OOB dataset has the expected format
    # if (!" OOB_Error" %in% colnames(oob)) {
    #   stop(paste("OOB dataset", name, "does not contain ' OOB_Error' column"))
    # }

    # Prepare data for density plot
    dens_data <-
      data.frame(OOB.med.err = oob$OOB_Error * 100, group = name)
    combined_data <- rbind(combined_data, dens_data)

    # Calculate statistics
    mean_value <- mean(dens_data$OOB.med.err)
    se_value <-
      qt(0.975, df = length(dens_data$OOB.med.err) - 1) * sd(dens_data$OOB.med.err) / sqrt(length(dens_data$OOB.med.err))
    stats_data <- data.frame(
      mean = mean_value,
      se = se_value,
      lower = mean_value - se_value,
      upper = mean_value + se_value,
      format = name
    )

    # Add to stats list
    stats_list[[name]] <- stats_data
  }

  # Combine all statistics into one data frame
  KDE_stats <- do.call(rbind, stats_list)
  KDE_stats$model <- variable

  # Optionally write to file
  # write.csv(KDE_stats, 'path/filename.csv', row.names=FALSE)

  # Colorblind-friendly palette (Okabe-Ito)
  colorblind_palette <- c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    "#999999",
    "#88CCEE", "#CC6677", "#DDCC77"
  )
  # Create the density plot
  KDEplot <- ggplot(combined_data, aes(x = OOB.med.err, fill = group)) +
    geom_density(alpha = 0.7) +
    scale_fill_manual(values = colorblind_palette)+
    theme_classic() +
    scale_x_continuous(limits = c(low, high)) +
    xlab("Mean OOB (%)") +
    ylab("Kernel Density Estimate") +
    #ggtitle(paste(variable, "Classification of Test Data")) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.justification = c(1, 1),
      legend.position = "right",
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.key.size = unit(0.2, "cm"),
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      legend.key = element_rect(color = "white")
    )

  # Add mean lines and annotations for each group
  for (i in seq_along(names(oobs))) {
    name <- names(oobs)[i]
    mean_value <- KDE_stats[KDE_stats$format == name, "mean"]
    y_position <- max(density(combined_data$OOB.med.err[combined_data$group == name])$y)

    KDEplot <- KDEplot +
      geom_vline(xintercept = mean_value,linetype="dashed") +
      annotate(
        "label",
        x = mean_value,
        y = y_position + y_position * 0.2,
        label = round(mean_value, 0),
        #color = colorblind_palette[i],  # Set text color to match the fill color of the corresponding group
        #fill = "white",   # Set fill color for label box
        size = 4,
        label.padding = unit(0.15, "lines"),
        label.size = 0.2 # Border thickness
      )
  }


  # Display the plot
  print(KDEplot)

  cat("# call function$KDE_plot for KDE plots \n")
  cat("call function$KDE_stats for total KDE stats :\n")
  #print(dim(importance_df))
  return(list(KDE_stats = KDE_stats, KDEplot = KDEplot))
}
