' run really random forest
#'Author: Cady Lancaster
#'Last Check: 4 November 2024
#' @description Run random forest with a new seed starting at prior to each group. The variable that is grouped for classification is randomized through the data.
#'
#' @param data the data matrix
#' @param variable response The name of the response variable (as a string).
#' @param num_iterations the number of iterations that the randomforest will reseed on
#' @param ntree are the number of trees for the randomforest to run on each iteration
#' @param datasplit the partition for the training set which subsides the test datasets
#'
#' @return random forest model stats.
#'
#' @examples
#' # example 1
#'
#' # Load the necessary data (replace with your actual data)
#' data <- your_data_frame  # Replace with your actual data frame
#' variable <- "your_response_variable"  # Replace with the name of your response variable
#'
#'dev Set parameters
#' num_iterations <- 5  # Number of iterations
#' ntree <- 250  # Number of trees
#' datasplit <- 0.7  # Training data split proportion
#' Correct function call
#' model <- randomized_rf(data, variable, num_iterations, ntree, datasplit, mtry, balance = TRUE)
#' model <- randomized_rf(test_data, "Species",5, 250, 0.7,15,balance =TRUE)
#'
#'
#' @export

randomized_rfv2 <- function(data, variable, num_iterations, ntree, datasplit, mtry, balance = TRUE) {
  # Load required packages
  listOfPackages <- c("caret", "randomForest", "tidyverse", "future.apply")
  invisible(lapply(listOfPackages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    require(pkg, character.only = TRUE)
  }))

  # Ensure response variable is a factor
  if (!is.factor(data[[variable]])) data[[variable]] <- as.factor(data[[variable]])

  # Levels of the response variable
  variable_levels <- levels(data[[variable]])
  num_levels <- length(variable_levels)

  # Define a single iteration process
  run_single_iteration <- function(seed_offset) {
    # Set seed for reproducibility
    set.seed(123 + seed_offset)


    # Randomize the values in the specified column
    data[[variable]] <- sample(data[[variable]])

    # Split data into training and testing sets
    train_index <- createDataPartition(data[[variable]], p = datasplit, list = FALSE)
    training_set <- data[train_index, ]
    testing_set <- data[-train_index, ]

    # Balance training set if required
    if (balance) {
      class_counts <- table(training_set[[variable]])
      min_class_size <- min(class_counts)
      training_set <- training_set %>%
        group_by(!!sym(variable)) %>%
        slice_sample(n = min_class_size) %>%
        ungroup()
    }

    # Track training class sizes
    training_class_size <- table(training_set[[variable]])

    # Train Random Forest
    fit <- randomForest(
      formula = as.formula(paste(variable, "~ .")),
      data = training_set,
      ntree = ntree,
      mtry = mtry,
      importance = TRUE
    )

    # Predictions on the test set
    predictions <- predict(fit, newdata = testing_set)

    # Confusion matrix
    current_conf_matrix <- table(
      factor(predictions, levels = variable_levels),
      factor(testing_set[[variable]], levels = variable_levels)
    )

    # Variable importance
    importance_scores <- as.data.frame(importance(fit, type = 1))
    importance_scores$variable <- rownames(importance_scores)

    # OOB error
    oob_error <- 1 - fit$err.rate[nrow(fit$err.rate), "OOB"]

    # Misclassified instances
    misclassified <- as.numeric(predictions != testing_set[[variable]])

    return(list(
      fit = fit,
      conf_matrix = current_conf_matrix,
      importance = importance_scores,
      oob_error = oob_error,
      misclassified = misclassified,
      train_index = train_index,
      training_class_size = training_class_size
    ))
  }

  # Parallel execution using future_lapply
  cat("Starting parallel execution...\n")
  plan(multisession)  # Use multiple CPU cores
  results <- future_lapply(1:num_iterations, run_single_iteration, future.seed = TRUE)

  # Aggregate results
  cat("Aggregating results...\n")
  overall_conf_matrix <- Reduce("+", lapply(results, `[[`, "conf_matrix"))
  importance_df <- bind_rows(lapply(results, `[[`, "importance"))
  oob_errors <- sapply(results, `[[`, "oob_error")
  fits <- lapply(results, `[[`, "fit")
  training_sizes <- lapply(results, `[[`, "training_class_size")

  # Calculate mean prediction errors
  prediction_errors <- matrix(0, nrow = nrow(data), ncol = num_iterations)
  for (i in seq_along(results)) {
    prediction_errors[-results[[i]]$train_index, i] <- results[[i]]$misclassified
  }
  mean_errors <- rowMeans(prediction_errors)

  # Normalize confusion matrix
  col_sums <- colSums(overall_conf_matrix)
  conf_matrix_percent <- sweep(overall_conf_matrix, 2, col_sums, FUN = "/") * 100
  conf_matrix_percent <- round(conf_matrix_percent, 1)

  # Compute overall accuracy
  overall_accuracy <- round(sum(diag(overall_conf_matrix)) / sum(overall_conf_matrix) * 100, 1)

  # Average importance
  mean_importance <- importance_df %>%
    group_by(variable) %>%
    summarize(MeanDecreaseAccuracy = mean(MeanDecreaseAccuracy))

  cat("Execution complete.\n")

  return(list(
    overall_accuracy = overall_accuracy,
    conf_matrix = overall_conf_matrix,
    conf_matrix_percent = conf_matrix_percent,
    fit.list = fits,
    err.obs = data.frame(OOB_Error = oob_errors),
    training_class_size = training_class_size,
    prediction_errors = mean_errors,
    mean_importance = mean_importance
  ))
}
