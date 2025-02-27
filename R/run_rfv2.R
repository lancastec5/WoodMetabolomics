#' Run Random Forest with Iterative Re-seeding
#' Author: Cady A. Lancaster
#' Last Check: 4 November 2024
#' @description This function trains a random forest model over multiple iterations, with a new seed set for each iteration to enhance reproducibility. It returns model metrics, including confusion matrices, prediction errors, and variable importance measures.
#'
#' Each iteration generates its own model, stores confusion matrices, and tracks prediction errors across the test set. This function also includes an option to balance class sizes in the training set to mitigate potential class imbalance.
#'
#' @param data A data frame containing the variables and response variable.
#' @param variable A string specifying the name of the response variable.
#' @param num_iterations The number of iterations to run, with reseeding at each iteration.
#' @param ntree The number of trees for the random forest in each iteration.
#' @param datasplit The proportion of the dataset to allocate to the training set (e.g., 0.7 for 70% training data).
#' @param mtry The number of variables randomly sampled at each split in the random forest.
#' @param balance Logical (default = TRUE). If TRUE, balances the classes in the training set by downsampling to match the smallest class size.
#'
#' @return A list containing:
#' - `overall_accuracy`: The overall_accuracy of the model calculated from the diagonal of the confusion matrix.
#'   - `err.obs`: A data frame of out-of-bag (OOB) error rates for each iteration.
#'   - `conf_matrix`: The cumulative confusion matrix across all iterations.
#'   - `fit.list`: A list of random forest model objects, one for each iteration.
#'   - `training_class_size`: The class distribution in the training set after balancing.
#'   - `prediction_errors`: A vector of mean errors per observation across all iterations.
#'   - `conf_matrix_percent`: The normalized confusion matrix (as percentages).
#'
#' @examples
#' # Example usage:
#' model <- run_rf(data = test_data, variable = "Species", num_iterations = 5, ntree = 250, datasplit = 0.7, mtry = 2)
#'
#' @export

run_rf <- function(data, variable, num_iterations, ntree, datasplit, mtry, balance = TRUE) {
  start_time <- Sys.time()

  if (!is.factor(data[[variable]])) {
    data[[variable]] <- as.factor(data[[variable]])
  }

  # Required packages
  listOfPackages <- c("caret", "randomForest", "tidyverse")
  for (pkg in listOfPackages) {
    if (!(pkg %in% installed.packages())) {
      install.packages(pkg, dependencies = TRUE)
    }
    require(pkg, character.only = TRUE)
  }

  # Initialize lists
  fit.list <- list()
  conf_matrix_list <- list()
  importance_df <- data.frame()
  OOB_Error <- numeric(num_iterations)

  # Initialize an error tracking matrix for all data points
  prediction_errors <- matrix(0, nrow = nrow(data), ncol = num_iterations)

  # Initialize confusion matrix
  variable_levels <- levels(data[[variable]])
  conf_matrix <-
    matrix(0,
           nrow = length(variable_levels),
           ncol = length(variable_levels))
  colnames(conf_matrix) <-
    rownames(conf_matrix) <- variable_levels
  cat("Model Started... \n")


  for (i in 1:num_iterations) {
    #cat("Repetition number ", i, "\n")

    # Set seed for reproducibility
    set.seed(123 + i)


    # Split the data
    train_index <- createDataPartition(y = data[[variable]], p = datasplit, list = FALSE)
    training_set <- data[train_index, ]
    testing_set <- data[-train_index, ]

    # Ensure factor levels
    training_set[[variable]] <- as.factor(training_set[[variable]])
    testing_set[[variable]] <- as.factor(testing_set[[variable]])

    # Balance the training set if the balance parameter is TRUE
    if (balance) {
      # Count the number of samples in each class
      class_counts <- table(training_set[[variable]])
      min_class_size <- min(class_counts)

      # Create a balanced training set by sampling
      balanced_data <- training_set %>%
        group_by(!!sym(variable)) %>%
        slice_sample(n = min_class_size) %>%
        ungroup()

      training_set <- balanced_data


    }
    training_class_size <- table(training_set[[variable]])

    # Fit random forest
    fit.forest <- randomForest(
      formula = as.formula(paste(variable, "~ .")),
      mtry = mtry,
      data = training_set,
      ntree = ntree,
      importance = TRUE
    )
    fit.list[[i]] <- fit.forest

    # OOB error
    oob_error <- 1 - fit.forest$err.rate[nrow(fit.forest$err.rate), "OOB"]
    OOB_Error[i] <- oob_error

    # Predictions and errors for the testing set
    predictions <- predict(fit.forest, newdata = testing_set)

    # Create confusion matrix for the current iteration
    current_conf_matrix <- table(
      factor(predictions, levels = variable_levels),
      factor(testing_set[[variable]], levels = variable_levels)
    )

    # Convert current_conf_matrix to matrix with same dimensions as conf_matrix
    current_conf_matrix <- as.matrix(current_conf_matrix)
    dimnames(current_conf_matrix) <-
      list(levels(factor(testing_set[[variable]], levels = variable_levels)),
           levels(factor(testing_set[[variable]], levels = variable_levels)))

    # Update the overall confusion matrix
    conf_matrix <- conf_matrix + current_conf_matrix

    # Store the current confusion matrix
    conf_matrix_list[[i]] <- current_conf_matrix

    # Extract and store variable importance
    importance_scores <-
      as.data.frame(importance(fit.forest, type = 1))
    importance_scores$variable <- rownames(importance_scores)
    importance_df <- rbind(importance_df, importance_scores)

    # Track individual errors for this iteration (1 = error, 0 = correct)
    misclassified <- as.numeric(predictions != testing_set[[variable]])

    # Save these errors back into the error matrix
    prediction_errors[-train_index, i] <- misclassified

    # Save confusion matrix, importance, etc. as before
  }

  # Calculate mean error for each data point across iterations
  mean_errors_per_point <- rowMeans(prediction_errors)

  # Calculate overall metrics or return necessary results
  # For example, you can calculate mean importance scores across iterations
  mean_importance <-
    aggregate(. ~ variable, data = importance_df, FUN = mean)

  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  cat("Elapsed time:", elapsed_time, "\n")

  # Normalize the confusion matrix by column sums to get percentages
  col_sums <- colSums(conf_matrix)
  conf_matrix_percent <- sweep(conf_matrix, 2, col_sums, FUN = "/") * 100

  # Round to one decimal place
  conf_matrix_percent <- round(conf_matrix_percent, 1)

  # Compute statistics
  overall_accuracy <- round(sum(diag(conf_matrix)) / sum(conf_matrix)*100, digits = 1)

   # Display results
  cat("Accuracy of validation(test) dataset: ", overall_accuracy, "% \n")


  return(list(
    overall_accuracy =  overall_accuracy,
    err.obs = data.frame(OOB_Error),
    conf_matrix = conf_matrix,
    fit.list = fit.list,
    training_class_size = training_class_size,
    prediction_errors = mean_errors_per_point,
    conf_matrix_percent = conf_matrix_percent,
    importance_df = importance_df,
    mean_importance = mean_importance
  ))
}
