
#' Optimized Random Forest Model with Hyperparameter Tuning and Iterative Re-seeding
#' Author: Cady A. Lancaster
#' Last Check: 4 November 2024
#' @description This function performs iterative training of a random forest model with hyperparameter tuning, ensuring reproducibility by re-seeding the random number generator with a new seed for each iteration. The function computes model performance metrics, including accuracy, accuracy standard deviation, and confusion matrices, for each iteration, providing an in-depth evaluation of model performance across different random splits of the dataset.
#'
#' The function supports an optional class balancing mechanism in the training set to address class imbalances by downsampling the majority class to the size of the minority class. This can improve the model's ability to handle imbalanced data.
#'
#' @param data A data frame containing the predictor variables and the response variable.
#' @param variable A string specifying the name of the response variable.
#' @param num_iterations The number of iterations to run, with reseeding at each iteration to ensure reproducibility.
#' @param ntree The number of trees to grow in the random forest model during each iteration.
#' @param datasplit The proportion of the dataset to allocate to the training set (e.g., 0.7 for 70% training data).
#' @param mtry The number of predictor variables to sample at each split in the random forest model.
#' @param balance Logical (default = TRUE). If TRUE, the training set will be balanced by downsampling to match the smallest class size.
#'
#' @return A data frame containing:
#' - `Accuracy`: The mean accuracy of the model across all iterations.
#' - `AccuracySD`: The standard deviation of the accuracy across all iterations.
#' - `ntree`: The number of trees used in the random forest model.
#' - `mtry`: The number of variables randomly sampled at each split.
#'
#' @examples
#' # Example usage:
#' model <- optimise_preprocessing(data = test_data, variable = "Species", num_iterations = 5, ntree = 250, datasplit = 0.7, mtry = 2)
#'
#' @export
optimise_preprocessing <- function(data, variable, num_mtry, num_iterations, ntree, datasplit, balance = TRUE, goodcolumns) {
  start_time <- Sys.time()

  # Clean up the dataset (remove unwanted columns)
  tryCatch({
    data <- eval(parse(text = paste0("data", goodcolumns)))
  }, error = function(e) {
    stop("Error in column selection: ", e$message)
  })

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

  # Initialize dataframe to store mtry and accuracy results
  model.df.temp <- data.frame(ntree=rep(ntree, num_iterations),
                              mtry=rep(0, num_iterations),
                              Accuracy=rep(0, num_iterations),
                              AccuracySD=rep(0, num_iterations))

  # Initialize confusion matrix
  variable_levels <- levels(data[[variable]])
  conf_matrix <- matrix(0, nrow = length(variable_levels), ncol = length(variable_levels))
  colnames(conf_matrix) <- rownames(conf_matrix) <- variable_levels

  cat("data preparing \n")
  for (i in 1:num_iterations) {
    mtry_value <- mtry  # Use a fixed mtry or adjust here for tuning
    set.seed(123 + i)

    # Split the data using repeated random sampling
    train_index <- createDataPartition(y = data[[variable]], p = datasplit, list = FALSE)
    training_set <- data[train_index, ]
    testing_set <- data[-train_index, ]

    # Ensure factor levels
    training_set[[variable]] <- as.factor(training_set[[variable]])
    testing_set[[variable]] <- as.factor(testing_set[[variable]])

    # Balance the training set if the balance parameter is TRUE
    #balance <- TRUE
    tryCatch({
      if (balance) {
        class_counts <- table(training_set[[variable]])
        min_class_size <- min(class_counts)
        balanced_data <- training_set %>%
          group_by(!!sym(variable)) %>%
          slice_sample(n = min_class_size) %>%
          ungroup()
        training_set <- balanced_data
      }
    }, error = function(e) {
      stop("Balancing error: ", e$message) # Debugging: Catch and show column selection errors
    })


    #cat("balanced \n")
   #cat("Model Started... \n")
    # Fit random forest
    fit.forest <- randomForest(
      formula = as.formula(paste(variable, "~ .")),
      mtry = mtry_value,
      data = training_set,
      ntree = ntree,
      importance = TRUE
    )

    # Predictions and errors for the testing set
    predictions <- predict(fit.forest, newdata = testing_set)
    accuracy <- sum(predictions == testing_set[[variable]]) / length(testing_set[[variable]])

    current_conf_matrix <- table(
      factor(predictions, levels = variable_levels),
      factor(testing_set[[variable]], levels = variable_levels)
    )

    # Align dimensions of the confusion matrix
    current_conf_matrix <- as.matrix(current_conf_matrix)
    missing_levels <- setdiff(variable_levels, rownames(current_conf_matrix))
    for (level in missing_levels) {
      current_conf_matrix <- rbind(current_conf_matrix, setNames(rep(0, ncol(current_conf_matrix)), colnames(current_conf_matrix)))
      rownames(current_conf_matrix)[nrow(current_conf_matrix)] <- level
    }
#confusion matrix is here for debugging
    conf_matrix <- conf_matrix + current_conf_matrix

    # Store results in model.df
    model.df.temp$Accuracy[i] <- accuracy * 100
    model.df.temp$AccuracySD[i] <- sd(as.numeric(predictions == testing_set[[variable]])) * 100
  }

  model.df <- data.frame(ntree=ntree,
                         mtry=mtry,
                         Accuracy=mean(model.df.temp$Accuracy),
                         AccuracySD=sd(model.df.temp$Accuracy)
                         )

  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  cat("Elapsed time for model:", elapsed_time, "\n")
  print(conf_matrix)
  return(list(model.df,conf_matrix))
}


