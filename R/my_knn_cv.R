#' Linear Model Function
#'
#' This function fits a linear model.
#'
#' @param train input data frame with covariates of the data.
#' @param cl true labels of each observation.
#' @param k_nn integer representing the number of neighbors.
#' @param k_cv integer representing the number of folds.
#' @keywords prediction
#'
#' @return A list with objects \code{class} a vector of the predicted class for all observations, \code{cv_err} a numeric with the cross-validation misclassification error.
#'
#' @examples
#' my_knn_cv(my_penguins[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")], my_penguins$species, 5, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # remove NAs from train and cl
  temp <- cbind(train, cl)
  temp <- na.omit(temp)
  train <- temp[, -5]
  cl <- temp[, 5]
  # assign folds to each observation
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  train$fold <- fold
  # initialize lists
  yhat_cv <- list()
  missclass_err <- list()
  # cross validation for each fold
  for (i in 1:k_cv) {
    # find indexes for the test fold
    inds <- train$fold == i
    # predict species of test fold based on knn of training folds
    train_folds <- train[!inds, -5]
    cl_train_folds <- cl[!inds]
    test_fold <- train[inds, -5]
    cl_test_fold <- cl[inds]
    yhat_cv[[i]] <- class::knn(train = train_folds, test = test_fold, cl = cl_train_folds, k = k_nn)
    # record misclassification error
    correct_pred <- cl_test_fold == yhat_cv[[i]]
    missclass_err[[i]] <- as.double(length(correct_pred[correct_pred == F])) / length(cl_test_fold)
  }
  # compute knn with full data
  class <- class::knn(train = train, test = train, cl = cl, k = k_nn)
  # compute average misclassification rate from cross validation
  cv_error <- mean(unlist(missclass_err))
  output <- list("class" = class, "cv_error" = cv_error)
  return(output)
}
