#' Linear Model Function
#'
#' This function fits a linear model.
#'
#' @param formula object of class formula to fit a linear model for.
#' @param data input data frame.
#'
#' @return A matrix summarizing the coefficients of the linear fit.
#'
#' @importFrom stats model.frame model.matrix model.response na.omit predict pt sd
#' @importFrom dplyr filter
#' @examples
#' my_lm(mpg ~ hp + wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  # extract model matrix X
  X <- model.matrix(formula, data)
  # extract a model frame object
  model_frame <- model.frame(formula, data)
  # extract model response Y
  Y <- model.response(model_frame)
  # solve for linear regression coefficients
  coeffs <- solve((t(X) %*% X)) %*% (t(X) %*% Y)
  # compute degrees of freedom: sample size minus num of covariates
  df <- nrow(X) - ncol(X)
  # estimate residual variance
  var <- sum(((Y - X %*% coeffs) ^ 2) / df)
  # estimate standard error for each coefficient
  se <- sqrt(var * solve(t(X) %*% X))
  se_diag <- diag(se)
  # compute t values
  t_vals <- coeffs / se_diag
  # compute p values
  p_vals <- 2 * pt(abs(t_vals), df, lower.tail = FALSE)
  # create output table
  output <- cbind(coeffs, se_diag, t_vals, p_vals)
  # set column names
  colnames(output) <- c("Estimate", "Std. error", "t value", "Pr(>|t|)")
  return(output)
}
