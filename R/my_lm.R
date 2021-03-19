#' Linear Model Function
#'
#' This function fits a linear model.
#'
#' @param formula object of class formula to fit a linear model for.
#' @param data input data frame.
#'
#' @return A list with elements \code{Coefficients} which is a matrix containing
#'  coefficients, standard error, t value, and p value, \code{ybar} the predictions
#'
#' @examples
#' my_lm(mpg ~ hp + wt, data = mtcars)
#' my_lm(lifeExp ~ gdpPerCap + continent, data = my_gapminder)
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
  # create predictions
  ybar = X %*% coeffs
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
  # create Coefficients matrix
  Coefficients = cbind(coeffs, se_diag, t_vals, p_vals)
  # set column names
  colnames(Coefficients) <- c("Estimate", "Std. error", "t value", "Pr(>|t|)")
  # create output list
  output <- list("Coefficients" = Coefficients, "ybar" = ybar)

  return(output)
}
