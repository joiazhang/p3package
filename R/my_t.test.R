#' t-test function
#'
#' This function performs a one sample t-test.
#'
#' @param x Numeric vector of data.
#' @param alternative character string specifying the alternative hypothesis
#'  and this should be \code{"two.sided"}, \code{"less"}, or \code{"greater"}
#'  otherwise an error will be thrown. Default is \code{"two.sided"}.
#' @param mu a numeric indicating the null hypothesis value of the mean.
#'
#' @return A list with elements: \code{test_stat} the numeric test
#'  statistic, \code{df} the degrees of freedom, \code{alternative} the value of
#'  the parameter \code{alternative}, and \code{p_val} the numeric p value.
#'
#' @importFrom stats model.frame model.matrix model.response na.omit predict pt sd
#' @importFrom dplyr filter
#' @examples
#' my_t.test(x = sims, alternative = "greater", mu = true_mean)
#' my_t.test(x = sims, alternative = "less", mu = true_mean)
#' my_t.test(x = sims, mu = true_mean)
#'
#' @export
my_t.test <- function(x, alternative = "two.sided", mu) {
  # print error message if value for alternate is invalid
  if(alternative != "less" & alternative != "greater" & alternative != "two.sided") {
    stop("The value of alternative must one of: \"two.sided\", \"less\", or \"greater\"")
  }
  # compute estimate
  est <- mean(x)
  # define degrees of freedom
  df <- length(x) - 1
  # compute standard error
  se <- sd(x) / sqrt(length(x))
  # calculate t-tests
  t_obs <- (est - mu) / se

  # compute p-value depending on the value of alternate
  if (alternative == "less") {
    p_val <- pt(t_obs, df)
  } else if (alternative == "greater") {
    p_val <- pt(t_obs, df, lower.tail = FALSE)
  } else {
    # alternative is "two.sided"
    p_val <- 2 * pt(abs(t_obs), df, lower.tail = FALSE)
  }
  # create output list
  output <- list("test_stat" = t_obs,
                 "df" = df,
                 "alternative" = alternative,
                 "p_val" = p_val)
  return(output)
}
