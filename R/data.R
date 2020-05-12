


#' Simulated logistic regression data.
#'
#'
#' @description As the title suggests.
#'
#' @format A data frame with 100 observations and 2 variables.
#'
#' \describe{
#'  \item{x1}{quantitative predictor.}
#'  \item{x2}{quantitative predictor.}
#'  \item{y}{Bernoulli response.}
#' }
#'
#' @examples
#' library(TailorBayes)
#' data(out)
#' fit <- glm(y ~ x1 + x2, family = binomial, data = out)
#' summary(fit)
#'
#'
#' @keyword{datasets}
#'
