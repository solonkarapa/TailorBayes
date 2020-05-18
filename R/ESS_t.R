
#' Effective Sample Size for Tailoring
#'
#' @description
#' Calculate the effective sample size for tailoring.
#'
#' @inheritParams metrop_tailor
#'
#' @details
#'  The Effective Sample Size is calculated as follows:
#'
#'  \deqn{ESS_t = \frac{\sum_{i=1}{^n} w_i}{n}, }
#'
#'  where \eqn{w_i = \exp - \{ \lambda h(pi_u, t, \epsilon) \} } and \eqn{i = 1, \ldots, n} (the sample size).
#'
#'  The default function \eqn{h()} is the squared error with \eqn{\epsilon = 0}, i.e
#'
#'  \eqn{h(pi_u, t, \epsilon) := (pi_u - t)^2}.
#'
#' @return Scalar between 0 and 1.
#'
#' @export
#'
# calculate ESS_t ---------------------------

ESS_t <- function(lambda, pi_u, t, distance_measure, epsilon, h_function = NULL, ...){

    if (is.null(h_function)) {
        w <- weights(lambda, pi_u, t, distance_measure, epsilon)
        } else {
            h_function <- do.call(h_function, list(pi_u, t, ...))
            w <- weights(lambda, pi_u, t, distance_measure, epsilon, h_function, ...)
            }

    ess <- sum(w)/length(w)

    return(ess)
}


#' @examples
#' data(out)
#' set_lambda <- 4
#' p <- rbeta(nrow(out), shape1 = 3, shape2 = 7) # simulated probabilities from beta with mean 0.3
#' threshold <- 0.3
#'
#' # ESS using default values of 'distance_measure' and 'epsilon'
#' ESS_t(set_lambda, pi_u = p, t = threshold)
#'
#' # ESS using "asolute" loss as distance measure
#' ESS_t(set_lambda, pi_u = p, t = threshold, distance_measure = "absolute")
#'
#' # ESS using "asolute" loss as distance measure and epsilon-insensitivity param 0.01
#' ESS_t(set_lambda, pi_u = p, t = threshold, distance_measure = "absolute", epsilon = 0.01)
#'

