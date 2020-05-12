

# helper functions ---------------------------
expit <- function(x) exp(x)/(1 + exp(x))
logit <- function(p) log(p/(1 - p))

# prior on log(param) ---------------------------
#prior_default <- function(param) {

#    betas <- sum(dnorm(param, mean = 0, sd = 1000, log = TRUE))

#    return(betas)

#    }

prior_default <- function(prior_mean = 0, prior_sd = 1000)function(param) {

    betas <- sum(dnorm(param, mean = prior_mean, sd = prior_sd, log = TRUE))

    return(betas)

}

# unnormalised posterior ---------------------------
posterior <- function(formula, data, param, lambda, pi_u, t, distance_measure, epsilon,
                      h_function = NULL, prior_fun, ...) {

    like(formula, data, param, lambda, pi_u, t, distance_measure, epsilon, h_function) + prior_fun(param, ...)

    }

#posterior(y ~ x, dat, param, lambda, p, t)

# proposal kernels ---------------------------
#library(MASS)
proposal_fun <- function(param, sigma) {

    param <- MASS::mvrnorm(1, mu = param, sigma)

    return(param)
}

