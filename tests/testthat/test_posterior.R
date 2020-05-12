

library(TailorBayes)

# tolerance level
tolerance <- 1e-6

data(out)

# args
set_lambda <- 0
p <- rep(1, nrow(out))
threshold <- 0.5
init <- c(1, 1, 1)

# SET 1: likelihood ---------------------------
# calculate likelihood value given data and initial parameters
like_value <- like(y ~ x1 + x2, data = out, param = init, lambda = set_lambda, pi_u = p, t = threshold)

test_that("likelihood values match", {
    expect_equal(like_value, -64.15592, tolerance)
})

# SET 2: prior ---------------------------
# independent normal prior (this is the default)
prior_normal <- function(prior_mean = 0, prior_sd = 1000)function(param) {
    betas <- sum(dnorm(param, mean = prior_mean, sd = prior_sd, log = TRUE))
    }

# independent Cauchy prior
logpriorfun <- function(location, scale)function(param){
    sum(dcauchy(param, location, scale, log = TRUE))
    }

test_that("prior values match", {
    expect_equal(prior_normal()(init), -23.48008, tolerance)
    expect_equal(prior_normal(0, 0.001)(init), -1499982, tolerance)
    expect_equal(logpriorfun(0, 10)(init), -10.3718, tolerance)
})

# SET 3: posterior ---------------------------
test_that("posterior values match", {
    post <- function(arg){
        posterior(y ~ x1 + x2, data = out, param = init, lambda = set_lambda, pi_u = p, t = threshold,
                  prior_fun = arg)
        }

    expect_equal(post(prior_normal()), -87.636, tolerance)
    expect_equal(post(prior_normal(0, 0.001)), -1500046, tolerance)
    expect_equal(post(logpriorfun(0, 10)), -74.52772, tolerance)
})



