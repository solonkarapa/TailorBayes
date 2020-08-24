

# test .... (function)


# test output

library(TailoredBayes)

#tolerance <- 1e-10

set.seed(42)

n <- 100
rho <- 0.5
beta0 <- 0.25
beta1 <- 1
beta2 <- 0.5

x1 <- rnorm(n)
x2 <- rho * x1 + sqrt(1 - rho^2) * rnorm(n)
eta <- beta0 + beta1 * x1 + beta2 * x2
p <- 1 / (1 + exp(- eta))
y <- as.numeric(runif(n) < p)

data = data.frame(y = y, x1 = x1, x2 = x2)

# args
set_lambda <- 0
p <- rep(1, nrow(data))
threshold <- 0.5
# distance_measure and epsilon take the default values: "squared" and 0, respectively
# initial values is a vector of zeros

# SET 1: unspecified h_function ---------------------------
metropol <- metrop_tailor(y ~ x1 + x2, data, lambda = set_lambda, pi_u = p, t = threshold,
                          warm_up = 1000, burn_in = 1000, n_mcmc = 5000,
                          tune = 2.38, iter_adapt = 200, proposal_function = proposal_fun)

test_that("input/output matching when h_function is null", {
    #dplyr::between(metropol$accept, 0, 1)
    expect_equal(metropol$accept, 0.324, tolerance = 0.1)
    expect_equal(metropol$lambda, set_lambda)
    expect_equal(metropol$pi_u, p)
    expect_equal(metropol$t, threshold)
    expect_equal(metropol$distance_measure, "squared")
    expect_equal(metropol$epsilon, 0)
    expect_equal(metropol$h_function, NULL)
    expect_equal(metropol$initial, c(0, 0, 0))

})

# SET 2: specified h_function ---------------------------
my_h_function <- function(ind) function(pi_u, t){
    if(ind == 1){
        (pi_u - t)^2
    } else {
        abs(pi_u - t)
    }
}

metropol <- metrop_tailor(y ~ x1 + x2, data, lambda = set_lambda, pi_u = p, t = threshold,
                          h_function = my_h_function(1),
                          warm_up = 1000, burn_in = 1000, n_mcmc = 5000,
                          tune = 2.38, iter_adapt = 200)


test_that("input/output matching when h_function is not null", {
    expect_equal(metropol$accept, 0.298, tolerance = 0.1)
    expect_equal(metropol$lambda, set_lambda)
    expect_equal(metropol$pi_u, p)
    expect_equal(metropol$t, threshold)
    expect_equal(metropol$distance_measure, "NULL")
    expect_equal(metropol$epsilon, "NULL")
    expect_equal(length(metropol$h_function), nrow(data))
})

# SET 3: specified h_function and prior ---------------------------


