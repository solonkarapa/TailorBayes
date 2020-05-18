
# test .... (function)

# tests for ESS function

library(TailorBayes)

tolerance <- 1e-4

set.seed(42)
n <- 100
p <- runif(n, min = 0.01, max = 0.99)
threshold <- 0.5

# SET 1.1: unspecified h_function ---------------------------
# distance and epsilon as defaults
test_that("ESS_t works for different lambdas with default distance and epsilon", {
        ESS_test <- function(lambda) ESS_t(lambda, pi_u = p, t = threshold)

        expect_equal(ESS_test(0), 1)
        expect_equal(ESS_test(1), 0.9191, tolerance)
        expect_equal(ESS_test(10), 0.5333, tolerance)
        expect_equal(ESS_test(100), 0.1667, tolerance)
})

# SET 1.2: unspecified h_function ---------------------------
# only epsilon as default
test_that("ESS_t works for different lambdas with default epsilon", {
        ESS_test <- function(lambda) ESS_t(lambda, pi_u = p, t = threshold, distance_measure = "absolute")

        expect_equal(ESS_test(0), 1)
        expect_equal(ESS_test(1), 0.7818584, tolerance)
        expect_equal(ESS_test(10), 0.1906191, tolerance)
        expect_equal(ESS_test(100), 0.01393055, tolerance)
})

# SET 1.3: unspecified h_function ---------------------------
test_that("ESS_t works for different lambdas", {
        ESS_test <- function(lambda) ESS_t(lambda, pi_u = p, t = threshold,
                                           distance_measure = "absolute", epsilon = 0.01)

        expect_equal(ESS_test(0), 1)
        expect_equal(ESS_test(1), 0.7897162, tolerance)
        expect_equal(ESS_test(10), 0.2106666, tolerance)
        expect_equal(ESS_test(100), 0.03786716, tolerance)
})

# SET 2.1: specified h_function ---------------------------
my_h_function <- function(ind) function(pi_u, t){
        if(ind == 1){
                (pi_u - t)^2
        } else {
                abs(pi_u - t)
        }
}

test_that("ESS_t works for different lambdas with custom h_function and default distance and epsilon", {
        ESS_test <- function(lambda) ESS_t(lambda, pi_u = p, t = threshold, h_function = my_h_function(1))

        expect_equal(ESS_test(0), 1)
        expect_equal(ESS_test(1), 0.9191, tolerance)
        expect_equal(ESS_test(10), 0.5333, tolerance)
        expect_equal(ESS_test(100), 0.1667, tolerance)
})

#ESS_t(1, p, threshold, h_function = my_h_function, ind = 1)
