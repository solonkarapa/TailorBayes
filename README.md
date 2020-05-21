
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TailorBayes

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/solonkarapa/TailorBayes.svg?branch=master)](https://travis-ci.org/solonkarapa/TailorBayes)
[![codecov](https://codecov.io/gh/solonkarapa/TailorBayes/branch/master/graph/badge.svg)](https://codecov.io/gh/solonkarapa/TailorBayes)

Contains functions to perform Tailored Bayesian inference for binary
classification. Markov Chain Monte Carlo (MCMC) is used to simulate the
posterior. The output is a ‘coda’ mcmc object that can then be
summarised using the
[coda](https://cran.r-project.org/web/packages/coda/index.html) package.

## Installation

``` r
# Install from GitHub
devtools::install_github("solonkarapa/TailorBayes")
```

## Usage

Get started with

``` r
library(TailorBayes)
```

The main function of the package is `metrop_tailor()`

``` r
# Run to see the documentation
?metrop_tailor
```

A vignette will follow soon.
