
<!-- README.md is generated from README.Rmd. Please edit that file -->

# missMethods

<!-- badges: start -->

[![R build
status](https://github.com/torockel/missMethods/workflows/R-CMD-check/badge.svg)](https://github.com/torockel/missMethods/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/missMethods)](https://CRAN.R-project.org/package=missMethods)
<!-- badges: end -->

The goal of missMethods is to make the creation and handling of missing
data as well as the evaluation of missing data methods easier.

## Installation

You can install the released version of missMethods from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("missMethods")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("torockel/missMethods")
```

## Usage

missMethods mainly provides three types of functions:

  - `delete_` functions for generating missing values
  - `impute_` functions for imputing missing values
  - `evaluate_` functions for evaluating missing data methods

Run `help(package = "missMethods")` to see all functions. More details
for the `delete_` functions are given in a vignette (run
`vignette("Generating-missing-values")`).

## Example

This is a very basic workflow to generate missing values, impute the
generated missing values and evaluate the imputation result:

``` r
library(missMethods)
set.seed(123)
ds_comp <- data.frame(X = rnorm(100), Y = rnorm(100))
ds_miss <- delete_MCAR(ds_comp, 0.3)
ds_imp <- impute_mean(ds_miss)
evaluate_imputed_values(ds_imp, ds_comp, "RMSE")
#> [1] 0.5328238
```
