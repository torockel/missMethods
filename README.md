
<!-- README.md is generated from README.Rmd. Please edit that file -->

# missMethods

<!-- badges: start -->

[![R build
status](https://github.com/torockel/missMethods/workflows/R-CMD-check/badge.svg)](https://github.com/torockel/missMethods/actions)
<!-- badges: end -->

The goal of missMethods is to make the comparison of missing data
methods via simulation easier.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("torockel/missMethods")
```

<!-- You can install the released version of missMethods from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("missMethods") -->

<!-- ``` -->

<!-- And the development version from [GitHub](https://github.com/) with: -->

<!-- ``` r -->

<!-- # install.packages("devtools") -->

<!-- devtools::install_github("torockel/missMethods") -->

<!-- ``` -->

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(missMethods)
set.seed(123)
ds_comp <- data.frame(X = rnorm(100), Y = rnorm(100))
ds_miss <- delete_MCAR(ds_comp, 0.3)
ds_imp <- impute_mean(ds_miss)
evaluate_imputed_values(ds_imp, ds_comp, "RMSE")
#> [1] 0.5328238
```
