## Basic tests for impute_LS_gene() -------------------------------------------
test_that("impute_LS_gene() works (basic test, only check for anyNA)", {
  set.seed(1234)
  ds_miss <- MASS::mvrnorm(20, rep(0, 5), diag(1, 5))
  ds_miss <- missMethods::delete_MCAR(ds_miss, 0.2, 1:4)
  expect_false(anyNA(impute_LS_gene(ds_miss)))
})

test_that("impute_LS_gene() works with completely missing row", {
  set.seed(1234)
  ds_miss <- MASS::mvrnorm(20, rep(0, 5), diag(1, 5))
  ds_miss[5, ] <- NA
  ds_miss[1:3, 2:4] <- NA # some additional NAs
  ds_imp <- expect_warning(
    impute_LS_gene(ds_miss),
    "No observed value in row 5. This row is imputed with column means.",
    fixed = TRUE,
    all = TRUE
  )
  expect_false(anyNA(ds_imp))
  expect_equal(ds_imp[5, ], colMeans(ds_miss, na.rm = TRUE))
})

test_that("impute_LS_gene() works when there is no suitable row", {
  # this ds_miss would result in the following error in the jar-file from
  # Bo et al. (2004): "Error in imputation engine"
  set.seed(123)
  ds_miss <- MASS::mvrnorm(11, rep(0, 6), diag(1, 6))
  ds_miss[1, 1] <- NA
  ds_miss[2:11, 2] <- NA
  ds_imp <- expect_warning(impute_LS_gene(ds_miss, min_common_obs = 5),
                 "No suitable row for the imputation of row")
  expect_false(anyNA(ds_imp))
})


## Comparing impute_LS_gene() to original results from Bo et al. --------------
# Some remarks:
# Bo et al. (2004) provide a jar-file to use their imputation methods:
# http://www.ii.uib.no/~trondb/imputation/
# The following datasets where exported and imputed with this jar-file.
# The jar-file rounds the output to 3 digits, so all datasets are rounded, too.

test_that("impute_LS_gene() imputes row mean values, if to less objects are observed in a row", {
  # The missing values in this file were created with upper.tri, which results in a monotone pattern.
  # The rows 1:15 have 1:15 observed values.
  ds_LS_gene_row_mean_miss <- readRDS(test_path(file.path("datasets", "ds_LS_gene_row_mean_miss.rds")))
  # The jar-file from Bo et al. imputes rows 1:4 with mean values:
  ds_LS_gene_row_mean_Bo <- readRDS(test_path(file.path("datasets", "ds_LS_gene_row_mean_Bo.rds")))
  # Therefore, min_common_obs = 5 (test with other datasets showed the same pattern)
  expect_equal(
    ds_LS_gene_row_mean_Bo,
    round(impute_LS_gene(ds_LS_gene_row_mean_miss, min_common_obs = 5), 3)
  )
})

test_that("impute_LS_gene() imputes like Bo et al. (2004) (MCAR, 100x7)", {

  ds_100x7_LS_gene_Bo <- readRDS(test_path(file.path("datasets", "ds_100x7_LS_gene_Bo.rds")))

  ds_100x7_miss_MCAR <- readRDS(test_path(file.path("datasets", "ds_100x7_miss_MCAR.rds")))
  # cure some rounding problems due to saving:
  ds_miss <- ds_100x7_LS_gene_Bo
  ds_miss[is.na(ds_100x7_miss_MCAR)] <- NA

  ds_imp <- round(impute_LS_gene(ds_miss, min_common_obs = 5), 3)
  # need some tolerance for rounding:
  expect_equal(ds_100x7_LS_gene_Bo, ds_imp, tolerance = 0.005)
  # less than 2 % of the imputed values have a difference bigger difference than 0.005 (round 3 digits!)
  expect_true(sum(abs(ds_100x7_LS_gene_Bo - ds_imp) > 0.005) / sum(is.na(ds_miss)) < 0.02)
  # all differences are smaller than 0.015:
  expect_equal(sum(abs(ds_100x7_LS_gene_Bo - ds_imp) > 0.015), 0)

  # Conclusion: Both methods seem to return the same imputation values (only deviations because of rounding)
})


## check helpers --------------------------------------------------------------
test_that("calc_lm_coefs_simple_reg() returns correct values", {
  expect_equal(unname(calc_lm_coefs_simple_reg(1:10, 1:10)), c(0, 1))
  expect_equal(unname(calc_lm_coefs_simple_reg(2 + 3 * (1:10) , 1:10)), c(2, 3))
})


test_that("calc_max_common_obs() works", {
  M <- matrix(c(FALSE, FALSE, FALSE, FALSE,
                TRUE, TRUE, FALSE, TRUE,
                FALSE, FALSE, FALSE, TRUE,
                TRUE, TRUE, FALSE, FALSE,
                FALSE, FALSE, TRUE, TRUE,
                TRUE, FALSE, TRUE, FALSE), ncol = 4, byrow = TRUE)
  expect_equal(calc_common_obs(M, M[1, ]), c(4, 1, 3, 2, 2, 2))
  expect_equal(calc_common_obs(M, M[2, ]), c(1, 1, 1, 1, 0, 0))
  expect_equal(calc_common_obs(M, M[3, ]), c(3, 1, 3, 1, 2, 1))
  expect_equal(calc_common_obs(M, M[4, ]), c(2, 1, 1, 2, 0, 1))
})

