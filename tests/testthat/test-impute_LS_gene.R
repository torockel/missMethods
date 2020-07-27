## Basic tests for impute_LS_gene() -------------------------------------------
test_that("impute_LS_gene() works (basic test, only check for anyNA)", {
  set.seed(1234)
  ds_mis <- mvtnorm::rmvnorm(20, rep(0, 5), diag(1, 5))
  ds_mis <- delete_MCAR(ds_mis, 0.2, 1:4)
  expect_false(anyNA(impute_LS_gene(ds_mis, verbose = FALSE)))
})

test_that("impute_LS_gene() works with completely missing rows and gives a message", {
  set.seed(1234)
  ds_mis <- mvtnorm::rmvnorm(20, rep(0, 5), diag(1, 5))
  ds_mis[5:6, ] <- NA
  ds_imp <- expect_silent(impute_LS_gene(ds_mis, verbose = FALSE))
  expect_false(anyNA(ds_imp))
  expect_equal(ds_imp[5, ], colMeans(ds_mis, na.rm = TRUE))

  ds_imp_verbose <- expect_message(
    impute_LS_gene(ds_mis, verbose = TRUE),
    "No observed value in row(s) 5, 6. These rows were imputed with column means.",
    fixed = TRUE,
    all = TRUE
  )
  expect_false(anyNA(ds_imp_verbose))
  expect_equal(ds_imp_verbose[5, ], colMeans(ds_mis, na.rm = TRUE))
})

test_that("impute_LS_gene() works when there is no suitable row and give a message", {
  # 1. check:
  # Every row has at least min_common_obs observations.
  # However, no suitable row for any row!
  # This ds_mis would result in the following error in the jar-file from
  # Bo et al. (2004): "Error in imputation engine"
  set.seed(123)
  ds_mis <- mvtnorm::rmvnorm(11, rep(0, 6), diag(1, 6))
  ds_mis[1, 1] <- NA
  ds_mis[2:11, 2] <- NA
  ds_imp <- expect_silent(impute_LS_gene(ds_mis, min_common_obs = 5, verbose = FALSE))
  expect_false(anyNA(ds_imp))
  ds_imp_verbose <- expect_message(
    impute_LS_gene(ds_mis, min_common_obs = 5, verbose = TRUE),
    "No suitable row for the imputation of (parts of) row(s) 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 found",
    fixed = TRUE,
    all = TRUE
  )
  expect_false(anyNA(ds_imp_verbose))

  # 2. check:
  # One row with less than min_common_obs observations
  ds_mis2 <- mvtnorm::rmvnorm(11, rep(0, 6), diag(1, 6))
  ds_mis2[3, 3:5] <- NA
  ds_imp2 <- expect_silent(impute_LS_gene(ds_mis2, min_common_obs = 5, verbose = FALSE))
  expect_false(anyNA(ds_imp2))
  expect_equal(ds_imp2[3, 3:5], rep(mean(ds_mis2[3, ], na.rm = TRUE), 3))
  ds_imp2_warn <- expect_message(
    impute_LS_gene(ds_mis2, min_common_obs = 5, verbose = TRUE),
    "Not enough observed values in row(s) 3. These rows were imputed with osbserved row means.",
    fixed = TRUE
  )
  expect_false(anyNA(ds_imp2_warn))
})


## Test argument checking -----------------------------------------------------

test_that("k is checked", {
  expect_error(
    impute_LS_gene(data.frame(X = 1:2), k = 3),
    "k must be smaller as nrow(ds)",
    fixed = TRUE
  )
})

test_that("min_common_obs is checked", {
  expect_error(
    impute_LS_gene(data.frame(X = 1:20), min_common_obs = 2),
    "min_common_obs should be bigger as 2 to allow calculations for correlations and regression models",
    fixed = TRUE
  )
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
  ds_triangle_mis <- readRDS(test_path(file.path("datasets", "ds_triangle_mis.rds")))
  # The jar-file from Bo et al. imputes rows 1:4 with mean values:
  ds_triangle_LS_gene_Bo <- readRDS(test_path(file.path("datasets", "ds_triangle_LS_gene_Bo.rds")))
  # Therefore, min_common_obs = 5 (test with other datasets showed the same pattern)
  expect_equal(
    ds_triangle_LS_gene_Bo,
    round(impute_LS_gene(ds_triangle_mis, min_common_obs = 5), 3)
  )
})

test_that("impute_LS_gene() imputes like Bo et al. (2004) (MCAR, 100x7)", {
  ds_100x7_LS_gene_Bo <- readRDS(test_path(file.path("datasets", "ds_100x7_LS_gene_Bo.rds")))

  ds_100x7_mis_MCAR <- readRDS(test_path(file.path("datasets", "ds_100x7_mis_MCAR.rds")))
  # Cure some rounding problems due to saving:
  ds_mis <- ds_100x7_LS_gene_Bo
  ds_mis[is.na(ds_100x7_mis_MCAR)] <- NA

  ds_imp <- round(impute_LS_gene(ds_mis, min_common_obs = 5), 3)
  # Need some tolerance for rounding:
  expect_equal(ds_100x7_LS_gene_Bo, ds_imp, tolerance = 0.005)
  # Less than 2 % of the imputed values have a difference bigger difference than 0.005 (round 3 digits!)
  expect_true(sum(abs(ds_100x7_LS_gene_Bo - ds_imp) > 0.005) / sum(is.na(ds_mis)) < 0.02)
  # All differences are smaller than 0.015:
  expect_equal(sum(abs(ds_100x7_LS_gene_Bo - ds_imp) >= 0.015), 0)

  # Conclusion: Both methods seem to return the same imputation values (only deviations because of rounding)
})


## check helpers --------------------------------------------------------------
test_that("calc_lm_coefs_simple_reg() returns correct values", {
  expect_equal(unname(calc_lm_coefs_simple_reg(1:10, 1:10)), c(0, 1))
  expect_equal(unname(calc_lm_coefs_simple_reg(2 + 3 * (1:10), 1:10)), c(2, 3))
})


test_that("calc_max_common_obs() works", {
  M <- matrix(c(
    FALSE, FALSE, FALSE, FALSE,
    TRUE, TRUE, FALSE, TRUE,
    FALSE, FALSE, FALSE, TRUE,
    TRUE, TRUE, FALSE, FALSE,
    FALSE, FALSE, TRUE, TRUE,
    TRUE, FALSE, TRUE, FALSE
  ), ncol = 4, byrow = TRUE)
  expect_equal(calc_common_obs(M, M[1, ]), c(4, 1, 3, 2, 2, 2))
  expect_equal(calc_common_obs(M, M[2, ]), c(1, 1, 1, 1, 0, 0))
  expect_equal(calc_common_obs(M, M[3, ]), c(3, 1, 3, 1, 2, 1))
  expect_equal(calc_common_obs(M, M[4, ]), c(2, 1, 1, 2, 0, 1))
})
