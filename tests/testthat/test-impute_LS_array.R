## Basic tests for impute_LS_array() -------------------------------------------
test_that("impute_LS_array() works (basic test, only check for anyNA)", {
  set.seed(1234)
  ds_miss <- MASS::mvrnorm(20, rep(0, 5), diag(1, 5))
  ds_miss <- missMethods::delete_MCAR(ds_miss, 0.2, 1:4)
  expect_false(anyNA(impute_LS_array(ds_miss)))
})

test_that("impute_LS_array() works with completely missing row", {
  set.seed(1234)
  ds_miss <- MASS::mvrnorm(20, rep(0, 5), diag(1, 5))
  ds_miss[5, ] <- NA
  ds_miss[1:3, 2:4] <- NA # some additional NAs
  ds_imp <- expect_warning(
    impute_LS_array(ds_miss),
    "No observed value in row 5. This row is imputed with column means.",
    fixed = TRUE,
    all = TRUE
  )
  expect_false(anyNA(ds_imp))
  expect_equal(ds_imp[5, ], suppressWarnings(colMeans(impute_LS_gene(ds_miss))))
})

test_that("impute_LS_array() works with small data frames", {
  expect_equal(
    impute_LS_array(data.frame(X = 1:11, Y = c(1:10, NA))),
    data.frame(X = 1:11, Y = 1:11)
  )
})


## Comparing impute_LS_array() to original results from Bo et al. -------------
# For some remarks see test-impute_LS_gene.R

test_that("impute_LS_array() works with dataset triangle miss", {
  # The missing values in this file were created with upper.tri, which results in a monotone pattern.
  # The rows 1:15 have 1:15 observed values.
  ds_triangle_miss <- readRDS(test_path(file.path("datasets", "ds_triangle_miss.rds")))
  ds_triangle_LS_array_Bo <- readRDS(test_path(file.path("datasets", "ds_triangle_LS_array_Bo.rds")))

  expect_equal(
    ds_triangle_LS_array_Bo,
    round(impute_LS_array(ds_triangle_miss, min_common_obs = 5), 3)
  )
})

test_that("impute_LS_array() imputes like Bo et al. (2004) (MCAR, 100x7)", {

  # Use LS_gene imputed dataset from Bo et al. (2004) as base for LS_array
  ds_100x7_LS_gene_Bo <- readRDS(test_path(file.path("datasets", "ds_100x7_LS_gene_Bo.rds")))

  ds_100x7_miss_MCAR <- readRDS(test_path(file.path("datasets", "ds_100x7_miss_MCAR.rds")))
  # Cure some rounding problems due to saving:
  ds_miss <- ds_100x7_LS_gene_Bo
  ds_miss[is.na(ds_100x7_miss_MCAR)] <- NA

  ds_imp <- round(impute_LS_array(ds_miss, min_common_obs = 5, ds_impute_LS_gene = ds_100x7_LS_gene_Bo), 3)

  ds_100x7_LS_array_Bo <- readRDS(test_path(file.path("datasets", "ds_100x7_LS_array_Bo.rds")))
  # Need some tolerance because of rounding:
  expect_equal(ds_100x7_LS_array_Bo, ds_imp, tolerance = 0.005)
  # All differences are smaller than 0.002: (round 3 digits!)
  expect_equal(sum(abs(ds_100x7_LS_array_Bo - ds_imp) >= 0.002), 0 )

  # Conclusion: Both methods seem to return the same imputation values (only deviations because of rounding)
})
