## Basic tests for impute_LS_combined() -------------------------------------------
test_that("impute_LS_combined() works (basic test, only check for anyNA)", {
  set.seed(1234)
  ds_miss <- MASS::mvrnorm(20, rep(0, 5), diag(1, 5))
  ds_miss <- missMethods::delete_MCAR(ds_miss, 0.2, 1:4)
  expect_false(anyNA(impute_LS_combined(ds_miss)))
})

test_that("impute_LS_combined() works for small data frames", {
  set.seed(123)
  expect_false(anyNA(impute_LS_combined(data.frame(X = 1:11, Y = c(1:10, NA)))))
})


## Comparing impute_LS_combined() to original results from Bo et al. ----------
# For some remarks see test-impute_LS_gene.R

test_that("impute_LS_combined() works with dataset triangle miss", {
  # The missing values in this file were created with upper.tri, which results in a monotone pattern.
  # The rows 1:15 have 1:15 observed values.
  ds_triangle_miss <- readRDS(test_path(file.path("datasets", "ds_triangle_miss.rds")))
  ds_triangle_LS_array_Bo <- readRDS(test_path(file.path("datasets", "ds_triangle_LS_array_Bo.rds")))
  ds_triangle_LS_gene_Bo <- readRDS(test_path(file.path("datasets", "ds_triangle_LS_gene_Bo.rds")))

  set.seed(1234)
  ds_imp <- round(impute_LS_combined(ds_triangle_miss, min_common_obs = 5), 3)
  # Check that LS_combined is a mixture of LS_array and LS_gene:
  expect_true(all(ds_imp <= pmax(ds_triangle_LS_array_Bo, ds_triangle_LS_gene_Bo)))
  expect_true(all(ds_imp >= pmin(ds_triangle_LS_array_Bo, ds_triangle_LS_gene_Bo)))

  ds_triangle_LS_combined_Bo <- readRDS(test_path(file.path("datasets", "ds_triangle_LS_combined_Bo.rds")))

  # There is some randomness in impute_LS_combined, that we cannot control in the jar-file
  # So we need some tolerance, because exact matches would be rather surprising
  expect_equal(
    ds_triangle_LS_array_Bo,
    ds_imp,
    tolerance = 0.5
  )
})


test_that("impute_LS_combined() imputes like Bo et al. (2004) (MCAR, 100x7)", {
  ds_100x7_LS_array_Bo <- readRDS(test_path(file.path("datasets", "ds_100x7_LS_array_Bo.rds")))
  ds_100x7_LS_gene_Bo <- readRDS(test_path(file.path("datasets", "ds_100x7_LS_gene_Bo.rds")))

  ds_100x7_miss_MCAR <- readRDS(test_path(file.path("datasets", "ds_100x7_miss_MCAR.rds")))
  # Cure some rounding problems due to saving:
  ds_miss <- ds_100x7_LS_gene_Bo
  ds_miss[is.na(ds_100x7_miss_MCAR)] <- NA

  set.seed(1234)
  ds_imp <- round(impute_LS_combined(ds_miss, min_common_obs = 5), 3)

  # Check that LS_combined is a mixture of LS_array and LS_gene:
  expect_true(all(ds_imp <= pmax(ds_100x7_LS_array_Bo, ds_100x7_LS_gene_Bo)))
  expect_true(all(ds_imp >= pmin(ds_100x7_LS_array_Bo, ds_100x7_LS_gene_Bo)))

  ds_100x7_LS_combined_Bo <- readRDS(test_path(file.path("datasets", "ds_100x7_LS_combined_Bo.rds")))

  # Need some tolerance because of randomness:
  expect_equal(ds_100x7_LS_combined_Bo, ds_imp, tolerance = 0.1)

  # Conclusion: Both methods seem to return the "same" imputation values (deviations because of randomness)
})
