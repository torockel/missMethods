## Basic tests for impute_LS_array() -------------------------------------------
test_that("impute_LS_array() works (basic test, only check for anyNA)", {
  set.seed(1234)
  ds_mis <- MASS::mvrnorm(20, rep(0, 5), diag(1, 5))
  ds_mis <- delete_MCAR(ds_mis, 0.2, 1:4)
  expect_false(anyNA(impute_LS_array(ds_mis)))
})

test_that("impute_LS_array() works with completely missing row and verbose", {
  set.seed(1234)
  ds_mis <- MASS::mvrnorm(20, rep(0, 5), diag(1, 5))
  ds_mis[5, ] <- NA

  # verbosity = 0
  ds_imp_silent <- expect_silent(
    impute_LS_array(ds_mis, verbosity = 0L)
  )
  expect_false(anyNA(ds_imp_silent))
  expect_equal(ds_imp_silent[5, ], suppressWarnings(colMeans(impute_LS_gene(ds_mis))))

  # verbosity = 1
  ds_imp_verb1 <- expect_warning(
    impute_LS_array(ds_mis, verbosity = 1L),
    "No observed value in row 5. This row is imputed with column means.",
    fixed = TRUE,
    all = TRUE
  )
  expect_equal(ds_imp_verb1, ds_imp_silent)

  # verbosity = 2
  ds_imp_verb2 <- expect_warning(
    impute_LS_array(ds_mis, verbosity = 2L),
    "The missing values of following rows were imputed with (parts of) mu: 5",
    fixed = TRUE,
    all = TRUE
  )
  expect_equal(ds_imp_verb2, ds_imp_silent)

  # verbosity = 3
  verify_output(test_path("test-impute_LS_array-verbosity.txt"),
                ds_imp <- impute_LS_array(ds_mis, verbosity = 3L))

  ds_imp_verb3 <- suppressWarnings(impute_LS_array(ds_mis, verbosity = 3L))
  expect_equal(ds_imp_verb3, ds_imp_silent)

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
  ds_triangle_mis <- readRDS(test_path(file.path("datasets", "ds_triangle_mis.rds")))
  ds_triangle_LS_array_Bo <- readRDS(test_path(file.path("datasets", "ds_triangle_LS_array_Bo.rds")))

  expect_equal(
    ds_triangle_LS_array_Bo,
    round(impute_LS_array(ds_triangle_mis, min_common_obs = 5), 3)
  )
})

test_that("impute_LS_array() imputes like Bo et al. (2004) (MCAR, 100x7)", {

  # Use LS_gene imputed dataset from Bo et al. (2004) as base for LS_array
  ds_100x7_LS_gene_Bo <- readRDS(test_path(file.path("datasets", "ds_100x7_LS_gene_Bo.rds")))

  ds_100x7_mis_MCAR <- readRDS(test_path(file.path("datasets", "ds_100x7_mis_MCAR.rds")))
  # Cure some rounding problems due to saving:
  ds_mis <- ds_100x7_LS_gene_Bo
  ds_mis[is.na(ds_100x7_mis_MCAR)] <- NA

  ds_imp <- round(impute_LS_array(ds_mis, min_common_obs = 5, ds_impute_LS_gene = ds_100x7_LS_gene_Bo), 3)

  ds_100x7_LS_array_Bo <- readRDS(test_path(file.path("datasets", "ds_100x7_LS_array_Bo.rds")))
  # Need some tolerance because of rounding:
  expect_equal(ds_100x7_LS_array_Bo, ds_imp, tolerance = 0.005)
  # All differences are smaller than 0.002: (round 3 digits!)
  expect_equal(sum(abs(ds_100x7_LS_array_Bo - ds_imp) >= 0.002), 0 )

  # Conclusion: Both methods seem to return the same imputation values (only deviations because of rounding)
})
