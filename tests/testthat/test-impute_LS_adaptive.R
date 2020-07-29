## Basic tests for impute_LS_adaptive() ---------------------------------------
test_that("impute_LS_adaptive() works (basic test, check for anyNA and warning)", {
  set.seed(1234)
  ds_mis <- mvtnorm::rmvnorm(20, rep(0, 8), diag(1, 8))
  ds_mis <- delete_MCAR(ds_mis, 0.2, 1:4)
  ds_imp <- expect_warning(
    impute_LS_adaptive(ds_mis, r_max_min = 43, warn_r_max = TRUE),
    "Not enough data for r_max_min = 43. r_max_min reduced to 7!",
    fixed = TRUE,
    all = TRUE
  )
  expect_false(anyNA(ds_imp))
})

test_that("impute_LS_adaptive() works for small matrices", {
  set.seed(1234)
  ds_mis <- mvtnorm::rmvnorm(20, rep(0, 5), diag(1, 5))
  ds_mis <- delete_MCAR(ds_mis, 0.2, 1:4)
  ds_imp <- expect_warning(impute_LS_adaptive(ds_mis, warn_r_max = TRUE),
    "Not enough data for r_max_min = 100. r_max_min reduced to 0!",
    fixed = TRUE,
    all = TRUE
  )
  expect_false(anyNA(ds_imp))
  expect_equal(ds_imp, impute_LS_array(ds_mis))
})

test_that("impute_LS_adaptive() works for data frames", {
  set.seed(123)
  ds_mis <- as.data.frame(mvtnorm::rmvnorm(30, rep(0, 9), diag(2, 9)))
  ds_mis <- delete_MCAR(ds_mis, 0.1)
  ds_imp <- expect_warning(impute_LS_adaptive(ds_mis),
    "Not enough data for r_max_min = 100. r_max_min reduced to 10!",
    fixed = TRUE,
    all = TRUE
  )
  expect_false(anyNA(ds_imp))
})


## Test verbosity -------------------------------------------------------------
test_that("impute_LS_adaptive() works with completely missing row and verbose", {
  set.seed(1234)
  ds_mis <- mvtnorm::rmvnorm(20, rep(0, 7), diag(1, 7))
  ds_mis[5, ] <- NA

  # silent
  ds_imp_silent <- expect_silent(
    impute_LS_adaptive(ds_mis,
      warn_r_max = FALSE, verbose_gene = FALSE, verbose_array = FALSE
    )
  )
  expect_false(anyNA(ds_imp_silent))
  # Completely missing rows are imputed with observed column means from LS_gene
  # and LS_array. So, independent of p, the imputed values will be these means.
  expect_equal(ds_imp_silent[5, ], suppressWarnings(colMeans(impute_LS_gene(ds_mis))))

  # verbose_gene
  ds_imp_verb1 <- expect_message(
    impute_LS_adaptive(ds_mis,
      warn_r_max = FALSE,
      verbose_gene = TRUE, verbose_array = FALSE
    ),
    "No observed value in row(s) 5. These rows were imputed with column means.",
    fixed = TRUE,
    all = TRUE
  )
  expect_equal(ds_imp_verb1, ds_imp_silent)

  # verbose_array
  ds_imp_verb2 <- expect_message(
    impute_LS_adaptive(ds_mis,
      warn_r_max = FALSE,
      verbose_gene = FALSE, verbose_array = TRUE
    ),
    "The missing values of following rows were imputed with (parts of) mu: 5",
    fixed = TRUE,
    all = TRUE
  )
  expect_equal(ds_imp_verb2, ds_imp_silent)

  # verbose_gene and verbose_array
  verify_output(
    test_path("test-impute_LS_adaptive-verbosity.txt"),
    ds_imp_verb3 <- impute_LS_adaptive(ds_mis,
      warn_r_max = FALSE,
      verbose_gene = TRUE, verbose_array = TRUE
    )
  )
  expect_equal(ds_imp_verb3, ds_imp_silent)

  # verbose_gene_p
  ds_imp_verb4 <- expect_message(
    impute_LS_adaptive(ds_mis,
      warn_r_max = FALSE,
      verbose_gene_p = TRUE, verbose_array_p = FALSE
    ),
    "No observed value in row(s) 5. These rows were imputed with column means.",
    fixed = TRUE,
    all = TRUE
  )
  expect_equal(ds_imp_verb4, ds_imp_silent)

  # verbose_array_p
  ds_imp_verb5 <- expect_message(
    impute_LS_adaptive(ds_mis,
      warn_r_max = FALSE,
      verbose_gene_p = FALSE, verbose_array_p = TRUE
    ),
    "The missing values of following rows were imputed with (parts of) mu: 5",
    fixed = TRUE,
    all = TRUE
  )
  expect_equal(ds_imp_verb5, ds_imp_silent)
})


## Comparing impute_LS_adaptive() to original results from Bo et al. ----------
# For some remarks see test-impute_LS_gene.R
# Comparing the results from the jar file and impute_LS_adaptive() seems to be
# not really constructive. The results of the jar file have a very high variance
# (if the seed is changed, the same is true for impute_LS_adaptive()!)
# So, merely a check is done, if the mixing is correct.

test_that("impute_LS_adaptive() works with dataset triangle miss", {
  # The missing values in this file were created with upper.tri, which results in a monotone pattern.
  # The rows 1:15 have 1:15 observed values.
  ds_triangle_mis <- readRDS(test_path(file.path("datasets", "ds_triangle_mis.rds")))
  ds_triangle_LS_array_Bo <- readRDS(test_path(file.path("datasets", "ds_triangle_LS_array_Bo.rds")))
  ds_triangle_LS_gene_Bo <- readRDS(test_path(file.path("datasets", "ds_triangle_LS_gene_Bo.rds")))

  set.seed(1234)
  ds_imp <- expect_warning(round(impute_LS_adaptive(ds_triangle_mis, min_common_obs = 5), 3),
    "Not enough data for r_max_min = 100. r_max_min reduced to 24!",
    fixed = TRUE,
    all = TRUE
  )
  # Check that LS_combined is a mixture of LS_array and LS_gene:
  expect_true(all(ds_imp <= pmax(ds_triangle_LS_array_Bo, ds_triangle_LS_gene_Bo)))
  expect_true(all(ds_imp >= pmin(ds_triangle_LS_array_Bo, ds_triangle_LS_gene_Bo)))
})


test_that("impute_LS_adaptive() works with dataset MCAR, 100x7", {
  ds_100x7_LS_array_Bo <- readRDS(test_path(file.path("datasets", "ds_100x7_LS_array_Bo.rds")))
  ds_100x7_LS_gene_Bo <- readRDS(test_path(file.path("datasets", "ds_100x7_LS_gene_Bo.rds")))

  ds_100x7_mis_MCAR <- readRDS(test_path(file.path("datasets", "ds_100x7_mis_MCAR.rds")))
  # Cure some rounding problems due to saving:
  ds_mis <- ds_100x7_LS_gene_Bo
  ds_mis[is.na(ds_100x7_mis_MCAR)] <- NA

  set.seed(1234)
  ds_imp <- round(impute_LS_adaptive(ds_mis, warn_r_max = FALSE), 3)

  # Check that LS_combined is a mixture of LS_array and LS_gene:
  tol <- 0.002 # add some tolerance for rounding !
  expect_true(all(ds_imp <= pmax(ds_100x7_LS_array_Bo, ds_100x7_LS_gene_Bo) + tol))
  expect_true(all(ds_imp >= pmin(ds_100x7_LS_array_Bo, ds_100x7_LS_gene_Bo) - tol))
})

# Local tests showed, that the difference between impute_LS_adaptive() and the jar file is
# roughly the same as the difference between to runs of the jar file.
# So, methods seem to return the "same" imputation values
