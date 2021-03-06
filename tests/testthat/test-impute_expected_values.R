test_that("impute_expected_values() works with problematic rows", {

  ## One row with no observed value -------------------------------------------
  # stochastic = FALSE
  ds_imp <- expect_message(
    impute_expected_values(
      data.frame(X = c(1:3, NA), Y = c(11:13, NA)),
      mu = c(2.5, 12.5), S = matrix(c(1, 1, 1, 1), nrow = 2),
      verbose = TRUE
    ),
    "The missing values of following rows were imputed with (parts of) mu: 4",
    fixed = TRUE, all = TRUE
  )
  expect_equal(ds_imp, data.frame(X = c(1:3, 2.5), Y = c(11:13, 12.5)))

  # stochastic = TRUE
  ds_imp <- expect_message(
    impute_expected_values(
      data.frame(X = c(1:3, NA), Y = c(11:13, NA)),
      mu = c(2.5, 12.5), S = matrix(c(1, 1, 1, 1), nrow = 2),
      stochastic = TRUE,
      verbose = TRUE
    ),
    "The missing values of following rows were imputed with (parts of) mu and a residuum: 4",
    fixed = TRUE, all = TRUE
  )
  expect_false(anyNA(ds_imp))

  ## S_22 not invertible ------------------------------------------------------
  ds_imp <- expect_message(
    impute_expected_values(
      data.frame(X = 1:4, Y = 11:14, Z = c(21, 22, NA, NA)),
      mu = c(2.5, 12.5, 22.5), S = matrix(1, nrow = 3, ncol = 3),
      verbose = TRUE
    ),
    "The missing values of following rows were imputed with (parts of) mu: 3, 4",
    fixed = TRUE, all = TRUE
  )
  expect_equal(ds_imp, data.frame(X = 1:4, Y = 11:14, Z = c(21, 22, 22.5, 22.5)))

  ## S_22 not invertible and one row with no observed value -------------------
  ds_imp <- expect_message(
    impute_expected_values(
      data.frame(X = c(1:7, rep(NA, 3)), Y = c(11:18, rep(NA, 2)), Z = c(21:29, NA)),
      mu = c(5.5, 15.5, 25.5), S = matrix(1, nrow = 3, ncol = 3),
      verbose = TRUE
    ),
    "The missing values of following rows were imputed with (parts of) mu: 8, 10",
    fixed = TRUE, all = TRUE
  )
  expect_equal(
    ds_imp,
    data.frame(X = c(1:7, 5.5, 9, 5.5), Y = c(11:19, 15.5), Z = c(21:29, 25.5))
  )
})


test_that("impute_expected_values() works with tibbles", {
  imp_ds <- tbl_XY_XY_mis
  imp_ds[is.na(tbl_XY_XY_mis)[, 1], 1] <- 1
  imp_ds[is.na(tbl_XY_XY_mis)[, 2], 2] <- 3
  expect_equal(
    impute_expected_values(tbl_XY_XY_mis,
      mu = c(1, 3), diag(1, 2),
      stochastic = FALSE
    ),
    imp_ds
  )
})
