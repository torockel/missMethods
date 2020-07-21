test_that("impute_expected_values() works with problematic rows", {
  # One row with no observed value
  ds_imp <- expect_warning(
    impute_expected_values(
      data.frame(X = c(1:3, NA), Y = c(11:13, NA)),
      mu = c(2.5, 12.5), S = matrix(c(1, 1, 1, 1), nrow = 2),
      warn_problematic_rows = TRUE
    ),
    "The missing values of following rows were imputed with (parts of) mu: 4",
    fixed = TRUE, all = TRUE
  )
  expect_equal(ds_imp, data.frame(X = c(1:3, 2.5), Y = c(11:13, 12.5)))

  # S_22 not invertible
  ds_imp <- expect_warning(
    impute_expected_values(
      data.frame(X = 1:4, Y = 11:14, Z = c(21, 22, NA, NA)),
      mu = c(2.5, 12.5, 22.5), S = matrix(1, nrow = 3, ncol = 3),
      warn_problematic_rows = TRUE
    ),
    "The missing values of following rows were imputed with (parts of) mu: 3, 4",
    fixed = TRUE, all = TRUE
  )
  expect_equal(ds_imp, data.frame(X = 1:4, Y = 11:14, Z = c(21, 22, 22.5, 22.5)))

  # S_22 not invertible and one row with no observed value
  ds_imp <- expect_warning(
    impute_expected_values(
      data.frame(X = c(1:7, rep(NA, 3)), Y = c(11:18, rep(NA, 2)), Z = c(21:29, NA)),
      mu = c(5.5, 15.5, 25.5), S = matrix(1, nrow = 3, ncol = 3),
      warn_problematic_rows = TRUE
    ),
    "The missing values of following rows were imputed with (parts of) mu: 8, 10",
    fixed = TRUE, all = TRUE
  )
  expect_equal(
    ds_imp,
    data.frame(X = c(1:7, 5.5, 9, 5.5), Y = c(11:19, 15.5), Z = c(21:29, 25.5))
  )
})
