test_that("calc_evaluation_criterion() works", {
  # define some vectors, matrices and data frames --------
  estimate_vec <- c(1:4)
  true_val_vec <- c(3, 2:4)
  estimate_matrix <- matrix(1:4, nrow = 2)
  true_val_matrix <- matrix(c(1:2, 4:5), nrow = 2)
  estimate_df <- data.frame(X = 1:2, Y = 11:12)
  true_val_df <- data.frame(X = 4:5, Y = 15:16)
  est_mixed_df <- data.frame(X = 4:6, Y = factor(letters[4:6]))
  true_mixed_df <- data.frame(X = c(4, 1, 2), Y = factor(letters[1:3]))

  # check for atomic vectors that are not matrices --------
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec), 1)

  # check for matrices ------------------------------------
  expect_equal(calc_evaluation_criterion(estimate_matrix, true_val_matrix), 1 / sqrt(2))

  # check for data frames ---------------------------------
  expect_equal(calc_evaluation_criterion(estimate_df, true_val_df), 5 / sqrt(2))

  # check criterion --------------------------------------
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "bias"), -0.5)
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "bias_rel"), -2 / (3 * 4))
  expect_equal(
    calc_evaluation_criterion(estimate_vec, true_val_vec, "cor"),
    cor(estimate_vec, true_val_vec)
  )
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "MAE"), 0.5)
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "MAE_rel"), 2 / (3 * 4))
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "MSE"), 1)
  expect_equal(
    calc_evaluation_criterion(estimate_vec, true_val_vec, "NRMSE_tot_mean"),
    calc_evaluation_criterion(estimate_vec, true_val_vec, "RMSE") / mean(true_val_vec)
  )
  expect_equal(
    calc_evaluation_criterion(estimate_vec, true_val_vec, "NRMSE_tot_mean_sq"),
    calc_evaluation_criterion(estimate_vec, true_val_vec, "RMSE") / sqrt(mean(true_val_vec^2))
  )
  expect_equal(
    calc_evaluation_criterion(estimate_vec, true_val_vec, "NRMSE_tot_sd"),
    calc_evaluation_criterion(estimate_vec, true_val_vec, "RMSE") / sd(true_val_vec)
  )
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "nr_equal"), 3)
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "precision"), 3 / 4)
  expect_equal(calc_evaluation_criterion(est_mixed_df, true_mixed_df, "precision"), 1 / 6)

  # error: not implemented
  expect_error(
    calc_evaluation_criterion(
      estimate_vec, true_val_vec,
      "notImplementedCriterion"
    ),
    "'arg' should be one of"
  )


  # check M -----------------------------------------------
  # atomic vectors that are not matrices
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec,
    M = c(TRUE, rep(FALSE, 3))
  ), 2)

  # M for matrices and data frames
  M1 <- matrix(c(TRUE, TRUE, FALSE, FALSE), nrow = 2)
  M_11_22 <- matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2)
  # matrices
  expect_equal(calc_evaluation_criterion(estimate_matrix, true_val_matrix, M = M1), 0)
  expect_equal(
    calc_evaluation_criterion(estimate_matrix, true_val_matrix, M = M_11_22),
    1 / sqrt(2)
  )
  # data frames
  expect_equal(calc_evaluation_criterion(estimate_df, true_val_df, M = M1), 3)
  expect_equal(
    calc_evaluation_criterion(estimate_df, true_val_df, M = M_11_22),
    5 / sqrt(2)
  )

  # M for tibbles not implemented
  expect_error(calc_evaluation_criterion(tbl_XY_20, tbl_XY_20, M = TRUE),
               "logical subsetting by 'M' for tibbles is only supported for")

  # check tolerance -----------------------------------------------------------
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "nr_equal",
    tolerance = 0.5
  ), 3)
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "nr_equal",
    tolerance = 3
  ), 4) # diff is 2
})

test_that("count_equals() works", {
  expect_equal(count_equals(1:10, 1:10), 10)
  expect_equal(count_equals(1:10, c(1, 20, 3, 40, 5, 6, 70, 8:10)), 7)
  expect_equal(count_equals(1:10, 10:1), 0)
  expect_equal(count_equals(factor(1:10), factor(1:10)), 10)
  expect_equal(count_equals(factor(1:10), factor(10:1)), 0)
  expect_equal(count_equals(letters[1:10], letters[1:10]), 10)
  expect_equal(count_equals(letters[1:10], LETTERS[1:10]), 0)
  # check tolerance
  expect_equal(count_equals(1:10, 1:10 - 1e-20, tolerance = 1e-10), 10)
  expect_equal(count_equals(1:10, 1:10 - 1e-9, tolerance = 1e-10), 0)
  expect_equal(count_equals(1:10, 10:1, tolerance = 1.01), 2)
})
