test_that("calc_evaluation_criterion()", {
  # define some vectors, matrices and data frames --------
  estimate_vec <- c(1:4)
  true_val_vec <- c(3, 2:4)
  estimate_matrix <- matrix(1:4, nrow = 2)
  true_val_matrix <- matrix(c(1:2, 4:5), nrow = 2)
  estimate_df <- data.frame(X = 1:2, Y = 11:12)
  true_val_df <- data.frame(X = 4:5, Y = 15:16)

  # check for atomic vectors that are not matrices --------
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec), 1)

  # check for matrices ------------------------------------
  expect_equal(calc_evaluation_criterion(estimate_matrix, true_val_matrix), 1 / sqrt(2))

  # check for data frames ---------------------------------
  expect_equal(calc_evaluation_criterion(estimate_df, true_val_df), 5 / sqrt(2))

  # check criterion --------------------------------------
  # bias
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "bias"), -0.5)

  # bias_rel
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "bias_rel"), -2/(3*4))

  # correlation (cor)
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "cor"),
               cor(estimate_vec, true_val_vec))

  # MAE
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "MAE"), 0.5)

  # MAE_rel
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "MAE_rel"), 2/(3*4))

  # MSE
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "MSE"), 1)

  # error: not implemented
  expect_error(calc_evaluation_criterion(estimate_vec, true_val_vec,
                                         "notImplementedCriterion"),
               "criterion notImplementedCriterion is not implemented")


  # check M -----------------------------------------------
  # atomic vectors that are not matrices
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec,
                                         M = c(TRUE, rep(FALSE, 3))), 2)

  # M for matrices and data frames
  M1 <- matrix(c(TRUE, TRUE, FALSE, FALSE), nrow = 2)
  M_11_22 <- matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2)
  # matrices
  expect_equal(calc_evaluation_criterion(estimate_matrix, true_val_matrix, M = M1), 0)
  expect_equal(calc_evaluation_criterion(estimate_matrix, true_val_matrix, M = M_11_22),
               1 / sqrt(2))
  # data frames
  expect_equal(calc_evaluation_criterion(estimate_df, true_val_df, M = M1), 3)
  expect_equal(calc_evaluation_criterion(estimate_df, true_val_df, M = M_11_22),
               5 / sqrt(2))
})
