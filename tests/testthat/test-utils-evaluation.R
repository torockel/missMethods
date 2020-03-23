test_that("calc_evaluation_criterion() works", {
  # calc_evaluation_criterion -----------------------------
  # define some vectors, matrices and data frames --------
  estimate_vec <- c(1:4)
  true_val_vec <- c(3, 2:4)
  estimate_matrix <- matrix(1:4, nrow = 2)
  true_val_matrix <- matrix(c(1:2, 4:5), nrow = 2)
  estimate_df <- data.frame(X = 1:2, Y = 11:12)
  true_val_df <- data.frame(X = 4:5, Y = 15:16)
  estimate_tbl <- tibble::as_tibble(estimate_df)
  true_val_tbl <- tibble::as_tibble(true_val_df)
  est_mixed_df <- data.frame(X = 4:6, Y = factor(letters[4:6]))
  true_mixed_df <- data.frame(X = c(4, 1, 2), Y = factor(letters[1:3]))

  # check for atomic vectors that are not matrices --------
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec), 1)

  # check for matrices ------------------------------------
  expect_equal(calc_evaluation_criterion(estimate_matrix, true_val_matrix), 1 / sqrt(2))

  # check for data frames ---------------------------------
  expect_equal(calc_evaluation_criterion(estimate_df, true_val_df), 5 / sqrt(2))

  # check for tibbles -------------------------------------
  expect_equal(calc_evaluation_criterion(estimate_tbl, true_val_tbl), 5 / sqrt(2))
  expect_equal(calc_evaluation_criterion(estimate_tbl, true_val_tbl, criterion = "NRMSE_col_mean"),
               calc_evaluation_criterion(estimate_df, true_val_df, criterion = "NRMSE_col_mean"))

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

  # check equality for NRMSE_tot and NRMSE_col for special cases (mean and mean_sq)
  df_NRMSE_est <- data.frame(X = c(3, 2, 3), Y = 3:1)
  df_NRMSE_true <- data.frame(X = 1:3, Y = 3:1)
  # mean(X) == mean(Y) == mean(c(X, Y)) # for df_NRMSE_true
  expect_equal(
    calc_evaluation_criterion(df_NRMSE_est, df_NRMSE_true, "NRMSE_tot_mean"),
    calc_evaluation_criterion(df_NRMSE_est, df_NRMSE_true, "NRMSE_col_mean")
  )

  expect_equal(
    calc_evaluation_criterion(df_NRMSE_est, df_NRMSE_true, "NRMSE_tot_mean_sq"),
    calc_evaluation_criterion(df_NRMSE_est, df_NRMSE_true, "NRMSE_col_mean_sq")
  )

  calc_evaluation_criterion(df_NRMSE_est, df_NRMSE_true, "NRMSE_tot_sd")
  calc_evaluation_criterion(df_NRMSE_est, df_NRMSE_true, "NRMSE_col_sd")

  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "nr_equal"), 3)

  # check mixed data frame with nr_NA
  mixed_df <- data.frame(1:3, 5:3)
  mixed_miss_df <- data.frame(factor(c(NA, 1:2)), c(5, NA, 3))
  expect_equal(calc_evaluation_criterion(mixed_miss_df, mixed_df, criterion = "nr_NA"), 2)
  expect_equal(calc_evaluation_criterion(mixed_miss_df, mixed_df, criterion = "nr_NA",
                                         M = is.na(mixed_miss_df)), 2)

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
  expect_equal(
    calc_evaluation_criterion(estimate_matrix, true_val_matrix, "precision", M = M_11_22),
    1 / 2
  )
  # data frames
  expect_equal(calc_evaluation_criterion(estimate_df, true_val_df, M = M1), 3)
  expect_equal(
    calc_evaluation_criterion(estimate_df, true_val_df, M = M_11_22),
    5 / sqrt(2)
  )
  expect_equal(
    calc_evaluation_criterion(estimate_df, true_val_df, "precision", M = M_11_22),
    0
  )

  # data frames columnwise with one column only FALSE in M
  M_NRMSE <- matrix(c(TRUE, TRUE, rep(FALSE, 4)), ncol = 2)
  expect_equal(
    calc_evaluation_criterion(df_NRMSE_est, df_NRMSE_true, "NRMSE_tot_mean", M = M_NRMSE),
    calc_evaluation_criterion(df_NRMSE_est, df_NRMSE_true, "NRMSE_col_mean", M = M_NRMSE)
  )

  expect_equal(
    calc_evaluation_criterion(df_NRMSE_est, df_NRMSE_true, "NRMSE_tot_mean_sq", M = M_NRMSE),
    calc_evaluation_criterion(df_NRMSE_est, df_NRMSE_true, "NRMSE_col_mean_sq", M = M_NRMSE)
  )

  # check tolerance -----------------------------------------------------------
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "nr_equal",
    tolerance = 0.5
  ), 3)
  expect_equal(calc_evaluation_criterion(estimate_vec, true_val_vec, "nr_equal",
    tolerance = 3
  ), 4) # diff is 2
})

test_that("calc_evaluation_criterion_vec() works", {
  # calc_evaluation_criterion_vec -------------------------
  # define some vectors -----------------------------------
  estimate_vec <- c(1:4)
  true_val_vec <- c(3, 2:4)
  m_k <- c(TRUE, TRUE, FALSE, FALSE)

  # check error / warning:
  expect_error(calc_evaluation_criterion_vec(1:3, 1:4),
               "estimate and true_val must be of same length")
  # NA not allowed if criterion != "nr_NA"
  expect_warning(calc_evaluation_criterion_vec(c(NA, 1), c(1, 1), criterion = "RMSE"),
                 "NAs in estimate or true_val may lead to NA")
  expect_equal(calc_evaluation_criterion_vec(c(NA, 1), c(1, 1), criterion = "nr_NA"),
               1)

  # check criterion --------------------------------------
  # every criterion with and without m_k

  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "RMSE"), 1)
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "RMSE", m_k), sqrt(4/2))

  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "bias"), -0.5)
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "bias", m_k), -1)

  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "bias_rel"), -2 / (3 * 4))
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "bias_rel", m_k), -2 / (3 * 2))

  expect_equal(
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "cor"),
    cor(estimate_vec, true_val_vec)
  )
  expect_equal(
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "cor", m_k),
    cor(estimate_vec[m_k], true_val_vec[m_k])
  )

  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "MAE"), 0.5)
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "MAE", m_k), 1)

  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "MAE_rel"), 2 / (3 * 4))
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "MAE_rel", m_k), 2 / (3 * 2))

  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "MSE"), 4 / 4)
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "MSE", m_k), 4 / 2)

  expect_equal(
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "NRMSE_tot_mean"),
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "RMSE") / mean(true_val_vec)
  )
  expect_equal(
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "NRMSE_tot_mean", m_k),
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "RMSE", m_k) / mean(true_val_vec)
  )


  expect_equal(
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "NRMSE_tot_mean_sq"),
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "RMSE") / sqrt(mean(true_val_vec^2))
  )
  expect_equal(
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "NRMSE_tot_mean_sq", m_k),
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "RMSE", m_k) / sqrt(mean(true_val_vec^2))
  )

  expect_equal(
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "NRMSE_tot_sd"),
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "RMSE") / sd(true_val_vec)
  )
  expect_equal(
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "NRMSE_tot_sd", m_k),
    calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "RMSE", m_k) / sd(true_val_vec)
  )

  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "nr_equal"), 3)
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "nr_equal", m_k), 1)

  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "nr_NA"), 0)
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "nr_NA", m_k), 0)

  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "nr_NA"), 0)
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "nr_NA", m_k), 0)
  expect_equal(calc_evaluation_criterion_vec(c(NA, 1:3), criterion = "nr_NA"), 1)
  expect_equal(calc_evaluation_criterion_vec(c(NA, 1:3), criterion = "nr_NA", m_k = m_k), 1)

  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "precision"), 3 / 4)
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "precision", m_k), 1 / 2)

  # error: not implemented
  expect_error(
    calc_evaluation_criterion_vec(
      estimate_vec, true_val_vec,
      "notImplementedCriterion"
    ),
    "criterion notImplementedCriterion is not implemented"
  )

  # check tolerance -----------------------------------------------------------
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "nr_equal",
                                         tolerance = 0.5
  ), 3)
  expect_equal(calc_evaluation_criterion_vec(estimate_vec, true_val_vec, "nr_equal",
                                         tolerance = 3
  ), 4) # diff is 2
})

test_that("count_equals() works", {
  # count_equals ------------------------------------------
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
