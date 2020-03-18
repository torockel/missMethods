test_that("impute_sRHD() simple random Hot-Deck imputation", {
  set.seed(12345)
  # check any completely missing column --------------------
  expect_error(
    impute_sRHD(data.frame(X = rep(NA, 10), Y = 1:10, Z = rep(NA, 10))),
    "imputation not possible: column\\(s\\) .* are completely missing"
  )

  # check donor_limit errors ------------------------------
  expect_error(
    impute_sRHD(df_XY_X_miss, donor_limit = 1:4),
    "donor_limit must be of length 1"
  )
  expect_error(
    impute_sRHD(df_XY_X_miss, donor_limit = -5),
    "donor_limit must be a number >= 1 or the string 'min'"
  )
  expect_error(
    impute_sRHD(df_XY_X_miss, donor_limit = "asdf"),
    "donor_limit must be a number >= 1 or the string 'min'"
  )

  # check cols_seq ----------------------------------------
  expect_false(anyNA(impute_sRHD(df_XY_X_miss, type = "cols_seq")))
  expect_false(anyNA(impute_sRHD(df_XY_XY_miss, type = "cols_seq")))
  expect_false(anyNA(impute_sRHD(df_XY_no_comp_obs, type = "cols_seq")))

  # check cols_seq with donor limit -----------------------
  df_XY_X_miss_50_obs_imp_dl_1 <- impute_sRHD(df_XY_X_miss_50_obs,
    type = "cols_seq",
    donor_limit = 1
  )
  expect_equal(as.vector(table(df_XY_X_miss_50_obs_imp_dl_1[, "X"])), rep(2, 50))
  df_XY_X_miss_50_obs_imp_dl_3 <- impute_sRHD(df_XY_X_miss_50_obs,
    type = "cols_seq",
    donor_limit = 3
  )
  expect_true(all(table(df_XY_X_miss_50_obs_imp_dl_3[, "X"]) <= 4))
  expect_condition(
    impute_sRHD(df_XY_X_miss_one_comp_obs,
      type = "cols_seq",
      donor_limit = "min"
    ),
    "donor_limit is set to: 99"
  )
  df_XY_X_miss_one_comp_obs_min <- impute_sRHD(df_XY_X_miss_one_comp_obs,
    type = "cols_seq",
    donor_limit = "min"
  )
  expect_false(anyNA(df_XY_X_miss_one_comp_obs_min))

  df_XY_X_miss_one_comp_obs_99 <- impute_sRHD(df_XY_X_miss_one_comp_obs,
    type = "cols_seq",
    donor_limit = 99
  )
  expect_equal(df_XY_X_miss_one_comp_obs_min, df_XY_X_miss_one_comp_obs_99)
  expect_equal(df_XY_X_miss_one_comp_obs_min[, "X", drop = TRUE], rep(100, 100))
  expect_error(
    impute_sRHD(df_XY_X_miss_one_comp_obs,
      type = "cols_seq",
      donor_limit = 20
    ),
    "donor_limit = 20 is to low! It must be at least 99"
  )

  # check sim_comp ----------------------------------------
  expect_false(anyNA(impute_sRHD(df_XY_X_miss, type = "sim_comp")))
  expect_false(anyNA(impute_sRHD(df_XY_XY_miss, type = "sim_comp")))
  expect_error(
    impute_sRHD(df_XY_no_comp_obs, type = "sim_comp"),
    "not possible: there is no completely observed object"
  )

  # check sim_comp with donor limit -----------------------
  df_XY_X_miss_50_obs_imp_dl_1 <- impute_sRHD(df_XY_X_miss_50_obs,
    type = "sim_comp",
    donor_limit = 1
  )
  expect_equal(as.vector(table(df_XY_X_miss_50_obs_imp_dl_1[, "X"])), rep(2, 50))
  df_XY_X_miss_50_obs_imp_dl_3 <- impute_sRHD(df_XY_X_miss_50_obs,
    type = "sim_comp",
    donor_limit = 3
  )
  expect_true(all(table(df_XY_X_miss_50_obs_imp_dl_3[, "X"]) <= 4))
  expect_condition(
    impute_sRHD(df_XY_X_miss_one_comp_obs,
      type = "sim_comp",
      donor_limit = "min"
    ),
    "donor_limit is set to: 99"
  )
  df_XY_X_miss_one_comp_obs_min <- impute_sRHD(df_XY_X_miss_one_comp_obs,
    type = "sim_comp",
    donor_limit = "min"
  )
  expect_false(anyNA(df_XY_X_miss_one_comp_obs_min))
  df_XY_X_miss_one_comp_obs_99 <- impute_sRHD(df_XY_X_miss_one_comp_obs,
    type = "sim_comp",
    donor_limit = 99
  )
  expect_equal(df_XY_X_miss_one_comp_obs_min, df_XY_X_miss_one_comp_obs_99)
  expect_equal(df_XY_X_miss_one_comp_obs_min[, "X", drop = TRUE], rep(100, 100))
  expect_error(
    impute_sRHD(df_XY_X_miss_one_comp_obs,
      type = "sim_comp",
      donor_limit = 20
    ),
    "donor_limit = 20 is to low! It must be at least 99"
  )


  # check sim_part ----------------------------------------
  expect_false(anyNA(impute_sRHD(df_XY_X_miss, type = "sim_part")))
  expect_false(anyNA(impute_sRHD(df_XY_XY_miss, type = "sim_part")))
  expect_error(
    impute_sRHD(df_XY_no_comp_obs, type = "sim_part"),
    "there is no appropriate donor for the object\\(s\\)"
  )

  ds_sim_part_miss1 <- matrix(c(NA, 1, 2, NA), ncol = 2)
  ds_sim_part_correct1 <- matrix(c(1, 1, 2, 2), ncol = 2)
  expect_equal(
    impute_sRHD(ds_sim_part_miss1, type = "sim_part"),
    ds_sim_part_correct1
  )

  ds_sim_part_impossible <- matrix(c(
    NA, NA, 13,
    NA, 22, NA,
    31, NA, NA
  ), ncol = 3)
  expect_error(
    impute_sRHD(ds_sim_part_impossible, type = "sim_part"),
    "there is no appropriate donor for the object\\(s\\)"
  )
  expect_error(
    impute_sRHD(df_XY_X_miss, type = "sim_part", donor_limit = 2),
    "donor_limit is not implemented for type = sim_part"
  )

  # check special cases -----------------------------------
  expect_error(
    impute_sRHD(df_one_comp_missing_col),
    "imputation not possible: column\\(s) 2 are completely missing"
  )
  expect_false(anyNA(impute_sRHD(df_first_row_comp_missing)))
  expect_false(anyNA(impute_sRHD(df_no_comp_obs)))
  expect_error(
    impute_sRHD(df_all_NA),
    "imputation not possible: column\\(s) 1, 2 are completely missing."
  )
})

test_that("impute_sRHD() works with matrices", {
  # check types
  expect_false(anyNA(impute_sRHD(matrix_100_2_miss, type = "cols_seq")))
  expect_false(anyNA(impute_sRHD(matrix_100_2_miss, type = "sim_comp")))
  expect_false(anyNA(impute_sRHD(matrix_100_2_miss, type = "sim_part")))

  # check donor limit with types
  expect_false(anyNA(impute_sRHD(matrix_100_2_miss, type = "cols_seq", donor_limit = 2)))
  expect_false(anyNA(impute_sRHD(matrix_100_2_miss, type = "sim_comp", donor_limit = 2)))
  expect_error(
    impute_sRHD(matrix_100_2_miss, type = "sim_part", donor_limit = 2),
    "donor_limit is not implemented for type = sim_part"
  )
})

test_that("impute_sRHD() works with tibbles", {
  # check types
  expect_false(anyNA(impute_sRHD(tbl_XY_100, type = "cols_seq")))
  expect_false(anyNA(impute_sRHD(tbl_XY_100, type = "sim_comp")))
  expect_false(anyNA(impute_sRHD(tbl_XY_100, type = "sim_part")))

  # check donor limit with types
  expect_false(anyNA(impute_sRHD(tbl_XY_100, type = "cols_seq", donor_limit = 2)))
  expect_false(anyNA(impute_sRHD(tbl_XY_100, type = "sim_comp", donor_limit = 2)))
  expect_error(
    impute_sRHD(tbl_XY_100, type = "sim_part", donor_limit = 2),
    "donor_limit is not implemented for type = sim_part"
  )
})

# check helpers -------------------------------------------
test_that("min_donor_limit()", {
  M_i_donor <- list()
  for (i in seq_len(5)) {
    M_i_donor[[i]] <- matrix(c(rep(TRUE, 10), rep(FALSE, 2 * i)), byrow = TRUE, ncol = 2)
  }
  correct_i_min_donor_limit <- c(5, 3, 2, 2, 1)

  # check sim_comp ----------------------------------------
  for (i in seq_len(5)) {
    expect_equal(
      min_donor_limit(M_i_donor[[i]], type = "sim_comp"),
      correct_i_min_donor_limit[i]
    )
  }

  # check cols_seq ----------------------------------------
  for (i in seq_len(5)) {
    expect_equal(
      min_donor_limit(M_i_donor[[i]], type = "cols_seq"),
      correct_i_min_donor_limit[i]
    )
  }
  M <- matrix(c(TRUE, TRUE, rep(FALSE, 4)), ncol = 2)
  expect_equal(min_donor_limit(M, type = "cols_seq"), 2)

  # check errors --------------------------
  expect_error(
    min_donor_limit(matrix(TRUE, ncol = 1), type = "sim_comp"),
    "not possible: there is no completely observed object"
  )
  expect_error(
    min_donor_limit(matrix(c(TRUE, FALSE), ncol = 2), type = "cols_seq"),
    "not possible: there are completely unobserved variables"
  )
  expect_error(
    min_donor_limit(M_1_donor, type = "type_not_implemented"),
    "type = type_not_implemented not implemented"
  )
})
