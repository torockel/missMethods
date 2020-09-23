## delete_values() ------------------------------------------------------------
test_that("delete_values() works (basic test)", {
  expect_equal(
    count_NA(delete_values(
      "MAR", "rank", ds = df_XY_20, p = 0.1,
      cols_mis = "X", cols_ctrl = "Y", n_mis_stochastic = FALSE
    )),
    c(X = 2, Y = 0)
  )
})


test_that("delete_values() deprecate miss_cols", {
  # use delete_MAR_1_to_x() as calling function (arbitrary choice)
  ds_mis <- expect_warning(
    delete_MAR_1_to_x(df_XY_20, 0.1, x = 2, cols_ctrl = 2, miss_cols = 1),
    "miss_cols is deprecated; use cols_mis instead."
  )
  expect_equal(
    count_NA(ds_mis),
    c(X = 2, Y = 0)
  )
})


test_that("delete_values() deprecate ctrl_cols", {
  # use delete_MAR_1_to_x() as calling function (arbitrary choice)
  ds_mis <- expect_warning(
    delete_MAR_1_to_x(df_XY_20, 0.1, "X", x = 2, ctrl_cols = 2),
    "ctrl_cols is deprecated; use cols_ctrl instead."
  )
  expect_equal(
    count_NA(ds_mis),
    c(X = 2, Y = 0)
  )
})


test_that("delete_values() calls check_delete_args_general()", {
  expect_error(
    delete_MCAR(array(1:6, dim = c(1, 2, 3)), 0.3),
    "ds must be a data.frame or a matrix"
  )
})


test_that("delete_values() calls check_args_MCAR()", {
  expect_error(
    delete_MCAR(df_XY_2, 0.3, p_overall = "asdf"),
    "p_overall must be logical of length 1"
  )
})


test_that("delete_values() calls check_args_MAR()", {
  expect_error(
    delete_MAR_rank(df_XY_2, 0.3, 1, cols_ctrl = 42),
    "indices in cols_ctrl must be in 1:ncol\\(ds)"
  )
})


test_that("delete_values() calls check_args_MNAR()", {
  expect_error(
    delete_MNAR_rank(df_XY_X_mis, 0.3, cols_mis = "X"),
    "cols_mis must be completely observed; no NAs in ds\\[, cols_mis] allowed"
  )
})


test_that("delete_values() adjusts p", {
  expect_equal(
    count_NA(delete_values(
      "MAR", "rank", ds = df_XYZ_100, p = 0.1,
      cols_mis = c("X", "Z"), cols_ctrl = c("Y", "Y"), n_mis_stochastic = FALSE
    )),
    c(X = 10, Y = 0, Z = 10)
  )
})


## check_delete_args_general() ------------------------------------------------
test_that("check_delete_args_general() works", {
  # ds ----------------------------------------------------
  expect_error(
    delete_MCAR(array(1:6, dim = c(1, 2, 3)), 0.3),
    "ds must be a data.frame or a matrix"
  )

  # p -----------------------------------------------------
  expect_error(
    delete_MCAR(df_XY_100, p = rep(0.1, 3)),
    "p must be of length 1 or length must equal cols_mis"
  )
  expect_error(
    delete_MCAR(df_XY_100, p = rep(0.1, 2), "X"),
    "p must be of length 1 or length must equal cols_mis"
  )
  expect_error(
    delete_MCAR(df_XYZ_100, p = rep(0.1, 2), 1:3),
    "p must be of length 1 or length must equal cols_mis"
  )
  expect_error(
    delete_MCAR(df_XY_100, p = c(1.2, 0.9)),
    "probabilties in p must be between 0 and 1"
  )
  expect_error(delete_MCAR(df_XY_100, p = "a"), "p must be numeric")

  # cols_mis ---------------------------------------------
  expect_error(
    delete_MCAR(df_XY_100, 0.1, cols_mis = c(2, 3)),
    "indices in cols_mis must be in 1:ncol\\(ds)"
  )
  expect_error(
    delete_MCAR(df_XY_100, 0.1, cols_mis = c("X", "Z")),
    "all entries of cols_mis must be in colnames\\(ds)"
  )
  expect_error(
    delete_MCAR(df_XY_100, 0.1, c(TRUE, FALSE)),
    "cols_mis must be a vector of column names or indices of ds"
  )
  expect_warning(
    delete_MCAR(df_XY_100, 0.1, c(1, 1)),
    "there are duplicates in cols_mis:"
  )

  # n_mis_stochastic --------------------------------------------
  expect_error(
    delete_MCAR(df_XY_100, 0.1, n_mis_stochastic = "asdf"),
    "n_mis_stochastic must be logical"
  )
  expect_error(
    delete_MCAR(df_XY_100, 0.1, n_mis_stochastic = c(TRUE, TRUE)),
    "the length of n_mis_stochastic must be 1"
  )
})


## check_args_MCAR() ----------------------------------------------------------
test_that("check_args_MCAR() works", {
  expect_error(
    delete_MCAR(df_XY_100, 0.1, p_overall = "A"),
    "p_overall must be logical of length 1"
  )
  expect_error(
    delete_MCAR(df_XY_100, c(0.1, 0.2), p_overall = TRUE),
    "if p_overall = TRUE, then length"
  )
})


## check_args_MAR() -----------------------------------------------------------
test_that("check_args_MAR() works", {
  # cols_ctrl (special errors) ----------------------------
  expect_error(
    delete_MAR_1_to_x(df_XY_100, 0.1, 1, cols_ctrl = 3, x = 2),
    "indices in cols_ctrl must be in 1:ncol\\(ds)"
  )
  expect_error(
    delete_MAR_1_to_x(df_XY_100, 0.1, 1, cols_ctrl = "Z", x = 2),
    "all entries of cols_ctrl must be in colnames\\(ds)"
  )
  expect_error(
    delete_MAR_1_to_x(df_XY_100, 0.1, 1, cols_ctrl = factor("X"), x = 2),
    "cols_ctrl must be a vector of column names or indices of ds"
  )

  expect_error(
    delete_MAR_1_to_x(df_XY_X_mis, 0.1, "Y", cols_ctrl = "X", x = 3),
    "cols_ctrl must be completely observed; no NAs in ds\\[, cols_ctrl\\] allowed"
  )
  expect_error(
    delete_MAR_1_to_x(df_XYZ_100, 0.1, cols_mis = "X", cols_ctrl = c("Y", "Z"), x = 3),
    "length\\(cols_mis) must equal length"
  )

  # special errors for check_delete_args_MAR:
  expect_error(
    delete_MAR_1_to_x(df_XYZ_100, 0.1, cols_mis = "X", cols_ctrl = "X", x = 4),
    "to ensure MAR no ctrl_col is allowed to be in cols_mis"
  )
})


## check_args_MNAR() ----------------------------------------------------------
test_that("check_args_MNAR() works", {
  expect_error(
    delete_MNAR_1_to_x(df_XY_X_mis, 0.1, "X", x = 3),
    "cols_mis must be completely observed; no NAs in ds\\[, cols_mis\\] allowed"
  )
})
