# check_delete_args ---------------------------------------
test_that("check_delete_args()", {
  # ds ----------------------------------------------------
  expect_error(delete_MCAR(1:5, 0.3), "ds must be a data.frame or a matrix")

  # p -----------------------------------------------------
  expect_error(
    delete_MCAR(df_XY_100, p = rep(0.1, 3)),
    "length must equal miss_cols"
  )
  expect_error(
    delete_MCAR(df_XY_100, p = rep(0.1, 2), "X"),
    "length must equal miss_cols"
  )
  expect_error(
    delete_MCAR(df_XYZ_100, p = rep(0.1, 2), 1:3),
    "length must equal miss_cols"
  )
  expect_error(
    delete_MCAR(df_XY_100, p = c(1.2, 0.9)),
    "must be between 0 and 1"
  )
  expect_error(delete_MCAR(df_XY_100, p = "a"), "p must be numeric")

  # miss_cols ---------------------------------------------
  expect_error(
    delete_MCAR(df_XY_100, 0.1, miss_cols = c(2, 3)),
    "indices in miss_cols must"
  )
  expect_error(
    delete_MCAR(df_XY_100, 0.1, miss_cols = c("X", "Z")),
    "all entries of miss_cols"
  )
  expect_error(
    delete_MCAR(df_XY_100, 0.1, c(TRUE, FALSE)),
    "miss_cols must be a vector of column names or indices of ds"
  )
  expect_warning(
    delete_MCAR(df_XY_100, 0.1, c(1, 1)),
    "there are duplicates in miss_cols:"
  )

  # stochastic --------------------------------------------
  expect_error(
    delete_MCAR(df_XY_100, 0.1, stochastic = "asdf"),
    "stochastic must be logical"
  )
  expect_error(
    delete_MCAR(df_XY_100, 0.1, stochastic = c(TRUE, TRUE)),
    "the length of stochastic must be 1"
  )
})

# check_delete_args_MCAR ----------------------------------
test_that("check_delete_args_MCAR() works", {
  # check_delete_args_MCAR calls check_delete_args:
  expect_error(delete_MCAR(1:5, 0.3), "ds must be a data.frame or a matrix")

  # special errors for check_delete_args_MCAR:
  expect_error(
    delete_MCAR(df_XY_100, 0.1, p_overall = "A"),
    "p_overall must be logical of length 1"
  )
  expect_error(
    delete_MCAR(df_XY_100, c(0.1, 0.2), p_overall = TRUE),
    "if p_overall = TRUE, then length"
  )
})


# check_delete_args_MAR -----------------------------------
test_that("check_delete_args_MAR() works", {
  # check_delete_args_MAR calls check_delete_args:
  expect_error(
    delete_MAR_1_to_x(1:5, 0.3, "X", "Y"),
    "ds must be a data.frame or a matrix"
  )

  # ctrl_cols (special errors) ----------------------------
  expect_error(
    delete_MAR_1_to_x(df_XY_100, 0.1, 2, 1, ctrl_cols = 3),
    "indices in ctrl_cols must be in 1:ncol\\(ds)"
  )
  expect_error(
    delete_MAR_1_to_x(df_XY_100, 0.1, 2, 1, ctrl_cols = "Z"),
    "all entries of ctrl_cols must be in colnames\\(ds)"
  )
  expect_error(
    delete_MAR_1_to_x(df_XY_100, 0.1, 2, 1, ctrl_cols = factor("X")),
    "ctrl_cols must be a vector of column names or indices of ds"
  )

  expect_error(
    delete_MAR_1_to_x(df_XY_X_miss, 0.1, 3, "Y", ctrl_cols = "X"),
    "ctrl_cols must be completely observed; no NAs in ds\\[, ctrl_cols\\] allowed"
  )
  expect_error(
    delete_MAR_1_to_x(df_XYZ_100, 0.1, 3, miss_cols = "X", ctrl_cols = c("Y", "Z")),
    "length\\(miss_cols) must equal length"
  )

  # special errors for check_delete_args_MAR:
  expect_error(
    delete_MAR_1_to_x(df_XYZ_100, 0.1, 4, miss_cols = "X", ctrl_cols = "X"),
    "to ensure MAR no ctrl_col is allowed to be in miss_cols"
  )
})

# check_delete_args_MNAR ----------------------------------
test_that("check_delete_args_MCAR() works", {
  # check_delete_args_MNAR calls check_delete_args:
  expect_error(
    delete_MNAR_1_to_x(1:5, 0.3, "X", "Y"),
    "ds must be a data.frame or a matrix"
  )

  # special errors for check_delete_args_MNAR:
  expect_error(
    delete_MNAR_1_to_x(df_XY_X_miss, 0.1, 3, "X"),
    "miss_cols must be completely observed; no NAs in ds\\[, miss_cols\\] allowed"
  )
})


# check check_ctrl_cols_1_to_x ----------------------------
test_that("check_ctrl_cols_1_to_x()", {
  expect_true(check_ctrl_cols_1_to_x(df_XY_100, "X"))
  expect_error(
    check_ctrl_cols_1_to_x(
      data.frame(
        X = letters,
        Y = 1:26,
        Z = LETTERS
      ),
      c("X", "Y", "Z")
    ),
    "ordered factors;\nproblematic column\\(s): X, Z$"
  )
})

# check find_groups ---------------------------------------
test_that("find_groups() issues a warning for constant x", {
  expect_warning(
    find_groups(
      x = rep(1, 4), cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = FALSE
    ),
    "grouping not possible, because x is constant"
  )

  expect_warning(
    find_groups(
      x = factor("a"), cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = FALSE
    ),
    "grouping not possible, because x is constant"
  )
})

test_that("find_groups() works in general", {
  expect_equal(
    find_groups(
      x = 1:5, cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = FALSE
    ),
    list(g1 = 1:2, g2 = 3:5)
  )
})

test_that("find_groups() treats unordered factors correct", {
  expect_equal(
    find_groups(
      x = factor(1:5), cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = FALSE
    ),
    list(g1 = 1:3, g2 = 4:5)
  )

  expect_equal(
    find_groups(
      x = factor(1:5), cutoff_fun = median, prop = 0.5,
      use_lpSolve = FALSE,
      ordered_as_unordered = FALSE
    ),
    list(g1 = 1:3, g2 = 4:5)
  )
})

test_that("find_groups() treats ordered factors correct", {
  expect_equal(
    find_groups(
      x = ordered(1:5), cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = FALSE
    ),
    list(g1 = 1:2, g2 = 3:5)
  )

  expect_equal(
    find_groups(
      x = ordered(1:5), cutoff_fun = median, prop = 0.5,
      use_lpSolve = TRUE,
      ordered_as_unordered = TRUE
    ),
    list(g1 = 1:3, g2 = 4:5)
  )
})

# check find_groups_by_cutoff_val -------------------------
test_that("find_groups_by_cutoff_val()", {
  expect_equal(
    find_groups_by_cutoff_val(df_with_ord_factors[, "X"], "d"),
    list(g1 = 1:3, g2 = 4:20)
  )
  expect_equal(
    find_groups_by_cutoff_val(df_XY_100[, "X"], 25),
    list(g1 = 1:24, g2 = 25:100)
  )
  expect_equal(
    find_groups_by_cutoff_val(c(20:29, 19:48), 25),
    list(g1 = c(1:5, 11:16), g2 = c(6:10, 17:40))
  )

  expect_equal(
    find_groups_by_cutoff_val(c(rep(0, 10), 1), 0),
    list(g1 = c(1:10), g2 = 11)
  )
})

# check find_groups_by_prop -------------------------
test_that("find_groups_by_prop()", {
  expect_equal(
    find_groups_by_prop(df_with_ord_factors[, "X"], 0.25),
    list(g1 = 1:5, g2 = 6:20)
  )
  expect_equal(
    find_groups_by_prop(df_XY_100[, "X"], 0.4),
    list(g1 = 1:40, g2 = 41:100)
  )
  expect_equal(
    find_groups_by_prop(c(20:29, 19:48), 0.1),
    list(g1 = c(1:2, 12:13), g2 = c(3:11, 14:40))
  )


  # check lpSolve -----------------------------------------
  expect_equal(
    find_groups_by_prop(df_with_ord_factors[, "X"], 0.25,
      use_lpSolve = TRUE
    ),
    list(g1 = 1:5, g2 = 6:20)
  )
  expect_equal(
    find_groups_by_prop(df_with_ord_factors[, "X"], 0.25,
      use_lpSolve = FALSE
    ),
    list(g1 = 1:5, g2 = 6:20)
  )
  expect_equal(
    find_groups_by_prop(df_XY_100[, "X"], 0.4,
      use_lpSolve = TRUE
    ),
    list(g1 = 1:40, g2 = 41:100)
  )
  expect_equal(
    find_groups_by_prop(c(20:29, 19:48), 0.1,
      use_lpSolve = TRUE
    ),
    list(g1 = c(1:2, 12:13), g2 = c(3:11, 14:40))
  )

  # problematic for very basic heuristic:
  expect_equal(
    find_groups_by_prop(c("c", "c", "a", "d", "c"), 0.1,
      use_lpSolve = FALSE
    ),
    list(g1 = c(1:2, 5), g2 = 3:4)
  )
  # better use_lpSolve:
  expect_equal(
    find_groups_by_prop(c("c", "c", "a", "d", "c"), 0.1,
      use_lpSolve = TRUE
    ),
    list(g1 = 3, g2 = c(1:2, 4:5))
  )

  # g1 not empty, if x not constant
  expect_equal(
    find_groups_by_prop(df_with_ord_factors[, "X"], 0.0005,
      use_lpSolve = TRUE
    ),
    list(g1 = 1, g2 = 2:20)
  )
  expect_equal(
    find_groups_by_prop(df_with_ord_factors[, "X"], 0.0005,
      use_lpSolve = FALSE
    ),
    list(g1 = 1, g2 = 2:20)
  )
})



# check find_groups_by_values -----------------------------
test_that("find_groups_by_values()", {
  expect_equal(
    find_groups_by_values(
      df_with_ord_factors[, "X"],
      c("a", "b", "e")
    ),
    list(g1 = c(1, 2, 5), g2 = c(3, 4, 6:20))
  )
  expect_equal(
    find_groups_by_values(
      df_XY_100[, "X"],
      c(5:10, 25)
    ),
    list(g1 = c(5:10, 25), g2 = c(1:4, 11:24, 26:100))
  )
})

# check calc_nr_miss_g1 --------------------------------
test_that("calc_nr_miss_g1()", {
  expect_equal(calc_nr_miss_g1(50, 0.8, 50, 50, 4), 40)
  expect_equal(calc_nr_miss_g1(50, 0.25, 50, 20, 4), 12)
  expect_equal(calc_nr_miss_g1(50, 0.45, 50, 30, 3), 22)
  expect_equal(calc_nr_miss_g1(20, 1 / 17, 80, 20, 4), 1)

  expect_equal(calc_nr_miss_g1(50, 0.8, 0, 50, 4), 50)
})
