test_that("delete_MAR_censoring() and delete_cutoff(), which is called by
          delete_MAR_censoring(), work", {
  set.seed(12345)

  # check sorting = TRUE (the default) --------------------

  # check p and lower
  df_mis <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y")
  expect_equal(count_NA(df_mis), c(X = 30, Y = 0))
  expect_true(all(is.na(df_mis[1:30, "X"])))
  # To low p:
  expect_false(anyNA(delete_MAR_censoring(df_XY_100, 0.001, "X", "Y")))

  # check p and upper
  df_mis <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y", where = "upper")
  expect_equal(count_NA(df_mis), c(X = 30, Y = 0))
  expect_true(all(is.na(df_mis[71:100, "X"])))

  df_mis <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y", where = "both")
  expect_equal(count_NA(df_mis), c(X = 30, Y = 0))
  expect_true(all(is.na(df_mis[c(1:15, 86:100), "X"])))

  # check special ctrl_col cases
  # ctrl_col constant
  expect_equal(
    count_NA(delete_MAR_censoring(df_XY_X_constant, 0.2,
      cols_mis = "Y",
      cols_ctrl = "X"
    )),
    c(X = 0, Y = 4)
  )

  # ctr_col nearly constant
  expect_equal(
    count_NA(delete_MAR_censoring(df_XY_X_one_outlier, 0.2,
      cols_mis = "Y", cols_ctrl = "X"
    )),
    c(X = 0, Y = 4)
  )
  # ordered control column
  expect_equal(
    count_NA(delete_MAR_censoring(df_with_ord_factors, 0.2,
      cols_mis = "Y", cols_ctrl = "X"
    )),
    c(X = 0, Y = 4)
  )


  # check sorting = FALSE ---------------------------------

  # check p and lower
  df_mis <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y", sorting = FALSE)
  expect_equal(count_NA(df_mis), c(X = 30, Y = 0))
  expect_true(all(is.na(df_mis[1:30, "X"])))
  expect_equal(
    count_NA(delete_MAR_censoring(df_XY_100, 0.001, "X", "Y",
      sorting = FALSE
    )),
    c(X = 1, Y = 0)
  ) # here low p leads to one missing value

  # check p and upper
  df_mis <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y",
    where = "upper",
    sorting = FALSE
  )
  expect_equal(count_NA(df_mis), c(X = 30, Y = 0))
  expect_true(all(is.na(df_mis[71:100, "X"])))

  df_mis <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y",
    where = "both",
    sorting = FALSE
  )
  expect_equal(count_NA(df_mis), c(X = 30, Y = 0))
  expect_true(all(is.na(df_mis[c(1:15, 86:100), "X"])))

  # check special ctrl_col cases
  # ctrl_col constant
  expect_warning(
    delete_MAR_censoring(df_XY_X_constant, 0.2,
      cols_mis = "Y",
      cols_ctrl = "X", sorting = FALSE
    ),
    "the column X is constant; no missing values created"
  )

  # ctr_col nearly constant
  expect_equal(
    count_NA(delete_MAR_censoring(df_XY_X_one_outlier, 0.2,
      cols_mis = "Y", cols_ctrl = "X",
      sorting = FALSE
    )),
    c(X = 0, Y = 0)
  ) # no missing value

  # ordered control column
  expect_equal(
    count_NA(delete_MAR_censoring(df_with_ord_factors, 0.2,
      cols_mis = "Y", cols_ctrl = "X",
      sorting = FALSE
    )),
    c(X = 0, Y = 3)
  )
})

test_that("delete_MAR_censoring() (and delete_censoring(), which is called by
          delete_MAR_censoring()) works for matrices", {
  set.seed(12345)
  mat_mis <- delete_MAR_censoring(matrix_100_2, 0.2, 1, 2)
  expect_equal(count_NA(mat_mis), c(20, 0))
  expect_equal(count_NA(mat_mis[1:20, ]), c(20, 0))

  mat_mis <- delete_MAR_censoring(matrix_20_10, c(0.1, 0.2, 0.3), 1:3, 8:10,
    where = "upper"
  )
  expect_equal(count_NA(mat_mis), c(2, 4, 6, rep(0, 7)))
  expect_equal(count_NA(mat_mis[1:10, ]), c(2, 4, 6, rep(0, 7)))
})

test_that("delete_MAR_censoring() (and delete_censoring(), which is called by
          delete_MAR_censoring()) works for tibbles", {
  set.seed(12345)
  tbl_mis <- delete_MAR_censoring(tbl_XY_100, 0.2, 1, 2)
  expect_equal(count_NA(tbl_mis), c(X = 20, Y = 0))
  expect_equal(count_NA(tbl_mis[1:20, ]), c(X = 20, Y = 0))

  tbl_mis <- delete_MAR_censoring(tbl_XYZ_100, c(0.1, 0.2), 1:2, c(3, 3),
    where = "upper"
  )
  expect_equal(count_NA(tbl_mis), c(X = 10, Y = 20, Z = 0))
  expect_equal(count_NA(tbl_mis[1:20, ]), c(X = 10, Y = 20, Z = 0))
})

# check delete_MNAR_censoring -----------------------------
test_that("delete_MNAR_censoring() works", {
  df_mis <- delete_MNAR_censoring(df_XY_100, c(0.3, 0.1), c("X", "Y"))
  expect_equal(count_NA(df_mis), c(X = 30, Y = 10))
  expect_true(all(is.na(df_mis[1:30, "X"])))
  expect_true(all(is.na(df_mis[1:10, "Y"])))
})
