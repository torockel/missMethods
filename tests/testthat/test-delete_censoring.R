# check delete_MAR_censoring ------------------------------
test_that("delete_MAR_censoring() calls check_delete_args_MAR()", {
  expect_error(
    delete_MAR_censoring(df_XY_100, 0.1, 1, ctrl_cols = 3),
    "indices in ctrl_cols must be in 1:ncol\\(ds)"
  )
})

test_that("delete_MAR_censoring() and delete_cutoff(), which is called by
          delete_MAR_censoring(), work", {
  set.seed(12345)

  # check sorting = TRUE (the default) --------------------

  # check p and lower
  df_miss <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y")
  expect_equal(count_NA(df_miss), c(X = 30, Y = 0))
  expect_true(all(is.na(df_miss[1:30, "X"])))
  expect_false(anyNA(delete_MAR_censoring(df_XY_100, 0.001, "X", "Y"))) # to low p

  # check p and upper
  df_miss <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y", where = "upper")
  expect_equal(count_NA(df_miss), c(X = 30, Y = 0))
  expect_true(all(is.na(df_miss[71:100, "X"])))

  df_miss <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y", where = "both")
  expect_equal(count_NA(df_miss), c(X = 30, Y = 0))
  expect_true(all(is.na(df_miss[c(1:15, 86:100), "X"])))

  # check special ctrl_col cases
  # ctrl_col constant
  expect_equal(
    count_NA(delete_MAR_censoring(df_XY_X_constant, 0.2,
      cols_miss = "Y",
      ctrl_cols = "X"
    )),
    c(X = 0, Y = 4)
  )

  # ctr_col nearly constant
  expect_equal(
    count_NA(delete_MAR_censoring(df_XY_X_one_outlier, 0.2,
      cols_miss = "Y", ctrl_cols = "X"
    )),
    c(X = 0, Y = 4)
  )
  # ordered control column
  expect_equal(
    count_NA(delete_MAR_censoring(df_with_ord_factors, 0.2,
      cols_miss = "Y", ctrl_cols = "X"
    )),
    c(X = 0, Y = 4)
  )


  # check sorting = FALSE ---------------------------------

  # check p and lower
  df_miss <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y", sorting = FALSE)
  expect_equal(count_NA(df_miss), c(X = 30, Y = 0))
  expect_true(all(is.na(df_miss[1:30, "X"])))
  expect_equal(
    count_NA(delete_MAR_censoring(df_XY_100, 0.001, "X", "Y",
      sorting = FALSE
    )),
    c(X = 1, Y = 0)
  ) # here low p leads to one missing value

  # check p and upper
  df_miss <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y",
    where = "upper",
    sorting = FALSE
  )
  expect_equal(count_NA(df_miss), c(X = 30, Y = 0))
  expect_true(all(is.na(df_miss[71:100, "X"])))

  df_miss <- delete_MAR_censoring(df_XY_100, 0.3, "X", "Y",
    where = "both",
    sorting = FALSE
  )
  expect_equal(count_NA(df_miss), c(X = 30, Y = 0))
  expect_true(all(is.na(df_miss[c(1:15, 86:100), "X"])))

  # check special ctrl_col cases
  # ctrl_col constant
  expect_warning(
    delete_MAR_censoring(df_XY_X_constant, 0.2,
      cols_miss = "Y",
      ctrl_cols = "X", sorting = FALSE
    ),
    "the column X is constant; no missing values created"
  )

  # ctr_col nearly constant
  expect_equal(
    count_NA(delete_MAR_censoring(df_XY_X_one_outlier, 0.2,
      cols_miss = "Y", ctrl_cols = "X",
      sorting = FALSE
    )),
    c(X = 0, Y = 0)
  ) # no missing value

  # ordered control column
  expect_equal(
    count_NA(delete_MAR_censoring(df_with_ord_factors, 0.2,
      cols_miss = "Y", ctrl_cols = "X",
      sorting = FALSE
    )),
    c(X = 0, Y = 3)
  )
})

test_that("delete_MAR_censoring() (and delete_censoring(), which is called by
          delete_MAR_censoring()) works for matrices", {
  set.seed(12345)
  mat_miss <- delete_MAR_censoring(matrix_100_2, 0.2, 1, 2)
  expect_equal(count_NA(mat_miss), c(20, 0))
  expect_equal(count_NA(mat_miss[1:20, ]), c(20, 0))

  mat_miss <- delete_MAR_censoring(matrix_20_10, c(0.1, 0.2, 0.3), 1:3, 8:10,
    where = "upper"
  )
  expect_equal(count_NA(mat_miss), c(2, 4, 6, rep(0, 7)))
  expect_equal(count_NA(mat_miss[1:10, ]), c(2, 4, 6, rep(0, 7)))
})

test_that("delete_MAR_censoring() (and delete_censoring(), which is called by
          delete_MAR_censoring()) works for tibbles", {
  set.seed(12345)
  tbl_miss <- delete_MAR_censoring(tbl_XY_100, 0.2, 1, 2)
  expect_equal(count_NA(tbl_miss), c(X = 20, Y = 0))
  expect_equal(count_NA(tbl_miss[1:20, ]), c(X = 20, Y = 0))

  tbl_miss <- delete_MAR_censoring(tbl_XYZ_100, c(0.1, 0.2), 1:2, c(3, 3),
    where = "upper"
  )
  expect_equal(count_NA(tbl_miss), c(X = 10, Y = 20, Z = 0))
  expect_equal(count_NA(tbl_miss[1:20, ]), c(X = 10, Y = 20, Z = 0))
})

# check delete_MNAR_censoring -----------------------------
test_that("delete_MNAR_censoring() works", {

  # check that delete_MNAR_censoring() calls check_delete_args_MNAR()
  expect_error(
    delete_MNAR_censoring(df_XY_X_miss, 0.1, "X"),
    "cols_miss must be completely observed; no NAs in ds\\[, cols_miss\\] allowed"
  )

  df_miss <- delete_MNAR_censoring(df_XY_100, c(0.3, 0.1), c("X", "Y"))
  expect_equal(count_NA(df_miss), c(X = 30, Y = 10))
  expect_true(all(is.na(df_miss[1:30, "X"])))
  expect_true(all(is.na(df_miss[1:10, "Y"])))
})
