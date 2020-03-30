# delete_1_to_x and delete_MAR_1_to_x ---------------------
test_that("delete_MAR_1_to_x() calls check_delete_args_MAR()", {
  expect_error(
    delete_MAR_1_to_x(df_XY_100, 0.1, 1, ctrl_cols = 3, x = 2),
    "indices in ctrl_cols must be in 1:ncol\\(ds)"
  )
})

test_that("delete_MAR_1_to_x() (and delete_1_to_x(), which is called by
          delete_MAR_1_to_x()) works for data.frames", {
  set.seed(12345)
  # check p -----------------------------------------------
  df_miss <- delete_MAR_1_to_x(df_XYZ_100, c(0.2, 0.4), c("X", "Z"), c("Y", "Y"), x = 4)
  expect_equal(count_NA(df_miss), c(X = 20, Y = 0, Z = 40))
  expect_equal(count_NA(df_miss[1:50, ]), c(X = 4, Y = 0, Z = 8))
  expect_equal(count_NA(df_miss[51:100, ]), c(X = 16, Y = 0, Z = 32))

  # p too low to get missing values
  expect_equal(
    count_NA(delete_MAR_1_to_x(df_XY_100, 0.001,
      miss_cols = "Y", ctrl_cols = "X", x = 3
    )),
    c(X = 0, Y = 0)
  )

  # check x -----------------------------------------------
  # x errors
  expect_error(
    delete_MAR_1_to_x(df_XY_100, 0.2, "X", "Y", x = 1:2),
    "x must be a single number"
  )
  expect_error(
    delete_MAR_1_to_x(df_XY_100, 0.2, "X", "Y", x = "asdf"),
    "x must be a single number"
  )
  expect_error(
    delete_MAR_1_to_x(df_XY_100, 0.2, "X", "Y", x = 0),
    "x must be greater than 0 and finite"
  )
  expect_error(
    delete_MAR_1_to_x(df_XY_100, 0.2, "X", "Y", x = Inf),
    "x must be greater than 0 and finite"
  )

  # normal x, p combination
  df_miss <- delete_MAR_1_to_x(df_XYZ_100, 0.2, "X", "Y", x = 4)
  expect_equal(count_NA(df_miss), c(X = 20, Y = 0, Z = 0))
  expect_equal(count_NA(df_miss[1:50, ]), c(X = 4, Y = 0, Z = 0))
  expect_equal(count_NA(df_miss[51:100, ]), c(X = 16, Y = 0, Z = 0))


  # very small x
  df_small_x <- delete_MAR_1_to_x(df_XYZ_100, 0.2, "X", "Y", x = 1e-5)
  expect_equal(count_NA(df_small_x), c(X = 20, Y = 0, Z = 0))
  expect_equal(count_NA(df_small_x[1:50, ]), c(X = 19, Y = 0, Z = 0))

  expect_warning(
    df_small_x <- delete_MAR_1_to_x(df_XYZ_100, 0.6, "X", "Y", x = 1e-5),
    "p is too high or x to low; x is set to 0.2 to get expected"
  )
  expect_equal(count_NA(df_small_x), c(X = 60, Y = 0, Z = 0))
  expect_equal(count_NA(df_small_x[1:50, ]), c(X = 50, Y = 0, Z = 0))

  # very huge x
  df_big_x <- delete_MAR_1_to_x(df_XYZ_100, 0.2, "X", "Y", x = 1e10)
  expect_equal(count_NA(df_big_x), c(X = 20, Y = 0, Z = 0))
  expect_equal(count_NA(df_big_x[51:100, ]), c(X = 20, Y = 0, Z = 0))

  expect_warning(
    df_big_x <- delete_MAR_1_to_x(df_XYZ_100, 0.6, "X", "Y", x = 1e10),
    "p \\(or x) is too high; x is set to 5 to get expected"
  )
  expect_equal(count_NA(df_big_x), c(X = 60, Y = 0, Z = 0))
  expect_equal(count_NA(df_big_x[51:100, ]), c(X = 50, Y = 0, Z = 0))

  # too high x, p combination:
  expect_warning(
    df_miss <- delete_MAR_1_to_x(df_XY_100, 0.9, "X", "Y", x = 3),
    "p \\(or x) is too high; x is set to 1.25"
  )
  expect_equal(count_NA(df_miss), c(X = 90, Y = 0))
  expect_equal(count_NA(df_miss[51:100, ]), c(X = 50, Y = 0))

  # too low x (for high p):
  expect_warning(
    df_miss <- delete_MAR_1_to_x(df_XY_100, 0.9, "X", "Y", x = 0.1),
    "p is too high or x to low; x is set to 0.8"
  )
  expect_equal(count_NA(df_miss), c(X = 90, Y = 0))
  expect_equal(count_NA(df_miss[51:100, ]), c(X = 40, Y = 0))

  # check cutoff_fun --------------------------------------
  # median via stats::quantile()
  df_miss <- delete_MAR_1_to_x(df_XYZ_100, 0.2, "X", "Y",
    x = 4,
    cutoff_fun = stats::quantile,
    probs = 0.5
  )
  expect_equal(count_NA(df_miss), c(X = 20, Y = 0, Z = 0))
  expect_equal(count_NA(df_miss[1:50, ]), c(X = 4, Y = 0, Z = 0))
  expect_equal(count_NA(df_miss[51:100, ]), c(X = 16, Y = 0, Z = 0))

  df_miss <- delete_MAR_1_to_x(df_XYZ_100, 0.2, "X", "Y",
    x = 4,
    cutoff_fun = stats::quantile,
    probs = 0.2
  )
  expect_equal(count_NA(df_miss), c(X = 20, Y = 0, Z = 0))
  expect_equal(count_NA(df_miss[1:20, ]), c(X = 1, Y = 0, Z = 0))
  expect_equal(count_NA(df_miss[21:100, ]), c(X = 19, Y = 0, Z = 0))

  # check stochastic = TRUE -------------------------------
  expect_false(anyNA(delete_MAR_1_to_x(df_XYZ_100, 0, "X", "Y",
    x = 3,
    stochastic = TRUE
  )))
  miss_09 <- delete_MAR_1_to_x(df_XYZ_100, 0.9, "X", "Y",
    x = 1.2,
    stochastic = TRUE
  )
  expect_true(anyNA(miss_09))
  expect_true(count_NA(miss_09)["X"] > 0)
  expect_equal(count_NA(miss_09)[c("Y", "Z")], c(Y = 0, Z = 0))

  N <- 1000
  res <- matrix(nrow = N, ncol = 3)
  colnames(res) <- c("X1", "X2", "Y")
  for (i in seq_len(N)) {
    miss_ds <- delete_MAR_1_to_x(df_XY_100, 0.5, "X", "Y",
      x = 4,
      stochastic = TRUE
    )
    res[i, "Y"] <- sum(is.na(miss_ds[, "Y"]))
    res[i, "X1"] <- sum(is.na(miss_ds[1:50, "X"]))
    res[i, "X2"] <- sum(is.na(miss_ds[51:100, "X"]))
  }
  sum_X1 <- sum(res[, "X1"])
  sum_X2 <- sum(res[, "X2"])
  x_hat <- sum_X2 / sum_X1
  p_X <- (sum_X1 + sum_X2) / (100 * N)
  expect_true(0.4 < p_X & p_X < 0.6)
  expect_true(3 < x_hat & x_hat < 5)
  expect_equal(sum(res[, "Y"]), 0)

  # adjusting x correctly, a warning will be generated each time and suppressed
  test_max_x <- suppressWarnings(replicate(
    1000,
    sum(is.na(delete_MAR_1_to_x(df_XY_20, 0.9, "X", "Y",
      x = 3,
      stochastic = TRUE
    )[1:10, "X"]))
  ))
  expect_true(mean(test_max_x) < 9 && mean(test_max_x) > 7)

  # check special ctrl_col cases --------------------------
  # ctrl_col constant
  expect_warning(
    df_miss <- delete_MAR_1_to_x(df_XY_X_constant, 0.2, miss_cols = "Y", ctrl_cols = "X", x = 3),
    "is constant"
  )
  expect_equal(count_NA(df_miss), c(X = 0, Y = 4))
  # ctr_col nearly constant
  expect_equal(
    count_NA(delete_MAR_1_to_x(df_XY_X_one_outlier, 0.2,
      miss_cols = "Y", ctrl_cols = "X", x = 3
    )),
    c(X = 0, Y = 4)
  )

  # ctr_col nearly constant: expected odds are 1:x?
  test <- replicate(1000, is.na(delete_MAR_1_to_x(df_XY_X_unequal_dummy, 0.1,
    miss_cols = "Y",
    ctrl_cols = "X", x = 3,
    stochastic = TRUE
  )[11, "Y"]))
  realized_x <- sum(test) / ((nrow(df_XY_X_unequal_dummy) * 1000 * 0.1 - sum(test)) / 10)
  expect_true(2 < realized_x && realized_x < 4)



  # check add_realized_x = TRUE ------------------------
  expect_equal(
    attr(delete_MAR_1_to_x(df_XYZ_100, 0.2, "X", "Y",
      x = 4,
      add_realized_x = TRUE
    ),
    "realized_x",
    exact = TRUE
    ),
    c(X = 4)
  )

  df_miss <- delete_MAR_1_to_x(df_XYZ_100, 0.3, "X", "Y",
    x = 3,
    add_realized_x = TRUE
  )
  count_NA(df_miss[1:50, ])
  count_NA(df_miss[51:100, ])
  expect_equal(
    attr(delete_MAR_1_to_x(df_XYZ_100, 0.3, "X", "Y",
      x = 3,
      add_realized_x = TRUE
    ),
    "realized_x",
    exact = TRUE
    ),
    c(X = 23 / 7)
  )
})

test_that("delete_MAR_1_to_x() (and delete_1_to_x(), which is called by
          delete_MAR_1_to_x()) works for matrices", {
  set.seed(12345)
  mat_miss <- delete_MAR_1_to_x(matrix_100_2, 0.2, 1, 2, x = 4)
  expect_equal(count_NA(mat_miss), c(20, 0))
  expect_equal(count_NA(mat_miss[1:50, ]), c(4, 0))

  mat_miss <- delete_MAR_1_to_x(matrix_20_10, c(0.1, 0.2, 0.3), 1:3, 8:10, x = 3)
  expect_equal(count_NA(mat_miss), c(2, 4, 6, rep(0, 7)))
  expect_equal(count_NA(mat_miss[1:10, ]), c(2, 3, 5, rep(0, 7)))

  # stochastic = TRUE
  mat_miss <- delete_MAR_1_to_x(matrix_100_2, 0.6, 1, 2, x = 5, stochastic = TRUE)
  expect_equal(count_NA(mat_miss[51:100, ]), c(50, 0))
  expect_true(count_NA(mat_miss[1:50, 1, drop = FALSE]) <= 25)
  # prob for false:
  # pbinom(25, 50, 0.1, lower.tail = FALSE) # 1.074847e-13
})

test_that("delete_MAR_1_to_x() (and delete_1_to_x(), which is called by
          delete_MAR_1_to_x()) works for tibbles", {
  set.seed(12345)
  tbl_miss <- delete_MAR_1_to_x(tbl_XY_100, 0.2, 1, 2, x = 4)
  expect_equal(count_NA(tbl_miss), c(X = 20, Y = 0))
  expect_equal(count_NA(tbl_miss[1:50, ]), c(X = 4, Y = 0))

  tbl_miss <- delete_MAR_1_to_x(tbl_XYZ_100, c(0.1, 0.2), 1:2, c(3, 3), x = 9)
  expect_equal(count_NA(tbl_miss), c(X = 10, Y = 20, Z = 0))
  expect_equal(count_NA(tbl_miss[1:50, ]), c(X = 9, Y = 18, Z = 0))

  # stochastic = TRUE
  tbl_miss <- delete_MAR_1_to_x(tbl_XY_100, 0.6, 1, 2, stochastic = TRUE, x = 5)
  expect_equal(count_NA(tbl_miss[51:100, ]), c(X = 50, Y = 0))
  expect_true(count_NA(tbl_miss[1:50, 1, drop = FALSE]) <= 25)
  # prob for false:
  # pbinom(25, 50, 0.1, lower.tail = FALSE) # 1.074847e-13
})

# delete_MNAR_1_to_x --------------------------------------
# delete_MNAR_1_to_x only calls delete_1_to_x() with ctrl_cols = miss_cols
# so we only test if the missing values are in the correct variables.
# The rest of delete_1_to_x() is tested with delete_MAR_1_to_x()
test_that("delete_MNAR_1_to_x", {
  # check that delete_MNAR_1_to_x() calls check_delete_args_MNAR()
  expect_error(
    delete_MNAR_1_to_x(df_XY_X_miss, 0.1, "X", x = 3),
    "miss_cols must be completely observed; no NAs in ds\\[, miss_cols\\] allowed"
  )

  expect_equal(
    count_NA(delete_MNAR_1_to_x(df_XYZ_100, 0.1, c("X", "Z"), x = 3)),
    c("X" = 10, "Y" = 0, "Z" = 10)
  )
  expect_equal(
    attr(delete_MNAR_1_to_x(df_XYZ_100, 0.1, c("X", "Z"),
      x = 3.5,
      add_realized_x = TRUE
    ),
    "realized_x",
    exact = TRUE
    ),
    c(X = 4, Z = 4)
  )
})
