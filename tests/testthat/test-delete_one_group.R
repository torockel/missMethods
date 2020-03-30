test_that("delete_MAR_one_group() calls check_delete_args_MAR()", {
  expect_error(
    delete_MAR_one_group(df_XY_100, 0.1, 1, ctrl_cols = 3),
    "indices in ctrl_cols must be in 1:ncol\\(ds)"
  )
})

test_that("delete_one_group() and delete_MAR_one_group() works", {
  set.seed(12345)

  # check p too low to get missing values with stochastic = FALSE -----
  expect_equal(
    count_NA(delete_MAR_one_group(df_XY_100, 0.001,
      miss_cols = "Y", ctrl_cols = "X",
      stochastic = FALSE
    )),
    c(X = 0, Y = 0)
  )

  # check default arguments -------------------------------
  df_miss <- delete_MAR_one_group(df_XYZ_100, 0.2, "X", "Y")
  expect_equal(count_NA(df_miss), c(X = 20, Y = 0, Z = 0))
  expect_true(isTRUE(all.equal(count_NA(df_miss[1:50, ]), c(X = 20, Y = 0, Z = 0))) ||
    isTRUE(all.equal(count_NA(df_miss[51:100, ]), c(X = 20, Y = 0, Z = 0))))

  # check p -----------------------------------------------
  df_miss <- delete_MAR_one_group(df_XYZ_100, c(0.1, 0.4), c("X", "Y"), c("Z", "Z"))
  expect_equal(count_NA(df_miss), c(X = 10, Y = 40, Z = 0))
  expect_true(isTRUE(all.equal(count_NA(df_miss[1:50, ]), c(X = 10, Y = 40, Z = 0))) ||
    isTRUE(all.equal(count_NA(df_miss[1:50, ]), c(X = 10, Y = 0, Z = 0))) ||
    isTRUE(all.equal(count_NA(df_miss[1:50, ]), c(X = 0, Y = 40, Z = 0))) ||
    isTRUE(all.equal(count_NA(df_miss[1:50, ]), c(X = 0, Y = 0, Z = 0))))

  # to high p
  expect_warning(
    df_miss <- delete_MAR_one_group(df_XYZ_100, 0.9, "X", "Y"),
    "not enough objects in miss_group in column Y to reach p"
  )
  expect_equal(count_NA(df_miss), c(X = 50, Y = 0, Z = 0))


  # check FUN ---------------------------------------------
  # median via stats::quantile()
  df_miss <- delete_MAR_one_group(df_XYZ_100, 0.2, "X", "Y",
    FUN = stats::quantile,
    probs = 0.5
  )
  expect_equal(count_NA(df_miss), c(X = 20, Y = 0, Z = 0))
  expect_true(isTRUE(all.equal(count_NA(df_miss[1:50, ]), c(X = 20, Y = 0, Z = 0))) ||
    isTRUE(all.equal(count_NA(df_miss[51:100, ]), c(X = 20, Y = 0, Z = 0))))

  # unequal groups via stats::quantile()
  df_miss <- delete_MAR_one_group(df_XYZ_100, 0.2, "X", "Y",
    FUN = stats::quantile,
    probs = 0.2
  )
  expect_equal(count_NA(df_miss), c(X = 20, Y = 0, Z = 0))
  expect_true(isTRUE(all.equal(count_NA(df_miss[1:20, ]), c(X = 20, Y = 0, Z = 0))) ||
    isTRUE(all.equal(count_NA(df_miss[21:100, ]), c(X = 20, Y = 0, Z = 0))))

  # check unorderd factor as ctrl_col and prob ------------
  df_miss <- delete_MAR_one_group(df_with_unord_factor, 0.4, "Y", "X")
  expect_equal(count_NA(df_miss), c(X = 0, Y = 8))
  expect_true(isTRUE(all.equal(count_NA(df_miss[1:10, ]), c(X = 0, Y = 8))) ||
    isTRUE(all.equal(count_NA(df_miss[11:20, ]), c(X = 0, Y = 8))))

  df_miss <- delete_MAR_one_group(df_with_unord_factor, 0.4, "Y", "X", prob = 0.4)
  expect_equal(count_NA(df_miss), c(X = 0, Y = 8))
  expect_true(isTRUE(all.equal(count_NA(df_miss[1:8, ]), c(X = 0, Y = 8))) ||
    isTRUE(all.equal(count_NA(df_miss[9:20, ]), c(X = 0, Y = 8))))


  # check stochastic = TRUE -------------------------------
  expect_false(anyNA(delete_MAR_one_group(df_XYZ_100, 0, "X", "Y",
    stochastic = TRUE
  )))

  N <- 1000
  res <- matrix(nrow = N, ncol = 3)
  colnames(res) <- c("X1", "X2", "Y")
  for (i in seq_len(N)) {
    miss_ds <- delete_MAR_one_group(df_XY_100, 0.2, "X", "Y",
      stochastic = TRUE
    )
    res[i, "Y"] <- sum(is.na(miss_ds[, "Y"]))
    res[i, "X1"] <- sum(is.na(miss_ds[1:50, "X"]))
    res[i, "X2"] <- sum(is.na(miss_ds[51:100, "X"]))
  }
  sum_X1 <- sum(res[, "X1"])
  sum_X2 <- sum(res[, "X2"])

  expect_true(9350 < sum_X1 & sum_X1 < 10650)
  expect_true(9350 < sum_X2 & sum_X2 < 10650)
  # pbinom(9350, 1e5, 0.1) + pbinom(10649, 1e5, 0.1, lower.tail = FALSE) # 8.237197e-12
  expect_equal(sum(res[, "Y"]), 0)

  # check nr_unique == 2 ----------------------------------
  df_miss <- delete_MAR_one_group(df_XY_X_binary, 0.4, "Y", "X")
  expect_equal(count_NA(df_miss), c(X = 0, Y = 8))
  expect_true(isTRUE(all.equal(count_NA(df_miss[1:10, ]), c(X = 0, Y = 8))) ||
    isTRUE(all.equal(count_NA(df_miss[11:20, ]), c(X = 0, Y = 8))))

  # check ordered_as_unordered ----------------------------
  df_cutoff_problematic <- data.frame(
    X = ordered(c(0, 0, rep(1, 6), 2, 2)),
    Y = 101:110
  )
  for (i in 1:20) {
    df_miss <- delete_MAR_one_group(df_cutoff_problematic, 0.3, "Y", "X",
      ordered_as_unordered = TRUE
    )
    expect_equal(count_NA(df_miss), c(X = 0, Y = 3))
    expect_true(isTRUE(all.equal(count_NA(df_miss[3:8, ]), c(X = 0, Y = 3))) ||
      isTRUE(all.equal(count_NA(df_miss[3:8, ]), c(X = 0, Y = 0))))
  } # if ordered_as_unordered = FALSE (or argument does not work),
  # delete_MAR_one_group will issue a warning with probability 1 - 1/2^20.


  # check special ctrl_col cases --------------------------
  # ctrl_col constant
  expect_warning(
    miss_df <- delete_MAR_one_group(df_XY_X_constant, 0.2,
      miss_cols = "Y", ctrl_cols = "X"
    ),
    "is constant"
  )
  expect_equal(count_NA(miss_df), c(X = 0, Y = 4))

  # ctr_col nearly constant
  # things depend on the (random) choice of miss_group:
  # warning and to less missing objects or everything fine
  # not really testable?
  # expect_equal(count_NA(delete_MAR_one_group(df_XY_X_one_outlier, 0.2,
  #                                         miss_cols = "Y", ctrl_cols = "X")),
  #              c(X = 0, Y = 4))
})

test_that("delete_MAR_one_group() (and delete_one_group(), which is called by
          delete_MAR_one_group()) works for matrices", {
  set.seed(12345)
  mat_miss <- delete_MAR_one_group(matrix_100_2, 0.2, 1, 2)
  expect_equal(count_NA(mat_miss), c(20, 0))
  expect_true(isTRUE(all.equal(count_NA(mat_miss[1:50, ]), c(20, 0))) ||
    isTRUE(all.equal(count_NA(mat_miss[1:50, ]), c(0, 0))))

  mat_miss <- delete_MAR_one_group(matrix_20_10, c(0.1, 0.2, 0.3), 1:3, 8:10)
  expect_equal(count_NA(mat_miss), c(2, 4, 6, rep(0, 7)))
  expect_true(isTRUE(all.equal(count_NA(mat_miss[1:10, 3:4]), c(6, 0))) ||
    isTRUE(all.equal(count_NA(mat_miss[1:10, 3:4]), c(0, 0))))
})

test_that("delete_MAR_one_group() (and delete_one_group(), which is called by
          delete_MAR_one_group()) works for tibbles", {
  set.seed(12345)
  tbl_miss <- delete_MAR_one_group(tbl_XY_100, 0.2, 1, 2)
  expect_equal(count_NA(tbl_miss), c(X = 20, Y = 0))
  expect_true(isTRUE(all.equal(count_NA(tbl_miss[1:50, ]), c(X = 20, Y = 0))) ||
                isTRUE(all.equal(count_NA(tbl_miss[1:50, ]), c(X = 0, Y = 0))))

  tbl_miss <- delete_MAR_one_group(tbl_XYZ_100, c(0.1, 0.2), 2:3, c(1, 1))
  expect_equal(count_NA(tbl_miss), c(X = 0, Y = 10, Z = 20))
  expect_true(isTRUE(all.equal(count_NA(tbl_miss[1:50, 2]), c(Y = 10))) ||
                isTRUE(all.equal(count_NA(tbl_miss[1:50, 2]), c(Y = 00))))
})

