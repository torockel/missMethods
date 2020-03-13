test_that("delete_MAR_rank() calls check_delete_args_MAR()", {
  expect_error(delete_MAR_rank(df_XY_100, 0.1, 1, ctrl_cols = 3),
               "indices in ctrl_cols must be in 1:ncol\\(ds)")
})


test_that("delete_MAR_rank() and delete_rank() works", {
  set.seed(12345)

    # check p
  df_miss <- delete_MAR_rank(df_XY_100, 0.3, "X", "Y")
  expect_equal(count_NA(df_miss), c(X = 30, Y = 0))
  expect_false(anyNA(delete_MAR_rank(df_XY_100, 0.001, "X", "Y"))) # to low p

  # check rank probabilities

  N <- 1000
  rank_correct <- sum(replicate(N, is.na(delete_MAR_rank(df_XY_2, 0.5, "X", "Y")[2, 1])))
  expect_true(rank_correct > 566 && rank_correct < 766)
  # probability for FALSE:
  # pbinom(566, 1000, 2/3) + pbinom(765, 1000, 2/3, lower.tail = FALSE)
  # 2.683384e-11 # not very likely...


  # check special ctrl_col cases
  # ctrl_col constant
  expect_equal(count_NA(delete_MAR_rank(df_XY_X_constant, 0.2, miss_cols = "Y",
                                             ctrl_cols = "X")),
               c(X = 0, Y = 4))

  # ctr_col nearly constant
  expect_equal(count_NA(delete_MAR_rank(df_XY_X_one_outlier, 0.2,
                                             miss_cols = "Y", ctrl_cols = "X")),
               c(X = 0, Y = 4))
  # ordered control column
  expect_equal(count_NA(delete_MAR_rank(df_with_ord_factors, 0.2,
                                             miss_cols = "Y", ctrl_cols = "X")),
               c(X = 0, Y = 4))

})


test_that("delete_MAR_rank() (and delete_rank(), which is called by
          delete_MAR_rank()) works for matrices", {

  set.seed(12345)
  mat_miss <- delete_MAR_rank(matrix_100_2, 0.2, 1, 2)
  expect_equal(count_NA(mat_miss), c(20, 0))

  mat_miss <- delete_MAR_rank(matrix_20_10, c(0.1, 0.2, 0.3), 1:3, 8:10)
  expect_equal(count_NA(mat_miss), c(2, 4, 6, rep(0, 7)))

})

# check delete_MNAR_rank -----------------------------
test_that("delete_MNAR_rank() works", {
  # check that delete_MNAR_rank() calls check_delete_args_MNAR()
  expect_error(delete_MNAR_rank(df_XY_X_miss, 0.1, "X"),
               "miss_cols must be completely observed; no NAs in ds\\[, miss_cols\\] allowed")

  df_miss <- delete_MNAR_rank(df_XY_100, c(0.3, 0.1), c("X", "Y"))
  expect_equal(count_NA(df_miss), c(X = 30, Y = 10))
})
