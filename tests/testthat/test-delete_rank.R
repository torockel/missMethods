test_that("delete_MAR_rank() and delete_rank() works", {
  set.seed(12345)

  # check p
  df_mis <- delete_MAR_rank(df_XY_100, 0.3, "X", "Y")
  expect_equal(count_NA(df_mis), c(X = 30, Y = 0))
  expect_false(anyNA(delete_MAR_rank(df_XY_100, 0.001, "X", "Y"))) # to low p

  # check rank probabilities

  N <- 1000
  rank_correct <- sum(
    replicate(N, is.na(delete_MAR_rank(df_XY_2, 0.5, "X", "Y")[2, 1]))
  )
  expect_true(rank_correct > 566 && rank_correct < 766)
  # probability for FALSE:
  # pbinom(566, 1000, 2/3) + pbinom(765, 1000, 2/3, lower.tail = FALSE)
  # 2.683384e-11 # not very likely...

  # check n_mis_stochastic

  set.seed(12345)
  N <- 1000
  n_mis <- mean(replicate(N, count_NA(delete_MAR_rank(df_XY_20, 0.125, "X", "Y", n_mis_stochastic = TRUE)["X"])))
  expect_true(2 < n_mis, n_mis < 3)


  # check special ctrl_col cases
  # ctrl_col constant
  expect_equal(
    count_NA(delete_MAR_rank(df_XY_X_constant, 0.2,
      cols_mis = "Y",
      cols_ctrl = "X"
    )),
    c(X = 0, Y = 4)
  )

  # ctr_col nearly constant
  expect_equal(
    count_NA(delete_MAR_rank(df_XY_X_one_outlier, 0.2,
      cols_mis = "Y", cols_ctrl = "X"
    )),
    c(X = 0, Y = 4)
  )
  # ordered control column
  expect_equal(
    count_NA(delete_MAR_rank(df_with_ord_factors, 0.2,
      cols_mis = "Y", cols_ctrl = "X"
    )),
    c(X = 0, Y = 4)
  )
})


test_that("delete_MAR_rank() (and delete_rank(), which is called by
          delete_MAR_rank()) works for matrices", {
  set.seed(12345)
  mat_mis <- delete_MAR_rank(matrix_100_2, 0.2, 1, 2)
  expect_equal(count_NA(mat_mis), c(20, 0))

  mat_mis <- delete_MAR_rank(matrix_20_10, c(0.1, 0.2, 0.3), 1:3, 8:10)
  expect_equal(count_NA(mat_mis), c(2, 4, 6, rep(0, 7)))
})

test_that("delete_MAR_rank() (and delete_rank(), which is called by
          delete_MAR_rank()) works for tibbles", {
  set.seed(12345)
  tbl_mis <- delete_MAR_rank(tbl_XY_100, 0.2, 1, 2)
  expect_equal(count_NA(tbl_mis), c(X = 20, Y = 0))

  tbl_mis <- delete_MAR_rank(tbl_XYZ_100, c(0.1, 0.2), 1:2, c(3, 3))
  expect_equal(count_NA(tbl_mis), c(X = 10, Y = 20, Z = 0))
})

# check delete_MNAR_rank -----------------------------
test_that("delete_MNAR_rank() works", {
  df_mis <- delete_MNAR_rank(df_XY_100, c(0.3, 0.1), c("X", "Y"))
  expect_equal(count_NA(df_mis), c(X = 30, Y = 10))
})
