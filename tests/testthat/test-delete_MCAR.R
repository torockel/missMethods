# check if delete_MCAR() calls check_delete_args_MCAR -----
test_that("delete_MCAR() calls check_delete_args_MCAR()", {
  # special error only from check_delete_args_MCAR:
  expect_error(
    delete_MCAR(df_XY_100, 0.1, p_overall = "A"),
    "p_overall must be logical of length 1"
  )
})

# delete_MCAR ---------------------------------------------
test_that("delete_MCAR() creates MCAR", {
  set.seed(123454)

  # check stochastic = FALSE (default) --------------------
  df_MCAR <- delete_MCAR(df_XY_100, 0.1)
  expect_equal(count_NA(df_MCAR), c(X = 10, Y = 10))

  df_MCAR <- delete_MCAR(df_XY_100, 1)
  expect_equal(count_NA(df_MCAR), c(X = 100, Y = 100))

  df_MCAR <- delete_MCAR(df_XY_100, 0)
  expect_equal(count_NA(df_MCAR), c(X = 0, Y = 0))

  # check miss_cols ---------------------------------------
  df_MCAR <- delete_MCAR(df_XY_100, 0.2, miss_cols = 2)
  expect_equal(count_NA(df_MCAR), c(X = 0, Y = 20))

  df_MCAR <- delete_MCAR(df_XY_100, 0.2, miss_cols = "X")
  expect_equal(count_NA(df_MCAR), c(X = 20, Y = 0))

  # check stochastic = TRUE -------------------------------
  df_MCAR <- delete_MCAR(df_XY_100, p = 0.5, stochastic = TRUE)
  expect_true(anyNA(df_MCAR)) # very, very, very bad luck if delete works

  df_MCAR <- delete_MCAR(df_XY_100, p = 1, stochastic = TRUE)
  expect_equal(count_NA(df_MCAR), c(X = 100, Y = 100))

  df_MCAR <- delete_MCAR(df_XY_100, p = 0, stochastic = TRUE)
  expect_equal(count_NA(df_MCAR), c(X = 0, Y = 0))

  N <- 1000
  res <- 0
  for (i in seq_len(N)) {
    res <- res + sum(count_NA(delete_MCAR(df_XY_100, p = 0.2, stochastic = TRUE)))
  }
  expect_true(res / prod(dim(df_XY_100), N) < 0.3 & res / prod(dim(df_XY_100), N) > 0.1)

  # check p_overall = TRUE --------------------------------
  df_MCAR <- delete_MCAR(df_XY_100, p = 0.2, p_overall = TRUE)
  expect_equal(sum(count_NA(df_MCAR)), 40)

  df_MCAR <- delete_MCAR(df_XY_100, p = 1, p_overall = TRUE)
  expect_equal(sum(count_NA(df_MCAR)), 200)

  df_MCAR <- delete_MCAR(df_XY_100, p = 0, p_overall = TRUE)
  expect_equal(sum(count_NA(df_MCAR)), 0)

})

test_that("delete_MCAR() works with matrices", {
  set.seed(123454)
  ds_m_MCAR <- delete_MCAR(matrix_100_2, p = 0.4)
  expect_equal(count_NA(ds_m_MCAR), c(40, 40))

  ds_m_MCAR <- delete_MCAR(matrix_100_2, p = 0.4, miss_cols = 2)
  expect_equal(count_NA(ds_m_MCAR), c(0, 40))

  ds_m_MCAR <- delete_MCAR(matrix_20_10, p = c(0.1, 0.2, 0.3), miss_cols = 2:4)
  expect_equal(count_NA(ds_m_MCAR), c(0, 2, 4, 6, rep(0, 6)))
})

test_that("delete_MCAR() works with tibbles", {
  set.seed(123454)

  tbl_MCAR <- delete_MCAR(tbl_XY_100, p = 0.4)
  expect_equal(count_NA(tbl_MCAR), c(X = 40, Y = 40))

  tbl_MCAR <- delete_MCAR(tbl_XY_100, p = 0.4, miss_cols = 2)
  expect_equal(count_NA(tbl_MCAR), c(X = 0, Y = 40))

  tbl_MCAR <- delete_MCAR(tbl_XYZ_100, p = c(0.1, 0.2, 0.3), miss_cols = 1:3)
  expect_equal(count_NA(tbl_MCAR), c(X = 10, Y = 20, Z = 30))
})
