# is_df_or_matrix -----------------------------------------
test_that("is_df_or_matrix()", {
  expect_true(is_df_or_matrix(data.frame(X = 1)))
  expect_true(is_df_or_matrix(matrix(1:4, ncol = 2)))
  expect_false(is_df_or_matrix(c(1, 4)))
})


# resample ------------------------------------------------
test_that("resample() works", {
  # beware of sample() this!
  expect_equal(replicate(10, resample(30, 1)), rep(30, 10))
  expect_error(resample(10, 2),
               "resampling of size 2 not possible without replacement")
})
