test_that("count_NA() works with vectors", {
  expect_equal(count_NA(c(1, NA, NA, NA, 2)), 3)
})

test_that("count_NA() works with data frames", {
  test_df <- data.frame(X1 = rep(c(1, NA), 5), X2 = c(1:9, NA))
  expect_equal(count_NA(test_df), count_NA(test_df, "cols")) # default is cols
  expect_equal(count_NA(test_df, "all"), 6)
  expect_equal(count_NA(test_df, "cols"), c(X1 = 5, X2 = 1))
  expect_equal(count_NA(test_df, "rows"), c(rep(c(0, 1), 4), 0, 2))
})

test_that("count_NA() works with matrices", {
  test_mat <- matrix(c(rep(c(1, NA), 5), c(1:9, NA)), ncol = 2)
  expect_equal(count_NA(test_mat), count_NA(test_mat, "cols")) # default is cols
  expect_equal(count_NA(test_mat, "all"), 6)
  expect_equal(count_NA(test_mat, "cols"), c(5, 1))
  expect_equal(count_NA(test_mat, "rows"), c(rep(c(0, 1), 4), 0, 2))
})
