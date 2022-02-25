test_that("simulate_data() works", {
  true_sigma <- matrix(c(2, 0.5, 0.5, 2), nrow = 2)
  test <- simulate_data(50, sigma = true_sigma, p = 0.3)
  expect_equal(
    names(test),
    c("ds_comp", "pars_true", "ds_mis")
  )
  expect_equal(
    test$pars_true,
    list(mean = c(0, 0), sigma = true_sigma)
  )
  expect_true(is.matrix(test$ds_comp))
  expect_equal(dim(test$ds_comp), c(50, 2))
  expect_true(is.matrix(test$ds_mis))
  expect_equal(dim(test$ds_mis), c(50, 2))
  expect_equal(sum(is.na(test$ds_mis)), 2 * 50 * 0.3)
})
