
test_that("get_EM_parameters() handles norm crashes", {
  expect_error(
    suppressWarnings(get_EM_parameters(data.frame(X = 1:2), maxit = factor("a"))),
    "The EM algorithm of norm produced an error; no imputation was done")
})

test_that("get_EM_parameters() checks iterations", {
  expect_warning(
    get_EM_parameters(data.frame(X = 1:4, Y = c(NA, NA, NA, 2)), maxits = 2),
    "EM did not converge with maxits = 2."
  )
})


test_that("get_EM_parameters() returns correct values", {
  return_val <- get_EM_parameters(data.frame(X = 1:4, Y = c(1:3, NA)))
  expect_true(is.numeric(attr(return_val, "iterations", TRUE)))
  expect_equal(return_val$mu[1], mean(1:4))
  expect_equal(return_val$sigma[1, 1], var(1:4) * 3 / 4)
  expect_equal(return_val$mu[2], mean(1:4), tolerance = 0.1)
  expect_equal(return_val$sigma, matrix(1.25, 2, 2), tolerance = 0.1, check.attributes = FALSE)
})


test_that("impute_EM() works (basic test)", {
  expect_equal(
    impute_EM(data.frame(X = 1:4, Y = c(1:3, NA)), stochastic = FALSE),
    data.frame(X = 1:4, Y = 1:4), tolerance = 0.1)

  expect_equal(
    impute_EM(data.frame(X = 1:4, Y = c(1:3, NA)), stochastic =  TRUE),
    data.frame(X = 1:4, Y = 1:4), tolerance = 0.2)
})
