test_that("evaluate_parameters()", {
  true_par <- 1:4
  est_par <- 2:5
  expect_equal(evaluate_parameters(est_par, true_par, "RMSE"), 1)
  expect_error(evaluate_parameters(1:3, true_par, "RMSE"),
               "the dimensions of est_par and true_par must be equal")
})
