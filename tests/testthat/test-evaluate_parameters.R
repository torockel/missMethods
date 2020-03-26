test_that("evaluate_parameters()", {
  true_pars <- 1:4
  est_pars <- 2:5
  expect_equal(evaluate_parameters(est_pars, true_pars, "RMSE"), 1)
  expect_error(
    evaluate_parameters(1:3, true_pars, "RMSE"),
    "the dimensions of est_pars and true_pars must be equal"
  )
})
