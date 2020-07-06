test_that("evaluate_parameters()", {
  pars_true <- 1:4
  pars_est <- 2:5
  expect_equal(evaluate_parameters(pars_est, pars_true, "RMSE"), 1)
  expect_error(
    evaluate_parameters(1:3, pars_true, "RMSE"),
    "the dimensions of pars_est and pars_true must be equal"
  )
})
