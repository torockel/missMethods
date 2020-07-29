test_that("imputation methods works via simulation", {
  skip("Too long run time to check every time or on CRAN")
  set.seed(123)
  expect_equal(
    rowMeans(
      replicate(
        1000,
        colMeans(
          suppressWarnings(
            impute_EM(
              delete_MCAR(
                mvtnorm::rmvnorm(100, rep(0, 7)),
                p = 0.2
              ),
              stochastic = TRUE
            )
          )
        )
      )
    ),
    rep(0, 7),
    tolerance = 0.01
  )
})
