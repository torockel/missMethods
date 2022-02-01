test_that("weighted_av_gmc() works", {
  expect_equal(
    weighted_av_gmc(list(c(1, 1), c(1, 1)), gmc_parameters_2d_2k, 2),
    c(1, 1)
  )
  expect_equal(
    weighted_av_gmc(list(c(0, 0), c(3, 3)), gmc_parameters_2d_2k, 2),
    0.7 * c(3, 3)
  )

  expect_equal(
    weighted_av_gmc(list(c(-1, 0), c(4, 3)), gmc_parameters_2d_2k, 2),
    0.3 * c(-1, 0) + 0.7 * c(4, 3)
  )

  expect_equal(
    weighted_av_gmc(list(c(2, -1), c(100, 100)), gmc_parameters_2d_2k, 2),
    c(2, -1)
  )

  w1 <- dmvnorm(c(3, -1)) * 0.3
  w2 <- dmvnorm(c(3, 7), c(3, 3)) * 0.7
  expect_equal(
    weighted_av_gmc(list(c(3, -1), c(3, 7)), gmc_parameters_2d_2k, 2),
    (c(3, -1) * w1 + c(3, 7) * w2) / (w1 + w2)
  )
})

test_that("impute_gmc_estimate() works for k = 1", {
  expect_equal(
    impute_gmc_estimate(df_XY_X_mis, gmc_parameters_2d_2k, k = 1),
    impute_expected_values(df_XY_X_mis, gmc_parameters_2d_2k$mu[[1]], S = gmc_parameters_2d_2k$sigma[[1]])
  )
})
