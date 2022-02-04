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

  w1 <- mvtnorm::dmvnorm(c(3, -1)) * 0.3
  w2 <- mvtnorm::dmvnorm(c(3, 7), c(3, 3)) * 0.7
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

test_that("impute_gmc_estimate() works for k = 2", {
  ds <- ds_rmvnorm_2d
  ds_1 <- impute_expected_values(ds, gmc_parameters_2d_2k$mu[[1]], S = gmc_parameters_2d_2k$sigma[[1]])
  ds_2 <- impute_expected_values(ds, gmc_parameters_2d_2k$mu[[2]], S = gmc_parameters_2d_2k$sigma[[2]])
  ds_ges <- ds
  for (i in 1:7) {
    row_values <- list(ds_1[i, ], ds_2[i, ])
    ds_ges[i, ] <- weighted_av_gmc(row_values, gmc_parameters_2d_2k, k = 2)
  }
  expect_equal(
    ds_ges,
    impute_gmc_estimate(ds, gmc_parameters_2d_2k, k = 2)
  )
})


test_that("K_estimate() initial imputation works", {
  set.seed(123)
  ds <- ds_rmvnorm_2d
  ds_comp <- ds[complete.cases(ds), ]
  gmc_paras <- mixtools::mvnormalmixEM(ds_comp, k = 2)
  ds_imp <- impute_gmc_estimate(ds, gmc_paras, k = 2)
  ds_imp_K_est <- K_estimate(ds, k = 2, max_iter = 0)
  expect_false(anyNA(ds_imp_K_est))
  skip_on_cran() # to random (=risky) for CRAN
  expect_lt(mean(abs(ds_imp - ds_imp_K_est)), 1)
})

test_that("K_estimate() works for k = 2", {
  set.seed(123)
  ds <- ds_rmvnorm_2d
  ds_imp <- K_estimate(ds, k = 2)
  expect_false(anyNA(ds_imp))
  K_estimate(ds, k = 2) # noch ein tryCatch um mixtools


})

# k = 1 muss ich extra machen! mvnormalmixEM will kein k = 1
