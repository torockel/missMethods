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
  ds_imp_K_est <- K_estimate(ds, k = 2, imp_max_iter = 0)
  expect_false(anyNA(ds_imp_K_est))
  skip_on_cran() # to random (=risky) for CRAN
  expect_lt(mean(abs(ds_imp - ds_imp_K_est)), 1)
})

test_that("K_estimate() works for k = 1", {
  set.seed(123)
  ds <- ds_rmvnorm_2d

  # no iterations
  ds_comp_cases <- ds[complete.cases(ds), ]
  mu <- colMeans(ds_comp_cases)
  sigma <- stats::cov(ds_comp_cases)
  ds_imp <- impute_expected_values(ds, mu, sigma)
  ds_K_estimate <- K_estimate(ds, k = 1, imp_max_iter = 0)
  expect_equal(ds_imp, ds_K_estimate, check.attributes = FALSE)

  # one iteration
  mu <- colMeans(ds_imp)
  sigma <- stats::cov(ds_imp)
  ds_imp <- impute_expected_values(ds_imp, mu, sigma, M = is.na(ds))
  ds_K_estimate <- K_estimate(ds, k = 1, imp_max_iter = 1)
  expect_equal(ds_imp, ds_K_estimate, check.attributes = FALSE)
})

test_that("K_estimate() works for k = 2", {
  set.seed(123)
  ds <- ds_rmvnorm_2d
  ds_imp <- K_estimate(ds, k = 2)
  expect_false(anyNA(ds_imp))
  ds_imp <- K_estimate(ds, k = 2, max_tries_restart = 1L) # without tryCatch() this throws an error
  expect_false(anyNA(ds_imp))
  expect_true(attr(ds_imp, "mixtools_error"))
})

test_that("K_estimate() works for k = 3 with three clusters", {
  set.seed(123)
  mu_low <- -10
  mu_mid <- 20
  mu_high <- -20
  ds <- rbind(
    mvtnorm::rmvnorm(50, c(-10, mu_low)),
    mvtnorm::rmvnorm(50, c(10, mu_mid)),
    mvtnorm::rmvnorm(50, c(20, mu_high))
  )
  ind_mis_low <- c(1, 4, 10)
  ind_mis_mid <- c(55, 60, 73, 84)
  ind_mis_high <- c(112, 133)
  ds[c(ind_mis_low, ind_mis_mid, ind_mis_high), 2] <- NA
  ds_imp <- K_estimate(ds, k = 3)

  expect_true(mean(abs(ds_imp[ind_mis_low, 2] - mu_low)) < 2)
  expect_true(mean(abs(ds_imp[ind_mis_mid, 2] - mu_mid)) < 2)
  expect_true(mean(abs(ds_imp[ind_mis_high, 2] - mu_high)) < 2)

  ds_imp2 <- K_estimate(ds, k = 2) # errors without tryCatch initial imputation
  expect_false(anyNA(ds_imp2))
})

test_that("K_estimate() works with no complete observation", {
  expect_false(anyNA(K_estimate(df_XY_no_comp_obs, 1)))
  set.seed(123)
  mu_low <- -20
  mu_mid <- 0
  mu_high <- 20
  ds <- rbind(
    mvtnorm::rmvnorm(50, rep(mu_low, 3)),
    mvtnorm::rmvnorm(50, rep(mu_mid, 3)),
    mvtnorm::rmvnorm(50, rep(mu_high, 3))
  )
  M <- matrix(c(TRUE, FALSE, FALSE,
                TRUE, TRUE, FALSE,
                FALSE, TRUE, FALSE,
                FALSE, FALSE, TRUE,
                TRUE, FALSE, TRUE),
              nrow = nrow(ds), ncol = ncol(ds), byrow = TRUE)
  M <- matrix(c(TRUE, FALSE, FALSE,
                FALSE, TRUE, FALSE,
                FALSE, FALSE, TRUE),
              nrow = nrow(ds), ncol = ncol(ds), byrow = TRUE)
  ds_no_comp_obs <- ds
  ds_no_comp_obs[M] <- NA
  ds_imp <- K_estimate(ds_no_comp_obs, 3)
  expect_false(anyNA(ds_imp))
  # but results are not really sensible, gmc does not finde the clusters:
  # mu_matrix <- matrix(rep(c(mu_low, mu_mid, mu_high), each = 150), nrow = 150, byrow = TRUE)
  # ds_imp[M] - mu_matrix[M]
})

test_that("impute_GMC() works for k_max = 3", {
  set.seed(12345)
  mu_low <- -10
  mu_mid <- 20
  mu_high <- -20
  ds <- rbind(
    mvtnorm::rmvnorm(50, c(-10, mu_low)),
    mvtnorm::rmvnorm(50, c(10, mu_mid)),
    mvtnorm::rmvnorm(50, c(20, mu_high))
  )
  ind_mis_low <- c(1, 4, 10)
  ind_mis_mid <- c(55, 60, 73, 84)
  ind_mis_high <- c(112, 133)
  ind_mis <- c(ind_mis_low, ind_mis_mid, ind_mis_high)
  ds[ind_mis, 2] <- NA
  ds_imp1 <- K_estimate(ds, k = 1)
  ds_imp2 <- K_estimate(ds, k = 2)
  ds_imp3 <- K_estimate(ds, k = 3)
  ds_test <- (ds_imp1 + ds_imp2 + ds_imp3) / 3
  ds_imp_GMC <- impute_GMC(ds, 3)
  expect_false(anyNA(ds_imp_GMC))
  skip_on_cran() # too risky for CRAN
  expect_true(mean(abs(ds_test[ind_mis, 2] - ds_imp_GMC[ind_mis, 2])) < 5)
})

test_that("impute_GMC() works with no complete obs")

test_that("impute_GMC() works with data frames", {
  expect_false(anyNA(impute_GMC(df_XY_XY_mis, 2)))
  cbind(df_XY_no_comp_obs, impute_GMC(df_XY_no_comp_obs, 2))


  impute_EM(df_XY_no_comp_obs)
})
