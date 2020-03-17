test_that("evaluate_imputation_parameters() works", {
  imp_ds <- df_XY_20
  imp_ds[1:10, 1] <- 1
  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20),
               evaluate_parameters(colMeans(imp_ds), colMeans(df_XY_20)))

  # check parameter ---------------------------------------
  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              parameter = "mean"),
               evaluate_parameters(colMeans(imp_ds), colMeans(df_XY_20)))

  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              parameter = "median"),
               evaluate_parameters(sapply(imp_ds, median), sapply(df_XY_20, median)))

  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              parameter = "var"),
               evaluate_parameters(var(imp_ds), var(df_XY_20)))

  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              parameter = "sd"),
               evaluate_parameters(sapply(imp_ds, sd), sapply(df_XY_20, sd)))

  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              parameter = "quantile", probs = 0.3),
               evaluate_parameters(sapply(imp_ds, stats::quantile, probs = 0.3),
                                   sapply(df_XY_20, stats::quantile, probs = 0.3)))

  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              parameter = "quantile"),
               evaluate_parameters(sapply(imp_ds, quantile),
                                   sapply(df_XY_20, quantile)))

  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              parameter = "cov"),
               evaluate_parameters(cov(imp_ds), cov(df_XY_20)))

  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              parameter = "cor"),
               evaluate_parameters(cor(imp_ds), cor(df_XY_20)))

  # check parameter error
  expect_error(evaluate_imputation_parameters(imp_ds, df_XY_20,
                                              parameter = "asdf"),
               "'arg' should be one of ")



  # check conflict orig_ds, true_pars ---------------------

  # nothing supplied
  expect_error(evaluate_imputation_parameters(imp_ds),
               "exactly one of 'orig_ds' or 'true_pars' must be supplied")

  # both supplied
  expect_error(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              true_pars = c(10, 100)),
               "exactly one of 'orig_ds' or 'true_pars' must be supplied")


  # true_pars ---------------------------------------------
  expect_equal(evaluate_imputation_parameters(imp_ds, true_pars = c(10, 100)),
               evaluate_parameters(colMeans(imp_ds), c(10, 100)))

  # criterion ---------------------------------------------
  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              criterion = "MAE"),
               evaluate_parameters(colMeans(imp_ds), colMeans(df_XY_20),
                                   criterion = "MAE"))

  # which_pars --------------------------------------------
  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              which_pars = "Y"), 0)
  expect_equal(evaluate_imputation_parameters(imp_ds, orig_ds = df_XY_20,
                                              which_pars = "X"),
               evaluate_parameters(mean(imp_ds$X), mean(df_XY_20$X)))

})


# check make_col_fun --------------------------------------
test_that("make_col_fun() works", {
  colMeans2 <- make_col_fun(mean)
  expect_equal(colMeans2(df_XY_100), colMeans(df_XY_100))

  colSd <- make_col_fun(sd)
  expect_equal(colSd(df_XY_100), c(X = sd(df_XY_100$X), Y = sd(df_XY_100$Y)))

  colQuantile <- make_col_fun(quantile)
  expect_equal(colQuantile(df_XY_100, 0.2), c(X = quantile(df_XY_100$X, 0.2),
                                              Y = quantile(df_XY_100$Y, 0.2)))

  colTable <- make_col_fun(table)
  expect_equal(colTable(df_XY_2), c(X.1 = 1, X.2 = 1, Y.101 = 1, Y.102 = 1))

  colPropt <- make_col_fun(function(x) prop.table(table(x)))
  expect_equal(colPropt(df_XY_2), c(X.1 = 0.5, X.2 = 0.5, Y.101 = 0.5, Y.102 = 0.5))

})
