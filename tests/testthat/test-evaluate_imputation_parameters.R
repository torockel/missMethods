test_that("evaluate_imputation_parameters() works with data frames", {
  # check data frames ---------------------------------------
  orig_ds <- df_XY_20
  imp_ds <- orig_ds
  imp_ds[1:10, 1] <- 1
  expect_equal(
    evaluate_imputation_parameters(imp_ds, orig_ds = orig_ds),
    evaluate_parameters(colMeans(imp_ds), colMeans(orig_ds))
  )

  # check parameter ---------------------------------------
  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "mean"
    ),
    evaluate_parameters(colMeans(imp_ds), colMeans(orig_ds))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "median"
    ),
    evaluate_parameters(sapply(imp_ds, median), sapply(orig_ds, median))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "var"
    ),
    evaluate_parameters(diag(var(imp_ds)), diag(var(orig_ds)))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "sd"
    ),
    evaluate_parameters(sapply(imp_ds, sd), sapply(orig_ds, sd))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "quantile", probs = 0.3
    ),
    evaluate_parameters(
      sapply(imp_ds, stats::quantile, probs = 0.3),
      sapply(orig_ds, stats::quantile, probs = 0.3)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "quantile"
    ),
    evaluate_parameters(
      sapply(imp_ds, stats::quantile),
      sapply(orig_ds, stats::quantile)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "cov"
    ),
    evaluate_parameters(cov(imp_ds), cov(orig_ds))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "cor"
    ),
    evaluate_parameters(cor(imp_ds), cor(orig_ds))
  )

  # check parameter error
  expect_error(
    evaluate_imputation_parameters(imp_ds, orig_ds,
      parameter = "asdf"
    ),
    "'arg' should be one of "
  )

  # check conflict orig_ds, true_pars ---------------------

  # nothing supplied
  expect_error(
    evaluate_imputation_parameters(imp_ds),
    "exactly one of 'orig_ds' or 'true_pars' must be supplied"
  )

  # both supplied
  expect_error(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      true_pars = c(10, 100)
    ),
    "exactly one of 'orig_ds' or 'true_pars' must be supplied"
  )

  # true_pars ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(imp_ds, true_pars = c(10, 100)),
    evaluate_parameters(colMeans(imp_ds), c(10, 100))
  )

  # criterion ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      criterion = "MAE"
    ),
    evaluate_parameters(colMeans(imp_ds), colMeans(orig_ds),
      criterion = "MAE"
    )
  )

  # which_cols --------------------------------------------
  expect_equal(evaluate_imputation_parameters(imp_ds,
    orig_ds = orig_ds,
    which_cols = "Y"
  ), 0)
  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      which_cols = "X"
    ),
    evaluate_parameters(mean(imp_ds$X), mean(orig_ds$X))
  )

  # tolerance
  expect_equal(evaluate_imputation_parameters(imp_ds,
    orig_ds = orig_ds,
    criterion = "precision",
    tolerance = 11
  ), 1)
})

test_that("evaluate_imputation_parameters() works with matrices", {
  # check matrices -----------------------------------------
  orig_ds <- matrix_20_2
  imp_ds <- orig_ds
  imp_ds[1:10, 1] <- 1
  expect_equal(
    evaluate_imputation_parameters(imp_ds, orig_ds = orig_ds),
    evaluate_parameters(colMeans(imp_ds), colMeans(orig_ds))
  )

  # check parameter ---------------------------------------
  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "mean"
    ),
    evaluate_parameters(colMeans(imp_ds), colMeans(orig_ds))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "median"
    ),
    evaluate_parameters(apply(imp_ds, 2, median), apply(orig_ds, 2, median))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "var"
    ),
    evaluate_parameters(diag(var(imp_ds)), diag(var(orig_ds)))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "sd"
    ),
    evaluate_parameters(apply(imp_ds, 2, sd), apply(orig_ds, 2, sd))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "quantile", probs = 0.3
    ),
    evaluate_parameters(
      apply(imp_ds, 2, stats::quantile, probs = 0.3),
      apply(orig_ds, 2, stats::quantile, probs = 0.3)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "quantile"
    ),
    evaluate_parameters(
      apply(imp_ds, 2, stats::quantile),
      apply(orig_ds, 2, stats::quantile)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "cov"
    ),
    evaluate_parameters(cov(imp_ds), cov(orig_ds))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "cor"
    ),
    evaluate_parameters(cor(imp_ds), cor(orig_ds))
  )

  # check parameter error
  expect_error(
    evaluate_imputation_parameters(imp_ds, orig_ds,
      parameter = "asdf"
    ),
    "'arg' should be one of "
  )

  # check conflict orig_ds, true_pars ---------------------

  # nothing supplied
  expect_error(
    evaluate_imputation_parameters(imp_ds),
    "exactly one of 'orig_ds' or 'true_pars' must be supplied"
  )

  # both supplied
  expect_error(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      true_pars = c(10, 100)
    ),
    "exactly one of 'orig_ds' or 'true_pars' must be supplied"
  )

  # true_pars ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(imp_ds, true_pars = c(10, 100)),
    evaluate_parameters(colMeans(imp_ds), c(10, 100))
  )

  # criterion ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      criterion = "MAE"
    ),
    evaluate_parameters(colMeans(imp_ds), colMeans(orig_ds),
      criterion = "MAE"
    )
  )

  # which_cols --------------------------------------------
  expect_equal(evaluate_imputation_parameters(imp_ds,
    orig_ds = orig_ds,
    which_cols = 2
  ), 0)
  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      which_cols = 1
    ),
    evaluate_parameters(mean(imp_ds[, 1]), mean(orig_ds[, 1]))
  )

  # tolerance
  expect_equal(evaluate_imputation_parameters(imp_ds,
    orig_ds = orig_ds,
    criterion = "precision",
    tolerance = 11
  ), 1)
})

test_that("evaluate_imputation_parameters() works with tibbles", {
  # check tibbles ---------------------------------------
  orig_ds <- tbl_XY_20
  imp_ds <- orig_ds
  imp_ds[1:10, 1] <- 1
  expect_equal(
    evaluate_imputation_parameters(imp_ds, orig_ds = orig_ds),
    evaluate_parameters(colMeans(imp_ds), colMeans(orig_ds))
  )

  # check parameter ---------------------------------------
  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "mean"
    ),
    evaluate_parameters(colMeans(imp_ds), colMeans(orig_ds))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "median"
    ),
    evaluate_parameters(sapply(imp_ds, median), sapply(orig_ds, median))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "var"
    ),
    evaluate_parameters(diag(var(imp_ds)), diag(var(orig_ds)))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "sd"
    ),
    evaluate_parameters(sapply(imp_ds, sd), sapply(orig_ds, sd))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "quantile", probs = 0.3
    ),
    evaluate_parameters(
      sapply(imp_ds, stats::quantile, probs = 0.3),
      sapply(orig_ds, stats::quantile, probs = 0.3)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "quantile"
    ),
    evaluate_parameters(
      sapply(imp_ds, stats::quantile),
      sapply(orig_ds, stats::quantile)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "cov"
    ),
    evaluate_parameters(cov(imp_ds), cov(orig_ds))
  )

  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      parameter = "cor"
    ),
    evaluate_parameters(cor(imp_ds), cor(orig_ds))
  )

  # check parameter error
  expect_error(
    evaluate_imputation_parameters(imp_ds, orig_ds,
      parameter = "asdf"
    ),
    "'arg' should be one of "
  )

  # check conflict orig_ds, true_pars ---------------------

  # nothing supplied
  expect_error(
    evaluate_imputation_parameters(imp_ds),
    "exactly one of 'orig_ds' or 'true_pars' must be supplied"
  )

  # both supplied
  expect_error(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      true_pars = c(10, 100)
    ),
    "exactly one of 'orig_ds' or 'true_pars' must be supplied"
  )

  # true_pars ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(imp_ds, true_pars = c(10, 100)),
    evaluate_parameters(colMeans(imp_ds), c(10, 100))
  )

  # criterion ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      criterion = "MAE"
    ),
    evaluate_parameters(colMeans(imp_ds), colMeans(orig_ds),
      criterion = "MAE"
    )
  )

  # which_cols --------------------------------------------
  expect_equal(evaluate_imputation_parameters(imp_ds,
    orig_ds = orig_ds,
    which_cols = "Y"
  ), 0)
  expect_equal(
    evaluate_imputation_parameters(imp_ds,
      orig_ds = orig_ds,
      which_cols = "X"
    ),
    evaluate_parameters(mean(imp_ds$X), mean(orig_ds$X))
  )

  # tolerance
  expect_equal(evaluate_imputation_parameters(imp_ds,
    orig_ds = orig_ds,
    criterion = "precision",
    tolerance = 11
  ), 1)
})



# check make_col_fun --------------------------------------
test_that("make_col_fun() works", {
  colMeans2 <- make_col_fun(mean)
  expect_equal(colMeans2(df_XY_100), colMeans(df_XY_100))

  colSd <- make_col_fun(sd)
  expect_equal(colSd(df_XY_100), c(X = sd(df_XY_100$X), Y = sd(df_XY_100$Y)))

  colQuantile <- make_col_fun(stats::quantile)
  expect_equal(colQuantile(df_XY_100, 0.2), c(
    X = stats::quantile(df_XY_100$X, 0.2),
    Y = stats::quantile(df_XY_100$Y, 0.2)
  ))

  colTable <- make_col_fun(table)
  expect_equal(colTable(df_XY_2), c(X.1 = 1, X.2 = 1, Y.101 = 1, Y.102 = 1))

  colPropt <- make_col_fun(function(x) prop.table(table(x)))
  expect_equal(colPropt(df_XY_2), c(X.1 = 0.5, X.2 = 0.5, Y.101 = 0.5, Y.102 = 0.5))
})
