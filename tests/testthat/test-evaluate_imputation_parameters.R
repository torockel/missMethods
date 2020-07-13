test_that("evaluate_imputation_parameters() works with data frames", {
  # check data frames ---------------------------------------
  ds_orig <- df_XY_20
  ds_imp <- ds_orig
  ds_imp[1:10, 1] <- 1
  expect_equal(
    evaluate_imputation_parameters(ds_imp, ds_orig = ds_orig),
    evaluate_parameters(colMeans(ds_imp), colMeans(ds_orig))
  )

  # check parameter ---------------------------------------
  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "mean"
    ),
    evaluate_parameters(colMeans(ds_imp), colMeans(ds_orig))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "median"
    ),
    evaluate_parameters(sapply(ds_imp, median), sapply(ds_orig, median))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "var"
    ),
    evaluate_parameters(diag(var(ds_imp)), diag(var(ds_orig)))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "sd"
    ),
    evaluate_parameters(sapply(ds_imp, sd), sapply(ds_orig, sd))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "quantile", probs = 0.3
    ),
    evaluate_parameters(
      sapply(ds_imp, stats::quantile, probs = 0.3),
      sapply(ds_orig, stats::quantile, probs = 0.3)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "quantile"
    ),
    evaluate_parameters(
      sapply(ds_imp, stats::quantile),
      sapply(ds_orig, stats::quantile)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "cov"
    ),
    evaluate_parameters(cov(ds_imp), cov(ds_orig))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "cor"
    ),
    evaluate_parameters(cor(ds_imp), cor(ds_orig))
  )

  # check parameter error
  expect_error(
    evaluate_imputation_parameters(ds_imp, ds_orig,
      parameter = "asdf"
    ),
    "'arg' should be one of "
  )

  # check conflict ds_orig, pars_true ---------------------

  # nothing supplied
  expect_error(
    evaluate_imputation_parameters(ds_imp),
    "exactly one of 'ds_orig' or 'pars_true' must be supplied"
  )

  # both supplied
  expect_error(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      pars_true = c(10, 100)
    ),
    "exactly one of 'ds_orig' or 'pars_true' must be supplied"
  )

  # pars_true ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(ds_imp, pars_true = c(10, 100)),
    evaluate_parameters(colMeans(ds_imp), c(10, 100))
  )

  # criterion ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      criterion = "MAE"
    ),
    evaluate_parameters(colMeans(ds_imp), colMeans(ds_orig),
      criterion = "MAE"
    )
  )

  # cols_which --------------------------------------------
  expect_equal(evaluate_imputation_parameters(ds_imp,
    ds_orig = ds_orig,
    cols_which = "Y"
  ), 0)
  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      cols_which = "X"
    ),
    evaluate_parameters(mean(ds_imp$X), mean(ds_orig$X))
  )

  # tolerance
  expect_equal(evaluate_imputation_parameters(ds_imp,
    ds_orig = ds_orig,
    criterion = "precision",
    tolerance = 11
  ), 1)
})

test_that("evaluate_imputation_parameters() works with matrices", {
  # check matrices -----------------------------------------
  ds_orig <- matrix_20_2
  ds_imp <- ds_orig
  ds_imp[1:10, 1] <- 1
  expect_equal(
    evaluate_imputation_parameters(ds_imp, ds_orig = ds_orig),
    evaluate_parameters(colMeans(ds_imp), colMeans(ds_orig))
  )

  # check parameter ---------------------------------------
  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "mean"
    ),
    evaluate_parameters(colMeans(ds_imp), colMeans(ds_orig))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "median"
    ),
    evaluate_parameters(apply(ds_imp, 2, median), apply(ds_orig, 2, median))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "var"
    ),
    evaluate_parameters(diag(var(ds_imp)), diag(var(ds_orig)))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "sd"
    ),
    evaluate_parameters(apply(ds_imp, 2, sd), apply(ds_orig, 2, sd))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "quantile", probs = 0.3
    ),
    evaluate_parameters(
      apply(ds_imp, 2, stats::quantile, probs = 0.3),
      apply(ds_orig, 2, stats::quantile, probs = 0.3)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "quantile"
    ),
    evaluate_parameters(
      apply(ds_imp, 2, stats::quantile),
      apply(ds_orig, 2, stats::quantile)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "cov"
    ),
    evaluate_parameters(cov(ds_imp), cov(ds_orig))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "cor"
    ),
    evaluate_parameters(cor(ds_imp), cor(ds_orig))
  )

  # check parameter error
  expect_error(
    evaluate_imputation_parameters(ds_imp, ds_orig,
      parameter = "asdf"
    ),
    "'arg' should be one of "
  )

  # check conflict ds_orig, pars_true ---------------------

  # nothing supplied
  expect_error(
    evaluate_imputation_parameters(ds_imp),
    "exactly one of 'ds_orig' or 'pars_true' must be supplied"
  )

  # both supplied
  expect_error(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      pars_true = c(10, 100)
    ),
    "exactly one of 'ds_orig' or 'pars_true' must be supplied"
  )

  # pars_true ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(ds_imp, pars_true = c(10, 100)),
    evaluate_parameters(colMeans(ds_imp), c(10, 100))
  )

  # criterion ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      criterion = "MAE"
    ),
    evaluate_parameters(colMeans(ds_imp), colMeans(ds_orig),
      criterion = "MAE"
    )
  )

  # cols_which --------------------------------------------
  expect_equal(evaluate_imputation_parameters(ds_imp,
    ds_orig = ds_orig,
    cols_which = 2
  ), 0)
  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      cols_which = 1
    ),
    evaluate_parameters(mean(ds_imp[, 1]), mean(ds_orig[, 1]))
  )

  # tolerance
  expect_equal(evaluate_imputation_parameters(ds_imp,
    ds_orig = ds_orig,
    criterion = "precision",
    tolerance = 11
  ), 1)
})

test_that("evaluate_imputation_parameters() works with tibbles", {
  # check tibbles ---------------------------------------
  ds_orig <- tbl_XY_20
  ds_imp <- ds_orig
  ds_imp[1:10, 1] <- 1
  expect_equal(
    evaluate_imputation_parameters(ds_imp, ds_orig = ds_orig),
    evaluate_parameters(colMeans(ds_imp), colMeans(ds_orig))
  )

  # check parameter ---------------------------------------
  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "mean"
    ),
    evaluate_parameters(colMeans(ds_imp), colMeans(ds_orig))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "median"
    ),
    evaluate_parameters(sapply(ds_imp, median), sapply(ds_orig, median))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "var"
    ),
    evaluate_parameters(diag(var(ds_imp)), diag(var(ds_orig)))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "sd"
    ),
    evaluate_parameters(sapply(ds_imp, sd), sapply(ds_orig, sd))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "quantile", probs = 0.3
    ),
    evaluate_parameters(
      sapply(ds_imp, stats::quantile, probs = 0.3),
      sapply(ds_orig, stats::quantile, probs = 0.3)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "quantile"
    ),
    evaluate_parameters(
      sapply(ds_imp, stats::quantile),
      sapply(ds_orig, stats::quantile)
    )
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "cov"
    ),
    evaluate_parameters(cov(ds_imp), cov(ds_orig))
  )

  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      parameter = "cor"
    ),
    evaluate_parameters(cor(ds_imp), cor(ds_orig))
  )

  # check parameter error
  expect_error(
    evaluate_imputation_parameters(ds_imp, ds_orig,
      parameter = "asdf"
    ),
    "'arg' should be one of "
  )

  # check conflict ds_orig, pars_true ---------------------

  # nothing supplied
  expect_error(
    evaluate_imputation_parameters(ds_imp),
    "exactly one of 'ds_orig' or 'pars_true' must be supplied"
  )

  # both supplied
  expect_error(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      pars_true = c(10, 100)
    ),
    "exactly one of 'ds_orig' or 'pars_true' must be supplied"
  )

  # pars_true ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(ds_imp, pars_true = c(10, 100)),
    evaluate_parameters(colMeans(ds_imp), c(10, 100))
  )

  # criterion ---------------------------------------------
  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      criterion = "MAE"
    ),
    evaluate_parameters(colMeans(ds_imp), colMeans(ds_orig),
      criterion = "MAE"
    )
  )

  # cols_which --------------------------------------------
  expect_equal(evaluate_imputation_parameters(ds_imp,
    ds_orig = ds_orig,
    cols_which = "Y"
  ), 0)
  expect_equal(
    evaluate_imputation_parameters(ds_imp,
      ds_orig = ds_orig,
      cols_which = "X"
    ),
    evaluate_parameters(mean(ds_imp$X), mean(ds_orig$X))
  )

  # tolerance
  expect_equal(evaluate_imputation_parameters(ds_imp,
    ds_orig = ds_orig,
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
