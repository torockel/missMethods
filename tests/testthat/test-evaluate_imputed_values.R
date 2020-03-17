# most of the testing is done in the tests of calc_evaluation_criterion()
test_that("evaluate_imputed_values()", {
  df_XY_20_imp <- df_XY_20
  df_XY_20_imp[1, 1] <- 2
  expect_equal(
    evaluate_imputed_values(df_XY_20_imp,
      df_XY_20,
      criterion = "MAE"
    ),
    1 / 40
  )
  expect_error(
    evaluate_imputed_values(df_XY_20_imp, df_XY_100),
    "the dimensions of imp_ds and orig_ds must be equal"
  )
})
