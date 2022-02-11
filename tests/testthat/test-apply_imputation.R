# apply_imputation() -----------------------------
# most of the time apply_imputation() is called via impute_mean()
test_that("apply_imputation() works with data.frames", {
  # errors
  expect_error(
    apply_imputation(c(1, 2), FUN = mean),
    "ds must be a data frame or a matrix"
  )
  expect_error(
    apply_imputation(df_XY_X_mis, FUN = "asdf"),
    "object 'asdf' of mode 'function' was not found"
  )
  expect_error(
    apply_imputation(df_XY_X_mis, FUN = sum()),
    "'sum\\()' is not a function, character or symbol"
  )
  expect_error(
    apply_imputation(df_XY_X_mis, FUN = mean, type = "notImplementendType"),
    "'arg' should be one of"
  )

  # mean is default for FUN:
  df_XY_XY_mean_imp <- apply_imputation(df_XY_XY_mis)
  expect_false(anyNA(df_XY_XY_mean_imp))
  expect_equal(
    colMeans(df_XY_XY_mean_imp),
    colMeans(df_XY_XY_mis, na.rm = TRUE)
  )

  expect_false(anyNA(apply_imputation(df_XY_mis_with_comp_chars,
    FUN = mean
  )))

  # check special cases for columnwise --------------------
  expect_warning(
    impute_mean(df_one_comp_missing_col),
    "in column 2 all values are NA; the column cannot be imputed"
  )
  expect_false(anyNA(impute_mean(df_first_row_comp_missing)))
  expect_false(anyNA(impute_mean(df_no_comp_obs)))
  expect_warning(
    impute_mean(df_all_NA),
    "all values in ds are NA, no imputation possible"
  )


  # check rowwise -----------------------------------------
  expect_equal(
    impute_mean(df_XY_X_mis,
      type = "rowwise"
    )[is.na(df_XY_X_mis), "X"],
    df_XY_X_mis[is.na(df_XY_X_mis), "Y"]
  )

  # check special cases for rowwise -----------------------
  expect_warning(
    impute_mean(df_one_comp_missing_col, type = "rowwise"),
    "in row .* all values are NA; the row cannot be imputed"
  )
  expect_warning(
    impute_mean(df_first_row_comp_missing, type = "rowwise"),
    "in row 1 all values are NA; the row cannot be imputed"
  )
  expect_false(anyNA(impute_mean(df_no_comp_obs[-c(11), ],
    type = "rowwise"
  )))
  expect_warning(
    impute_mean(df_no_comp_obs, type = "rowwise"),
    "in row 11 all values are NA; the row cannot be imputed"
  )
  expect_warning(
    impute_mean(df_all_NA, type = "rowwise"),
    "all values in ds are NA, no imputation possible"
  )

  # check total -------------------------------------------
  expect_equal(
    impute_mean(df_XY_XY_mis, type = "total")[is.na(df_XY_XY_mis)],
    rep(
      mean(unlist(df_XY_XY_mis), na.rm = TRUE),
      sum(is.na(df_XY_XY_mis))
    )
  )

  # check special cases for total -------------------------
  expect_false(anyNA(impute_mean(df_one_comp_missing_col,
    type = "total"
  )))
  expect_false(anyNA(impute_mean(df_first_row_comp_missing,
    type = "total"
  )))
  expect_false(anyNA(impute_mean(df_no_comp_obs, type = "total")))
  expect_warning(
    impute_mean(df_all_NA, type = "total"),
    "all values in ds are NA, no imputation possible"
  )

  # check Two-Way -----------------------------------------
  expect_equal(
    suppressWarnings(impute_mean(df_XY_XY_mis,
      type = "Two-Way"
    ))[1, 1],
    mean(df_XY_XY_mis[, 1], na.rm = TRUE) + df_XY_XY_mis[1, 2] -
      mean(unlist(df_XY_XY_mis), na.rm = TRUE)
  )

  expect_equal(
    suppressWarnings(impute_mean(df_XY_XY_mis,
      type = "Two-Way"
    ))[4, 2],
    mean(df_XY_XY_mis[, 2], na.rm = TRUE) + df_XY_XY_mis[4, 1] -
      mean(unlist(df_XY_XY_mis), na.rm = TRUE)
  )

  # check special cases for Two-Way -----------------------
  expect_warning(
    impute_mean(df_one_comp_missing_col, type = "Two-Way"),
    "all values are NA; the [rowcolumn]"
  )
  expect_warning(
    impute_mean(df_first_row_comp_missing, type = "Two-Way"),
    "in row 1 all values are NA; the row cannot be imputed"
  )
  expect_false(anyNA(impute_mean(df_no_comp_obs[-11, ], type = "Two-Way")))
  expect_warning(
    impute_mean(df_no_comp_obs, type = "Two-Way"),
    "in row 11 all values are NA; the row cannot be imputed"
  )
  expect_warning(
    impute_mean(df_all_NA, type = "Two-Way"),
    "all values in ds are NA, no imputation possible"
  )

  # check Winer -------------------------------------------
  expect_equal(
    suppressWarnings(impute_mean(df_XY_XY_mis,
      type = "Winer"
    ))[1, 1],
    (mean(df_XY_XY_mis[, 1], na.rm = TRUE) + df_XY_XY_mis[1, 2]) / 2
  )


  expect_equal(
    suppressWarnings(impute_mean(df_XY_XY_mis,
      type = "Winer"
    ))[4, 2],
    (mean(df_XY_XY_mis[, 2], na.rm = TRUE) + df_XY_XY_mis[4, 1]) / 2
  )

  # check special cases for Two-Way -----------------------
  expect_warning(
    impute_mean(df_one_comp_missing_col, type = "Winer"),
    "all values are NA; the [rowcolumn]"
  )
  expect_warning(
    impute_mean(df_first_row_comp_missing, type = "Winer"),
    "in row 1 all values are NA; the row cannot be imputed"
  )
  expect_false(anyNA(impute_mean(df_no_comp_obs[-11, ], type = "Winer")))
  expect_warning(
    impute_mean(df_no_comp_obs, type = "Winer"),
    "in row 11 all values are NA; the row cannot be imputed"
  )
  expect_warning(
    impute_mean(df_all_NA, type = "Winer"),
    "all values in ds are NA, no imputation possible"
  )
})

test_that("apply_imputation() works with matrices", {
  # check types
  expect_false(anyNA(impute_mean(matrix_100_2_mis, type = "columnwise")))
  expect_false(anyNA(impute_mean(matrix_100_2_mis[-c(5, 30:40), ],
    type = "rowwise"
  )))
  expect_false(anyNA(impute_mean(matrix_100_2_mis, type = "total")))
  expect_false(anyNA(impute_mean(matrix_100_2_mis[-c(5, 30:40), ],
    type = "Two-Way"
  )))
  expect_false(anyNA(impute_mean(matrix_100_2_mis[-c(5, 30:40), ],
    type = "Winer"
  )))
})

test_that("apply_imputation() works with tibbles", {
  # apply_imputation is rather tricky with tibbles
  # before tibble version 3.0.0 subsetting with logical matrices was not supported,
  # but integer columns were converted to doubles without even a warning, if needed.
  # Now with tibble version >= 3.0.0 subsetting with logical matrices work,
  # but now integer columns throw an error, if a double is imputed..
  # So, testing highly depends on the used version of tibbles.

  # If columns are first converted to doubles, all versions of tibble should
  # work with the types "columnwise", "rowwise" and "Winer"

  tbl_XY_XY_mis_dbl <- tbl_XY_XY_mis
  tbl_XY_XY_mis_dbl$X <- as.double(tbl_XY_XY_mis_dbl$X)
  tbl_XY_XY_mis_dbl$Y <- as.double(tbl_XY_XY_mis_dbl$Y)

  expect_false(anyNA(impute_mean(tbl_XY_XY_mis_dbl, type = "columnwise")))
  expect_false(anyNA(impute_mean(tbl_XY_XY_mis_dbl[-c(5, 30:40), ],
    type = "rowwise"
  )))
  expect_false(anyNA(impute_mean(tbl_XY_XY_mis_dbl[-c(5, 30:40), ],
    type = "Winer"
  )))

  # Furthermore, if tibble version >= these solution should also work for the
  # tpyes "total" and "Two-Way":
  if (utils::packageVersion("tibble") >= package_version("3.0.0")) {
    expect_false(anyNA(impute_mean(tbl_XY_XY_mis_dbl, type = "total")))
    expect_false(anyNA(impute_mean(tbl_XY_XY_mis_dbl[-c(5, 30:40), ],
      type = "Two-Way"
    )))
  }

  # Check that a meaningfull error is thrown for the types "total" and "Two-Way",
  # if version of tibble < 2.99.99.9012
  # https://github.com/tidyverse/tibble/releases/tag/v2.99.99.9012

  if (utils::packageVersion("tibble") < package_version("2.99.99.9012")) {
    expect_error(
      impute_mean(tbl_XY_XY_mis, type = "total"),
      "ds is a tibble and logical subsetting, which is needed for"
    )
    expect_error(
      impute_mean(tbl_XY_XY_mis[-c(5, 30:40), ], type = "Two-Way"),
      "ds is a tibble and logical subsetting, which is needed for"
    )
  }

  # All other tests and their possible errors would highly depend on the
  # version of tibble and are omitted.
})

# tests for mixed data sets -----------------------------------------

test_that("apply_imputation() works with mixed data frames", {
  df_imp <- impute_mode(df_mixed_mis, type = "columnwise")
  expect_false(anyNA(df_imp))
  expect_equal(
    sapply(df_imp, class),
    sapply(df_mixed, class)
  )
})

test_that("apply_imputation() works with mixed tibbles", {
  df_imp <- impute_mode(df_mixed_mis, type = "columnwise")
  expect_false(anyNA(df_imp))
  expect_equal(
    sapply(df_imp, class),
    sapply(df_mixed, class)
  )
})


# mean imputation -----------------------------------------
# most of the general checking is done in apply_imputation(),
# which is called by impute_mean().
test_that("impute_mean()", {
  df_XY_XY_mean_imp <- impute_mean(df_XY_XY_mis)
  expect_false(anyNA(df_XY_XY_mean_imp))
  expect_equal(
    colMeans(df_XY_XY_mean_imp),
    colMeans(df_XY_XY_mis, na.rm = TRUE)
  )

  expect_false(anyNA(impute_mean(df_XY_mis_with_comp_chars)))
  # check type
  expect_false(anyNA(impute_mean(df_XY_X_mis, type = "rowwise")))
})


# median imputation ---------------------------------------
# most of the general checking is done in apply_imputation(),
# which is called by impute_median().
test_that("impute_median()", {
  df_XY_XY_median_imp <- impute_median(df_XY_XY_mis)
  expect_false(anyNA(df_XY_XY_median_imp))
  expect_equal(
    sapply(df_XY_XY_median_imp, median),
    sapply(df_XY_XY_median_imp, median, na.rm = TRUE)
  )
  df_ordered_imp <- impute_median(df_ordered_mis)
  expect_false(anyNA(df_ordered_imp))
  expect_equal(
    sapply(df_ordered_imp, median),
    sapply(df_ordered_mis, median, na.rm = TRUE)
  )

  expect_false(anyNA(impute_median(df_XY_mis_with_comp_chars)))
  expect_false(isTRUE(all.equal(
    impute_median(df_with_ord_factors_mis,
      ordered_low = FALSE
    ),
    impute_median(df_with_ord_factors_mis,
      ordered_low = TRUE
    )
  )))

  # check type
  expect_false(anyNA(impute_median(df_XY_X_mis, type = "rowwise")))
})

# mode imputation -----------------------------------------
# most of the general checking is done in apply_imputation(),
# which is called by impute_mode().
test_that("impute_mode()", {
  df_XY_XY_mode_imp <- impute_mode(df_XY_XY_mis)
  expect_false(anyNA(df_XY_XY_mode_imp))

  # check type
  expect_false(anyNA(impute_mode(df_XY_X_mis, type = "rowwise")))
})
