## assign_imputed_values ------------------------------------------------------

test_that("assign_imputed_values() assigns the correct values", {
  ds_imp <- matrix(c(1:30), ncol = 3)
  colnames(ds_imp) <- c("X", "Y", "Z")
  ds <- ds_imp
  ds[c(1, 5, 14, 22)] <- NA

  expect_equal(
    assign_imputed_values(ds, ds_imp),
    ds_imp
  )

  expect_equal(
    assign_imputed_values(as.data.frame(ds), ds_imp),
    as.data.frame(ds_imp)
  )

  expect_equal(
    assign_imputed_values(tibble::as_tibble(ds), ds_imp),
    tibble::as_tibble(ds_imp)
  )
})

## is_df_or_matrix ------------------------------------------------------------
test_that("is_df_or_matrix()", {
  expect_true(is_df_or_matrix(data.frame(X = 1)))
  expect_true(is_df_or_matrix(matrix(1:4, ncol = 2)))
  expect_false(is_df_or_matrix(c(1, 4)))
})


## resample -------------------------------------------------------------------
test_that("resample() works", {
  # beware of sample() this!
  expect_equal(replicate(10, resample(30, 1)), rep(30, 10))
  expect_error(
    resample(10, 2),
    "resampling of size 2 not possible without replacement"
  )
})


## check_for_packages ---------------------------------------------------------
test_that("check_for_packages() works", {
  expect_true(check_for_packages("stats"))
  expect_error(check_for_packages(c("not_exist_pkg_xul1", "not_exist_pkg_xul2")),
    "The following package(s) are needed, but not installed: not_exist_pkg_xul1, not_exist_pkg_xul2.",
    fixed = TRUE
  )
})


## check_renamed_arg -----------------------------------------------------------
test_that("check_renamed_arg() works", {
  f_two_args <- function(new_arg, old_arg) {
    check_renamed_arg(old_arg, new_arg)
    new_arg
  }
  # Only old arg is used
  expect_equal(
    expect_warning(
      f_two_args(old_arg = 3),
      "old_arg is deprecated; use new_arg instead."
    ),
    3
  )

  # Only new arg is used
  expect_equal(
    expect_silent(
      f_two_args(42)
    ),
    42
  )

  # Both args are used
  expect_error(
    f_two_args(42, 3),
    "old_arg is deprecated and replaced by new_arg. Please supply"
  )

  # No arg is used
  expect_error(
    f_two_args(),
    "argument \"new_arg\" is missing, with no default"
  )
})
