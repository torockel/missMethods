## Test for numeric datasets --------------------------------------------------
test_numeric_datasets <- function(fun_imp, ...,
                                  n = 150, m = 10,
                                  p_MCAR = 0.2, cols_mis = 3:5,
                                  seed = 123,
                                  check_tibble = TRUE) {
  fun_imp <- match.fun(fun_imp)
  set.seed(seed)

  ## Create datasets ----------------------------------------------------------
  ds_matrix <- mvtnorm::rmvnorm(n = n, rep(0, m))
  colnames(ds_matrix) <- paste0("X", seq_len(m))
  ds_matrix <- delete_MCAR(ds_matrix, p = p_MCAR, cols_mis = cols_mis)
  ds_data_frame <- as.data.frame(ds_matrix)
  ds_tibble <- tibble::as_tibble(ds_matrix)

  ## Check imputation function ------------------------------------------------
  expect_false(anyNA(fun_imp(ds_matrix, ...)))
  expect_false(anyNA(fun_imp(ds_data_frame, ...)))
  if (check_tibble) {
    expect_false(anyNA(fun_imp(ds_tibble, ...)))
  }

}

# Test all imputation functions, which are suitable for numeric datasets
# Tests are listed alphabetically
test_that("impute_EM() works with numeric datasets", {
  test_numeric_datasets(impute_EM)
})

test_that("impute_hot_deck_in_classes() works with numeric datasets", {
  test_numeric_datasets(impute_hot_deck_in_classes, cols_class = 1, breaks = 3)
})

test_that("impute_LS_adaptive() works with numeric datasets", {
  test_numeric_datasets(impute_LS_adaptive, warn_r_max = FALSE)
})

test_that("impute_LS_array() works with numeric datasets", {
  test_numeric_datasets(impute_LS_array)
})

test_that("impute_LS_combined() works with numeric datasets", {
  test_numeric_datasets(impute_LS_combined)
})

test_that("impute_LS_gene() works with numeric datasets", {
  test_numeric_datasets(impute_LS_gene)
})

test_that("impute_mean() works with numeric datasets", {
  test_numeric_datasets(impute_mean)
})

test_that("impute_median() works with numeric datasets", {
  test_numeric_datasets(impute_median)
})

test_that("impute_mode() works with numeric datasets", {
  test_numeric_datasets(impute_mode)
})

test_that("impute_sRHD() works with numeric datasets", {
  test_numeric_datasets(impute_sRHD)
})
