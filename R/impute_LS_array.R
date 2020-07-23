#' @eval document_LSimpute("array")
#'
#' @details
#'
#' The mean vector and covariance matrix for the imputation in LSimpute_array is
#' based on a imputed dataset from LSimpute_gene. This dataset can be supplied
#' directly via `ds_impute_LS_gene` or will automatically be created with
#' [impute_LS_gene()] (if `ds_impute_LS_gene` is `NULL`). The imputation values
#' are the expected values given the estimated parameters and the observed
#' values. They are calculated via [impute_expected_values()].
#'
#' Depending on `verbosity` the function will warn for some special cases.
#' `verbosity` controls the setting of `warn_special_cases` from
#' `impute_LS_gene()` and `warn_problematic_rows` from
#' `impute_expected_values()`:
#' * `verbosity = 0`: no warnings for special cases in `impute_LS_gene()` or
#'   `impute_expected_values()`
#' * `verbosity = 1`: warnings from `impute_LS_gene()`, no warnings from
#'   `impute_expected_values()`
#' * `verbosity = 2`: no warnings from `impute_LS_gene()`, warnings from
#'   `impute_expected_values()`
#' * `verbosity >= 3`: warnings from `impute_LS_gene()` and from
#'   `impute_expected_values()`
#'
#' @param k directly passed to [impute_LS_gene()]
#' @param eps directly passed to [impute_LS_gene()]
#' @param min_common_obs directly passed to [impute_LS_gene()]
#' @param ds_impute_LS_gene result of imputing `ds` with `ds_impute_LS_gene()`,
#'   if this already exists
#' @param verbosity controls the amount of feedback given by the function (see
#'   details)
#'
impute_LS_array <- function(ds, k = 10, eps = 1e-6, min_common_obs = 5, ds_impute_LS_gene = NULL,
                            verbosity = 0L) {

  ## Estimate the parameters via the imputed dataset from impute_LS_gene() ----
  if (is.null(ds_impute_LS_gene)) { # dataset not given -> impute via impute_LS_gene()
    ds_impute_LS_gene <- impute_LS_gene(ds, k = k, eps = eps,
                                        min_common_obs = min_common_obs,
                                        warn_special_cases = ifelse(verbosity == 1L || verbosity >= 3L, TRUE, FALSE))
  }
  # Bo et al. (2004) use the empirical covariance matrix (divisor: n -1), stats::cov() uses n as divisor
  S <- stats::cov(ds_impute_LS_gene) * nrow(ds_impute_LS_gene) / (nrow(ds_impute_LS_gene) - 1)
  col_means <- colMeans(ds_impute_LS_gene)
  # The LS_gene imputed dataset is not needed anymore after the parameters are estimated
  remove(ds_impute_LS_gene)

  ## Impute the missing values ------------------------------------------------
  # Impute least squares estimates of the missing values, given S and col_means
  impute_expected_values(ds,
    mu = col_means, S = S,
    stochastic = FALSE,
    warn_problematic_rows = ifelse(verbosity >= 2L, TRUE, FALSE)
  )
}
