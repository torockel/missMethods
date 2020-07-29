#' @eval document_LSimpute("array")
#'
#' @details
#'
#' The mean vector and covariance matrix for the imputation in LSimpute_array is
#' based on a imputed dataset from LSimpute_gene. This dataset can be supplied
#' directly via `ds_impute_LS_gene` or will automatically be created with
#' [impute_LS_gene()] (if `ds_impute_LS_gene` is `NULL`). The imputation values
#' are the expected values given the estimated parameters and the observed
#' values. They are calculated via [impute_expected_values()]. The amount of
#' feedback from these two functions is controlled via `verbose_gene`  and
#' `verbose_expected_values`. The values of these two arguments are passed on to
#' the argument `verbose` from `impute_LS_gene()` and
#' `impute_expected_values()`.
#'
#'
#' @param k Directly passed to [impute_LS_gene()].
#' @param eps Directly passed to [impute_LS_gene()].
#' @param min_common_obs Directly passed to [impute_LS_gene()].
#' @param ds_impute_LS_gene Result of imputing `ds` with `ds_impute_LS_gene()`,
#'   if this already exists (see details).
#' @param verbose_gene Should `impute_LS_gene()` be `verbose`?
#' @param verbose_expected_values Should `impute_expected_values()` be `verbose`?
#'
impute_LS_array <- function(ds, k = 10, eps = 1e-6, min_common_obs = 5,
                            ds_impute_LS_gene = NULL,
                            verbose_gene = FALSE,
                            verbose_expected_values = FALSE) {

  ## Estimate the parameters via the imputed dataset from impute_LS_gene() ----
  if (is.null(ds_impute_LS_gene)) { # dataset not given -> impute via impute_LS_gene()
    ds_impute_LS_gene <- impute_LS_gene(ds,
      k = k, eps = eps,
      min_common_obs = min_common_obs,
      verbose = verbose_gene
    )
  }
  # Bo et al. (2004) use the empirical covariance matrix (divisor: n -1),
  # stats::cov() uses n as divisor
  S <- stats::cov(ds_impute_LS_gene) * nrow(ds_impute_LS_gene) /
    (nrow(ds_impute_LS_gene) - 1)
  col_means <- colMeans(ds_impute_LS_gene)
  # After parameter estimation the LS_gene imputed dataset is not needed anymore
  remove(ds_impute_LS_gene)

  ## Impute the missing values ------------------------------------------------
  # Impute least squares estimates of the missing values, given S and col_means
  impute_expected_values(ds,
    mu = col_means, S = S,
    stochastic = FALSE,
    verbose = verbose_expected_values
  )
}
