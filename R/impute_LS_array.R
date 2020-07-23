#' @eval document_LSimpute("array")
#'
#' @details The covariance matrix for the imputation in LSimpute_array is based
#'   on a imputed dataset from LSimpute_gene. This dataset can be supplied
#'   directly via `ds_impute_LS_gene` or will automatically be created with
#'   [impute_LS_gene()] (if `ds_impute_LS_gene` is `NULL`).
#'
#'
#' @param k directly passed to [impute_LS_gene()]
#' @param eps directly passed to [impute_LS_gene()]
#' @param min_common_obs directly passed to [impute_LS_gene()]
#' @param ds_impute_LS_gene result of imputing `ds` with `ds_impute_LS_gene()`,
#'   if this already exists
#'
impute_LS_array <- function(ds, k = 10, eps = 1e-6, min_common_obs = 5, ds_impute_LS_gene = NULL) {

  ## Estimate the parameters via the imputed dataset from impute_LS_gene() ----
  if (is.null(ds_impute_LS_gene)) { # dataset not given -> impute via impute_LS_gene()
    ds_impute_LS_gene <- impute_LS_gene(ds, k = k, eps = eps, min_common_obs = min_common_obs)
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
    warn_problematic_rows = FALSE
  )
}
