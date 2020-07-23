#' @eval document_LSimpute("combined")
#'
#' @details LSimpute_combined combines imputation values from [impute_LS_gene()]
#'   and [impute_LS_array()] using a global approach for the mixing coefficient
#'   *p*.
#'
#' @inheritParams impute_LS_array
#' @param p_mis_sim percentage of observed values that are set `NA` to estimate
#'   the mixing coefficient *p*. The default value (0.05) corresponds to the
#'   choice of Bo et al. (2004).
#'
impute_LS_combined <- function(ds, k = 10, eps = 1e-6, min_common_obs = 5, p_mis_sim = 0.05) {

  ## Define some variables ----------------------------------------------------
  ds_mat <- as.matrix(ds) # for subsetting by indices_new_mis a matrix is needed
  M <- is.na(ds_mat)
  ds_for_p <- ds_mat
  indices_observed <- which(!M)

  ## Estimate p ---------------------------------------------------------------
  # delete and estimate p_mis_sim of the known values to determine p (a mixing coefficient)
  indices_new_mis <- sample(indices_observed,
    size = round(length(indices_observed) * p_mis_sim, 0)
  )
  ds_for_p[indices_new_mis] <- NA
  # impute with impute_LS_gene and impute_LS_array to determine errors
  ds_for_p_imp_gene <- impute_LS_gene(ds_for_p,
    k = k, eps = eps,
    min_common_obs = min_common_obs
  )
  # Use ds_for_p_imp_gene for impute_LS_array to save some time
  ds_for_p_imp_array <- impute_LS_array(ds_for_p,
    k = k, eps = eps,
    min_common_obs = min_common_obs,
    ds_impute_LS_gene = ds_for_p_imp_gene
  )
  e_g <- ds_for_p_imp_gene[indices_new_mis] - ds_mat[indices_new_mis]
  e_a <- ds_for_p_imp_array[indices_new_mis] - ds_mat[indices_new_mis]
  # find "optimal" p
  e_c <- function(p, e_g, e_a) {
    sum(p^2 * e_g^2 + 2 * p * (1 - p) * e_g * e_a + (1 - p)^2 * e_a^2)
  }
  p <- stats::optimize(e_c, c(0, 1), e_a = e_a, e_g = e_g, maximum = FALSE)$minimum

  ## Calculate imputation values ----------------------------------------------
  # First: impute with impute_LS_gene and impute_LS_array
  y_g <- impute_LS_gene(ds, k = k, eps = eps, min_common_obs = min_common_obs)
  y_a <- impute_LS_array(ds,
    k = k, eps = eps, min_common_obs = min_common_obs,
    ds_impute_LS_gene = y_g
  )
  # Second: combine both results using p for mixing
  ds[M] <- p * y_g[M] + (1 - p) * y_a[M]
  return(ds)
}
