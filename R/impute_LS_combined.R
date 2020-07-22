#' LSimpute_combined
#'
#'
#' Perform LSimpute_combined as described by Bo et al. (2004)
#'
#' @template impute
#'
#' @details This function performs LSimpute_combined as described by Bo et al.
#'   (2004). The function assumes that the genes are the rows of `ds`.
#'
#' @inheritParams impute_LS_array
#'
#' @seealso [impute_LS_gene()] and [impute_LS_array()], which imputation values are combined
#'
#' @references Bo, T. H., Dysvik, B., & Jonassen, I. (2004). LSimpute: accurate
#'   estimation of missing values in microarray data with least squares methods.
#'   Nucleic acids research, 32(3), e34
#'
#' @export
#'
#' @examples
#' impute_LS_combined(data.frame(X = 1:11, Y = c(1:10, NA)))
impute_LS_combined <- function(ds, k = 10, eps = 1e-6, min_common_obs = 5) {

  ## Estimate p ---------------------------------------------------------------
  # delete and estimate 5% of the known values to determine p (a mixing coefficient)
  ds_mat <- as.matrix(ds) # for subsetting by indices_new_mis a matrix is needed
  ds_for_p <- ds_mat
  M <- is.na(ds_for_p)
  indices_observed <- which(!M)
  indices_new_mis <- sample(indices_observed,
                            size = round(length(indices_observed)* 0.05), 0)
  ds_for_p[indices_new_mis] <- NA
  # impute with impute_LS_gene and impute_LS_array to determine errors
  ds_for_p_imp_gene <- impute_LS_gene(ds_for_p, k = k, eps = eps,
                                      min_common_obs = min_common_obs)
  # Use ds_for_p_imp_gene for impute_LS_array to save some time
  ds_for_p_imp_array <-  impute_LS_array(ds_for_p, k = k, eps = eps,
                                         min_common_obs = min_common_obs,
                                         ds_impute_LS_gene = ds_for_p_imp_gene)
  e_g <- ds_for_p_imp_gene[indices_new_mis] - ds_mat[indices_new_mis]
  e_a <- ds_for_p_imp_array[indices_new_mis] - ds_mat[indices_new_mis]
  # find "optimal" p
  e_c <- function(p, e_g, e_a) {
    sum(p^2 * e_g^2 + 2*p*(1-p)*e_g*e_a + (1-p)^2 * e_a^2)
  }
  p <- stats::optimize(e_c, c(0, 1), e_a = e_a, e_g = e_g, maximum = FALSE)$minimum

  ## Calculate imputation values ----------------------------------------------
  # First: impute with impute_LS_gene and impute_LS_array
  y_g <- impute_LS_gene(ds, k = k, eps = eps, min_common_obs = min_common_obs)
  y_a <- impute_LS_array(ds, k = k, eps = eps, min_common_obs = min_common_obs,
                         ds_impute_LS_gene = y_g)
  # Second: combine both results using p for mixing
  ds[M] <- p * y_g[M] + (1-p) * y_a[M]
  return(ds)
}
