#' @eval document_LSimpute("combined")
#'
#' @details
#'
#' LSimpute_combined combines imputation values from [impute_LS_gene()] and
#' [impute_LS_array()] using a global approach for the mixing coefficient *p*.
#' The amount of feedback given from this underlying functions is controlled
#' via `verbosity`:
#' * `verbosity = 0`: No feedback from `impute_LS_gene()` (`warn_special_cases = FALSE`)
#'   and `impute_LS_array()`
#'  * `verbosity = 1`: Feedback from `impute_LS_gene()` (`warn_special_cases = TRUE`)
#'    and no feedback `impute_LS_array()`
#' * `verbosity = 2`: No feedback from `impute_LS_gene()` (`warn_special_cases = FALSE`)
#'   but all feedback from `impute_LS_array()`
#' * `verbosity = 3`: Feedback from `impute_LS_gene()` (`warn_special_cases = TRUE`)
#'   and `impute_LS_array()`
#'
#' Internally,  the imputed dataset from `impute_LS_gene()` is passed on to
#' `impute_LS_array()`. Therefore, all warnings from `impute_LS_gene()` are
#' truly from `impute_LS_gene()` and not a part of `impute_LS_array()`, which
#' never calls `impute_LS_gene()` in this case. Furthermore, all warnings from
#' [impute_expected_values()] belong to `impute_LS_array()`. Independent of the
#' choice of `verbosity`, no feedback is given from the first runs of `
#' impute_LS_gene()` and `impute_LS_array()` to estimate *p*.
#'
#' @inheritParams impute_LS_array
#' @param p_mis_sim percentage of observed values that are set `NA` to estimate
#'   the mixing coefficient *p*. The default value (0.05) corresponds to the
#'   choice of Bo et al. (2004).
#'
impute_LS_combined <- function(ds, k = 10, eps = 1e-6, min_common_obs = 5, p_mis_sim = 0.05,
                               verbosity = 0L) {

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
    min_common_obs = min_common_obs,
    warn_special_cases = FALSE
  )
  # Use ds_for_p_imp_gene for impute_LS_array to save some time
  ds_for_p_imp_array <- impute_LS_array(ds_for_p,
    k = k, eps = eps,
    min_common_obs = min_common_obs,
    ds_impute_LS_gene = ds_for_p_imp_gene,
    verbosity = 0L
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
  y_g <- impute_LS_gene(ds, k = k, eps = eps, min_common_obs = min_common_obs,
                        warn_special_cases = ifelse(verbosity == 1L || verbosity >= 3, TRUE, FALSE))
  y_a <- impute_LS_array(ds,
    k = k, eps = eps, min_common_obs = min_common_obs,
    ds_impute_LS_gene = y_g,
    verbosity = ifelse(verbosity >= 2L, 3L, 0L)
  )
  # Second: combine both results using p for mixing
  ds[M] <- p * y_g[M] + (1 - p) * y_a[M]
  return(ds)
}
