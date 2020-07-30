#' @eval document_LSimpute("adaptive")
#'
#' @details
#'
#' LSimpute_adaptive combines imputation values from [impute_LS_gene()] and
#' [impute_LS_array()] using a local (adaptive) approach for the mixing
#' coefficient *p*.
#'
#' If the dataset is too small or has too many missing values, there are some
#' fallback systems implemented. First, if `ncol(ds) <= min_common_obs`
#' (normally, this should not the case!), values are imputed through
#' [impute_LS_array()]. Second, `r_max_min` is automatically adjusted, if it is
#' too high. In this case, a warning will be given, if `warn_r_max = TRUE`.
#' Third, if there are not enough observed values in a row (less than
#' `min_common_obs`), the calculation of the mixing coefficient is not possible
#' and missing values of these rows are imputed with the values from
#' `impute_LS_array()`.
#'
#' The amount of feedback given from `impute_LS_gene()` and `impute_LS_array()`
#' is controlled via `verbose_gene`, `verbose_array`, `verbose_gene_p` and
#' `verbose_array_p`. The last two control the amount of feedback while
#' estimating *p* and the first two the amount of feedback during the estimation
#' of the values that are mixed with *p*. Internally,  the imputed dataset from
#' `impute_LS_gene()` is passed on to `impute_LS_array()`. Therefore, all
#' messages from `impute_LS_gene()` are truly from `impute_LS_gene()` and not a
#' part of `impute_LS_array()`, which never calls `impute_LS_gene()` in this
#' case. Furthermore, all messages from [impute_expected_values()] belong to
#' `impute_LS_array()`.
#'
#' @inheritParams impute_LS_combined
#' @param r_max_min Minimum number of nearest genes used for imputation. The
#'   default value (100) corresponds to the choice of Bo et al. (2004).
#' @param warn_r_max Should a warning be given, if `r_max_min` is set too high?
#'
#' @md
impute_LS_adaptive <- function(ds, k = 10, eps = 1e-6, min_common_obs = 5,
                               r_max_min = 100, p_mis_sim = 0.05,
                               warn_r_max = TRUE,
                               verbose_gene = FALSE, verbose_array = FALSE,
                               verbose_gene_p = FALSE, verbose_array_p = FALSE) {

  ## Define some variables ----------------------------------------------------
  ds_mat <- as.matrix(ds) # for subsetting by indices_new_mis a matrix is needed
  M <- is.na(ds_mat)
  ds_for_p <- ds_mat
  indices_observed <- which(!M)

  ## Preparations to estimate p -----------------------------------------------
  # Delete and estimate p_mis_sim percent of the known values
  indices_new_mis <- sample(
    indices_observed,
    size = round(length(indices_observed) * p_mis_sim, 0)
  )
  ds_for_p[indices_new_mis] <- NA
  ds_for_p_imp_gene <- impute_LS_gene(ds_for_p,
    k = k, eps = eps,
    min_common_obs = min_common_obs,
    return_r_max = TRUE,
    verbose = verbose_gene_p
  )
  ds_for_p_imp_gene$r_max[M] <- NA # don't use NA values from ds!
  ds_for_p_imp_array <- impute_LS_array(ds_for_p,
    ds_impute_LS_gene = ds_for_p_imp_gene$imp,
    verbose_expected_values = verbose_array_p
  )
  # calculate errors
  e_g_all <- ds_for_p_imp_gene$imp - ds_mat
  e_a_all <- ds_for_p_imp_array - ds_mat
  e_c <- function(p, e_g, e_a) {
    sum(p^2 * e_g^2 + 2 * p * (1 - p) * e_g * e_a + (1 - p)^2 * e_a^2)
  }

  ## Check if r_max_min is possible ---------------------------------------------
  # if we do not have enough estimated r_max, we must reduce r_max_min
  max_possible_r_max_min <- sum(!is.na(ds_for_p_imp_gene$r_max))
  if (max_possible_r_max_min < r_max_min) {
    r_max_old <- r_max_min
    r_max_min <- max_possible_r_max_min
    if (warn_r_max) {
      warning(
        "Not enough data for r_max_min = ", r_max_old, ". ",
        "r_max_min reduced to ", r_max_min, "!"
      )
    }
    if (max_possible_r_max_min == 0) { # calculation of neighbors not possible!
      # jar from Bo et al. get caught in an infinite loop (at least if ncol(ds) <= 5)
      # just return the result of impute_LS_array() (better than infinite loop or?)
      return(impute_LS_array(ds, k = k, eps = eps, min_common_obs = min_common_obs))
    }
  }


  ## Impute the truly missing values with LS gene/array -----------------------
  y_g_list <- impute_LS_gene(ds_mat,
    k = k, eps = eps, min_common_obs = min_common_obs,
    return_r_max = TRUE,
    verbose = verbose_gene
  )
  y_a <- impute_LS_array(ds_mat,
    ds_impute_LS_gene = y_g_list$imp,
    verbose_expected_values = verbose_array
  )

  ## Impute value by value ----------------------------------------------------
  for (index in which(M)) {
    ## Calculate "optimal" p for missing value --------------------------------
    r_max <- y_g_list$r_max[index]
    if (is.na(r_max)) { # to many missing values in a row to get a r_max (impute_LS_gene imputed row mean!)
      # Calculation of p not possible!
      ds_mat[index] <- y_a[index] # impute value from impute_LS_array
    } else { # r_max is known
      r_max_dist <- abs(ds_for_p_imp_gene$r_max - r_max)
      if (sum(r_max_dist <= 0.05, na.rm = TRUE) >= r_max_min) { # enough data in r_max +-0.05
        index_for_mixing <- which(r_max_dist <= 0.05)
      } else { # find the r_max_min nearest r_max
        index_for_mixing <- order(r_max_dist, decreasing = FALSE)[1:r_max_min]
      }
      p <- stats::optimize(e_c, c(0, 1),
        e_a = e_a_all[index_for_mixing],
        e_g = e_g_all[index_for_mixing], maximum = FALSE
      )$minimum
      # Impute with "optimal" p
      ds_mat[index] <- p * y_g_list$imp[index] + (1 - p) * y_a[index]
    }
  }

  ## Return imputed ds --------------------------------------------------------
  assign_imputed_values(ds, ds_mat, M)
}
