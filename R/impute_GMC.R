weighted_av_gmc <- function(row_values, gmc_parameters, k,
                            tolerance_denominator = .Machine$double.xmin,
                            warn_low_denominator = FALSE) {
  denominator <- 0
  numerator <- 0
  for (i in seq_len(k)) {
    weighted_density_i <- gmc_parameters$lambda[i] *
      EMCluster::dmvn(row_values[[i]], gmc_parameters$mu[i, ], gmc_parameters$LTSigma[i, ])
    weighted_density_i <- ifelse(is.finite(weighted_density_i), weighted_density_i, 0) # https://github.com/snoweye/EMCluster/issues/10
    denominator <- denominator + weighted_density_i
    numerator <- numerator + row_values[[i]] * weighted_density_i
  }
  if (denominator < tolerance_denominator) { # all densities * lambda close to 0
    if (warn_low_denominator) {
      warning("denominator was too low")
    }
    return(rowMeans(as.data.frame(row_values)))
  } else {
    return(numerator / denominator)
  }

}

# This function is called EM_estimate() in Ouyang et al. 2004
impute_gmc_estimate <- function(ds, gmc_parameters, k, M = is.na(ds)) {
  ds_imp <- as.matrix(ds) # need a matrix for %*%
  rows_incomplete <- which(apply(M, 1, any))
  for(row_ind in rows_incomplete) {
    M_row_inc <- M[row_ind, ]
    row_values <- list()
    for (i in seq_len(k)) {
      ## Impute expected values for row_ind with the i-th parameter set -------
      mu <- gmc_parameters$mu[i, ]
      S <- gmc_parameters$sigma[[i]]
      ##### copied from impute_expected_values() ##############################
      y_1_mean <- mu[M_row_inc] # mu for not observed part of row i
      y_2_mean <- mu[!M_row_inc] # mu for observed part of row i
      y_2 <- ds_imp[row_ind, !M_row_inc] # observed part of row row_ind
      S_12 <- S[M_row_inc, !M_row_inc, drop = FALSE] # part of covariance matrix (not observed, observed)
      S_22 <- S[!M_row_inc, !M_row_inc, drop = FALSE] # part of covariance matrix (observed, observed)

      ## Calculate S_22_inv -------------------------------------------------
      S_22_inv <- tryCatch(solve(S_22), # try to invert S_22
                           error = function(cond) { # S_22 not invertible or no observed value in row i
                             NULL
                           }
      )

      if (is.null(S_22_inv)) { # S_22 was not invertible
        # Assigning a 0-matrix to S_22_inv will impute mu
        S_22_inv <- matrix(0, nrow = nrow(S_22), ncol = nrow(S_22))
      }

      ## Calculate imputation values ----------------------------------------
      y_imp <- y_1_mean + S_12 %*% S_22_inv %*% (y_2 - y_2_mean)
      ##### end copied from impute_expected_values() ##########################
      row_values[[i]] <- ds_imp[row_ind, ]
      row_values[[i]][M_row_inc] <- y_imp
    }
    ds_imp[row_ind, ] <- weighted_av_gmc(row_values, gmc_parameters, k)
  }
  assign_imputed_values(ds, ds_imp, M)
}

get_GMC_parameters <- function(ds, k, max_tries_restart = 3L, ...) {
  gmc_parameters <- NULL
  iter <- 0L
  if (nrow(ds) <= 2 * k) {
    return(NULL) # EMCluster does not like such things...
    # https://github.com/snoweye/EMCluster/issues/9
  }
  # GMC may fails and needs a manual restart...
  while (is.null(gmc_parameters) && iter < max_tries_restart){
    iter <- iter + 1L
    gmc_parameters <- tryCatch(
      EMCluster::emcluster(
        ds,
        emobj = EMCluster::simple.init(ds, nclass = k), EMC = EMCluster::.EMC,
        assign.class = TRUE
      ),
      error = function(cond) {
        NULL
      },
      warning = function(cond) {
        NULL
      }
    )
  }
  if(is.null(gmc_parameters)) {
    return(NULL)
  }
  transform_gmc_parameters(gmc_parameters, ds)
}

K_estimate <- function(ds, k, M = is.na(ds), imp_max_iter = 10L, max_tries_restart = 3L) {
  rows_comp <- !apply(M, 1, any)
  ds_comp_cases <- ds[rows_comp, ]

  iter <- 0L
  max_iter_stop <- FALSE
  gmc_error <- FALSE

  if (k == 1L) { # special treatment, because mixtools does not like k = 1 (is this still needed for EMCLuster?)
    if (nrow(ds_comp_cases) <= 3) { # 3 or less comp observed objects
      ds_imp <- impute_mean(ds)
    } else {
      mu <- colMeans(ds_comp_cases)
      sigma <- stats::cov(ds_comp_cases)
      ds_imp <- impute_expected_values(ds, mu, sigma, M = M)
    }
    if (imp_max_iter >= 1L){
      # no loop needed because clusters do not change (only one "cluster")
      iter <- 1L
      mu <- colMeans(ds_imp)
      sigma <- stats::cov(ds_imp)
      ds_imp <- impute_expected_values(ds_imp, mu, sigma, M = M)
    }
  } else { # k > 1
    gmc_parameters <- get_GMC_parameters(ds_comp_cases, k, max_tries_restart = max_tries_restart)
    if (is.null(gmc_parameters)) {
      # GMC did not like the data set (did not work)...
      ds_imp <- impute_sRHD(ds) # "better" than mean imputation?
    } else {
      ds_imp <- impute_gmc_estimate(ds, gmc_parameters, k = k, M = M)
    }

    iter <- 0L
    assigned_cluster <- NULL
    max_iter_stop <- FALSE
    while(iter < imp_max_iter) {
      iter <- iter + 1L

      # Get GMC parameters, if possible ---------------------------------------
      gmc_parameters <- get_GMC_parameters(ds_imp, k = k, max_tries_restart = max_tries_restart)
      if (is.null(gmc_parameters)) { # no GMC parameters -> finish loop
        gmc_error <- TRUE
        break()
      }

      # Impute with GMC parameters and check for ending loop ------------------
      ds_imp <- impute_gmc_estimate(ds, gmc_parameters, k = k, M = M) # M is important!
      old_assigned_cluster <- assigned_cluster
      assigned_cluster <- gmc_parameters$class
      if (!is.null(old_assigned_cluster) &&
          are_clusters_identical(old_assigned_cluster, assigned_cluster)) {
        break()
      } else if (iter == imp_max_iter) {
        max_iter_stop <- TRUE
      }
    }
  }
  structure(ds_imp, k = k, iterations = iter, max_iter_stop = max_iter_stop, gmc_error = gmc_error)
}

#' Gaussian mixture clustering imputation
#'
#' Impute missing values in a data frame or a matrix using parameters estimated
#' via Gaussian mixture clustering
#'
#' @template impute
#'
#' @param k_max maximum number of clusters (called `S` by Ouyang et al. (2004))
#' @param imp_max_iter maximum number of iterations for `K_estimate()`
#'
#' @details
#' This function performs Gaussin mixture clustering (GMC) imputation as
#' described by Ouyang et al. (2004).
#'
#' @references Ouyang, M., Welsh, W. J., Georgopoulos, P. (2004): Gaussian
#'   Mixture Clustering and Imputation of Microarray Data.
#'   \emph{Bioinformatics}, 20(6), 917–923
#'
#' @export
impute_GMC <- function(ds, k_max, imp_max_iter = 10L) {
  M <- is.na(ds)
  res <- list()
  if (!requireNamespace("EMCluster", quietly = TRUE)) {
   stop("Package \"EMCluster\" needed. Please, install it.")
  }
  for (i in seq_len(k_max)) {
    res[[i]] <- K_estimate(ds, k = i, M = M, imp_max_iter = imp_max_iter)
  }
  ds_imp <- Reduce("+", res) / k_max
  assign_imputed_values(ds, ds_imp, M)
}
