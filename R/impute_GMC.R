weighted_av_gmc <- function(row_values, gmc_parameters, k,
                            tolerance_denominator = .Machine$double.xmin,
                            warn_low_denominator = FALSE) {
  denominator <- 0
  numerator <- 0
  for (i in seq_len(k)) {
    weighted_density_i <- gmc_parameters$lambda[i] *
      mixtools::dmvnorm(row_values[[i]], gmc_parameters$mu[[i]], gmc_parameters$sigma[[i]])
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

# This function is called EM_estimate in Ouyang et al. 2004
impute_gmc_estimate <- function(ds, gmc_parameters, k, M = is.na(ds)) {
  ds_imp <- as.matrix(ds) # need a matrix for %*%
  rows_incomplete <- which(apply(M, 1, any))
  for(row_ind in rows_incomplete) {
    M_row_inc <- M[row_ind, ]
    row_values <- list()
    for (i in seq_len(k)) {
      ## Impute expected values for row_ind with the i-th parameter set -------
      mu <- gmc_parameters$mu[[i]]
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
    weighted_av_gmc(row_values, gmc_parameters, k)
    ds_imp[row_ind, ] <- weighted_av_gmc(row_values, gmc_parameters, k)
  }
  assign_imputed_values(ds, ds_imp, M)
}

K_estimate <- function(ds, k, M = is.na(ds), max_iter = 10L) {
  rows_comp <- !apply(M, 1, any)
  ds_comp_cases <- ds[rows_comp, ]

  iter <- 0L
  max_iter_stop <- FALSE
  mixtools_error <- FALSE

  if (k == 1L) { # special treatment, because mixtools do not like k = 1
    mu <- colMeans(ds_comp_cases)
    sigma <- stats::cov(ds_comp_cases)
    ds_imp <- impute_expected_values(ds, mu, sigma, M = M)
    if (max_iter >= 1L){
      # no loop needed because cluster do not change (only one "cluster")
      iter <- 1L
      mu <- colMeans(ds_imp)
      sigma <- stats::cov(ds_imp)
      ds_imp <- impute_expected_values(ds_imp, mu, sigma, M = M)
    }
  } else { # k > 1
    gmc_parameters <- mixtools::mvnormalmixEM(ds_comp_cases, k = k)
    ds_imp <- impute_gmc_estimate(ds, gmc_parameters, k = k, M = M)

    iter <- 0L
    assigned_cluster <- NULL
    max_iter_stop <- FALSE
    while(iter < max_iter) {
      iter <- iter + 1L

      # Get GMC parameters, if possible ---------------------------------------
      gmc_parameters <- tryCatch(
        mixtools::mvnormalmixEM(ds_imp, k = k),
        error = function(cond) {
          NULL
        }
      )
      if (is.null(gmc_parameters)) { # no GMC parameters -> finish loop
        mixtools_error <- TRUE
        break()
      }

      # Impute with GMC parameters and check for ending loop ------------------
      ds_imp <- impute_gmc_estimate(ds, gmc_parameters, k = k, M = M) # M is important!
      old_assigned_cluster <- assigned_cluster
      assigned_cluster <- apply(gmc_parameters$posterior, 1, which.max)
      if (!is.null(old_assigned_cluster) &&
          are_clusters_identical(old_assigned_cluster, assigned_cluster)) {
        break()
      } else if (iter == max_iter) {
        max_iter_stop <- TRUE
      }
    }
  }
  structure(ds_imp, k = k, iterations = iter, max_iter_stop = max_iter_stop, mixtools_error = mixtools_error)
}
