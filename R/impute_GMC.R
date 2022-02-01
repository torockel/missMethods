weighted_av_gmc <- function(row_values, gmc_parameters, k) {
  denominator <- 0
  numerator <- 0
  for (i in seq_len(k)) {
    weighted_density_i <- gmc_parameters$lambda[i] *
      mixtools::dmvnorm(row_values[[i]], gmc_parameters$mu[[i]], gmc_parameters$sigma[[i]])
    denominator <- denominator + weighted_density_i
    numerator <- numerator + row_values[[i]] * weighted_density_i
  }
  numerator / denominator
}

impute_gmc_estimate <- function(ds, gmc_parameters, k, M = is.na(ds)) {
  ds_imp <- as.matrix(ds) # need a matrix for %*%
  rows_incomplete <- which(apply(M, 1, any))
  for(row_ind in rows_incomplete) {
    M_row_inc <- M[row_ind, ]
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
      ds_imp[row_ind, M_row_inc] <- y_imp
      ##### end copied from impute_expected_values() ##########################
    }
  }
  assign_imputed_values(ds, ds_imp, M)
}


