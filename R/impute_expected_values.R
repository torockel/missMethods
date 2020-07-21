#' Impute expected values
#'
#' Impute the missing values with expected values given the observed values and
#' estimated parameters assuming a multivariate normal distribution
#'
#' @details
#'
#' Imputing the missing values assuming a multivariate normal distributing is
#' equivalent to imputing the least squares estimate of the missing values in
#' some kind of way.
#'
#' If no values is observed in a row or the matrix `S_22`is not invertible, the
#' missing values are imputed with (parts of) `mu`. If `warn_problematic_rows =
#' TRUE`, the problematic rows will be listed in a warning. Otherwise, they will
#' be imputed without any message.
#'
#' @param ds a dataset with missing values
#' @param mu vector of expected values for the variables
#' @param S covariance matrix of the variables
#' @param stochastic logical, should a residual be added to the expected values
#' @param M missing data indicator matrix
#' @param warn_problematic_rows should warnings be given for problematic rows?
#'
#' @return a dataset of the same type as `ds`
#' @noRd
impute_expected_values <- function(ds, mu, S, stochastic = FALSE, M = is.na(ds), warn_problematic_rows = FALSE) {

  ## Define some variables ----------------------------------------------------
  ds_imp <- as.matrix(ds) # need a matrix for %*%
  problematic_rows <- integer(0)

  ## Impute row by row --------------------------------------------------------
  for (i in seq_len(nrow(ds))) {
    M_i <- M[i, ]
    if (any(M_i)) { # any missing value in row i?
      if (all(M_i)) { # no observed value in row i
        # impute mu
        ds_imp[i, ] <- mu
        problematic_rows <- c(problematic_rows, i)
      } else {

        ## Extract relevant parts of mu and Sigma ------------------------------
        y_1_mean <- mu[M_i] # mu for not observed part of row i
        y_2_mean <- mu[!M_i] # mu for observed part of row i
        y_2 <- ds_imp[i, !M_i] # observed part of row i
        S_12 <- S[M_i, !M_i, drop = FALSE] # part of covariance matrix (not observed, observed)
        S_22 <- S[!M_i, !M_i, drop = FALSE] # part of covariance matrix (observed, observed)

        ## Calculate S_22_inv -------------------------------------------------
        S_22_inv <- tryCatch(solve(S_22), #try to invert S_22
                             error = function(cond){ # S_22 not invertible!
                               NULL
                             })

        if (is.null(S_22_inv)) { # S_22 was not invertible
          # Assigning a 0-matrix to S_22_inv will impute mu (+ residuum)
          S_22_inv <- matrix(0, nrow = nrow(S_22), ncol = nrow(S_22))
          problematic_rows <- c(problematic_rows, i)
        }

        # Calculate imputation values -----------------------------------------
        y_imp <- y_1_mean + S_12 %*% S_22_inv %*% (y_2 - y_2_mean)
        if (stochastic) { # add residuum
          # Calculate needed variance
          S_11 <- S[M_i, M_i] # part of covariance matrix (not observed, not observed)
          S_21 <- S[!M_i, M_i] # part of covariance matrix (observed, not observed)
          var_y_imp <- S_11 - S_12 %*% S_22_inv %*% S_21
          # To guarantee symmetry of matrix (sometimes numeric accuracy problems with above calculation)
          var_y_imp <- (var_y_imp + t(var_y_imp)) / 2
          # Add residuum
          y_imp <- y_imp + MASS::mvrnorm(n = 1, mu = rep(0, sum(M_i)), Sigma = var_y_imp)
        }
        ds_imp[i, M_i] <- y_imp

      }
    }
  }

  if (warn_problematic_rows) {
    warning("The missing values of following rows were imputed with (parts of) mu: ",
            paste(problematic_rows, collapse = ", "))
  }

  # To return the type of ds, which maybe is not a matrix!
  ds[M] <- ds_imp[M]
  ds
}
