#' Impute expected values
#'
#' Impute the missing values with expected values given the observed values and
#' estimated parameters assuming a multivariate normal distribution
#'
#' @template impute
#'
#' @details
#'
#' Normally, this function is called by other imputation function and should not
#' be called directly. The function imputes the missing values assuming a
#' multivariate normal distribution. This is equivalent to imputing the least
#' squares estimate of the missing values in some kind of way.
#'
#' If no values is observed in a row or a relevant submatrix of the
#' covariance matrix (`S_22`) is not invertible, the missing values are imputed
#' with (parts of) `mu` (plus a residuum, if `stochastich = TRUE`). If
#' `verbose = TRUE`, these cases will be listed in a message. Otherwise, they
#' will be imputed silently.
#'
#' @param mu vector of means for the variables
#' @param S covariance matrix of the variables
#' @param stochastic logical, should residuals be added to the expected values
#' @param M missing data indicator matrix
#' @param verbose should messages be given for special cases (see details)
#'
#' @export
impute_expected_values <- function(ds, mu, S,
                                   stochastic = FALSE,
                                   M = is.na(ds), verbose = FALSE) {

  if (stochastic) {
    check_for_packages("mvtnorm")
  }

  ## Define some variables ----------------------------------------------------
  ds_imp <- as.matrix(ds) # need a matrix for %*%
  problematic_rows <- integer(0)

  ## Impute row by row --------------------------------------------------------
  for (i in seq_len(nrow(ds))) {
    M_i <- M[i, ]
    if (any(M_i)) { # any missing value in row i?

      ## Extract relevant parts of mu and Sigma ------------------------------
      y_1_mean <- mu[M_i] # mu for not observed part of row i
      y_2_mean <- mu[!M_i] # mu for observed part of row i
      y_2 <- ds_imp[i, !M_i] # observed part of row i
      S_12 <- S[M_i, !M_i, drop = FALSE] # part of covariance matrix (not observed, observed)
      S_22 <- S[!M_i, !M_i, drop = FALSE] # part of covariance matrix (observed, observed)

      ## Calculate S_22_inv -------------------------------------------------
      S_22_inv <- tryCatch(solve(S_22), # try to invert S_22
        error = function(cond) { # S_22 not invertible or no observed value in row i
          NULL
        }
      )

      if (is.null(S_22_inv)) { # S_22 was not invertible
        # Assigning a 0-matrix to S_22_inv will impute mu (+ residuum)
        S_22_inv <- matrix(0, nrow = nrow(S_22), ncol = nrow(S_22))
        problematic_rows <- c(problematic_rows, i)
      }

      ## Calculate imputation values ----------------------------------------
      y_imp <- y_1_mean + S_12 %*% S_22_inv %*% (y_2 - y_2_mean)
      if (stochastic) { # add residuum
        # Calculate needed variance
        S_11 <- S[M_i, M_i] # part of covariance matrix (not observed, not observed)
        S_21 <- S[!M_i, M_i] # part of covariance matrix (observed, not observed)
        var_y_imp <- S_11 - S_12 %*% S_22_inv %*% S_21
        # To guarantee symmetry of matrix (sometimes numeric accuracy problems with above calculation)
        var_y_imp <- (var_y_imp + t(var_y_imp)) / 2
        # Add residuum
        y_imp <- as.vector(y_imp) + mvtnorm::rmvnorm(n = 1, mean = rep(0, sum(M_i)), sigma = var_y_imp)
      }
      ds_imp[i, M_i] <- y_imp
    }
  }

  if (verbose && length(problematic_rows) > 0) {
    msg <- "The missing values of following rows were imputed with (parts of) mu"
    msg <- paste0(msg, ifelse(stochastic, " and a residuum: ", ": "))
    message(msg, paste(problematic_rows, collapse = ", "))
  }

  # To return the type of ds, which maybe is not a matrix!
  assign_imputed_values(ds, ds_imp, M)
}
