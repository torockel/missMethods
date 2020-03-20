# the workhorse for evaluate_imputed_values and evaluate_estimated_parameters
calc_evaluation_criterion <- function(estimate, true_val, criterion = "RMSE", M = NULL,
                                      tolerance = sqrt(.Machine$double.eps)) {
  criterion <- match.arg(criterion, c(
    "RMSE", "bias", "bias_rel", "cor", "MAE", "MAE_rel", "MSE",
    "NRMSE_col_mean", "NRMSE_col_mean_sq", "NRMSE_col_sd",
    "NRMSE_tot_mean", "NRMSE_tot_mean_sq", "NRMSE_tot_sd",
    "nr_equal", "nr_NA", "precision"
  ))

  if (requireNamespace("tibble", quietly = TRUE)) {
    if ((tibble::is_tibble(estimate) || tibble::is_tibble(estimate)) &&
        utils::packageVersion("tibble") < package_version("2.99.99.9012") &&
        !is.null(M)) {
      stop("logical subsetting by 'M' for tibbles is only supported for package tibble versions >= 2.99.99.9012;
            possible solutions:
            * convert inputs via as.data.frame
            * update package tibble
            * set 'M = NULL'", call. = FALSE)
    } # for more details see: https://github.com/tidyverse/tibble/releases/tag/v2.99.99.9012
  }

  if(is.null(M)) { # use all values -> all entries in M are TRUE
    if (is_df_or_matrix(estimate)) {
      M <- matrix(TRUE, nrow = nrow(estimate), ncol = ncol(estimate))
    } else {
      M <- rep(TRUE, length(estimate))
    }
  }

  colwise_crit <- c("NRMSE_col_mean", "NRMSE_col_mean_sq", "NRMSE_col_sd","nr_equal", "nr_NA", "precision")
  if ((criterion %in% colwise_crit &&
       is_df_or_matrix(estimate))) { # handle col by col
    crit_by_col <- numeric(ncol(estimate))
    any_M_col <- rep(TRUE, ncol(estimate))
    for(k in seq_len(ncol(estimate))) {
      m_k <- M[, k]
      if (any(m_k)) { # check if any value for calculation is available
        crit_by_col[k] <- calc_evaluation_criterion_vec(estimate[, k, drop = TRUE],
                                                        true_val[, k, drop = TRUE],
                                                        criterion = criterion,
                                                        m_k = m_k, tolerance = tolerance)
      } else { # no value for calculation is available
        any_M_col[k] <- FALSE
      }
    }
    crit_by_col <- crit_by_col[any_M_col] # only columns with available values
    if(grepl("^NRMSE", criterion)) {
      return(sqrt(mean(crit_by_col^2)))
    } else {
      return(switch(criterion,
                    nr_equal = sum(crit_by_col),
                    nr_NA = sum(crit_by_col),
                    precision = mean(crit_by_col)))
    }

  } else { # handle all elements of estimate and true_val in one call
    if (is.matrix(estimate)) {
      estimate <- as.vector(estimate)
      true_val <- as.vector(true_val)
    } else if (is.data.frame(estimate)) {
      estimate <- unlist(estimate)
      true_val <- unlist(true_val)
    }

    calc_evaluation_criterion_vec(estimate, true_val, criterion = criterion,
                                  m_k = as.vector(M), tolerance = tolerance)
  }
}


calc_evaluation_criterion_vec <- function(estimate, true_val = NULL, criterion = "RMSE",
                                          m_k = rep(TRUE, length(estimate)),
                                          tolerance = sqrt(.Machine$double.eps)) {
  if (criterion != "nr_NA" && length(estimate) != length(true_val)) {
    stop("estimate and true_val must be of same length")
  } else if (criterion != "nr_NA" && (anyNA(estimate) || anyNA(true_val))) {
    warning("NAs in estimate or true_val may lead to NA")
  }

  if (grepl("^[N]?[R]?MSE", criterion)) { # NRMSE, RMSE, MSE (NMSE)
    res <- mean((estimate[m_k] - true_val[m_k])^2) # MSE
    if(grepl("^[N]?R", criterion)) { # form of RMSE or NRMSE
      res <- sqrt(res)
    }
    if(grepl("^N", criterion)) { # Normalization
      normalization <- switch(sub("^[N]?[R]?MSE_[[:alpha:]]*_", "", criterion), # get ending: means, mean_sq, sd
                              mean = mean(true_val),
                              mean_sq = sqrt(mean(true_val^2)),
                              sd = stats::sd(true_val),
                              stop("normalization not implemented")
      )
      res <- res / normalization
    }
  }   else {
    estimate <- estimate[m_k]
    true_val <- true_val[m_k]
    res <-  switch(criterion,
                   bias = mean(estimate - true_val),
                   bias_rel =  mean((estimate - true_val) / abs(true_val)),
                   cor = stats::cor(estimate, true_val),
                   MAE = mean(abs(estimate - true_val)),
                   MAE_rel = mean(abs(estimate - true_val) / abs(true_val)),
                   nr_equal = count_equals(estimate, true_val, tolerance = tolerance),
                   nr_NA = sum(is.na(estimate)),
                   precision = count_equals(estimate, true_val, tolerance = tolerance) / length(estimate),
                   stop("criterion ", criterion, " is not implemented")
    )
  }
  res
}


count_equals <- function(x, y, tolerance = sqrt(.Machine$double.eps)) {
  if (is.numeric(x)) {
    return(sum(abs(x - y) < tolerance))
  } else if (is.factor(x)) { # to allow for factors with different levels
    return(sum(as.character(x) == as.character(y)))
  } else {
    return(sum(x == y))
  }
}
