# the workhorse for evaluate_imputed_values and evaluate_estimated_parameters
calc_evaluation_criterion <- function(estimate, true_val, criterion = "RMSE", M = NULL,
                                      tolerance = sqrt(.Machine$double.eps)) {
  criterion <- match.arg(criterion, c(
    "RMSE", "bias", "bias_rel", "cor", "MAE",
    "MAE_rel", "MSE", "NRMSE_tot_mean",
    "NRMSE_tot_mean_sq", "NRMSE_tot_sd",
    "nr_equal", "nr_NA", "precision"
  ))

  if ((criterion %in% c("nr_equal", "precision") &&
       is_df_or_matrix(estimate))) { # handle col by col
    if(is.null(M)) {
      M <- matrix(TRUE, nrow = nrow(estimate), ncol = ncol(estimate))
    }
    crit_by_col <- numeric(length(ncol(estimate)))
    for(k in seq_len(ncol(estimate))) {
      crit_by_col[k] <- calc_evaluation_criterion_vec(estimate[M[, k], k, drop = TRUE],
                                                      true_val[M[, k], k, drop = TRUE],
                                                      criterion, tolerance)
    }
    return(switch(criterion,
                  nr_equal = sum(crit_by_col),
                  precision = mean(crit_by_col)))
  } else { # handle all elements of estimate and true_val in one call
    if (is.null(M)) {
      if (is.matrix(estimate)) {
        estimate <- as.vector(estimate)
        true_val <- as.vector(true_val)
      } else if (is.data.frame(estimate)) {
        estimate <- unlist(estimate)
        true_val <- unlist(true_val)
      }
    } else {
      estimate <- estimate[M]
      true_val <- true_val[M]
    }

    if (criterion != "nr_NA" && (anyNA(estimate) || anyNA(true_val))) {
      warning("NAs in estimate or true_val will may lead to NA")
    }
    calc_evaluation_criterion_vec(estimate, true_val, criterion, tolerance)
  }
}


calc_evaluation_criterion_vec <- function(estimate, true_val, criterion, tolerance) {
  switch(criterion,
         RMSE = sqrt(mean((estimate - true_val)^2)),
         bias = mean(estimate - true_val),
         bias_rel =  mean((estimate - true_val) / abs(true_val)),
         cor = stats::cor(estimate, true_val),
         MAE = mean(abs(estimate - true_val)),
         MAE_rel = mean(abs(estimate - true_val) / abs(true_val)),
         MSE = mean((estimate - true_val)^2),
         NRMSE_tot_mean = sqrt(mean((estimate - true_val)^2)) / mean(true_val),
         NRMSE_tot_mean_sq = sqrt(mean((estimate - true_val)^2)) / sqrt(mean(true_val^2)),
         NRMSE_tot_sd = sqrt(mean((estimate - true_val)^2)) / stats::sd(true_val),
         nr_equal = count_equals(estimate, true_val, tolerance = tolerance),
         nr_NA = sum(is.na(estimate)),
         precision = count_equals(estimate, true_val, tolerance = tolerance) / length(estimate),
         stop("criterion ", criterion, " is not implemented")
  )
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
