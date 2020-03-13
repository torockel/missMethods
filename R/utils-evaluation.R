# the workhorse for evaluate_imputed_values and evaluate_estimated_parameters
calc_evaluation_criterion <- function(estimate, true_val, criterion = "RMSE", M = NULL) {
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

  switch(criterion,
         RMSE = sqrt(mean((estimate - true_val)^2)),
         bias = mean(estimate - true_val),
         bias_rel =  mean((estimate - true_val)/abs(true_val)),
         cor = stats::cor(estimate, true_val),
         MAE = mean(abs(estimate - true_val)),
         MAE_rel = mean(abs(estimate - true_val) / abs(true_val)),
         MSE = mean((estimate - true_val)^2),
         stop("criterion ", criterion, " is not implemented"))
}
