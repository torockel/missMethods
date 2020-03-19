#' Evaluate imputed values
#'
#' Compare imputed to true values
#'
#' The following \code{criterion}s are implemented to compare the imputed values
#' to the true values:
#' \itemize{
#' \item{"RMSE" (the default): The Root Mean Squared Error between the imputed
#' and true values}
#' \item{"bias": The mean difference between the imputed and the true values}
#' \item{"cor": The correlation between the imputed and true values}
#' \item{"MAE": The Mean Absolute Error between the imputed and true values}
#' \item{"MSE": The Mean Squared Error between the imputed and true values}
#' \item{"NRMSE_tot_mean": RMSE divided by the mean of the true values}
#' \item{"NRMSE_tot_mean_sq": RMSE divided by the square root of the mean of
#' the squared true values}
#' \item{"NRMSE_tot_sd": RMSE divided by the standard deviation of all true values}
#' \item{"nr_equal": number of imputed values that are equal to the true values}
#' \item{"nr_NA": number of values in \code{imp_ds} that are NA (not imputed)}
#' \item{"precision": proportion of imputed values that are equal to the true values}
#' }
#' Additionally there are relative versions of bias and MAE implemented.
#' In the relative versions, the differences are divided by the absolute values
#' of the true values.
#' These relative versions can be selected via "bias_rel" and "MAE_rel".
#'
#' The argument \code{which_cols} allows the selection of columns
#' for comparison (see examples).
#'
#' If \code{M = NULL} (the default), then all values of \code{imp_ds} and
#' \code{orig_ds} will be used for the calculation of the evaluation criterion.
#' If a missing data indicator matrix is given via \code{M}, only the truly
#' imputed values (values that are marked as missing via \code{M}) will be used
#' for the calculation.
#' If you want to provide \code{M}, \code{M} must be a logical matrix of the
#' same dimensions as \code{orig_ds} and missing values must be coded as TRUE.
#' This is the standard behavior, if you use \code{\link[base]{is.na}} on a
#' dataset with missing values to generate \code{M} (see examples).
#' It is possible to combine \code{M} and \code{which_cols}.
#'
#' @param imp_ds a data frame or matrix with imputed values
#' @param orig_ds a data frame or matrix with original (true) values
#' @param criterion a string specifying the used criterion for comparing the
#'   imputed and original values
#' @param which_cols indices or names of columns used for evaluation
#' @param M NULL (the default) or a missing data indicator matrix; the missing
#'   data indicator matrix is normally created via \code{is.na(miss_ds)}, where
#'   \code{miss_ds} is the dataset after deleting values from \code{orig_ds}
#' @param tolerance numeric, only used for \code{criterion = "precision"}:
#'   numeric differences smaller than tolerance are treated as zero/equal
#'
#' @return a numeric vector of length one
#' @export
#'
#' @examples
#' orig_ds <- data.frame(X = 1:10, Y = 101:110)
#' miss_ds <- delete_MCAR(orig_ds, 0.3)
#' M <- is.na(miss_ds)
#' imp_ds <- impute_mean(miss_ds)
#' evaluate_imputed_values(imp_ds, orig_ds, M = M)
#' # compare only the imputed values in column X
#' evaluate_imputed_values(imp_ds, orig_ds, M = M, which_cols = "X")
evaluate_imputed_values <- function(imp_ds, orig_ds, criterion = "RMSE", M = NULL,
                                    which_cols = seq_len(ncol(imp_ds)),
                                    tolerance = sqrt(.Machine$double.eps)) {
  if (!isTRUE(all.equal(dim(imp_ds), dim(orig_ds)))) {
    stop("the dimensions of imp_ds and orig_ds must be equal")
  }

  imp_ds <- imp_ds[, which_cols, drop = FALSE]
  orig_ds <- orig_ds[, which_cols, drop = FALSE]
  if (!is.null(M)) {
    M <- M[, which_cols, drop = FALSE]
  }

  calc_evaluation_criterion(imp_ds, orig_ds, criterion, M, tolerance = tolerance)
}
