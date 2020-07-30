#' Evaluate imputed values
#'
#' Compare imputed to true values
#'
#' @template evaluation
#'
#' @details The following \code{criterion}s are implemented to compare the
#' imputed values to the true values:
#' \itemize{
#' \item{"RMSE" (the default): The Root Mean Squared Error between the imputed
#' and true values}
#' \item{"bias": The mean difference between the imputed and the true values}
#' \item{"cor": The correlation between the imputed and true values}
#' \item{"MAE": The Mean Absolute Error between the imputed and true values}
#' \item{"MSE": The Mean Squared Error between the imputed and true values}
#' \item{"NRMSE_col_mean": For every column the RMSE divided by the mean of the
#' true values is calculated. Then these columnwise values are squared and
#' averaged. Finally, the square root of this average is returned.}
#' \item{"NRMSE_col_mean_sq": For every column the RMSE divided by the square
#' root of the mean of the squared true values is calculated. Then these
#' columnwise values are squared and averaged. Finally, the square root of this
#' average is returned.}
#' \item{"NRMSE_col_sd": For every column the RMSE divided by the standard
#' deviation of all true values is calculated. Then these columnwise values are
#' squared and averaged. Finally, the square root of this average is returned.}
#' \item{"NRMSE_tot_mean": RMSE divided by the mean of all true values}
#' \item{"NRMSE_tot_mean_sq": RMSE divided by the square root of the mean of
#' all squared true values}
#' \item{"NRMSE_tot_sd": RMSE divided by the standard deviation of all true values}
#' \item{"nr_equal": number of imputed values that are equal to the true values}
#' \item{"nr_NA": number of values in \code{ds_imp} that are NA (not imputed)}
#' \item{"precision": proportion of imputed values that are equal to the true values}
#' }
#' Additionally there are relative versions of bias and MAE implemented. In the
#' relative versions, the differences are divided by the absolute values of the
#' true values. These relative versions can be selected via "bias_rel" and
#' "MAE_rel". The "NRMSE_tot_" and "NRMSE_col_" are equal, if the columnwise
#' normalization values are equal to the total normalization value (see
#' examples).
#'
#' The argument \code{cols_which} allows the selection of columns
#' for comparison (see examples).
#'
#' If \code{M = NULL} (the default), then all values of \code{ds_imp} and
#' \code{ds_orig} will be used for the calculation of the evaluation criterion.
#' If a missing data indicator matrix is given via \code{M}, only the truly
#' imputed values (values that are marked as missing via \code{M}) will be used
#' for the calculation. If you want to provide \code{M}, \code{M} must be a
#' logical matrix of the same dimensions as \code{ds_orig} and missing values
#' must be coded as TRUE. This is the standard behavior, if you use
#' \code{\link[base]{is.na}} on a dataset with missing values to generate
#' \code{M} (see examples). It is possible to combine \code{M} and
#' \code{cols_which}.
#'
#' @param ds_imp A data frame or matrix with imputed values.
#' @param ds_orig A data frame or matrix with original (true) values.
#' @param cols_which Indices or names of columns used for evaluation.
#' @param M NULL (the default) or a missing data indicator matrix. The missing
#'   data indicator matrix is normally created via \code{is.na(ds_mis)}, where
#'   \code{ds_mis} is the dataset after deleting values from \code{ds_orig}.
#' @param imp_ds Deprecated, renamed to \code{ds_imp}.
#' @param orig_ds Deprecated, renamed to \code{ds_orig}.
#' @param which_cols Deprecated, renamed to \code{cols_which}.
#'
#' @export
#'
#' @references Kim, H., Golub, G. H., & Park, H. (2005). Missing value
#'   estimation for DNA microarray gene expression data: local least squares
#'   imputation. \emph{Bioinformatics}, 21(2), 187-198.
#'
#' @examples
#' ds_orig <- data.frame(X = 1:10, Y = 101:110)
#' ds_mis <- delete_MCAR(ds_orig, 0.3)
#' ds_imp <- impute_mean(ds_mis)
#' # compare all values from ds_orig and ds_imp
#' evaluate_imputed_values(ds_imp, ds_orig)
#' # compare only the imputed values
#' M <- is.na(ds_mis)
#' evaluate_imputed_values(ds_imp, ds_orig, M = M)
#' # compare only the imputed values in column X
#' evaluate_imputed_values(ds_imp, ds_orig, M = M, cols_which = "X")
#'
#' # NRMSE_tot_mean and NRMSE_col_mean are equal, if columnwise means are equal
#' ds_orig <- data.frame(X = 1:10, Y = 10:1)
#' ds_mis <- delete_MCAR(ds_orig, 0.3)
#' ds_imp <- impute_mean(ds_mis)
#' evaluate_imputed_values(ds_imp, ds_orig, "NRMSE_tot_mean")
#' evaluate_imputed_values(ds_imp, ds_orig, "NRMSE_col_mean")
evaluate_imputed_values <- function(ds_imp, ds_orig, criterion = "RMSE",
                                    M = NULL,
                                    cols_which = seq_len(ncol(ds_imp)),
                                    tolerance = sqrt(.Machine$double.eps),
                                    imp_ds, orig_ds, which_cols) {

  # Deprecate imp_ds, orig_ds, which_cols
  check_renamed_arg(imp_ds, ds_imp)
  check_renamed_arg(orig_ds, ds_orig)
  check_renamed_arg(which_cols, cols_which)

  if (!isTRUE(all.equal(dim(ds_imp), dim(ds_orig)))) {
    stop("the dimensions of ds_imp and ds_orig must be equal")
  }

  ds_imp <- ds_imp[, cols_which, drop = FALSE]
  ds_orig <- ds_orig[, cols_which, drop = FALSE]
  if (!is.null(M)) {
    M <- M[, cols_which, drop = FALSE]
  }

  calc_evaluation_criterion(ds_imp, ds_orig, criterion, M, tolerance = tolerance)
}
