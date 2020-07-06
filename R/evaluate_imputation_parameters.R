#' Evaluate estimated parameters after imputation
#'
#' Compares estimated parameters after imputation to true parameters or
#' estimates based on the original dataset
#'
#' @template evaluation
#' @template evaluate-parameter
#'
#' @details Either \code{ds_orig} or \code{true_pars} must be supplied and the other one
#' must be \code{NULL} (default: both are \code{NULL}, just supply one, see
#' examples). The following \code{parameter}s are implemented:
#' "mean", "median", "var", "sd", "quantile", "cov", "cor".
#' Some details follows:
#' \itemize{
#' \item{"var": only the variances of the columns (the diagonal elements of the
#' covariance matrix) are compared. The whole covariance matrix can be compared
#' with "cov".}
#' \item{"quantile": the quantiles can be set via the additional
#' argument \code{probs} (see examples). Otherwise, the default quantiles from
#' \code{\link[stats]{quantile}} will be used.}
#' }
#'
#' The argument \code{which_cols} allows the selection of columns for comparison
#' (see examples). If \code{true_pars} is used, it is assumed that only relevant
#' parameters are supplied (see examples).
#'
#' Possible choices for the argument \code{criterion} are documented in
#' \code{\link{evaluate_imputed_values}}
#'
#' @inheritParams evaluate_parameters
#' @inheritParams evaluate_imputed_values
#' @param parameter a string specifying the estimated parameters for comparison
#' @param ... further arguments passed to function for parameter estimation
#'
#' @export
#'
#' @examples
#' # only ds_orig known
#' ds_orig <- data.frame(X = 1:10, Y = 101:101)
#' ds_imp <- impute_mean(delete_MCAR(ds_orig, 0.4))
#' evaluate_imputation_parameters(ds_imp, ds_orig = ds_orig)
#'
#' # true parameters known
#' ds_orig <- data.frame(X = rnorm(100), Y = rnorm(100, mean = 10))
#' ds_imp <- impute_mean(delete_MCAR(ds_orig, 0.3))
#' evaluate_imputation_parameters(ds_imp, true_pars = c(0, 10), parameter = "mean")
#' evaluate_imputation_parameters(ds_imp, true_pars = c(1, 1), parameter = "var")
#'
#' # set quantiles
#' evaluate_imputation_parameters(ds_imp,
#'   true_pars = c(qnorm(0.3), qnorm(0.3, mean = 10)),
#'   parameter = "quantile", probs = 0.3
#' )
#'
#' # compare only column Y
#' evaluate_imputation_parameters(ds_imp,
#'   true_pars = c(Y = 10), parameter = "mean",
#'   which_cols = "Y"
#' )
evaluate_imputation_parameters <- function(ds_imp, ds_orig = NULL, true_pars = NULL,
                                           parameter = "mean", criterion = "RMSE",
                                           which_cols = seq_len(ncol(ds_imp)),
                                           tolerance = sqrt(.Machine$double.eps), ...) {
  if (!xor(is.null(ds_orig), is.null(true_pars))) {
    stop("exactly one of 'ds_orig' or 'true_pars' must be supplied and the
         other one must be NULL")
  }
  match.arg(parameter, c("mean", "median", "var", "sd", "quantile", "cov", "cor"))

  calc_pars <- switch(parameter,
    mean = colMeans,
    median = make_col_fun(median),
    var = function(x, ...) diag(stats::var(x, ...)),
    sd = make_col_fun(stats::sd),
    quantile = make_col_fun(stats::quantile),
    cov = stats::cov,
    cor = stats::cor
  )

  imp_pars <- calc_pars(ds_imp[, which_cols, drop = FALSE], ...)

  if (is.null(true_pars)) { # pars must be calculated
    true_pars <- calc_pars(ds_orig[, which_cols, drop = FALSE], ...)
  }

  evaluate_parameters(imp_pars, true_pars,
    criterion = criterion,
    tolerance = tolerance
  )
}


# helper---------------------------------------------------
make_col_fun <- function(FUN, unlist = TRUE) {
  function(ds, ...) {
    res <- list()
    for (k in seq_len(ncol(ds))) {
      res[[k]] <- FUN(ds[, k, drop = TRUE], ...)
    }
    if (!is.null(colnames(ds))) {
      names(res) <- colnames(ds)
    }
    if (unlist) {
      return(unlist(res))
    }
    res
  }
}
