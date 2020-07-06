#' Evaluate estimated parameters
#'
#' Compare estimated parameters to true parameters
#'
#' @template evaluation
#' @template evaluate-parameter
#'
#' @details The same \code{criterion}s are implemented for \code{evaluate_parameters}
#' and \code{\link{evaluate_imputed_values}}.
#' The possible choices are documented in \code{\link{evaluate_imputed_values}}.
#'
#' @param pars_est a vector or matrix of estimated parameters
#' @param pars_true true parameters, normally a vector or a matrix
#'
#' @export
#'
#' @examples
#' evaluate_parameters(1:4, 2:5, "RMSE")
evaluate_parameters <- function(pars_est, pars_true, criterion = "RMSE",
                                tolerance = sqrt(.Machine$double.eps)) {
  if (!isTRUE(all.equal(dim(pars_est), dim(pars_true))) ||
    length(pars_est) != length(pars_true)) {
    stop("the dimensions of pars_est and pars_true must be equal")
  }
  calc_evaluation_criterion(pars_est, pars_true, criterion,
    M = NULL,
    tolerance = tolerance
  )
}
