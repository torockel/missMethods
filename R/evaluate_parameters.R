#' Evaluate estimated parameters
#'
#' Compare estimated parameters to true parameters
#'
#' @template evaluation
#'
#' @details The same \code{criterion}s are implemented for \code{evaluate_parameters}
#' and \code{\link{evaluate_imputed_values}}.
#' The possible choices are documented in \code{\link{evaluate_imputed_values}}.
#'
#' @param est_pars a vector or matrix of estimated parameters
#' @param true_pars true parameters, normally a vector or a matrix
#'
#' @export
#'
#' @examples
#' evaluate_parameters(1:4, 2:5, "RMSE")
evaluate_parameters <- function(est_pars, true_pars, criterion = "RMSE",
                                tolerance = sqrt(.Machine$double.eps)) {
  if (!isTRUE(all.equal(dim(est_pars), dim(true_pars))) ||
    length(est_pars) != length(true_pars)) {
    stop("the dimensions of est_pars and true_pars must be equal")
  }
  calc_evaluation_criterion(est_pars, true_pars, criterion,
    M = NULL,
    tolerance = tolerance
  )
}
