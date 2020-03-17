#' Evaluate estimated parameters
#'
#' Compare estimated parameters to true parameters
#'
#' The same \code{criterion}s are implemented for \code{evaluate_parameters}
#' and \code{\link{evaluate_imputed_values}}.
#' The possible choices are documented in \code{\link{evaluate_imputed_values}}.
#'
#' @param est_par a vector or matrix of estimated parameters
#' @param true_par a vector or matrix of true parameters
#' @param criterion a string specifying the used criterion for comparing the
#' estimated and true parameters
#' @inheritParams evaluate_imputed_values
#'
#' @return a numeric vector of length one
#' @export
#'
#' @examples
#' evaluate_parameters(1:4, 2:5, "RMSE")
evaluate_parameters <- function(est_par, true_par, criterion = "RMSE",
                                tolerance = sqrt(.Machine$double.eps)) {
  if (!isTRUE(all.equal(dim(est_par), dim(true_par))) ||
      length(est_par) != length(true_par)) {
    stop("the dimensions of est_par and true_par must be equal")
  }
  calc_evaluation_criterion(est_par, true_par, criterion, M = NULL,
                            tolerance = tolerance)
}
