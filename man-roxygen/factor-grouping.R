#' @section Treatment of factors:
#'
#' If \code{ds[, ctrl_cols[i]]} is an unordered factor, then the concept of a
#' cutoff value is not meaningful and cannot be applied.
#' Instead, a combinations of the levels of the unordered factor is searched that
#' \itemize{
#' \item{guarantees at least a proportion of \code{prop} rows are in group 1}
#' \item{minimize the difference between \code{prop} and the proportion of
#' rows in group 1.}
#' }
#' This can be seen as a binary search problem, which is solved by the solver
#' from the package \code{lpSolve}, if \code{use_lpSolve = TRUE}.
#' If \code{use_lpSolve = FALSE}, a very simple heuristic is applied.
#' The heuristic only guarantees that at least a proportion of \code{prop} rows
#' are in group 1.
#' The choice \code{use_lpSolve = FALSE} is not recommend and should only be
#' considered, if the solver of lpSolve fails.
#'
#' If \code{ordered_as_unordered = TRUE}, then ordered factors will be treated
#' like unordered factors and the same binary search problem will be solved for
#' both types of factors.
#' If \code{ordered_as_unordered = FALSE} (the default), then ordered factors
#' will be grouped via \code{cutoff_fun} as described in Details.
