# the workhorse for delete_MAR_rank and delete_MNAR_rank
delete_rank <- function(ds, p, cols_mis, cols_ctrl, stochastic,
                        ties.method = "average") {

  # General checking is done in calling functions delete_MAR_rank and
  # delete_MNAR_rank. Only special cases are checked here.

  p <- adjust_p(p, cols_mis)

  for (i in seq_along(cols_mis)) {
    p_ranks <- rank(ds[, cols_ctrl[i], drop = TRUE])
    na_indices <- get_NA_indices(
      stochastic = stochastic,
      n = nrow(ds), p = p[i], prob = p_ranks
    )
    ds[na_indices, cols_mis[i]] <- NA
  }
  ds
}


#' Create MAR values using a ranking mechanism
#'
#' Create missing at random (MAR) values using a ranking mechanism in a data
#' frame or a matrix
#'
#' @template delete
#' @template MAR
#'
#' @details
#' The probability for a missing value in a row of \code{cols_mis[i]} is
#' proportional to the rank of the value in \code{cols_ctrl[i]} in the same row.
#' In total \code{round(nrow(ds) * p[i])} missing values are created in
#' \code{cols_mis[i]}.
#' The ranks are calculated via \code{\link[base]{rank}}.
#' The argument \code{ties.method} is directly passed to this function.
#' Possible choices for \code{ties.method} are documented in
#' \code{\link[base]{rank}}.
#'
#' @param ties.method How ties are handled. Passed to \code{\link[base]{rank}}.
#'
#' @export
#' @seealso \code{\link[base]{rank}}, \code{\link{delete_MNAR_rank}}
#' @examples
#' ds <- data.frame(X = 1:20, Y = 101:120)
#' delete_MAR_rank(ds, 0.2, "X", "Y")
delete_MAR_rank <- function(ds, p, cols_mis, cols_ctrl, stochastic = FALSE,
                            ties.method = "average",
                            miss_cols, ctrl_cols) {

  # Deprecate miss_cols, ctrl_cols
  check_renamed_arg(miss_cols, cols_mis)
  check_renamed_arg(ctrl_cols, cols_ctrl)

  check_delete_args_MAR(
    ds = ds, p = p, cols_mis = cols_mis,
    cols_ctrl = cols_ctrl, stochastic = stochastic
  )

  delete_rank(
    ds = ds, p = p, cols_mis = cols_mis, cols_ctrl = cols_ctrl,
    stochastic = stochastic, ties.method = ties.method
  )
}

#' Create MNAR values using a ranking mechanism
#'
#' Create missing not at random (MNAR) values using a ranking mechanism in a
#' data frame or a matrix
#'
#' @eval MNAR_documentation("rank")
#'
#' @examples
#' delete_MNAR_rank(ds, 0.2, "X")
delete_MNAR_rank <- function(ds, p, cols_mis, stochastic = FALSE,
                             ties.method = "average",
                             miss_cols) {

  # Deprecate miss_cols
  check_renamed_arg(miss_cols, cols_mis)

  # arg stochastic not used! (and method is not stochastic)
  check_delete_args_MNAR(
    ds = ds, p = p, cols_mis = cols_mis,
    stochastic = stochastic
  )

  delete_rank(
    ds = ds, p = p, cols_mis = cols_mis, cols_ctrl = cols_mis,
    stochastic = stochastic, ties.method = ties.method
  )
}
