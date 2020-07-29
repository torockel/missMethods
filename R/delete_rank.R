# the workhorse for delete_MAR_rank and delete_MNAR_rank
delete_rank <- function(ds, p, cols_mis, cols_ctrl,
                        ties.method = "average") {

  # General checking is done in calling functions delete_MAR_rank and
  # delete_MNAR_rank. Only special cases are checked here.

  p <- adjust_p(p, cols_mis)

  for (i in seq_along(cols_mis)) {
    n_mis <- round(nrow(ds) * p[i])
    if (n_mis > 0L) {
      p_ranks <- rank(ds[, cols_ctrl[i], drop = TRUE])
      p_ranks <- p_ranks / sum(p_ranks)
      na_indices <- sample.int(nrow(ds), n_mis, prob = p_ranks)
      ds[na_indices, cols_mis[i]] <- NA
    }
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
delete_MAR_rank <- function(ds, p, cols_mis, cols_ctrl,
                            ties.method = "average",
                            miss_cols, ctrl_cols) {

  ## deprecate miss_cols
  if (!missing(miss_cols)) {
    warning("miss_cols is deprecated; use cols_mis instead.")
    cols_mis <- miss_cols
  }

  ## deprecate ctrl_cols
  if (!missing(ctrl_cols)) {
    warning("ctrl_cols is deprecated; use cols_ctrl instead.")
    cols_ctrl <- ctrl_cols
  }

  # arg stochastic not used (and method is not stochastic)
  check_delete_args_MAR(
    ds = ds, p = p, cols_mis = cols_mis,
    cols_ctrl = cols_ctrl, stochastic = FALSE
  )

  delete_rank(
    ds = ds, p = p, cols_mis = cols_mis, cols_ctrl = cols_ctrl,
    ties.method = ties.method
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
delete_MNAR_rank <- function(ds, p, cols_mis, ties.method = "average",
                             miss_cols) {

  ## deprecate miss_cols
  if (!missing(miss_cols)) {
    warning("miss_cols is deprecated; use cols_mis instead.")
    cols_mis <- miss_cols
  }

  # arg stochastic not used! (and method is not stochastic)
  check_delete_args_MNAR(
    ds = ds, p = p, cols_mis = cols_mis,
    stochastic = FALSE
  )

  delete_rank(
    ds = ds, p = p, cols_mis = cols_mis, cols_ctrl = cols_mis,
    ties.method = ties.method
  )
}
