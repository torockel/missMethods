# the workhorse for delete_MAR_rank and delete_MNAR_rank
delete_rank <- function(ds, p, cols_mis, cols_ctrl, n_mis_stochastic,
                        ties.method = "average") {
  for (i in seq_along(cols_mis)) {
    p_ranks <- rank(ds[, cols_ctrl[i], drop = TRUE])
    na_indices <- get_NA_indices(
      n_mis_stochastic = n_mis_stochastic,
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
#' At first, the probability for a value to be missing is calculated. This
#' probability for a missing value in a row of \code{cols_mis[i]} is
#' proportional to the rank of the value in \code{cols_ctrl[i]} in the same row.
#' If \code{n_mis_stochastic = FALSE} these probabilities are given to the
#' \code{prob} argument of \code{\link[base]{sample}}. If \code{n_mis_stochastic
#' = TRUE}, they are scaled to sum up to \code{nrow(ds) * p[i]}. Then for each
#' probability a uniformly distributed random number is generated. If this
#' random number is less than the probability, the value in \code{cols_mis[i]}
#' is set \code{NA}.
#'
#' The ranks are calculated via \code{\link[base]{rank}}.
#' The argument \code{ties.method} is directly passed to this function.
#' Possible choices for \code{ties.method} are documented in
#' \code{\link[base]{rank}}.
#'
#' For high values of \code{p} it is mathematically not possible to get
#' probabilities proportional to the ranks. In this case, a warning is given.
#' This warning can be silenced by setting the option
#' \code{missMethods.warn.too.high.p} to false.
#'
#' @param ties.method How ties are handled. Passed to \code{\link[base]{rank}}.
#'
#' @export
#' @seealso \code{\link[base]{rank}}, \code{\link{delete_MNAR_rank}}
#' @examples
#' ds <- data.frame(X = 1:20, Y = 101:120)
#' delete_MAR_rank(ds, 0.2, "X", "Y")
delete_MAR_rank <- function(ds, p, cols_mis, cols_ctrl, n_mis_stochastic = FALSE,
                            ties.method = "average",
                            miss_cols, ctrl_cols) {
  do.call(delete_values, c(
    list(mech_type = "MAR_rank"),
    as.list(environment())
  ))
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
delete_MNAR_rank <- function(ds, p, cols_mis, n_mis_stochastic = FALSE,
                             ties.method = "average",
                             miss_cols) {
  do.call(delete_values, c(
    list(mech_type = "MNAR_rank"),
    as.list(environment())
  ))
}
