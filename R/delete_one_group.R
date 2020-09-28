# the workhorse for delete_MAR_groups and delete_MNAR_groups
delete_one_group <- function(ds, p, cols_mis, cols_ctrl,
                             cutoff_fun = median,
                             prop = 0.5, use_lpSolve = TRUE,
                             ordered_as_unordered = FALSE,
                             n_mis_stochastic = FALSE,
                             ...) {

  # General checking of arguments is done in delete_values().
  # Only special cases are checked here.
  cutoff_fun <- match.fun(cutoff_fun)

  for (i in seq_along(cols_mis)) {
    groups <- find_groups(
      ds[, cols_ctrl[i], drop = TRUE], cutoff_fun, prop, use_lpSolve,
      ordered_as_unordered, ...
    )
    if (is.null(groups$g2)) {
      warning("column ", cols_ctrl[i], " is constant, effectively MCAR")
      ds[, cols_mis[i]] <- delete_MCAR_vec(
        ds[, cols_mis[i], drop = TRUE],
        p[i], n_mis_stochastic
      )
    } else {
      miss_group <- groups[[sample.int(2, 1)]]
      prob_weights <- rep(0, nrow(ds))
      prob_weights[miss_group] <- 1
      na_indices <- get_NA_indices(n_mis_stochastic, n = nrow(ds), p = p[i], prob = prob_weights)
      ds[na_indices, cols_mis[i]] <- NA
    }
  }
  ds
}


#' Create MAR values by deleting values in one of two groups
#'
#' Create missing at random (MAR) values by deleting values in one of two
#' groups in a data frame or a matrix
#'
#' @template delete
#' @template delete-stochastic
#' @template MAR
#' @template factor-grouping
#'
# first lines copy + paste from delete_MAR_1_to_x
#' @details
#' At first, the rows of \code{ds} are divided into two groups.
#' Therefore, the \code{cutoff_fun} calculates a cutoff value for
#' \code{cols_ctrl[i]} (via \code{cutoff_fun(ds[, cols_ctrl[i]], ...)}.
#' The group 1 consists of the rows, whose values in
#' \code{cols_ctrl[i]} are below the calculated cutoff value.
#' If the so defined group 1 is empty, the rows that are equal to the
#' cutoff value will be added to this group (otherwise, these rows will
#' belong to group 2).
#' The group 2 consists of the remaining rows, which are not part of group 1.
#' Now one of these two groups is chosen randomly.
#' In the chosen group, values are deleted in \code{cols_mis[i]}.
#' In the other group, no missing values will be created in \code{cols_mis[i]}.
#'
#' If \code{p} is too high, it is possible that a group contains not enough
#' objects to reach \code{nrow(ds) * p} missing values. In this case, \code{p}
#' is reduced to the maximum possible value (given the (random) group with
#' missing data) and a warning is given. Obviously this case will occur
#' regularly, if \code{p > 0.5}. Therefore, this function should normally not be
#' called with \code{p > 0.5}. However, this can occur for smaller values
#' of \code{p}, too (depending on the grouping). The warning can be silenced by
#' setting the option \code{missMethods.warn.too.high.p} to false.
#'
#' @inheritParams delete_MAR_1_to_x
#'
#' @export
#'
#' @seealso \code{\link{delete_MNAR_one_group}}
#'
#' @examples
#' ds <- data.frame(X = 1:20, Y = 101:120)
#' delete_MAR_one_group(ds, 0.2, "X", "Y")
delete_MAR_one_group <- function(ds, p, cols_mis, cols_ctrl,
                                 cutoff_fun = median, prop = 0.5,
                                 use_lpSolve = TRUE,
                                 ordered_as_unordered = FALSE,
                                 n_mis_stochastic = FALSE, ...,
                                 miss_cols, ctrl_cols, stochastic) {
  do.call(delete_values, c(
    list(mechanism = "MAR", mech_type = "one_group"),
    as.list(environment()), list(...)
  ))
}


#' Create MNAR values by deleting values in one of two groups
#'
#' Create missing not at random (MNAR) values by deleting values in one of two
#' groups in a data frame or a matrix
#'
#' @eval MNAR_documentation("one_group")
#'
#' @examples
#' delete_MNAR_one_group(ds, 0.2, "X")
delete_MNAR_one_group <- function(ds, p, cols_mis,
                                  cutoff_fun = median, prop = 0.5,
                                  use_lpSolve = TRUE,
                                  ordered_as_unordered = FALSE,
                                  n_mis_stochastic = FALSE, ...,
                                  miss_cols, stochastic) {

  do.call(delete_values, c(
    list(mechanism = "MNAR", mech_type = "one_group"),
    as.list(environment()), list(...)
  ))
}
