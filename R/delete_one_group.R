# the workhorse for delete_MAR_groups and delete_MNAR_groups
delete_one_group <- function(ds, p, miss_cols, ctrl_cols, stochastic = FALSE,
                             cutoff_fun = median, prop = 0.5, use_lpSolve = TRUE,
                             ordered_as_unordered = FALSE, ...) {

  # General checking is done in calling functions.
  # Only special cases are checked here.
  cutoff_fun <- match.fun(cutoff_fun)

  p <- adjust_p(p, miss_cols)

  for (i in seq_along(miss_cols)) {
    groups <- find_groups(
      ds[, ctrl_cols[i]], cutoff_fun, prop, use_lpSolve,
      ordered_as_unordered, ...
    )
    if (is.null(groups$g2)) {
      warning("column ", ctrl_cols[i], " is constant, effectively MCAR")
      ds[, miss_cols[i]] <- delete_MCAR_vec(ds[, miss_cols[i]], p[i], stochastic)
    } else {
      miss_group <- groups[[sample.int(2, 1)]]
      if (length(miss_group) < round(nrow(ds) * p[i], 0)) {
        warning("not enough objects in miss_group in column ", ctrl_cols[i], " to reach p")
        ds[miss_group, miss_cols[i]] <- NA
      } else {
        eff_p <- p[i] * nrow(ds) / length(miss_group)
        ds[miss_group, miss_cols[i]] <- delete_MCAR_vec(ds[miss_group, miss_cols[i]], eff_p, stochastic)
      }
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
#'
# first lines copy + paste from delete_MAR_1_to_x
#' @details
#' At first, the rows of \code{ds} are divided in two groups.
#' Therefore, the \code{cutoff_fun} calculates a cutoff value for
#' \code{ctrl_cols[i]} (via \code{cutoff_fun(ds[, ctrl_cols[i]], ...)}.
#' The group 1 consists of the rows, whose values in
#' \code{ctrl_cols[i]} are below the calculated cutoff value.
#' If the so defined group 1 is empty, the rows that are equal to the
#' cutoff value will be added to this group (otherwise, these rows will
#' belong to group 2).
#' The group 2 consists of the remaining rows, which are not part of group 1.
#' Now one of these two groups is chosen randomly.
#' In the chosen group, values are deleted in \code{miss_cols[i]}.
#' In the other group, no missing values will be created in \code{miss_cols[i]}.
#'
# copy and paste from delete_MAR_one_group !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#'
#' If \code{ds[, ctrl_cols[i]]} is an unordered factor, then the concept of a
#' cutoff value is not meaningful and cannot be applied.
#' Instead, a combinations of the levels of the unordered factor is searched that
#' \itemize{
#' \item{guarantees at least a proportion of \code{prop} rows are in group 1}
#' \item{minimize the difference between \code{prop} and the proportion of
#' rows in group 1.}
#' } This can be seen as a binary search problem, which is solved by the solver
#' from the package \code{lpSolve}, if \code{use_lpSolve = TRUE}.
#' If \code{use_lpSolve = FALSE}, a very simple heuristic is applied.
#' The heuristic only guarantees that at least a proportion of \code{prop} rows
#' are in group 1.
#' The choice \code{use_lpSolve = FALSE} is not recommend and should only be
#' considered, if lpSolve fails.
#' If \code{ordered_as_unordered = TRUE}, then ordered factors will be treated
#' like unordered factors and the same binary search problem will be solved for
#' both types of factors.
#' If \code{ordered_as_unordered = FALSE} (the default), then ordered factors
#' will be grouped via \code{cutoff_fun} as described above.
#'
#' If \code{stochastic = FALSE} (the default), then \code{floor(nrow(ds) * p[i])}
#' or \code{ceiling(nrow(ds) * p[i])} values will be set \code{NA} in
#' column \code{miss_cols[i]} (depending on the grouping).
#' If \code{stochastic = TRUE}, each value in the group with missing values
#' will have a probability to be missing, to meet a proportion of
#' \code{p[i]} of missing values in \code{miss_cols[i]} in expectation.
#' The effect of \code{stochastic} is further discussed in
#' \code{\link{delete_MCAR}}.
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
delete_MAR_one_group <- function(ds, p, miss_cols, ctrl_cols, stochastic = FALSE,
                                 cutoff_fun = median, prop = 0.5, use_lpSolve = TRUE,
                                 ordered_as_unordered = FALSE, ...) {
  check_delete_args_MAR(
    ds = ds, p = p, miss_cols = miss_cols,
    ctrl_cols = ctrl_cols, stochastic = stochastic
  )

  delete_one_group(
    ds = ds, p = p, miss_cols = miss_cols, ctrl_cols = ctrl_cols,
    stochastic = stochastic, cutoff_fun = cutoff_fun, prop = prop,
    use_lpSolve = use_lpSolve,
    ordered_as_unordered = ordered_as_unordered, ...
  )
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
delete_MNAR_one_group <- function(ds, p, miss_cols, stochastic = FALSE,
                                  cutoff_fun = median, prop = 0.5, use_lpSolve = TRUE,
                                  ordered_as_unordered = FALSE, ...) {
  check_delete_args_MNAR(
    ds = ds, p = p, miss_cols = miss_cols,
    stochastic = stochastic
  )

  delete_one_group(
    ds = ds, p = p, miss_cols = miss_cols, ctrl_cols = miss_cols,
    stochastic = stochastic, cutoff_fun = cutoff_fun, prop = prop,
    use_lpSolve = use_lpSolve,
    ordered_as_unordered = ordered_as_unordered, ...
  )
}
