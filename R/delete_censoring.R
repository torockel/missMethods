# the workhorse for delete_MAR_censoring and delete_MNAR_censoring
delete_censoring <- function(ds, p, miss_cols, ctrl_cols, where = "lower", sorting = TRUE) {

  # general checking is done in calling functions delete_MAR_censoring and
  # delete_MNAR_censoring. Only special cases are checked here.
  where <- match.arg(where, c("lower", "upper", "both"))

  p <- adjust_p(p, miss_cols)

  # create missing values ---------------------------------
  for (i in seq_along(miss_cols)) {
    ds_ctrl_cols_i <- ds[, ctrl_cols[i], drop = TRUE]
    if (sorting) {
      n_mis <- round(nrow(ds) * p[i], 0)
      if (n_mis > 0) { # to avoid problems with seq and n_mis == 0
        ordered_indices <- order(ds_ctrl_cols_i)
        na_indices <- switch(where,
          lower = ordered_indices[seq_len(n_mis)],
          upper = ordered_indices[seq(
            from = nrow(ds) - n_mis + 1,
            to = nrow(ds)
          )],
          both = c(
            ordered_indices[seq_len(floor(n_mis / 2))],
            ordered_indices[seq(
              from = nrow(ds) - floor(n_mis / 2) + 1,
              to = nrow(ds)
            )]
          )
        )
      } else { # no missing values
        na_indices <- rep(FALSE, nrow(ds))
      }
    } else { # no sorting -> using quantile()
      if (length(unique(ds_ctrl_cols_i)) == 1L) {
        warning(
          "the column ", ctrl_cols[i], " is constant; no missing values ",
          "created with/in this column"
        )
        na_indices <- rep(FALSE, nrow(ds))
      } else {
        if (is.ordered(ds_ctrl_cols_i)) {
          type <- 1 # better than 3, because of
          # quantile(ordered(1:5), 0.21, type = 1) -> 2
          # quantile(ordered(1:5), 0.21, type = 3) -> 1
        } else {
          type <- 7 # the default for quantile{stats}
        }
        if (where == "lower") {
          na_indices <- ds_ctrl_cols_i < stats::quantile(ds_ctrl_cols_i,
            p[i],
            type = type
          )
        } else if (where == "upper") {
          na_indices <- ds_ctrl_cols_i > stats::quantile(ds_ctrl_cols_i,
            1 - p[i],
            type = type
          )
        } else if (where == "both") {
          na_indices_lower <- ds_ctrl_cols_i < stats::quantile(ds_ctrl_cols_i,
            p[i] / 2,
            type = type
          )
          na_indices_upper <-ds_ctrl_cols_i > stats::quantile(ds_ctrl_cols_i,
            1 - p[i] / 2,
            type = type
          )
          na_indices <- na_indices_lower | na_indices_upper
        }
      }
    }
    ds[na_indices, miss_cols[i]] <- NA
  }
  ds
}


#' Create MAR values using a censoring mechanism
#'
#' Create missing at random (MAR) values using a censoring mechanism in a data
#' frame or a matrix
#'
#' @template delete
#' @template MAR
#'
#' @details If \code{sorting = TRUE} (the default), the column
#' \code{ctrl_cols[i]} will be sorted. Then the rows with the
#' \code{round(nrow(ds) * p[i])} smallest values will be selected (if
#' \code{where = "lower"} (the default)). Now missing values will be created in
#' the column \code{miss_cols[i]} in these rows. This effectively censors the
#' proportion of \code{p[i]} rows of smallest values in \code{ctrl_cols[i]} in
#' \code{miss_cols[i]}.
#'
#' If \code{where = "upper"}, instead of the rows with the smallest values, the
#' rows with the highest values will be selected. For \code{where = "both"}, the
#' one half of the \code{round(nrow(ds) * p[i])} rows with missing values will
#' be the rows with the smallest values and the other half will be the rows with
#' the highest values. So the censoring rows are dived to the highest and
#' smallest values of \code{ctrl_cols[i]}.
#'
#' If \code{sorting = FALSE}, the rows of \code{ds} will not be sorted. Instead,
#' a quantile will be calculated (using \code{\link[stats]{quantile}}). If
#' \code{where = "lower"}, the \code{quantile(ds[, ctrl_cols[i]], p[i])} will be
#' calculated and all rows with values in \code{ds[, ctrl_cols[i]]} below this
#' quantile will have missing values in \code{miss_cols[i]}. For \code{where =
#' "upper"}, the \code{quantile(ds[, ctrl_cols[i]], 1 - p[i])} will be
#' calculated and all rows with values above this quantile will have missing
#' values. For \code{where = "both"}, the \code{quantile(ds[, ctrl_cols[i]],
#' p[i] / 2)} and \code{quantile(ds[, ctrl_cols[i]], 1 -  p[i] / 2)} will be
#' calculated. All rows with values in \code{ctrl_cols[i]} below the first
#' quantile or above the second quantile will have missing values in
#' \code{miss_cols[i]}.
#'
#' The option \code{sorting = TRUE} will always create exactly
#' \code{round(nrow(ds) * p[i])} missing values in \code{miss_cols[i]}. For
#' \code{sorting = FALSE}, the number of missing values will normally be close
#' to \code{nrow(ds) * p[i]}. But for \code{ctrl_cols} with many duplicates the
#' choice \code{sorting = FALSE} can be problematic, because of the calculation
#' of \code{quantile(ds[, ctrl_cols[i]], p[i])} and setting values \code{NA}
#' below this threshold (see examples). So, in most cases \code{sorting = TRUE}
#' is recommended.
#'
#'
#' @param where controls where missing values are created; one of "lower",
#'   "upper" or "both" (see details)
#' @param sorting logical; should sorting be used or a quantile as a threshold
#'
#' @export
#'
#' @seealso \code{\link{delete_MNAR_censoring}}
#'
#' @examples
#' ds <- data.frame(X = 1:20, Y = 101:120)
#' delete_MAR_censoring(ds, 0.2, "X", "Y")
#' # many dupplicated values can be problematic for sorting = FALSE:
#' ds_many_dup <- data.frame(X = 1:20, Y = c(rep(0, 10), rep(1, 10)))
#' delete_MAR_censoring(ds_many_dup, 0.2, "X", "Y") # 4 NAs as expected
#' quantile(ds_many_dup$Y, 0.2) # 0
#' # No value is BELOW 0 in ds_many_dup$Y, so no missing values will be created:
#' delete_MAR_censoring(ds_many_dup, 0.2, "X", "Y", sorting = FALSE) # No NA!
delete_MAR_censoring <- function(ds, p, miss_cols, ctrl_cols, where = "lower", sorting = TRUE) {

  # arg stochastic not used! (and method is not stochastic)
  check_delete_args_MAR(
    ds = ds, p = p, miss_cols = miss_cols,
    ctrl_cols = ctrl_cols, stochastic = FALSE
  )

  delete_censoring(
    ds = ds, p = p, miss_cols = miss_cols, ctrl_cols = ctrl_cols,
    where = where, sorting = sorting
  )
}



#' Create MNAR values using a censoring mechanism
#'
#' Create missing not at random (MNAR) values using a censoring mechanism in a
#' data frame or a matrix
#'
#'
#' @eval MNAR_documentation("censoring")
#'
#' @examples
#' delete_MNAR_censoring(ds, 0.2, "X")
delete_MNAR_censoring <- function(ds, p, miss_cols, where = "lower", sorting = TRUE) {

  # arg stochastic not used! (and method is not stochastic)
  check_delete_args_MNAR(
    ds = ds, p = p, miss_cols = miss_cols,
    stochastic = FALSE
  )

  delete_censoring(
    ds = ds, p = p, miss_cols = miss_cols, ctrl_cols = miss_cols,
    where = where, sorting = sorting
  )
}
