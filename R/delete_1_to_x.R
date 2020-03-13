# the workhorse for delete_MAR_1_to_x and delete_MNAR_1_to_x
delete_1_to_x <- function(ds, p, x, miss_cols, ctrl_cols,
                          cutoff_fun = median,
                          stochastic = FALSE,
                          add_realized_x = FALSE,
                          prop = 0.5,
                          use_lpSolve = TRUE,
                          ordered_as_unordered = FALSE,...) {

  # general checking is done in calling functions delete_MAR_1_to_x and
  # delete_MNAR_1_to_x, only special cases are checked here
  # check if ctrl_cols are numeric or ordered factor
  check_ctrl_cols_1_to_x(ds, ctrl_cols)

  # match cutoff_fun
  cutoff_fun <- match.fun(cutoff_fun)

  #check x
  if (length(x) != 1L || !is.numeric(x)) {
    stop("x must be a single number")
  } else if (x <= 0 || is.infinite(x)) {
    stop("x must be greater than 0 and finite")
  }

  p <- adjust_p(p, miss_cols)

  # create missing values -----------------------
  n <- nrow(ds)
  true_odds <- numeric(length(miss_cols))
  for(i in seq_along(miss_cols)) {
    groups <- find_groups(ds[, ctrl_cols[i]], cutoff_fun, prop, use_lpSolve,
                          ordered_as_unordered, ...)
    if (is.null(groups$g2)) {
      warning("column ", ctrl_cols[i], " is constant, effectively MCAR")
      ds[, miss_cols[i]] <- delete_MCAR_vec(ds[, miss_cols[i]], p[i], stochastic)
      true_odds[i] <- 0
    } else {

      # calculate p_miss for group 1 and group 2
      nr_g1 <- length(groups$g1)
      nr_g2 <- length(groups$g2)
      p_miss_g1 <- p[i] * n / (nr_g1 + nr_g2 * x)
      p_miss_g2 <- p[i] * n * x / (nr_g1 + nr_g2 * x)
      # check if p_miss_g1 or p_miss_g2 is out of range (>1)
      if (p_miss_g2 > 1) {
        x_max_i <- nr_g2 / (n * p[i] - nr_g1)
        warning("p (or x) is too high; x is set to ", x_max_i,
                " to get expected n * p missing values")
        p_miss_g2 <- 1 # setting x = x_max_i results in p_miss_g2 = 1
        p_miss_g1 <- (p[i] * n  - nr_g2)  / nr_g1
      } else if (p_miss_g1 > 1) {
        x_min_i <- (n * p[i] - nr_g2) / nr_g1
        warning("p is too high or x to low; x is set to ", x_min_i,
                " to get expected n * p missing values")
        p_miss_g1 <- 1
        p_miss_g2 <- (p[i] * n  - nr_g1)  / nr_g2
      }

      # delete values
      if(stochastic) { # stochastic = TRUE ------------------
        na_indices_g1 <- groups$g1[sample(c(TRUE, FALSE),
                                         nr_g1 ,
                                         replace = TRUE,
                                         prob = c(p_miss_g1,
                                                  1 - p_miss_g1))]
        na_indices_g2 <- groups$g2[sample(c(TRUE, FALSE),
                                          nr_g2 ,
                                          replace = TRUE,
                                          prob = c(p_miss_g2,
                                                   1 - p_miss_g2))]


      } else { # stochastic = FALSE ------------------
        nr_miss <- round(p[i] * n)
        nr_miss_g1 <- calc_nr_miss_g1(nr_g1, p_miss_g1, nr_g2, nr_miss, x)
        nr_miss_g2 <- nr_miss - nr_miss_g1

        # sample NA indices
        na_indices_g2 <- resample(groups$g2, nr_miss_g2)
        na_indices_g1 <- resample(groups$g1, nr_miss_g1)
      }
      na_indices <- c(na_indices_g1, na_indices_g2)
      ds[na_indices, miss_cols[i]] <- NA
      true_odds[i] <- (length(na_indices_g1) / nr_g1) /
        (length(na_indices_g2) / nr_g2)
    }
  }

  if (add_realized_x) {
    realized_x <- 1 / true_odds
    names(realized_x) <- miss_cols
    return(structure(ds, realized_x = realized_x))
  }
  ds
}




#' Create MAR values using MAR1:x
#'
#' Create missing at random (MAR) values using MAR1:x in a data frame or
#' a matrix.
#'
#' @template delete
#' @template delete-stochastic
#' @template MAR
#'
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
#' Now the probabilities for the rows in the two groups are set in the way
#' that the odds are 1:x against a missing value in \code{miss_cols[i]} for the
#' rows in group 1 compared to the rows in group 2.
#' That means, the propability for a value to be missing in group 1 divided by
#' the propability for a value to be missing in group 2 equals 1 divided
#' by x.
#' For example, for two equal sized groups 1 and 2, ideally the number of NAs in
#' group 1 divided by the number of NAs in group 2 should equal 1 divided by x.
#' But there are some restrictions, which can lead to some defiations from the
#' odds 1:x (see below).
#'
# copy and paste to delete_MAR_1_to_x !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
#' If \code{stochastic = FALSE} (the default),
#' then exactly \code{round(nrow(ds) * p[i])} values will be set \code{NA} in
#' column \code{miss_cols[i]}.
#' To achieve this, it is possible that the true odds differ from 1:x.
#' The number of observations that are deleted in group 1 and group 2 are
#' chosen to minimize the absolute difference between the realized odds and 1:x.
#' Furthermore, if \code{round(nrow(ds) * p[i])} == 0, then no missing value
#' will be created in \code{miss_cols[i]}.
#' If \code{stochastic = TRUE}, the number of missing values in \code{miss_cols[i]}
#' is a random variable.
#' This random variable is a sum of two binomial distributed variables (one for
#' group 1 and one for group 2).
#' If \code{p} is not too high and \code{x} is not too high or to low (see
#' below), then the odds 1:x will be met in expectation.
#' But in a single dataset the odds will be unequal to 1:x most of the time.
#'
#' If \code{p} is high and \code{x} is too high or too low, it is possible that
#' the odds 1:x and the proportion of missing values \code{p} cannot be
#' realized together.
#' For example, if p = 0.9, then a maximum of x = 1.25 is possible (assuming that
#' exactly 50 \% of the values are below and 50 \% of the values are above the
#' cutoff value in ctrl_col).
#' If a combination of \code{p} and \code{x} that cannot be realized together
#' is given to \code{delete_MAR_1_to_x}, then a warning will be generated and
#' \code{x} will be adjusted in such a way that \code{p} can be realized as
#' given to the function.
#'
#' The argument \code{add_realized_x} controls whether the x of the realized
#' odds are added to the return value or not.
#' If \code{add_realized_x = TRUE}, then the x values for all \code{miss_cols}
#' will be added as an attribute to the returned object.
#' For \code{stochastic = TRUE} these realized x will differ from the given
#' \code{p} most of the time and will change if the function is rerun without
#' setting a seed.
#' For \code{stochastic = FALSE}, it is also possible that the realized odds
#' differ (see above). However, the realized odds will be constant over multiple
#' runs.
#'
#' @param x numeric with length one (0 < x < \code{Inf}); odds are 1 to x  for a missing in group 1
#' against a missing in group 2
#' (see details)
#' @param cutoff_fun function that calculates the cutoff values in the
#' \code{ctrl_cols}
#' @param add_realized_x logical; if TRUE the realized odds for miss_cols will be
#' returned (as attribute)
#' @param prop numeric of length one; (minimum) proportion of rows in group 1
#' @param use_lpSolve logical; should lpSolve be used for the determination of
#' gropus, if \code{ctrl_cols[i]} is an unorderd factor
#' @param ordered_as_unordered logical; should ordered factors be treated like
#' unordered factors
#' @param ... further arguments passed to \code{cutoff_fun}
#'
#' @export
#' @seealso \code{\link{delete_MNAR_1_to_x}}
#'
#' @examples
#' ds <- data.frame(X = 1:20, Y = 101:120)
#' delete_MAR_1_to_x(ds, 0.2, 3, "X", "Y")
#' # beware of small datasets and stochastic = FALSE
#' attr(delete_MAR_1_to_x(ds, 0.4, 3, "X", "Y", add_realized_x = TRUE), "realized_x")
#' attr(delete_MAR_1_to_x(ds, 0.4, 4, "X", "Y", add_realized_x = TRUE), "realized_x")
#' attr(delete_MAR_1_to_x(ds, 0.4, 5, "X", "Y", add_realized_x = TRUE), "realized_x")
#' attr(delete_MAR_1_to_x(ds, 0.4, 7, "X", "Y", add_realized_x = TRUE), "realized_x")
#' # p = 0.4 and 20 values -> 8 missing values, possible combinations:
#' # either 6 above 2 below (x = 3) or
#' # 7 above and 1 below (x = 7)
#' # Too high combination of p and x:
#' delete_MAR_1_to_x(ds, 0.9, 3, "X", "Y")
#' delete_MAR_1_to_x(ds, 0.9, 3, "X", "Y", stochastic = TRUE)
delete_MAR_1_to_x <- function(ds, p, x, miss_cols, ctrl_cols,
                              cutoff_fun = median,
                              stochastic = FALSE,
                              add_realized_x = FALSE,
                              prop = 0.5,
                              use_lpSolve = TRUE,
                              ordered_as_unordered = FALSE, ...) {

  check_delete_args_MAR(ds = ds, p = p, miss_cols = miss_cols,
                          ctrl_cols = ctrl_cols, stochastic = stochastic)

  delete_1_to_x(ds, p, x, miss_cols, ctrl_cols, cutoff_fun, stochastic,
                add_realized_x, prop = prop,
                use_lpSolve = use_lpSolve,
                ordered_as_unordered = ordered_as_unordered, ...)
}


#' Create MNAR values using MNAR1:x
#'
#' Create missing not at random (MNAR) values using MNAR1:x in a data frame or
#' a matrix
#'
#'
#' @eval MNAR_documentation("1_to_x")
#'
#' @examples delete_MNAR_1_to_x(ds, 0.2, 3, "X")
delete_MNAR_1_to_x <-function(ds, p, x, miss_cols,
                              cutoff_fun = median,
                              stochastic = FALSE,
                              add_realized_x = FALSE,
                              prop = 0.5,
                              use_lpSolve = TRUE,
                              ordered_as_unordered = FALSE,...) {

  check_delete_args_MNAR(ds = ds, p = p, miss_cols = miss_cols,
                         stochastic = stochastic)

  delete_1_to_x(ds, p, x, miss_cols, ctrl_cols = miss_cols, cutoff_fun,
                stochastic, add_realized_x, prop = prop,
                use_lpSolve = use_lpSolve,
                ordered_as_unordered = ordered_as_unordered, ...)
}
