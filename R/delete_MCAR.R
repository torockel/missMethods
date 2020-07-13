#' Create MCAR values
#'
#' Create missing completely at random (MCAR) values in a data frame or a matrix
#'
#' @template delete
#' @template delete-stochastic
#'
#' @details This function creates missing completely at random (MCAR) values in the
#' dataset \code{ds}.
#' The proportion of missing values is specified with \code{p}.
#' The columns in which missing values are created can be set via \code{cols_miss}.
#' If \code{cols_miss} is not specified, then missing values are created in
#' every column.
#'
#  part about p copied from man-roxygen/MAR.R !!!!!!
#
#' The probability for missing values is controlled by \code{p}. If \code{p} is
#' a single number, then the overall probability for a value to be missing will
#' be \code{p} in all columns of \code{cols_miss}. (Internally \code{p} will be
#' replicated to a vector of the same length as \code{cols_miss}. So, all
#' \code{p[i]} in the following sections will be equal to the given single
#' number \code{p}.) Otherwise, \code{p} must be of the same length as
#' \code{cols_miss}. In this case, the overall probability for a value to be
#' missing will be \code{p[i]} in the column \code{cols_miss[i]}.
#'
#' If \code{stochastic = FALSE} and \code{p_overall = FALSE} (the default), then
#' exactly \code{round(nrow(ds) * p[i])} values will be set \code{NA} in column
#' \code{cols_miss[i]}. If \code{stochastic = FALSE} and \code{p_overall =
#' TRUE}, then \code{p} must be of length one and exactly \code{round(nrow(ds) *
#' p * length(cols_miss))} values will be set NA (over all columns in
#' \code{cols_miss}). This can result in a proportion of missing values in every
#' \code{miss_col} unequal to \code{p}, but the proportion of missing values in
#' all columns together will be close to \code{p}.
#'
#' If \code{stochastic = TRUE}, then each value in column \code{cols_miss[i]}
#' has the probability \code{p[i]} to be missing. In this case, the number of
#' missing values in \code{cols_miss[i]} is a random variable with a binomial
#' distribution \emph{B}(\code{nrow(ds)}, \code{p[i]}). This can (and will most of the time)
#' lead to more or less missing values than \code{round(nrow(ds) * p[i])} in
#' each column. If \code{stochastic = TRUE}, then the argument \code{p_overall}
#' is ignored because it is superfluous.
#'
#' @param p_overall logical; see details
#'
#' @export
#'
#' @examples
#' ds <- data.frame(X = 1:20, Y = 101:120)
#' delete_MCAR(ds, 0.2)
delete_MCAR <- function(ds, p, cols_miss = seq_len(ncol(ds)),
                        stochastic = FALSE, p_overall = FALSE, miss_cols) {

  ## deprecate miss_cols
  if (!missing(miss_cols)) {
    warning("miss_cols is deprecated; use cols_miss instead.")
    cols_miss <- miss_cols
  }

  ## checks and corrections for p -----------------------------------
  check_delete_args_MCAR(
    ds = ds, p = p, cols_miss = cols_miss,
    stochastic = stochastic, p_overall = p_overall
  )

  check_delete_args(ds = ds, p = p, cols_miss = cols_miss, stochastic = stochastic)

  p <- adjust_p(p, cols_miss)

  # create missing values -----------------------
  n <- nrow(ds)
  if (!p_overall || stochastic) {
    for (i in seq_along((cols_miss))) {
      ds[, cols_miss[i]] <- delete_MCAR_vec(ds[, cols_miss[i], drop = TRUE], p[i], stochastic)
    }
  } else { # p_overall = FALSE && stochastich = FALSE
    na_indices_overall <- sample.int(
      n * length(cols_miss),
      round(n * length(cols_miss) * p[1])
    )
    for (i in seq_along(cols_miss)) {
      na_indices <- na_indices_overall[(na_indices_overall - 1L) %/% n == (i - 1L)]
      na_indices <- ((na_indices - 1L) %% n) + 1L
      ds[na_indices, cols_miss[i]] <- NA
    }
  }
  ds
}

# delete values MCAR in a single vector
delete_MCAR_vec <- function(x, p, stochastic) {
  n <- length(x)
  if (stochastic) {
    na_indices <- which(sample(c(TRUE, FALSE), n,
      replace = TRUE, prob = c(p, 1 - p)
    ))
    # na_indices <- which(stats::runif(n) < p) # old
  } else {
    na_indices <- sample.int(n, round(n * p, digits = 0))
  }
  x[na_indices] <- NA
  x
}
