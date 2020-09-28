#' @param ds A data frame or matrix in which missing values will be created.
#' @param p A numeric vector with length one or equal to length \code{cols_mis};
#'   the probability that a value is missing.
#' @param cols_mis A vector of column names or indices of columns in which
#'   missing values will be created.
#' @param n_mis_stochastic Logical, should the number of missing values be
#'   stochastic? If \code{n_mis_stochastic = TRUE}, the number of missing values
#'   for a column with missing values \code{cols_mis[i]} is a random variable
#'   with expected value \code{nrow(ds) * p[i]}. If \code{n_mis_stochastic =
#'   FALSE}, the number of missing values will be deterministic. Normally, the
#'   number of missing values for a column with missing values
#'   \code{cols_mis[i]} is \code{round(nrow(ds) * p[i])}. Possible deviations
#'   from this value, if any exists, are documented in Details.
#' @param miss_cols Deprecated, use \code{cols_mis} instead.
#'
#'
#' @return An object of the same class as \code{ds} with missing values.
#'
#' @references Santos, M. S., Pereira, R. C., Costa, A. F., Soares, J. P.,
#'   Santos, J., & Abreu, P. H. (2019). Generating Synthetic Missing Data: A
#'   Review by Missing Mechanism. \emph{IEEE Access}, 7, 11651-11667
