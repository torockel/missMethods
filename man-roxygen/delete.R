#' @param ds a data frame or matrix in which missing values will be created
#' @param p a numeric vector with length one or equal to length \code{cols_miss};
#' the probability that a value is missing
#' @param cols_miss a vector of column names or indices of columns in which
#' missing values will be created
#' @param miss_cols deprecated, use cols_miss instead
#'
#'
#' @return An object of the same class as \code{ds} with missing values
#'
#' @references Santos, M. S., Pereira, R. C., Costa, A. F., Soares, J. P.,
#'   Santos, J., & Abreu, P. H. (2019). Generating Synthetic Missing Data: A
#'   Review by Missing Mechanism. \emph{IEEE Access}, 7, 11651-11667
