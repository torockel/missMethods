#' @family functions to create MAR
#' @param cols_ctrl a vector of column names or indices of columns, which
#' controls the creation of missing values in \code{cols_miss}. Must be of the
#' same length as \code{cols_miss}.
#' @param ctrl_cols deprecated, use cols_ctrl instead
#'
#' @details This function creates missing at random (MAR) values in the columns
#' specified by the argument \code{cols_miss}.
#' The probability for missing values is controlled by \code{p}.
#' If \code{p} is a single number, then the overall probability for a value to
#' be missing will be \code{p} in all columns of \code{cols_miss}.
#' (Internally \code{p} will be replicated to a vector of the same length as
#' \code{cols_miss}.
#' So, all \code{p[i]} in the following sections will be equal to the given
#' single number \code{p}.)
#' Otherwise, \code{p} must be of the same length as \code{cols_miss}.
#' In this case, the overall probability for a value to be missing will be
#' \code{p[i]} in the column \code{cols_miss[i]}.
#' The position of the missing values in \code{cols_miss[i]} is controlled by
#' \code{cols_ctrl[i]}.
#' The following procedure is applied for each pair of \code{cols_ctrl[i]} and
#' \code{cols_miss[i]} to determine the positions of missing values:
#'
# part about p is copied to delete_MCAR manually!
