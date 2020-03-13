#' Median for ordered factors
#'
#' Compute the median of an ordered factor
#'
#' Currently, the median for an ordered factor is not implemented in base R.
#' This function is a remedy for this.
#' It allows the computation of \dQuote{a median} for ordered factors (see below) and
#' overwrites the error message for unordered factors from
#' \code{\link[stats]{median.default}} (hence, the function name is
#' median.factor and not median.ordered).
#'
#' If the length of \code{x} is even, then the median will be the middle value
#' of the sorted list of elements from \code{x}.
#' If the length of \code{x} is odd and the two middle values of the sorted
#' list of elements from \code{x} are equal, then the median is one of these
#' middle values.
#' The only problematic case is an odd length \code{x} with unequal middle
#' values.
#' In this case, the median of a numeric vector is normally defined as the mean
#' of the two middle values.
#' However, for ordered factors the mean is not defined.
#' The argument \code{ordered_low} cures this problem.
#' If \code{ordered_low = FALSE} (the default), then the larger of the two
#' middle values is returned (this value is called \sQuote{hi-median} in
#' \code{\link[stats]{mad}}).
#' If \code{ordered_low = TRUE}, then the smaller of the two middle values is
#' returned (this value is called \sQuote{low-median} in
#' \code{\link[stats]{mad}}).
#'
#' @param x an ordered factor (for unordered factors an error will be thrown)
#' @param na.rm logical; should \code{NA} be removed before computation
#' @param ordered_low logical; only used if the length of x is even and the two
#' middle values are unequal (see details)
#' @param ... not used in this function
#'
#' @return a length-one factor
#' @export
#' @importFrom stats median
#'
#' @examples
#' ord_factor_odd <- ordered(letters[1:5])
#' median(ord_factor_odd) # calls median.factor, if package is loaded
#'
#' \dontrun{
#' # if only base R is loaded, median.default will be called and will throw an error:
#' median.default(ord_factor_odd)}
#'
#' ord_factor_even <- ordered(letters[1:4])
#' median(ord_factor_even, ordered_low = FALSE)
#' median(ord_factor_even, ordered_low = TRUE)
#'
median.factor <- function(x, na.rm = FALSE, ordered_low = FALSE, ...) {
  if (!is.ordered(x)){
    stop("median is not defined for unordered factors")
  }
  if (na.rm){
    x <- x[!is.na(x)]
  }

  n <- length(x)
  if (any(is.na(x)) || n == 0L) {
    return(x[FALSE][NA])
  }

  if (n %% 2L == 1L) { # n odd
    half <- (n + 1L) %/% 2L
  } else { # n even
    half <- ifelse(ordered_low, n %/% 2L,  n %/% 2L + 1L)
  }
  sort(x, partial = half)[half]
}
