#' @section A Note for tibble users:
#' If you use tibbles and an error like \sQuote{Lossy cast from `value` <double>
#' to `x` <integer>} occurs, you will first need to convert all integer columns
#' with missing values to double. Another solution is to convert the tibble with
#' as.data.frame() to a data frame. The data frame will automatically convert
#' integer columns to double columns, if needed.
