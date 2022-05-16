#' Count the number of `NA`s
#'
#' Count the number of `NA` values in a vector, matrix or data frame
#'
#' @md
#'
#' @param x a vector, matrix or data frame, in which `NA` values are counted
#' @param type How to count the `NA` values. Possible choices:
#'  * "default": If `x` is a matrix or a data frame, the number of missing
#'    values per column is returned. If `x` is something else, the total number
#'    of missing values in `x` is returned.
#'  * "all": The number of all missing values in `x` is returned.
#'  * "cols": The number of missing values per column is returned.
#'  * "rows": The number of missing values per row is returned.
#'
#' @return The number of missing values.
#' @export
#'
#' @examples
#' count_NA(c(1, NA, 3, NA, 5, NA))
#' test_df <- data.frame(X1 = rep(c(1, NA), 5), X2 = c(1:9, NA))
#' count_NA(test_df)
#' count_NA(test_df, "cols") # the default
#' count_NA(test_df, "rows")
#' count_NA(test_df, "all")
count_NA <- function(x, type = "default") {
  M <- is.na(x)
  if (type == "default") {
    type <- ifelse(is_df_or_matrix(x), "cols", "all")
  }
  switch(
    type,
    all = sum(M),
    cols = colSums(M),
    rows = rowSums(M)
  )
}
