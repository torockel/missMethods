#' @family location parameter imputation functions
#'
#' @section Warning for tibble users:
#'   The types "total" and "Two-way" are currently not supported for tibbles
#'   (package version 2.1.3). This issue is due to the way subsetting in
#'   tibbles works right now. It will be fixed with an update of the package
#'   tibble (for details see:
#'   \url{https://github.com/tidyverse/tibble/pull/687})
#'
#' @param type one of: "columnwise", "rowwise", "total", "Two-Way" or "Winer"
#'   (see details)
#'
#' @seealso \code{\link{apply_imputation}} the workhorse for this function
