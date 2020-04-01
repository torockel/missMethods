#' Mean imputation
#'
#' Impute an observed mean for the missing values
#'
#' @template location-par
#' @template impute
#' @template tibble-cast
#'
#' @details For every missing value the mean of some observed values is imputed.
#' The observed values to be used are specified via \code{type}.
#' For example, \code{type = "columnwise"} (the default) imputes the mean of
#' the observed values in a column for all missing values in the column.
#' This is normally meant, if someone speaks of "imputing the mean" or
#' "mean imputation".
#'
#' Other options for \code{type} are: "rowwise", "total", "Winer" and
#' "Two-way".
#' The option "rowwise" imputes all missing values in a row with the mean of the
#' observed values in the same row.
#' "total" will impute every missing value with the mean of all observed values
#' in \code{ds}.
#' "Winer" imputes the mean of the rowwise and columnwise mean.
#' Beland et al. (2016) called this method "Winer" and they attributed the
#' method to Winer (1971).
#' "Two-way" imputes the sum of rowwise and columnwise mean minus the total mean.
#' This method was suggested by D.B Rubin to Bernaards & Sijtsma, K. (2000).
#'
#'
#' @export
#'
#' @references
#' Bernaards, C. A., & Sijtsma, K. (2000). Influence of imputation and EM
#' methods on factor analysis when item nonresponse in questionnaire data is
#' nonignorable. \emph{Multivariate Behavioral Research}, 35(3), 321-364.
#'
#' Winer, B. J. (1971). \emph{Statistical principles in experimental design (2ed ed.)}
#' New York: McGraw-Hill
#'
#' @examples
#' ds <- data.frame(X = 1:20, Y = 101:120)
#' ds_miss <- delete_MCAR(ds, 0.2)
#' ds_imp <- impute_mean(ds_miss)
#' # completely observed columns can be of any type:
#' ds_miss_char <- cbind(ds_miss, letters[1:20])
#' ds_imp_char <- impute_mean(ds_miss_char)
impute_mean <- function(ds, type = "columnwise") {
  apply_imputation(ds, FUN = mean, type = type)
}


#' Median imputation
#'
#' Impute an observed median value for every missing value
#'
#' @template location-par
#' @template impute
#' @template tibble-cast
#'
#' @details This function behaves exactly like \code{\link{impute_mean}}.
#' The only difference is that it imputes a median instead of a mean.
#' All \code{type}s from \code{\link{impute_mean}} are also implemented for
#' \code{impute_median}.
#' They are documented in \code{\link{impute_mean}} and
#' \code{\link{apply_imputation}}.
#' The function \code{\link[stats]{median}} is used for the calculation of
#' the median values for imputation.
#'
#' @param ordered_low logical; used for the calculation of ordered factors (for
#'   details see: \link{median.factor})
#'
#' @export
#' @seealso
#' \code{\link[stats]{median}}, \code{\link{median.factor}}
#'
#' @examples
#' ds <- data.frame(X = 1:20, Y = ordered(LETTERS[1:20]))
#' ds_miss <- delete_MCAR(ds, 0.2)
#' ds_imp <- impute_median(ds_miss)
#' # completely observed columns can be of any type:
#' ds_miss_char <- cbind(ds_miss, letters[1:20])
#' ds_imp_char <- impute_median(ds_miss_char)
impute_median <- function(ds, type = "columnwise", ordered_low = FALSE) {
  apply_imputation(ds, FUN = median, ordered_low = ordered_low)
}


#' Mode imputation
#'
#' Impute an observed mode value for every missing value
#'
#' @template location-par
#' @template impute
#'
#' @details
#' This function behaves exactly like \code{\link{impute_mean}}. The only
#' difference is that it imputes a mode instead of a mean. All \code{type}s from
#' \code{\link{impute_mean}} are also implemented for \code{impute_mode}. They
#' are documented in \code{\link{impute_mean}} and
#' \code{\link{apply_imputation}}.
#'
#' A mode value of a vector \emph{x} is a most frequent value of \emph{x}.
#' If this value is not unique, the first occurring mode value in \emph{x} will
#' be used as imputation value.
#'
#' @export
#'
#' @examples
#' ds <- data.frame(X = c(1:12, rep(8, 8)), Y = 101:120)
#' ds_miss <- delete_MCAR(ds, 0.2)
#' ds_imp <- impute_mode(ds_miss)
impute_mode <- function(ds, type = "columnwise") {
  calc_mode <- function(x) {
    unique_x <- unique(x)
    unique_x_freq <- tabulate(match(x, unique_x))
    unique_x[which.max(unique_x_freq)]
  }
  apply_imputation(ds, FUN = calc_mode, type = type)
}
