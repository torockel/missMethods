#' @family evaluation functions
#'
#' @param criterion A string specifying the used criterion for comparing the
#'   imputed and original values.
#' @param tolerance Numeric, only used for \code{criterion = "precision"}:
#'   numeric differences smaller than tolerance are treated as zero/equal.
#'
#' @return A numeric vector of length one.
