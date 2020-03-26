#' @family evaluation functions
#'
#' @param criterion a string specifying the used criterion for comparing the
#'   imputed and original values
#' @param tolerance numeric, only used for \code{criterion = "precision"}:
#'   numeric differences smaller than tolerance are treated as zero/equal
#'
#' @return a numeric vector of length one
