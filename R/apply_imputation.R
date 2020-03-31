#' Apply a function for imputation
#'
#' Apply a function for imputation over rows, columns or combinations of both
#'
#' @template impute
#' @template tibble-cast
#'
#' @details  The functionality of \code{apply_imputation} is inspired by the
#' \code{\link[base]{apply}} function. The function applies a function
#' \code{FUN} to impute the missing values in \code{ds}. \code{FUN} must be a
#' function, which takes a vector as input and returns exactly one value. The
#' argument \code{type} is comparable to \code{\link[base]{apply}}'s
#' \code{MARGIN} argument. It specifies the values that are used for the
#' calculation of the imputation values. For example, \code{type = "columnwise"}
#' and \code{FUN = mean} will impute the mean of the observed values in a column
#' for all missing values in this column. In contrast, \code{type = "rowwise"}
#' and \code{FUN = mean} will impute the mean of the observed values in a row
#' for all missing values in this row.
#'
#' List of all implemented \code{types}:
#' \itemize{
#' \item{"columnwise" (the default): imputes column by column; all observed
#' values of a column are given  to \code{FUN} and the returned value is used
#' as the imputation value for all missing values of the column.}
#' \item{"rowwise": imputes row by row; all observed values of a row are given
#' to \code{FUN} and the returned value is used as the imputation value for all
#' missing values of the row.}
#' \item{"total": All observed values of \code{ds} are given to \code{FUN} and
#' the returned value is used as the imputation value for all missing values of
#' \code{ds}.}
#' \item{"Winer": The mean value from "columnwise" and "rowwise" is used as the
#' imputation value.}
#' \item{"Two-way": The sum of the values from "columnwise" and "rowwise" minus
#' "total" is used as the imputation value.}
#' }
#'
#' If no value can be given to \code{FUN} (for example, if no value in a column
#' is observed and \code{type = "columnwise"}), then a warning will be issued
#' and no value will be imputed in the corresponding column or row.
#'
#' @seealso A convenient interface exists for common cases like mean imputation:
#'   \code{\link{impute_mean}}, \code{\link{impute_median}},
#'   \code{\link{impute_mode}}. All these functions call
#'   \code{apply_imputation}.
#'
#' @param FUN the function to be applied for imputation
#' @param type a string specifying the values used for imputation, see details
#' @param ... further arguments passed to \code{FUN}
#'
#' @export
#'
#' @examples
#' ds <- data.frame(X = 1:20, Y = 101:120)
#' ds_miss <- delete_MCAR(ds, 0.2)
#' ds_imp_app <- apply_imputation(ds_miss, FUN = mean, type = "total")
#' # the same result can be achieved via impute_mean():
#' ds_imp_mean <- impute_mean(ds_miss, type = "total")
#' all.equal(ds_imp_app, ds_imp_mean)
apply_imputation <- function(ds, FUN = mean, type = "columnwise", ...) {
  # the workhorse for the location parameter imputation methods and other imputation methods

  # check args --------------------------------------------
  if (!is_df_or_matrix(ds)) {
    stop("ds must be a data frame or a matrix")
  }
  FUN <- match.fun(FUN)
  type <- match.arg(type, c("columnwise", "rowwise", "total", "Two-Way", "Winer"))

  if (requireNamespace("tibble", quietly = TRUE)) {
    if (tibble::is_tibble(ds) && type %in% c("total", "Two-Way") &&
      utils::packageVersion("tibble") < package_version("2.99.99.9012")) {
      stop("ds is a tibble and logical subsetting, which is needed for 'total' and 'Two-Way',
      is only supported for tibbles with package versions >= 2.99.99.9012;
      possible solutions:
      * update package tibble
      * convert ds to data frame via as.data.frame
      * do not use 'total' or 'Two-Way'",
        call. = FALSE
      )
    } # for more details see: https://github.com/tidyverse/tibble/releases/tag/v2.99.99.9012
  }

  # define M and check all NA
  M <- is.na(ds)
  if (all(M)) {
    warning("all values in ds are NA, no imputation possible")
    return(ds)
  }

  # impute ------------------------------------------------
  if (type == "columnwise") { # columnwise ---------------
    for (k in seq_len(ncol(ds))) {
      M_k <- M[, k]
      if (all(M_k)) { # only missing values in column
        warning(
          "in column ", k,
          " all values are NA; the column cannot be imputed"
        )
      } else if (any(M_k)) { # only for columns with missing values FUN is used
        ds[M_k, k] <- FUN(ds[!M_k, k, drop = TRUE], ...)
      }
    }
  } else if (type == "rowwise") { # rowwise ---------------
    for (i in seq_len(nrow(ds))) {
      M_i <- M[i, ]
      if (all(M_i)) { # only missing values in row
        warning(
          "in row ", i,
          " all values are NA; the row cannot be imputed"
        )
      } else if (any(M_i)) {
        ds[i, M_i] <- FUN(ds[i, !M_i, drop = TRUE], ...)
      }
    }
  } else if (type == "total") { # total -------------------
    ds[M] <- FUN(ds[!M], ...)
  } else if (type == "Two-Way" || type == "Winer") { # Two-Way and Winer ------
    M <- is.na(ds)
    if (type == "Two-Way") { # total only needed for Two-Way
      total <- FUN(ds[!M], ...)
    }
    for (k in seq_len(ncol(ds))) {
      M_k <- M[, k]
      if (all(M_k)) {
        warning(
          "in column ", k,
          " all values are NA; the column cannot be imputed"
        )
      } else if (any(M_k)) { # any missing values in column k?
        imp_k <- FUN(ds[!M_k, k, drop = TRUE], ...)
        for (i in which(M_k)) {
          if (all(M[i, ])) {
            if (k == 1) { # warn only once per row
              warning(
                "in row ", i,
                " all values are NA; the row cannot be imputed"
              )
            }
          } else {
            imp_i <- FUN(ds[i, !M[i, ], drop = TRUE], ...)
            if (type == "Two-Way") {
              ds[i, k] <- imp_i + imp_k - total
            } else if (type == "Winer") {
              ds[i, k] <- (imp_i + imp_k) / 2
            }
          }
        }
      }
    }
  } else {
    stop("type ", type, " is not implemented")
  }

  ds
}
