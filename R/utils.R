#' Assign imputed values
#'
#' Assign imputed values and take special care for tibbles
#'
#' Normal matrix and data frames can be subsetted by a logical matrix and than
#' values for this subset can be assigned. However, tibbles do not like this:
#' "Tibbles support indexing by a logical matrix, but only for a scalar RHS"
#' (source: https://tibble.tidyverse.org/articles/invariants.html). This
#' function merely exists to cure this problem.
#'
#' @param ds a dataset with missing values
#' @param ds_imp a dataset of the same dimensions as `ds` with imputed values
#'   for `ds`
#' @param M missing data indicator matrix
#'
#' @return An object of the same class as `ds` with missing values replaced by
#'   entries from `ds_imp`
#'
#' @noRd
assign_imputed_values <- function(ds, ds_imp, M = is.na(ds)) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    if (tibble::is_tibble(ds)) {
      for (col_with_mis in which(apply(M, 2, any))) {
        # https://tibble.tidyverse.org/articles/invariants.html#column-update-1
        ds[[col_with_mis]] <- ds_imp[, col_with_mis, drop = TRUE]
      }
      return(ds)
    }
  }
  ds[M] <- ds_imp[M]
  ds
}


is_df_or_matrix <- function(ds) {
  is.data.frame(ds) || is.matrix(ds)
}

# define resample to evade the "feature" of sample(x, ...),
# when x is numeric and has length 1
resample <- function(x, size, replace = FALSE, prob = NULL) {
  if (length(x) == 1L) {
    if (size == 1L || replace) {
      return(rep(x, size))
    } else {
      stop("resampling of size ", size, " not possible without replacement")
    }
  }
  sample(x = x, size = size, replace = replace, prob = prob)
}


#' Check if a package is installed
#'
#' @param pkg_names character vector with names from packages
#'
#' @return `TRUE`(invisible), if all packages are installed. Otherwise, an error
#'   is thrown.
#' @noRd
check_for_packages <- function(pkg_names) {
  okay_pkgs <- vapply(pkg_names, requireNamespace, logical(1), quietly = TRUE)
  if (any(!okay_pkgs)) {
    stop(
      "The following package(s) are needed, but not installed: ",
      paste(pkg_names[!okay_pkgs], collapse = ", "),
      ". Please install it/them to use this function."
    )
  }
  invisible(TRUE)
}

check_renamed_arg <- function(old, new) {

  # old is not used
  if (missing(old)) {
    return(invisible(TRUE))
  }

  old_name <- deparse(substitute(old))
  new_name <- deparse(substitute(new))

  # old was used
  if (missing(new)) {
    warning(
      old_name, " is deprecated; use ", new_name, " instead.",
      call. = FALSE
    )
    assign(new_name, old, pos = parent.frame(1))
  } else { # both are not missing!
    stop(
      old_name, " is deprecated and replaced by ", new_name, ". ",
      "Please supply only a value to ", new_name,
      " and not to both arguments!",
      call. = FALSE
    )
  }
}
