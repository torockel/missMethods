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


check_for_packages <- function(pkg_names) {
  okay_pkgs <- sapply(pkg_names, requireNamespace, quietly = TRUE)
  if (any(!okay_pkgs)) {
    stop("The following package(s) are needed, but not installed: ",
         paste(pkg_names[!okay_pkgs], collapse = ", "),
         ". Please install it/them to use this function.")
  }
  invisible(TRUE)
}
