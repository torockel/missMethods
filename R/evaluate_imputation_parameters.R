evaluate_imputation_parameters <- function(imp_ds, orig_ds = NULL, true_pars = NULL,
                                           parameter = "mean", criterion = "RMSE",
                                           which_pars = NULL, ...) {
  if (!xor(is.null(orig_ds), is.null(true_pars))) {
    stop("exactly one of 'orig_ds' or 'true_pars' must be supplied and the
         other one must be NULL")
  }
  match.arg(parameter, c("mean", "median", "var", "sd", "quantile", "cov", "cor"))

  calc_pars <- switch(parameter,
                     mean = colMeans,
                     median = make_col_fun(median),
                     var = stats::var,
                     sd = make_col_fun(stats::sd),
                     quantile = make_col_fun(stats::quantile),
                     cov = stats::cov,
                     cor = stats::cor)

  imp_pars <- calc_pars(imp_ds, ...)

  if (is.null(true_pars)) { #pars must be calculated
    true_pars <- calc_pars(orig_ds, ...)
  }

  if (!is.null(which_pars)) {
    imp_pars <- imp_pars[which_pars]
    true_pars <- true_pars[which_pars]
  }

  evaluate_parameters(imp_pars, true_pars, criterion)
}


# helper---------------------------------------------------
make_col_fun <- function(FUN, unlist = TRUE) {
  function(ds, ...) {
    res <- list()
    for (k in seq_len(ncol(ds))) {
      res[[k]] <- FUN(ds[, k], ...)
    }
    if (!is.null(colnames(ds))) {
      names(res) <- colnames(ds)
    }
    if (unlist) {
      return(unlist(res))
    }
    res
  }
}
