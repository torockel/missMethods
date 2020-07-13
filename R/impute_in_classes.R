#' Impute in classes
#'
#' Apply an imputation function inside imputation classes
#'
#' @template impute
#'
#' @details Imputation classes (sometimes also called adjustment cells) are
#' build using cross-validation of all `cols_class`. The classes are collapsed,
#' if they do not satisfy any of the criteria defined by `min_objs_in_class,
#' min_comp_obs, min_obs_per_col` or `donor_limit`. Collapsing starts from the
#' last value of `cols_class`. Internally a mixture of collapsing and early
#' stopping is used for the construction of the classes.
#'
#' @param cols_class columns that are used for constructing the imputation classes
#' @param FUN an imputation function that is applied to impute the missing values
#' @param breaks number of intervals / levels a column is broken into (see
#'   [cut()], which is used internally for cutting numeric columns). If `breaks
#'   = Inf` (the default), every unique value of a column will be in a separate
#'   class.
#' @param use_quantiles should quantiles be used for cutting numeric vectors?
#'   Normally, [cut()] divides the range of an vector into equal spaced intervals.
#'   If `use_quantiles = TRUE`, the classes will be of roughly equal content.
#' @param min_objs_in_class minimum objects (rows) in an imputation class
#' @param min_comp_obs minimum completely observed objects (rows) in an imputation class
#' @param min_obs_per_col minimum number of observed values in every column of an imputation class
#' @param donor_limit minimum odds between incomplete and complete values in a column, if `dl_type = cols_seq`; or minimum odds
#' between incomplete to complete rows, if `dl_type = sim_comp`
#' @param dl_type see `donor_limit`
#' @param add_imputation_classes should imputation classes be added as attributes to the imputed dataset?
#' @param ... arguments passed to `FUN`
#'
#'
#' @export
#'
#' @references
#' Andridge, R.R. and Little, R.J.A. (2010), A Review of Hot Deck Imputation for
#' Survey Non-response. International Statistical Review, 78: 40-64.
#' doi:10.1111/j.1751-5823.2010.00103.x
#'
#' @examples
#' # Mean imputation in classes
#' impute_in_classes(data.frame(X = 1:5, Y = c(NA, 12:15)), "X",
#' impute_mean, min_obs_per_col = 2)
impute_in_classes <- function(ds, cols_class, FUN, breaks = Inf, use_quantiles = FALSE,
                              min_objs_in_class = 1,
                              min_comp_obs = 0,
                              min_obs_per_col = 1,
                              donor_limit = Inf, dl_type = "cols_seq",
                              add_imputation_classes = FALSE,
                              ...) {
  ## check for missing argument cols_class, because subsetting "works" with missing argument...
  if (missing(cols_class)) {
    stop("cols_class must be specified")
  }

  FUN <- match.fun(FUN)

  imp_classes <- find_classes(ds = ds, cols_class = cols_class, breaks = breaks,
                              use_quantiles = use_quantiles,
                              min_objs_in_class = min_objs_in_class,
                              min_comp_obs = min_comp_obs,
                              min_obs_per_col = min_obs_per_col,
                              donor_limit = donor_limit, dl_type = dl_type)

  ## apply imputation function for every imputation class separate
  for (imp_cl in imp_classes) {
    if(anyNA(ds[imp_cl, ])) {
      ds[imp_cl, ] <- FUN(ds[imp_cl, ], ...)
    }
  }

  if(add_imputation_classes) {
    ds <- structure(ds, imputation_classes = imp_classes)
  }

  ds
}


#' Hot deck imputation in imputation classes
#'
#' Impute missing values in a data frame or a matrix using a hot deck with
#' imputation classes
#'
#' @template impute
#'
#' @details
#' This function is a combination of [impute_in_classes()] and [impute_sRHD()].
#' It applies [impute_sRHD()] inside of imputation classes (adjustment cells),
#' which are constructed via [impute_in_classes()]. More details can be found in
#' these two functions.
#'
#' @inheritParams impute_in_classes
#' @param type the type of hot deck (for details, see [impute_sRHD()])
#'
#' @seealso
#' [impute_in_classes()], which is used for the construction of the imputation classes
#'
#' [impute_sRHD()], which is used for the imputation
#'
#' @export
#'
#' @references
#' Andridge, R.R. and Little, R.J.A. (2010), A Review of Hot Deck Imputation for
#' Survey Non-response. International Statistical Review, 78: 40-64.
#' doi:10.1111/j.1751-5823.2010.00103.x
#'
#' @examples
#' impute_hot_deck_in_classes(data.frame(X = c(rep("A", 10), rep("B", 10)),
#'                                       Y = c(rep(NA, 5), 106:120)),
#'                            "X", donor_limit = 1)
impute_hot_deck_in_classes <- function(ds, cols_class, type = "cols_seq",
                                       breaks = Inf, use_quantiles = FALSE,
                                       min_objs_in_class = 1,
                                       min_comp_obs = 0,
                                       min_obs_per_col = 1,
                                       donor_limit = Inf,
                                       add_imputation_classes = FALSE) {

  impute_in_classes(ds, cols_class,
                    FUN = impute_sRHD,
                    breaks = breaks, use_quantiles = use_quantiles,
                    min_objs_in_class = min_objs_in_class,
                    min_comp_obs = min_comp_obs,
                    min_obs_per_col = min_obs_per_col,
                    donor_limit = donor_limit,
                    dl_type = type,
                    add_imputation_classes = add_imputation_classes,
                    type = type)

}

## Helpers for impute_in_classes() --------------------------------------------

find_classes <- function(ds, cols_class, breaks = Inf, use_quantiles = FALSE,
                         min_objs_in_class = 0,
                         min_comp_obs = 0,
                         min_obs_per_col = 0,
                         donor_limit = Inf, dl_type = "cols_seq") {

  # check for NA in cols_class
  if (anyNA(ds[, cols_class])) {
    stop("No NAs in ds[, cols_class] allowed")
  }

  find_classes_recursive(ds, cols_class, breaks = breaks, use_quantiles = use_quantiles,
                         min_objs_in_class = min_objs_in_class,
                         min_comp_obs = min_comp_obs,
                         min_obs_per_col = min_obs_per_col,
                         donor_limit = donor_limit, dl_type = dl_type,
                         act_cols = seq_len(nrow(ds)),
                         act_lvls = NULL,
                         imp_classes = list(),
                         M = is.na(ds))
}


## No defaults for the recursive function
## all arguments should be handed over by the calling function
## This will automatically throw an error, if a new argument is added to the
## function, but not in the calling statement(s)
find_classes_recursive <- function(ds, cols_class, breaks, use_quantiles,
                                   min_objs_in_class,
                                   min_comp_obs,
                                   min_obs_per_col,
                                   donor_limit, dl_type,
                                   act_cols, act_lvls = NULL, imp_classes,
                                   M) {


  # first check for fast return (no columns in act_cols or no more cols_class)
  if (length(act_cols) == 0L) { # no object in new class -> eliminate class
    return(imp_classes)
  } else if (length(cols_class) == 0L) { # no more columns to form classes
    if (is.null(act_lvls)) { # just one class for all
      act_lvls <- "everything"
    }
    imp_classes[[act_lvls]] <- act_cols
    return(imp_classes)
  }

  # no fast return:
  # we have objects and at least one column to form classes
  # we select only the objects from the act_cols
  grouping_factor <- cut_vector(ds[act_cols, cols_class[1], drop = TRUE],
    breaks = breaks,
    use_quantiles = use_quantiles
  )


  # check if all new formed classes are okay
  # if not -> join the problematic class(es)
  repeat {
    lvls <- levels(grouping_factor)
    new_classes <- list()
    new_lvls <- list()
    for (i in seq_along(lvls)) {
      new_classes[[i]] <- act_cols[grouping_factor == lvls[i]]
      new_lvls[[i]] <- ifelse(is.null(act_lvls), as.character(lvls[i]),
        paste(act_lvls, lvls[i], sep = ".")
      )
    }

    # remove empty classes
    empty_classes <- 0L == vapply(new_classes, length, integer(1))
    new_classes[empty_classes] <- NULL
    new_lvls[empty_classes] <- NULL
    okay_classes <- are_classes_okay(ds, new_classes,
                                     min_objs_in_class, min_comp_obs,
                                     min_obs_per_col = min_obs_per_col,
                                     donor_limit = donor_limit, dl_type = dl_type,
                                     M = M)
    if (all(okay_classes)) { # everything okay -> leave repeat loop
      break
    } else { # join first not okay_class and try again
      levels(grouping_factor) <- merge_lvls(
        grouping_factor,
        lvls[which(!okay_classes)[1]]
      )
    }
  }

  # call find_classes_recursive() for all new formed classes
  for (i in seq_along(new_classes)) {
    imp_classes <- find_classes_recursive(ds, cols_class[-1],
      use_quantiles = use_quantiles,
      breaks = breaks,
      min_objs_in_class = min_objs_in_class,
      min_comp_obs = min_comp_obs,
      min_obs_per_col = min_obs_per_col,
      donor_limit = donor_limit, dl_type = dl_type,
      act_cols = new_classes[[i]],
      act_lvls = new_lvls[[i]],
      imp_classes = imp_classes,
      M = M
    )
  }
  imp_classes
}

cut_vector <- function(x, breaks, use_quantiles = FALSE) {
  if (!is.finite(breaks)) { # breaks is infinite, no merging of lvls or cutting
    return(as.factor(x))
  }

  if (is.numeric(x)) { # cuts are ordered for possible later merging
    if (use_quantiles) {
      x <- cut(x,
        breaks = stats::quantile(x, seq(from = 0, to = 1, length.out = breaks + 1)),
        include.lowest = TRUE, ordered_result = TRUE
      )
    } else { # equal-sized classes
      x <- cut(x, breaks, ordered_result = TRUE)
    }
  } else { # not a numeric vector
    x <- as.factor(x)
    while (length(levels(x)) > breaks) {
      levels(x) <- merge_lvls(x)
    }
  }
  x
}

are_classes_okay <- function(ds, new_classes,
                             min_objs_in_class = 1,
                             min_comp_obs = 0,
                             min_obs_per_col = 0,
                             donor_limit = Inf,
                             dl_type = "cols_seq",
                             M = is.na(ds)) {

  res <- rep(TRUE, length(new_classes))

  for(i in seq_along(new_classes)) {
    M_class_i <- M[new_classes[[i]], ,drop = FALSE]

    ## check min_objs_in_class, if > 1 --------------------
    if (min_objs_in_class > 1) {
      if (length(new_classes[[i]]) < min_objs_in_class) {
        res[i] <- FALSE
      }
    }

    ## check min_comp_obs, if > 0 -------------------------
    if (min_comp_obs > 0) {
      n_incomp_obs_i <-  sum(apply(M_class_i, 1, any))
      n_comp_obs_i <- length(new_classes[[i]]) - n_incomp_obs_i

      if(n_comp_obs_i < min_comp_obs) {
        res[i] <- FALSE
      }
    }

    ## check min_obs_per_col, if > 0 -------------------------
    if (min_obs_per_col > 0) {
      obs_per_col <- apply(M_class_i, 2, function(x) sum(!x))
      if (any(obs_per_col < min_obs_per_col)) {
        res[i] <- FALSE
      }
    }

    ## check donor_limit, if donor_limit is finite ---------
    if(is.finite(donor_limit)) {
      if (min_donor_limit(M_class_i, dl_type) > donor_limit)
        res[i] <- FALSE
    }

  }
  res
}

merge_lvls <- function(grouping_factor, merging_lvl_1 = NULL) {
  lvls <- levels(grouping_factor)
  if (length(lvls) < 2L) {
    stop("merging only possible for two or more levels")
  }

  lvls_freq <- tabulate(match(grouping_factor, lvls))

  if (is.null(merging_lvl_1)) {
    merging_lvl_1 <- lvls[which.min(lvls_freq)]
  }

  if (is.ordered(grouping_factor)) { # join left or right, if possible
    pos_lvl_1 <- which(merging_lvl_1 == lvls)
    if (pos_lvl_1 == 1L) { # first level -> merge with second
      merging_lvl_2 <- lvls[2L]
    } else if (pos_lvl_1 == length(lvls)) { # last level -> merge with second to last
      merging_lvl_2 <- lvls[length(lvls) - 1L]
    } else { # neither first nor last level -> left or right
      lvls_freq_left <- lvls_freq[pos_lvl_1 - 1L]
      lvls_freq_right <- lvls_freq[pos_lvl_1 + 1L]
      merging_lvl_2 <- pos_lvl_1 + ifelse(lvls_freq_left < lvls_freq_right, -1L, +1L)
      merging_lvl_2 <- lvls[merging_lvl_2]
    }
  } else { # unordered factor
    lvls_freq_without_lvl_1 <- lvls_freq[-which(merging_lvl_1 == lvls)]
    lvls_without_lvl_1 <- lvls[-which(merging_lvl_1 == lvls)]
    merging_lvl_2 <- lvls_without_lvl_1[which.min(lvls_freq_without_lvl_1)]
  }

  new_lvl <- paste(merging_lvl_1, merging_lvl_2, sep = "_and_")
  lvls[lvls %in% c(merging_lvl_1, merging_lvl_2)] <- new_lvl
  lvls
}
