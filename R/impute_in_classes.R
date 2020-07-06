
find_classes <- function(ds, class_cols, breaks = Inf, use_quantiles = FALSE,
                         donor_limit = Inf, type = "cols_seq",
                         min_objs_in_class = 0,
                         min_comp = 0) {

  # check for NA in class_cols
  if (anyNA(ds[, class_cols])) {
    stop("No NAs in ds[, class_cols] allowed")
  }

  find_classes_recursive(ds, class_cols, breaks = breaks, use_quantiles = use_quantiles,
                         donor_limit = donor_limit, type = type,
                         min_objs_in_class = min_objs_in_class,
                         min_comp = min_comp,
                         M = is.na(ds))
}


find_classes_recursive <- function(ds, class_cols, breaks = Inf, use_quantiles = FALSE,
                                   donor_limit = Inf, type = "cols_seq",
                                   min_objs_in_class = 0,
                                   min_comp = 0,
                                   act_cols = seq_len(nrow(ds)),
                                   act_lvls = NULL,
                                   imp_classes = list(),
                                   M = is.na(ds)) {

  # first check for fast return (no columns in act_cols or no more class_cols)
  if (length(act_cols) == 0L) { # no object in new class -> eliminate class
    return(imp_classes)
  } else if (length(class_cols) == 0L) { # no more columns to form classes
    if (is.null(act_lvls)) { # just one class for all
      act_lvls <- "everything"
    }
    imp_classes[[act_lvls]] <- act_cols
    return(imp_classes)
  }

  # no fast return:
  # we have objects and at least one column to form classes
  # we select only the objects from the act_cols
  grouping_factor <- cut_vector(ds[act_cols, class_cols[1], drop = TRUE],
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
                                     donor_limit, type, min_objs_in_class,
                                     min_comp, M)
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
    imp_classes <- find_classes_recursive(ds, class_cols[-1],
      breaks = breaks,
      donor_limit = donor_limit, type = type,
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

are_classes_okay <- function(ds, new_classes, donor_limit = Inf,
                             type = "cols_seq", min_objs_in_class = 0,
                             min_comp = 0, M = is.na(ds)) {
  res <- rep(TRUE, length(new_classes))

  for(i in seq_along(new_classes)) {
    # M_class_i  <- is.na(ds[new_classes[[i]], , drop = FALSE])
    M_class_i <- M[new_classes[[i]], ]

    if(is.finite(donor_limit)) { # check donor_limit, if donor_limit is finite
      if (min_donor_limit(M_class_i, type) > donor_limit)
        res[i] <- FALSE
    }

    if (min_objs_in_class > 1) { # check min_objs_in_class, if > 1
      if (length(new_classes[[i]]) < min_objs_in_class) {
        res[i] <- FALSE
      }
    }

    if (min_comp > 0) { # check min_comp, if > 0

      if (type == "cols_seq") {
        if(any(apply(M_class_i, 2, function(x) sum(!x)) < min_comp)) {
          res[i] <- FALSE
        }
      } else if (type == "sim_comp") {
        if (length(new_classes[[i]]) - sum(apply(M_class_i, 1, any)) < min_comp) {
          res[i] <- FALSE
        }
      }
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
