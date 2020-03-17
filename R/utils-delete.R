# checking arguments --------------------------------------

# args checking for all mechanisms
# normally called from check_delete_args_MCAR, check_delete_args_MAR or
# check_delete_args_MNAR
check_delete_args <- function(ds, p, miss_cols, stochastic) {
  # check ds ------------------------------------
  if (!is_df_or_matrix(ds)) {
    stop("ds must be a data.frame or a matrix")
  }

  # check p -------------------------------------
  if (is.numeric(p)) {
    if (length(p) != 1L && length(p) != length(miss_cols)) {
      stop("p must be of length 1 or length must equal miss_cols")
    } else {
      if (any(p < 0 | p > 1)) {
        stop("probabilties in p must be between 0 and 1")
      }
    }
  } else {
    stop("p must be numeric")
  }

  # check miss_cols -----------------------------
  if (is.numeric(miss_cols)) {
    if (any(miss_cols < 1 | miss_cols > ncol(ds))) {
      stop("indices in miss_cols must be in 1:ncol(ds)")
    }
  } else if (is.character(miss_cols)) {
    if (!all(miss_cols %in% colnames(ds))) {
      stop("all entries of miss_cols must be in colnames(ds)")
    }
  } else {
    stop("miss_cols must be a vector of column names or indices of ds")
  }

  if (anyDuplicated(miss_cols) != 0) {
    duplicated_cols <- unique(miss_cols[duplicated(miss_cols)])
    warning(
      "there are duplicates in miss_cols:\n", duplicated_cols,
      "\n this may result in a too high percentage of missing values"
    )
  }

  # check stochastic ----------------------------
  if (!is.logical(stochastic)) {
    stop("stochastic must be logical")
  } else if (length(stochastic) != 1L) {
    stop("the length of stochastic must be 1")
  }
}

check_delete_args_MCAR <- function(ds, p, miss_cols, stochastic, p_overall) {
  # general checking
  check_delete_args(ds = ds, p = p, miss_cols = miss_cols, stochastic = stochastic)

  # special case: p_overall
  if (!is.logical(p_overall) | length(p_overall) != 1L) {
    stop("p_overall must be logical of length 1")
  } else if (p_overall & !stochastic & length(p) != 1L) {
    stop("if p_overall = TRUE, then length(p) must be 1")
  }
}

check_delete_args_MAR <- function(ds, p, miss_cols, ctrl_cols, stochastic) {
  # general checking
  check_delete_args(ds = ds, p = p, miss_cols = miss_cols, stochastic = stochastic)


  # check ctrl_cols -----------------------------
  if (!is.null(ctrl_cols)) {
    if (is.numeric(ctrl_cols)) {
      if (any(ctrl_cols < 1 | ctrl_cols > ncol(ds))) {
        stop("indices in ctrl_cols must be in 1:ncol(ds)")
      }
    } else if (is.character(ctrl_cols)) {
      if (!all(ctrl_cols %in% colnames(ds))) {
        stop("all entries of ctrl_cols must be in colnames(ds)")
      }
    } else {
      stop("ctrl_cols must be a vector of column names or indices of ds")
    }
  }
  # no NA in ctrl_cols
  if (any(is.na(ds[, ctrl_cols]))) {
    stop("ctrl_cols must be completely observed; no NAs in ds[, ctrl_cols] allowed")
  }

  if (length(miss_cols) != length(ctrl_cols)) {
    stop("length(miss_cols) must equal length(ctrl_cols)")
  }

  # check if any ctrl_col is in miss_cols
  if (any(ctrl_cols %in% miss_cols)) {
    stop(
      "to ensure MAR no ctrl_col is allowed to be in miss_cols;\n",
      "problematic ctrl_cols:\n",
      paste(ctrl_cols[ctrl_cols %in% miss_cols], collapse = ", ")
    )
  }
}

check_delete_args_MNAR <- function(ds, p, miss_cols, stochastic) {
  # general checking
  check_delete_args(ds = ds, p = p, miss_cols = miss_cols, stochastic = stochastic)
  #  no NA in miss_cols
  if (any(is.na(ds[, miss_cols]))) {
    stop("miss_cols must be completely observed; no NAs in ds[, miss_cols] allowed")
  }
}


check_ctrl_cols_1_to_x <- function(ds, ctrl_cols) {
  # check if ctrl_cols are numeric or ordered factor
  prob_cols <- integer(0)
  for (k in seq_along(ctrl_cols)) {
    if (!(is.ordered(ds[, ctrl_cols[k]]) | is.numeric(ds[, ctrl_cols[k]]))) {
      prob_cols <- c(prob_cols, ctrl_cols[k])
    }
  }
  if (length(prob_cols) > 0L) {
    stop(
      "all ctrl_cols must be numeric or ordered factors;\n",
      "problematic column(s): ",
      paste(prob_cols, collapse = ", ")
    )
  }
  TRUE
}

# finding groups ------------------------------------------

find_groups <- function(x, cutoff_fun, prop, use_lpSolve,
                        ordered_as_unordered, ...) {
  unique_x <- unique(x)
  nr_unique <- length(unique_x)
  if (nr_unique == 1) {
    warning("grouping not possible, because x is constant")
    groups <- list(g1 = seq_along(x), g2 = NULL)
  } else if (is.factor(x) && (ordered_as_unordered || !is.ordered(x))) {
    groups <- find_groups_by_prop(x, prop = prop, use_lpSolve)
  } else { # not an (unordered) factor and not constant
    cutoff_fun <- match.fun(cutoff_fun)
    groups <- find_groups_by_cutoff_val(x, cutoff_fun(x, ...))
  }
  groups
}

find_groups_by_cutoff_val <- function(x, cutoff_val) {
  # get rows below the cutoff value
  below <- x < cutoff_val
  # get rows greater than or equal to the cutoff value ("above")
  above <- which(!below)
  below <- which(below)
  # cure problem: no values below (but only if ctrl_cols[i] is not constant!)
  if ((length(below) == 0L) && length(unique(x)) != 1L) {
    # instead of only below, now accept below and equal
    below <- x <= cutoff_val
    above <- which(!below) # and above will be only above (not equal anymore)
    below <- which(below)
  }
  list(g1 = below, g2 = above)
}


find_groups_by_prop <- function(x, prop, use_lpSolve = TRUE) {
  ux <- unique(x) # order: first occurring in x
  nr_ux <- tabulate(match(x, ux))
  if (use_lpSolve) {
    if (requireNamespace("lpSolve", quietly = TRUE)) {
      solution <- lpSolve::lp(
        direction = "min", objective.in = nr_ux,
        const.mat = matrix(nr_ux, nrow = 1),
        const.dir = ">=",
        const.rhs = prop * length(x),
        all.bin = TRUE
      )
      if (solution$status == 0) {
        g1_ux <- ux[solution$solution > 0]
      } else {
        stop("lpSolve found no solution")
      }
    } else { # no lpSolve installed
      stop("Package \"lpSolve\" needed. Either install it or use \"use_lpSolve = FALSE\".")
    }
  } else {
    g1_ux <- cumsum(nr_ux) <= ceiling(prop * length(x))
    g1_ux <- ux[g1_ux]
  }
  # g1 should not be empty, but only if there is more than one different value in x
  if (length(g1_ux) == 0L && length(unique(x)) != 1) {
    g1_ux <- ux[1]
  }
  find_groups_by_values(x, g1_ux)
}

find_groups_by_values <- function(x, values) {
  g1 <- x %in% values
  g2 <- which(!g1)
  g1 <- which(g1)
  list(g1 = g1, g2 = g2)
}

# more helpers --------------------------------------------
adjust_p <- function(p, miss_cols) {
  if (length(p) == 1L) {
    p <- rep(p, length(miss_cols))
  }
  p
}

calc_nr_miss_g1 <- function(nr_g1, p_miss_g1,
                            nr_g2, nr_miss, x) {
  if (nr_miss == 0L) {
    nr_miss_g1 <- 0L
  } else if (nr_g2 == 0L) {
    nr_miss_g1 <- nr_miss
  } else {
    nr_miss_g1_ceil <- ceiling(nr_g1 * p_miss_g1)
    nr_miss_g1_floor <- floor(nr_g1 * p_miss_g1)
    odds_ceil <- nr_miss_g1_ceil / nr_g1 /
      ((nr_miss - nr_miss_g1_ceil) / nr_g2)
    odds_floor <- nr_miss_g1_floor / nr_g1 /
      ((nr_miss - nr_miss_g1_floor) / nr_g2)
    if (abs(1 / x - odds_ceil) < abs(1 / x - odds_floor)) {
      nr_miss_g1 <- nr_miss_g1_ceil
    } else {
      nr_miss_g1 <- nr_miss_g1_floor
    }
  }
  nr_miss_g1
}
