## Interface for all MCAR, MAR, MNAR functions --------------------------------
# The function delete_values() is normally called via, delete_MCAR(),
# delete_MAR_1_to_x() and friends. All of these functions call delete_values
# (via do.call()). Inside delete_values() arguments, which should not be passed
# on, must be removed (via (remove() or args$argName <-NULL). Finally,
# delete_values() calls delete_mech_type() or .delete_MCAR().
delete_values <- function(mechanism, mech_type, ds, p, cols_mis, n_mis_stochastic,
                          cols_ctrl,
                          p_overall,
                          miss_cols, ctrl_cols, stochastic,
                          ...) {

  ## Check for deprecated arguments -------------------------------------------
  check_renamed_arg(miss_cols, cols_mis)
  check_renamed_arg(ctrl_cols, cols_ctrl)
  check_renamed_arg(stochastic, n_mis_stochastic)
  remove(list = c("miss_cols", "ctrl_cols", "stochastic"))


  ## Check and adjust arguments -----------------------------------------------
  check_delete_args_general(ds, p, cols_mis, n_mis_stochastic)

  if (mechanism == "MCAR") {
    check_args_MCAR(p, p_overall)
    remove(cols_ctrl)
  } else if (mechanism == "MAR") {
    check_args_MAR(ds, cols_mis, cols_ctrl)
    remove(p_overall)
  } else if (mechanism == "MNAR") {
    check_args_MNAR(ds, cols_mis)
    cols_ctrl <- cols_mis
    remove(p_overall)
  } else {
    stop("mechanism must be one of MCAR, MAR or MNAR")
  }

  p <- adjust_p(p, cols_mis)


  ## Call delete function -----------------------------------------------------
  # Get needed arguments
  args <- c(as.list(environment()), list(...))
  args$mechanism <- NULL
  args$mech_type <- NULL

  # Construct function name
  fun_name <- if (mechanism == "MCAR") {
    ".delete_MCAR"
  } else {
    paste0("delete_", mech_type)
  }

  do.call(fun_name, args)
}

# Functions for checking arguments inside of delete_values() ------------------
# args checking for all mechanisms
check_delete_args_general <- function(ds, p, cols_mis, n_mis_stochastic) {
  # check ds ------------------------------------
  if (!is_df_or_matrix(ds)) {
    stop("ds must be a data.frame or a matrix")
  }

  # check p -------------------------------------
  if (is.numeric(p)) {
    if (length(p) != 1L && length(p) != length(cols_mis)) {
      stop("p must be of length 1 or length must equal cols_mis")
    } else {
      if (any(p < 0 | p > 1)) {
        stop("probabilties in p must be between 0 and 1")
      }
    }
  } else {
    stop("p must be numeric")
  }

  # check cols_mis -----------------------------
  if (is.numeric(cols_mis)) {
    if (any(cols_mis < 1 | cols_mis > ncol(ds))) {
      stop("indices in cols_mis must be in 1:ncol(ds)")
    }
  } else if (is.character(cols_mis)) {
    if (!all(cols_mis %in% colnames(ds))) {
      stop("all entries of cols_mis must be in colnames(ds)")
    }
  } else {
    stop("cols_mis must be a vector of column names or indices of ds")
  }

  if (anyDuplicated(cols_mis) != 0) {
    duplicated_cols <- unique(cols_mis[duplicated(cols_mis)])
    warning(
      "there are duplicates in cols_mis:\n", duplicated_cols,
      "\n this may result in a too high percentage of missing values"
    )
  }

  # check n_mis_stochastic ----------------------------
  if (!is.logical(n_mis_stochastic)) {
    stop("n_mis_stochastic must be logical")
  } else if (length(n_mis_stochastic) != 1L) {
    stop("the length of n_mis_stochastic must be 1")
  }
}

check_args_MCAR <- function(p, p_overall) {
  # special case: p_overall
  if (!is.logical(p_overall) || length(p_overall) != 1L) {
    stop("p_overall must be logical of length 1")
  } else if (p_overall && length(p) != 1L) {
    stop("if p_overall = TRUE, then length(p) must be 1")
  }
}

check_args_MAR <- function(ds, cols_mis, cols_ctrl) {
  # check cols_ctrl -----------------------------
  if (!is.null(cols_ctrl)) {
    if (is.numeric(cols_ctrl)) {
      if (any(cols_ctrl < 1 | cols_ctrl > ncol(ds))) {
        stop("indices in cols_ctrl must be in 1:ncol(ds)")
      }
    } else if (is.character(cols_ctrl)) {
      if (!all(cols_ctrl %in% colnames(ds))) {
        stop("all entries of cols_ctrl must be in colnames(ds)")
      }
    } else {
      stop("cols_ctrl must be a vector of column names or indices of ds")
    }
  }
  # no NA in cols_ctrl
  if (any(is.na(ds[, cols_ctrl]))) {
    stop("cols_ctrl must be completely observed; no NAs in ds[, cols_ctrl] allowed")
  }

  if (length(cols_mis) != length(cols_ctrl)) {
    stop("length(cols_mis) must equal length(cols_ctrl)")
  }

  # check if any ctrl_col is in cols_mis
  if (any(cols_ctrl %in% cols_mis)) {
    stop(
      "to ensure MAR no ctrl_col is allowed to be in cols_mis;\n",
      "problematic cols_ctrl:\n",
      paste(cols_ctrl[cols_ctrl %in% cols_mis], collapse = ", ")
    )
  }
}

check_args_MNAR <- function(ds, cols_mis) {
  #  no NA in cols_mis
  if (any(is.na(ds[, cols_mis]))) {
    stop("cols_mis must be completely observed; no NAs in ds[, cols_mis] allowed")
  }
}

## More helpers ---------------------------------------------------------------
adjust_p <- function(p, cols_mis) {
  if (length(p) == 1L) {
    p <- rep(p, length(cols_mis))
  }
  p
}
