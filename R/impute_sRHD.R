#' Simple random hot deck imputation
#'
#' Impute missing values in a data frame or a matrix using a simple random hot
#' deck
#'
#' There are three types of simple random hot decks implemented. They can be
#' selected via \code{type}:
#' \itemize{
#' \item{"cols_seq" (the default): Each variable (column) is handled
#' separately. If an object (row) has a missing value in a variable (column),
#' then one of the observed values in the same variable is chosen randomly and
#' the missing value is replaced with this chosen value. This is done for all
#' missing values.}
#' \item{"sim_comp": All missing variables (columns) of an object are imputed together
#' ("simultaneous"). For every object with missing values (such an object is
#' called a recipient in hot deck terms), one complete object is chosen randomly
#' and all missing values of the recipient are imputed with the values from the
#' complete object. A complete object used for imputation is called a donor.}
#' \item{"sim_part": All missing variables (columns) of an object are imputed
#' together ("simultaneous"). For every object with missing values (recipient)
#' one donor is chosen. The donor must have observed values in all the variables
#' that are missing in the recipient. The donor is allowed to have unobserved
#' values in the non-missing parts of the recipient. So, in contrast to
#' "sim_comp", the donor can be partly incomplete.} }
#'
#' The parameter \code{donor_limit} controls how often an object can be a donor.
#' This parameter is only implemented for types "cols_seq" and "sim_comp". If
#' \code{type = "sim_part"} and \code{donor_limit} is not \code{Inf}, then an
#' error will be thrown. For "sim_comp" the default value (\code{Inf}) allows
#' every object to be a donor for an infinite number of times (there is no
#' restriction on the times an object can be a donor). If a numeric value less
#' than \code{Inf} is chosen, then every object can be a donor at most
#' \code{donor_limit} times. For example \code{donor_limit = 1} ensures that
#' every object donates at most one time. If there are only few complete objects
#' and \code{donor_limit} is set too low, then an imputation might not be
#' possible with the chosen \code{donor_limit}. In this case, the
#' \code{donor_limit} will be adjusted (see examples). Setting \code{donor_limit
#' = "min"} chooses automatically the minimum value for \code{donor_limit} that
#' allows imputation of all missing values. For \code{type = "cols_seq"} the
#' donor limit is applied for every column separately.
#'
#'
#' @param ds a data frame or matrix with missing values
#' @param type the type of hot deck; the default ("cols_seq") is a random hot
#'   deck that imputes each column separately. Other choices are "sim_comp" and
#'   "sim_part". Both impute all missing values in an object (row)
#'   simultaneously using a single donor object. The difference between the two
#'   types is the choice of objects that can act as donors. "sim_comp:" only
#'   completely observed objects can be donors. "sim_part": all objects that
#'   have no missing values in the missing parts of a recipient can be donors.
#' @param donor_limit numeric of length one or "min"; how many times an object
#'   can be a donor. default is \code{Inf} (no restriction).
#'
#' @return An object of the same class as \code{ds} with imputed missing values
#' @references Andridge, R. R., & Little, R. J. (2010). A review of hot deck
#'   imputation for survey non-response. \emph{International statistical review},
#'   78(1), 40-64.
#'
#' @export
#'
#' @examples
#' ds <- data.frame(X = 1:20, Y = 101:120)
#' ds_miss <- delete_MCAR(ds, 0.2)
#' ds_imp <- impute_sRHD(ds_miss)
#' \donttest{
#' # Warning: donor limit to low
#' ds_miss_one_donor <- ds
#' ds_miss_one_donor[1:19, "X"] <- NA
#' impute_sRHD(ds_miss_one_donor, donor_limit = 3)
#' }
impute_sRHD <- function(ds, type = "cols_seq", donor_limit = Inf) {
  type <- match.arg(type, c("cols_seq", "sim_comp", "sim_part"))

  # missing data indicator matrix M and short M check -----
  M <- is.na(ds) # missing data indicator matrix
  completely_missing_col <- which(apply(M, 2, all))
  if (length(completely_missing_col) != 0) {
    stop(
      "imputation not possible: column(s) ",
      paste(completely_missing_col, collapse = ", "),
      " are completely missing (all entries are NA)"
    )
  }

  # check donor_limit -------------------------------------
  # and  convert donor_limit = "min" to appropriate limit
  if (type == "sim_part") {
    if (length(donor_limit) != 1 | !is.infinite(donor_limit)) {
      stop("donor_limit is not implemented for type = sim_part")
    }
  } else { # types: cols_seq or sim_comp
    if (is.numeric(donor_limit)) {
      if (length(donor_limit) != 1L) {
        stop("donor_limit must be of length 1")
      } else if (donor_limit < 1) {
        stop("donor_limit must be a number >= 1 or the string 'min'")
      } else { # donor_limit high enough?
        theo_min_donor_lim <- min_donor_limit(M, type)
        if (donor_limit < theo_min_donor_lim) {
          warning(
            "donor_limit = ", donor_limit, " is to low to impute all missing values; ",
            "it was set to ",
            theo_min_donor_lim
          )
          donor_limit <- theo_min_donor_lim
        }
      }
    } else if (is.character(donor_limit) & donor_limit == "min") {
      donor_limit <- min_donor_limit(M, type) # convert "min"
      message(paste("donor_limit is set to:", donor_limit))
    } else {
      stop("donor_limit must be a number >= 1 or the string 'min'")
    }
  }

  # imputation --------------------------------------------
  if (type == "cols_seq") { # cols_seq --------------------
    ds <- impute_sRHD_cols_seq(ds, M, donor_limit)
  } else if (type == "sim_comp") { # sim_comp -------------
    ds <- impute_sRHD_sim_comp(ds, M, donor_limit)
  } else if (type == "sim_part") { # sim_part ---------------
    ds <- impute_sRHD_sim_part(ds, M)
  }
  ds
}


impute_sRHD_cols_seq <- function(ds, M = is.na(ds), donor_limit) {
  if (is.infinite(donor_limit)) { # Inf donor_limit -> easy/faster implementation
    for (k in seq_len(ncol(ds))) {
      ds[M[, k], k] <- resample(ds[!M[, k], k, drop = TRUE], sum(M[, k]), replace = TRUE)
    }
  } else { # finite donor_limit ----------------------------
    for (k in seq_len(ncol(ds))) {
      recipients <- M[, k]
      pot_donors <- which(!recipients)
      recipients <- which(recipients)
      donor_limit_obj <- rep(donor_limit, nrow(ds)) # donor_limit for every col separate

      for (i in seq_along(recipients)) {
        pot_donors <- pot_donors[donor_limit_obj[pot_donors] > 0]
        donor <- resample(pot_donors, 1)
        ds[recipients[i], k] <- ds[donor, k]
        donor_limit_obj[donor] <- donor_limit_obj[donor] - 1
      }
    }
  }

  ds
}


impute_sRHD_sim_comp <- function(ds, M = is.na(ds), donor_limit) {
  recipients <- apply(M, 1, any)
  pot_donors <- which(!recipients)
  recipients <- which(recipients)
  # if (length(pot_donors) == 0) { # error is thrown in fun min_donor_limit
  #   stop("imputation not possible: there is no completely observed object")
  # }
  if (is.infinite(donor_limit) || isTRUE(all.equal(donor_limit, 1))) {
    # important special cases: no donor limit and sampling without replacement
    replace_donors <- ifelse(is.infinite(donor_limit), TRUE, FALSE)
    donors_match <- resample(pot_donors, length(recipients), replace = replace_donors)
    for (i in seq_along(recipients)) {
      ds[recipients[i], M[recipients[i], ]] <- ds[donors_match[i], M[recipients[i], ]]
    }
    # vectorized but ugly and does not work for tibbles!
    # ds[recipients, ][M[recipients, ]] <- ds[donors_match, ][M[recipients, ]]
  } else {
    donor_limit_obj <- rep(donor_limit, nrow(ds))
    for (i in seq_along(recipients)) {
      pot_donors <- pot_donors[donor_limit_obj[pot_donors] > 0]
      donor <- resample(pot_donors, 1)
      ds[recipients[i], M[recipients[i], ]] <- ds[donor, M[recipients[i], ]]
      donor_limit_obj[donor] <- donor_limit_obj[donor] - 1
    }
  }
  ds
}


impute_sRHD_sim_part <- function(ds, M = is.na(ds)) {
  md_patterns <- find_md_patterns(M)
  pattern_matrix <- md_patterns$pattern_matrix
  pattern_obj <- md_patterns$pattern_obj
  rm(md_patterns)
  for (pat_ind in seq_len(nrow(pattern_matrix))) {
    if (any(pattern_matrix[pat_ind, ])) { # if not -> completely observed
      pot_donor_pattern_nrs <- find_pot_donor_pattern_nrs(
        pattern_matrix,
        pattern_matrix[pat_ind, ]
      )
      if (length(pot_donor_pattern_nrs) == 0L) {
        stop(
          "there is no appropriate donor for the object(s) ",
          paste(pattern_obj[pat_ind], collapse = ", ")
        )
      }
      pot_donors <- unlist(pattern_obj[pot_donor_pattern_nrs])
      recipients <- pattern_obj[[pat_ind]]
      donors_match <- resample(pot_donors, length(recipients), replace = TRUE)
      for (i in seq_along(recipients)) {
        ds[recipients[i], M[recipients[i], ]] <- ds[donors_match[i], M[recipients[i], ]]
      }
    }
  }
  ds
}


# helper: min_donor_limit ---------------------------------
min_donor_limit <- function(M, type) {
  if (type == "sim_comp") {
    nr_incomp_obs <- sum(apply(M, 1, any))
    nr_comp_obs <- nrow(M) - nr_incomp_obs
    if (nr_comp_obs > 0) {
      min_limit <- ceiling(nr_incomp_obs / nr_comp_obs)
    } else {
      stop(
        "calculation of min donor_limit and Imputation with type = ",
        type,
        " not possible: there is no completely observed object"
      )
    }
  } else if (type == "cols_seq") {
    max_nr_incomp_in_one_col <- max(apply(M, 2, sum))
    min_nr_comp_in_one_col <- nrow(M) - max_nr_incomp_in_one_col
    if (min_nr_comp_in_one_col > 0) {
      min_limit <- ceiling(max_nr_incomp_in_one_col / min_nr_comp_in_one_col)
    } else {
      stop(
        "calculation of min donor_limit and Imputation with type = ",
        type,
        " not possible: there are completely unobserved variables"
      )
    }
  } else {
    stop("type = ", type, " not implemented")
  }
  min_limit
}
