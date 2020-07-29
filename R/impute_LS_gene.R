#' @eval document_LSimpute("gene")
#'
#' @details
#'
#' Bo et al. (2004) seem to have chosen `min_common_obs = 5`. However, they did
#' not document this behavior. This value emerged from inspecting
#' imputation results from the  original jar-file, which is provided by Bo et
#' al. (2004).
#'
#' If there are less than `min_common_obs` observed values in a row and at least
#' one observed value, the mean of the observed row values is imputed. If no
#' value is observed in a row, the observed column means are imputed for the
#' missing row values. This is the only known difference between this function
#' and the original one from Bo et al. (2004). The original function would not
#' impute such a row and return a dataset with missing values in this row. There
#' is one more case that needs a special treatment: If no suitable row can be
#' found to impute a row, the mean of the observed values is imputed, too. If
#' `verbose = TRUE`, a message will be given for the encountered instances of
#' the described special cases. If `verbose = FALSE`, the function will deal
#' with theses cases silently.
#'
#'
#' @param k Number of most correlated genes used for the imputation of a gene.
#' @param eps Used in the calculation of the weights (Bo et al. (2004) used
#'   `eps = 1e-6`).
#' @param min_common_obs A row can only take part in the imputation of another
#'   row, if both rows share at least `min_common_obs` columns with no missing
#'   values.
#' @param return_r_max Logical; normally, this should be `FALSE`. `TRUE` is
#'   used inside of `impute_LS_adaptive()` to speed up some computations.
#' @param verbose Should messages be given for special cases (see details)?
#'
#' @return If `return_r_max = TRUE`, a list with the imputed dataset and r_max.
#'
impute_LS_gene <- function(ds, k = 10, eps = 1e-6, min_common_obs = 5,
                           return_r_max = FALSE, verbose = FALSE) {

  ## Check some arguments -----------------------------------------------------
  if (k >= nrow(ds)) {
    stop("k must be smaller as nrow(ds)")
  }
  if (min_common_obs < 3) {
    stop(
      "min_common_obs should be bigger as 2 to allow calculations for ",
      "correlations and regression models"
    )
  }

  ## Define some variables for later ------------------------------------------
  if (return_r_max) {
    r_max_matrix <- matrix(NA, nrow = nrow(ds), ncol = ncol(ds))
  }
  ds_mat_original <- as.matrix(ds)
  ds_imp <- ds_mat_original
  M <- is.na(ds)
  rows_imp_with_colMeans <- integer(0)
  rows_imp_with_rowMeans_min_obs <- integer(0)
  rows_imp_with_rowMeans_no_suitable <- integer(0)

  ## Impute row by row --------------------------------------------------------
  for (i in seq_len(nrow(ds))) {
    M_i <- M[i, ]
    if (any(M_i)) { # only impute, if any missing value in row i

      ## Check for enough observations in row i -------------------------------
      if (all(M_i)) { # all values in row i are missing
        # Bo et al. do not impute in this case, they return ds with NA values!
        ds_imp[i, ] <- colMeans(ds, na.rm = TRUE)
        rows_imp_with_colMeans <- c(rows_imp_with_colMeans, i)

      } else if (sum(!M_i) < min_common_obs) { # less than min_common_obs observed values in row i
        # Bo et al. impute all values in these rows with the mean of the observed row values
        # (source: try and error with original jar-file, see test-file)
        ds_imp[i, M_i] <- mean(ds[i, !M_i])
        rows_imp_with_rowMeans_min_obs <- c(rows_imp_with_rowMeans_min_obs, i)

      } else {
        ## Enough observations in row i ------------------------------------
        # At least min_common_obs (>= 3) observed values in row i
        # ->  proceed with "normal" LSimpute_gene

        # Possible candidate rows (common obs!) for imputing and their correlation
        rows_candidates <- find_rows_candidates(ds_mat_original, i, M, M_i, min_common_obs)
        # Check if at least one row candidate is found
        if (nrow(rows_candidates) == 0) { # no rows_candidates found -> impute mean
          # This condition may crashes the jar-file from Bo et al. (2004)
          ds_imp[i, M_i] <- mean(ds[i, ], na.rm = TRUE)
          rows_imp_with_rowMeans_no_suitable <- c(rows_imp_with_rowMeans_no_suitable, i)

        } else { # at least one candidate row -> proceed with "normal" LSimpute_gene

          # Save calculated regression coefficients (later)
          betas <- matrix(NA_real_, nrow = nrow(rows_candidates), ncol = 2)
          # Idea for betas: colnames(betas) <- c("beta_0", "beta_1")

          ## Impute value by value in row i -----------------------------------
          for (j_ind in which(M_i)) {
            # find the k suitable rows for imputation and save their index from rows_candidates
            suitable_logic <- !M[rows_candidates$row_index, j_ind] # No NA in j_ind -> suitable
            suitable_index <- which(suitable_logic)
            suitable_index <- suitable_index[seq_len(min(k, length(suitable_index)))]

            if (length(suitable_index) == 0) { # no suitable row found -> impute mean
              # This condition may crashes the jar-file from Bo et al. (2004)
              ds_imp[i, M_i] <- mean(ds[i, ], na.rm = TRUE)
              rows_imp_with_rowMeans_no_suitable <- c(rows_imp_with_rowMeans_no_suitable, i)

            } else { # everything fine -> proceed with "normal" LSimpute_gene

              if (return_r_max) { # save highest correlation
                r_max_matrix[i, j_ind] <- rows_candidates$similarity[suitable_index[1]]
              }

              ## Calculate imputation values for all k neighbors --------------
              # Prior to that: check, if any regression coefficient is unknown
              row_indices <- rows_candidates$row_index[suitable_index]
              for (j in seq_along(row_indices)) { # check for unknown regression parameters
                if (is.na(betas[suitable_index[j], 1])) { # calculate regression parameters, if not already known
                  common_observed <- !(M_i | M[row_indices[j], ]) # this will be at least min_common_obs (>= 3) TRUEs, (requirement for suitable) -> regression possible
                  betas[suitable_index[j], ] <- calc_lm_coefs_simple_reg(
                    ds_mat_original[i, common_observed],
                    ds_mat_original[row_indices[j], common_observed]
                  )
                }
              }
              # Calculate the imputation values
              y <- betas[suitable_index, 1] + betas[suitable_index, 2] * ds[row_indices, j_ind]


              ## Calculate weights  ---------------------------------------------
              similarities_sq <- rows_candidates$similarity[suitable_index]^2
              w <- (similarities_sq /
                (1 - similarities_sq + eps))^2
              w <- w / sum(w)


              ## Calculate final imputation value -------------------------------
              ds_imp[i, j_ind] <- sum(w * y)
            }
          }
        }
      }
    }
  }

  ## Messages, if verbose = TRUE ----------------------------------------------
  if (verbose) {
    if (length(rows_imp_with_colMeans) > 0) {
      message(
        "No observed value in row(s) ",
        paste(rows_imp_with_colMeans, collapse = ", "), ". ",
        "These rows were imputed with column means.",
        appendLF = FALSE
      )
    }
    if (length(rows_imp_with_rowMeans_min_obs) > 0) {
      message(
        "Not enough observed values in row(s) ",
        paste(rows_imp_with_rowMeans_min_obs, collapse = ", "), ". ",
        "These rows were imputed with osbserved row means.",
        appendLF = FALSE
      )
    }
    if (length(rows_imp_with_rowMeans_no_suitable) > 0) {
      message(
        "No suitable row for the imputation of (parts of) row(s) ",
        paste(rows_imp_with_rowMeans_no_suitable, collapse = ", "), " found. ",
        "These rows (or parts of them) were imputed with osbserved row means.",
        appendLF = FALSE
      )
    }
  }

  ## Return value -------------------------------------------------------------
  ds <- assign_imputed_values(ds, ds_imp, M)
  if (return_r_max) {
    return(list(imp = ds, r_max = r_max_matrix))
  } else {
    return(ds)
  }
}

## Helpers for LSimpute_gene --------------------------------------------------

calc_common_obs <- function(M, M_i) {
  nr_vars <- ncol(M)
  nr_vars - colSums(t(M) | M_i)
}


#' Simple linear regression
#'
#' @param y numeric vector
#' @param x numeric vector
#'
#' @return a vector: first element beta_0, second beta_1
#' @noRd
calc_lm_coefs_simple_reg <- function(y, x) {
  sum_x <- sum(x)
  sum_y <- sum(y)
  sum_xy <- sum(x * y)
  sum_x_sq <- sum(x^2)
  n <- length(x)
  beta_1 <- (sum_xy - sum_x * sum_y / n) / (sum_x_sq - sum_x^2 / n)
  beta_0 <- sum_y / n - beta_1 * sum_x / n
  c(beta_0, beta_1)
}


calc_similarity <- function(ds, y) {
  similarity_i <- abs(stats::cor(t(ds), y, use = "pairwise.complete.obs"))
  as.vector(similarity_i)
}


#' Find candidates genes/rows for the imputation of a row i
#'
#' @param ds whole dataset
#' @param i index of a row of ds, for which candidates should be found
#' @param M missing data indicator matrix
#' @param M_i missing data indicator vector of row i
#' @param min_common_obs documented in impute_LS_gene()
#'
#' @return a data frame with two columns. First column: indices of candidates;
#'  second column: similarity (= abs(correlation)) between row i and candidate rows
#'  data frame is ordered (decreasing) by similarity
#' @noRd
find_rows_candidates <- function(ds, i, M = is.na(ds), M_i = M[i, ], min_common_obs) {
  rows_candidates_index <- which(calc_common_obs(M, M_i) >= min_common_obs & seq_len(nrow(ds)) != i)

  if (length(rows_candidates_index) == 0) { # no rows_candidates found
    return(data.frame(row_index = integer(0)))
  }

  # Calculate correlation
  similarity_i <- calc_similarity(ds[rows_candidates_index, ], ds[i, ])
  # Order rows by similarity
  orderd_index <- order(similarity_i, decreasing = TRUE)
  data.frame(
    row_index = rows_candidates_index[orderd_index],
    similarity = similarity_i[orderd_index]
  )
}
