#' LSimpute_gene
#'
#' Performance LSimpute_gene as described by Bo et al. (2004)
#'
#' @template impute
#'
#' @details
#'
#' This function performance LSimpute_gene as described by Bo et al. (2004). The
#' function assumes that the genes are the rows of `ds`.
#'
#' Bo et al. (2004) seem to have chosen `min_common_obs = 5`. However, they did
#' not documentation this behavior. This value emerged from inspecting
#' imputation results from the  original jar-file, which is provided by Bo et
#' al. (2004). The only known difference between this function and the original
#' one is that a row with only missing values is imputed by the observed column
#' means. In this case, a warning is given. The original function would not
#' impute such a column and return a dataset with missing values in this row.
#'
#' @param k number of most correlated genes used for the imputation of a gene
#' @param eps used in the calculation of the weights (Bo et al. (2004) used
#'  `eps = 1e-6`)
#' @param min_common_obs a row can only take part in the imputation of another
#'   row, if both rows share at least `min_common_obs` columns with no missing
#'   values. Rows with less observed values than `min_common_obs` are imputed by
#'   the mean of the observed row values.
#' @param return_r_max logical, normally this should be `FALSE`. `TRUE` is used
#'   inside of `impute_LSA` to speed up some computations.
#'
#' @return  If `return_r_max = TRUE`, a list with the imputed dataset and r_max
#'
#' @references Bo, T. H., Dysvik, B., & Jonassen, I. (2004). LSimpute: accurate
#'   estimation of missing values in microarray data with least squares methods.
#'   Nucleic acids research, 32(3), e34
#' @export
#'
#' @examples
#' impute_LS_gene(data.frame(X = 1:11, Y = c(1:10, NA)))
impute_LS_gene <- function(ds, k = 10, eps = 1e-6, min_common_obs = 5,
                           return_r_max = FALSE) {

  if (k >= nrow(ds)) {
    stop("k must be smaller as nrow(ds)")
  }

  if (min_common_obs < 3) {
    stop("min_common_obs should be bigger as 2 to allow calculations for correlations and regression models")
  }

  if(return_r_max) {
    r_max_matrix <- matrix(NA, nrow = nrow(ds), ncol = ncol(ds))
  }

  ds_imp <- ds
  M <- is.na(ds)

  for(i in 1:nrow(ds)) {
    M_i <- M[i, ]
    if(any(M_i)) {  # only impute, if any missing value in row i

      ## check some special cases ---------------------------------------------

      if(all(M_i)) { # all values in row i are missing
        ## Bo et al. do not impute in this case, they return ds with NA values!
        warning("No observed value in row " , i, ". This row is imputed with column means.")
        ds_imp[i, ] <- colMeans(ds, na.rm = TRUE)

      } else if (sum(!M_i) < min_common_obs) { # less than min_common_obs observed values in row i
        ## Bo et al. impute all values in these rows with the mean of the observed row values
        ## (source: try and error with original jar-file, see test-file)
        ds_imp[i, M_i] <- mean(ds[i, !M_i])

      } else { # at least min_common_obs (>= 3) observed values in row i -> "normal" LSimpute_gene possible

        ## LSimpute_gene for one row ------------------------------------------

        ## calculate correlation and order rows by correlation
        similarity_i <- calc_similarity(ds, i, M, M_i)
        ordered_similarity <- order(similarity_i, decreasing = TRUE)

        ## for every missing value j_ind in row i calculate the imputation value separately
        for(j_ind in which(M_i)) {
          ## find the k suitable rows for imputation and save their index in suitable
          suitable <- find_suitable_rows(k, ordered_similarity, M, M_i, j_ind, min_common_obs = min_common_obs)

          if (length(suitable) == 0) { # no suitable row found -> impute mean
            ## this condition may crashes the jar-file from Bo et al. (2004)
            ## we impute the observed row mean
            warning("No suitable row for the imputation of row ", i,
                    " and column ", j_ind, " found! Value is imputed with observed row mean.")
            ds_imp[i, M_i] <- mean(ds[i, ], na.rm = TRUE)

          } else { # everything fine -> proceed with "normal" LSimpute_gene

            if(return_r_max) { # save highest correlation
              r_max_matrix[i, j_ind] <- similarity_i[suitable[1]]
            }

            ## calculate imputation values (with regression) for every suitable row separate and save in y
            y <- calc_y_LS_gene(ds, i, j_ind, suitable, M , M_i)


            ## calculate weights  ---------------------------------------------
            w <- (similarity_i[suitable]^2 / ( 1- similarity_i[suitable]^2 + eps))^2
            w <- w / sum(w)


            ## calculate final imputation value -------------------------------
            ds_imp[i, j_ind] <- sum(w * y)
          }
        }
      }
    }
  }

  ## return value -------------------------------------------------------------
  if(return_r_max) {
    return(list(imp = ds_imp, r_max = r_max_matrix))
  } else {
    return(ds_imp)
  }
}

## helpers for LSimpute_gene --------------------------------------------------

calc_y_LS_gene <- function(ds, i, j_ind, suitable, M = is.na(ds), M_i = M[i, ]) {
  y <- numeric(length(suitable))
  for(j in 1:length(suitable)) {
    common_observed <- !(M_i | M[suitable[j], ])  # this will be at least min_common_obs (>= 3) TRUEs, (requirement for suitable) -> regression possible
    lm_coef <- calc_lm_coefs_simple_reg(ds[i, common_observed], ds[suitable[j], common_observed])
    y[j] <- lm_coef[1] + lm_coef[2] * ds[suitable[j], j_ind]
  }
  y
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
  sum_xy <- sum(x*y)
  sum_x_sq <- sum(x ^2)
  n <- length(x)
  beta_1 <- (sum_xy - sum_x * sum_y / n) / (sum_x_sq - sum_x ^2 / n)
  beta_0 <- sum_y / n - beta_1 * sum_x / n
  c(beta_0, beta_1)
}




calc_similarity <- function(ds, i, M = is.na(ds), M_i = M[i, ]) {
  similarity_i <- abs(stats::cor(t(ds), ds[i, ], use = "pairwise.complete.obs"))
  as.vector(similarity_i)
}


calc_common_obs <- function(M, M_i) {
  nr_vars <- ncol(M)
  apply(M, 1, function(x) {nr_vars - sum(x | M_i)})
}


find_suitable_rows <- function(k, ordered_similarity, M, M_i, j_ind, min_common_obs = 5) {
  kl <- min(k + 1, nrow(M))  # loop variable (number of checked neighbors)
  ## start with k + 1 neighbors, because row i will be one of the nearest neighbors
  nr_suitable <- 0
  while (nr_suitable < k) {
    potential_genes <- ordered_similarity[1:kl]   # the row to be imputed is removed later with the NA_in_j_ind check.
    min2_common <- calc_common_obs(M[potential_genes, ], M_i) >= min_common_obs #otherwise no lin. regression possible
    potential_genes <- potential_genes[min2_common]
    NA_in_j_ind <- M[potential_genes, j_ind]  # missing value in column j_ind (which should be imputed)?
    suitable <- potential_genes[!NA_in_j_ind] # these rows are not suitable (missing values in column j_ind)
    nr_suitable <- length(suitable)
    if (kl >= nrow(M)) {  # to prevent infinite loop, if there are not enough suitable rows
      # warning(paste("Not enough suitable rows for", i, ",", j_ind))
      break() # exit loop with the found suitable rows (there are no more additional rows to check!)
    } else {
      kl <- kl + 1
    }
  }
  return(suitable)
}
