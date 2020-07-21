#' LSimpute_array
#'
#'
#' Performance LSimpute_array as described by Bo et al. (2004)
#'
#' @details
#' This function performances LSimpute_array as described by Bo et al. (2004).
#' The function assumes that the genes are the rows of `ds`.
#'
#' @template impute
#'
#' @param k directly passed to [impute_LS_gene()]
#' @param eps directly passed to [impute_LS_gene()]
#' @param min_common_obs directly passed to [impute_LS_gene()]
#' @param ds_impute_LS_gene result of imputing `ds` with `ds_impute_LS_gene()`, if this already exists.
#'
#' @seealso [impute_LS_gene()], which is used for the first imputation of the missing values

#' @references Bo, T. H., Dysvik, B., & Jonassen, I. (2004). LSimpute: accurate
#'   estimation of missing values in microarray data with least squares methods.
#'   Nucleic acids research, 32(3), e34
#' @export
#'
#' @examples
#' impute_LS_array(data.frame(X = 1:11, Y = c(1:10, NA)))
impute_LS_array <- function(ds, k = 10, eps = 1e-6, min_common_obs = 5, ds_impute_LS_gene = NULL) {

  ## Estimate the parameters via the imputed dataset from impute_LS_gene() ----
  if (is.null(ds_impute_LS_gene)) { # dataset not given -> impute via impute_LS_gene()
    ds_impute_LS_gene <- impute_LS_gene(ds, k = k, eps = eps, min_common_obs = min_common_obs)
  }
  # Bo et al. (2004) use the empirical covariance matrix (divisor: n -1), stats::cov() uses n as divisor
  S <- stats::cov(ds_impute_LS_gene) * nrow(ds_impute_LS_gene) / (nrow(ds_impute_LS_gene) -1 )
  col_means <- colMeans(ds_impute_LS_gene)
  # The LS_gene imputed dataset is not needed anymore after the parameters are estimated
  remove(ds_impute_LS_gene)

  ## Define some variables ----------------------------------------------------
  M <- is.na(ds)
  ds_imp <- as.matrix(ds) # need a matrix for %*%

  ## Impute row by row --------------------------------------------------------
  for(i in 1:nrow(ds_imp)) { # rowwise imputation
    M_i <- M[i, ]
    if(any(M_i)) { # any missing value in row i?
      if(all(M_i)) { # no observed value in row i
        # imputation of colMeans
        ds_imp[i, ] <- col_means
      } else {
        y_1_mean <- col_means[M_i]
        y_2_mean <- col_means[!M_i]
        y <- ds_imp[i, ]
        y_2 <- y[!M_i]
        S_12 <- S[M_i, !M_i, drop = FALSE]
        S_22 <- S[!M_i, !M_i, drop = FALSE]
        S_22_inv <- tryCatch(solve(S_22),
                             error = function(cond){
                               warning("S_22_inv was singular")
                               matrix(0, nrow = nrow(S_22), ncol = nrow(S_22))
                             })  # if S_22 is singular -> impute col_means
        y_imp <- y_1_mean + S_12 %*% S_22_inv %*% (y_2 - y_2_mean)
        ds_imp[i, M_i] <- y_imp
      }
    }
  }
  # to return the type of ds, which maybe is not a matrix!
  ds[M] <- ds_imp[M]
  ds
}
