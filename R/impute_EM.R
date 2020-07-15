# maxit and criterion are passed to norm::em.norm!
# if EM does not converge: warning

get_EM_parameters <- function(ds, maxits = 1000, criterion = 0.0001) {

  ## check for norm and MASS
  check_for_packages(c("norm", "MASS"))

  ## get EM parameters from norm (1. part) ------------------------------------
  input_for_norm <- norm::prelim.norm(as.matrix(ds)) # prelim only accepts matrices as input!

  iterations <- tryCatch(utils::capture.output(EM_parm <- norm::em.norm(input_for_norm,
                                                              showits = TRUE,
                                                              maxits = maxits + 1)),
                         error = function(cnd){ # if EM crashes, return error
                           return(cnd)
                         })


  ## check for EM problems ----------------------------------------------------
  # check for crash inside of norm
  if (any(class(iterations) == "error")) {
    stop("The EM algorithm of norm produced an error; no imputation was done.",
    "\nError message from norm:", iterations, call. = FALSE)
  }

  # check if number of iterations > maxits
  iterations <- iterations[2]
  iterations <- substr(iterations, regexpr("[[:digit:]]*\\.\\.\\.$", iterations),
                       nchar(iterations) - 3)
  iterations <- as.integer(iterations)
  if(iterations > maxits) {
    warning("EM did not converge with maxits = ", maxits, ". Some imputation values are maybe unreliable.")
  }

  ## get EM parameters from norm (2. part) ------------------------------------
  EM_parm <- norm::getparam.norm(input_for_norm, EM_parm)

  structure(EM_parm, iterations = iterations)
}


impute_EM <- function(ds, stochastic = TRUE, maxits = 1000, criterion = 0.0001) {

  EM_parm <- get_EM_parameters(ds, maxits = maxits, criterion = criterion)
  M <- is.na(ds)
  problematic_rows <- integer(0)

  for (i in seq_len(nrow(ds))) {
    M_i <- M[i, ]
    if (any(M_i)) { # only impute, if any missing value in row i
      sigma_22_inv <- tryCatch(solve(EM_parm$sigma[!M_i, !M_i]), # try to invert matrix
        error = function(e) { # if matrix singular -> add row to problematic_rows
          return(NULL)
        }
      )
      if (!is.null(sigma_22_inv)) { # Sigma_22 is invertible -> normal EM-Imputation
        y_imp <- EM_parm$mu[M_i] + EM_parm$sigma[M_i, !M_i] %*% sigma_22_inv %*% (ds[i, !M_i] - EM_parm$mu[!M_i])
        y_imp <- as.vector(y_imp)
        if (stochastic) {
          var_y_imp <- EM_parm$sigma[M_i, M_i] - EM_parm$sigma[M_i, !M_i] %*% sigma_22_inv %*% EM_parm$sigma[!M_i, M_i]
          # to guarantee symmetry of matrix (sometimes numeric accuracy problems with above calculation):
          var_y_imp <- (var_y_imp + t(var_y_imp)) / 2
          y_imp <- y_imp + MASS::mvrnorm(n = 1, mu = rep(0, sum(M_i)), Sigma = var_y_imp)
        }
        ds[i, M_i] <- y_imp
      } else { # Simga_22 is not invertible -> add row to problematic_rows
        problematic_rows <- c(problematic_rows, i)
      }
    }
  }

  ## handle rows, for which sigma_22 is not invertible ------------------------
  if (length(problematic_rows) > 0) {
    ds_imp <- impute_mean(ds)
    ds[problematic_rows, ][M[problematic_rows, ]] <- ds_imp[problematic_rows, ][M[problematic_rows, ]]
    warning(
      "Row(s) ", paste(problematic_rows, collapse = ", "),
      " were imputed with mean values, ",
      "because EM covariance matrix is not positive-definite."
    )
  }

  ds
}
