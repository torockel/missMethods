are_clusters_identical <- function(clus1, clus2) {
  values_unique1 <- unique(clus1) # in the order of appearance!
  values_unique2 <- unique(clus2) # see doc: "duplicate elements/rows removed"
  if (length(values_unique1) != length(values_unique2)) {
    return(FALSE)
  }
  for(i in seq_along(values_unique1)) {
    # compare "normalized" clusters
    if (any((clus1 == values_unique1[i]) != (clus2 == values_unique2[i]))) {
      return(FALSE)
    }
  }
  TRUE
}

matrix_from_lower_tri_values <- function(lower_tri_values, dim_m) {
  M <- matrix(0, dim_m, dim_m)
  ind_values <- 1L
  for(i in seq_len(dim_m)) {
    for(j in seq_len(i)) {
      M[i, j] <- lower_tri_values[ind_values]
      ind_values <- ind_values + 1L
    }
  }
  # M[lower.tri(M, TRUE)] <- lower_tri_values # does not work because of order of lower_tri_values
  M[upper.tri(M)] <- t(M)[upper.tri(M)]
  M
}

get_cov_matrices <- function(LTSigma, dim_m) {
  res <- list()
  for(i in seq_len(nrow(LTSigma))) {
    res[[i]] <- matrix_from_lower_tri_values(LTSigma[i, ], dim_m)
  }
  res
}

transform_gmc_parameters <- function(gmc_parameters, ds) {
  list(
    lambda = gmc_parameters$pi,
    mu = gmc_parameters$Mu,
    sigma = get_cov_matrices(gmc_parameters$LTSigma, ncol(ds)),
    LTSigma = gmc_parameters$LTSigma,
    class = gmc_parameters$class
  )
}
