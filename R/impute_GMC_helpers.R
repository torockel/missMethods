are_clusters_identical <- function(clus1, clus2) {
  values_unique1 <- unique(clus1) # in the order of appearance!
  values_unique2 <- unique(clus2) # see doc: "duplicate elements/rows removed"
  if (length(values_unique1) != length(values_unique2)) {
    return(FALSE)
  }
  all(match(clus1, values_unique1) == match(clus2, values_unique2))
}

get_cov_matrices <- function(LTSigma, dim_m) {
  res <- list()
  for(i in seq_len(nrow(LTSigma))) {
    res[[i]] <- EMCluster::LTsigma2var(LTSigma[i, ], dim_m)
  }
  res
}

transform_gmc_parameters <- function(gmc_parameters, ds) {
  res <- list(
    lambda = gmc_parameters$pi,
    mu = gmc_parameters$Mu,
    sigma = get_cov_matrices(gmc_parameters$LTSigma, ncol(ds)),
    LTSigma = gmc_parameters$LTSigma,
    class = gmc_parameters$class
  )
  if (any(vapply(res, anyNA, logical(1)))) {
    return(simpleWarning("EMCluster did not like the dataset."))
  }
  res
}
