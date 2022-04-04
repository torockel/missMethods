are_clusters_identical <- function(clus1, clus2) {
  values_unique1 <- unique(clus1) # in the order of appearance!
  values_unique2 <- unique(clus2) # see doc: "duplicate elements/rows removed"
  if (length(values_unique1) != length(values_unique2)) {
    return(FALSE)
  }
  all(match(clus1, values_unique1) == match(clus2, values_unique2))
}

transform_gmc_parameters <- function(gmc_parameters, ds) {
  mu <- do.call(rbind, gmc_parameters$theta$mu)
  list(
    lambda = gmc_parameters$theta$pie,
    mu = mu,
    sigma = gmc_parameters$theta$sigma,
    class = apply(gmc_parameters$kappa, 1, which.max)
  )
}
