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
