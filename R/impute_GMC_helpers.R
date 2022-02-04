change_nr <- function(clus_change, clus2, i) {
  nr_new <- clus2[i]
  nr_old <- clus_change[i]
  index_new <- clus_change == nr_old
  index_old <- clus_change == nr_new
  clus_change[index_new] <- nr_new
  clus_change[index_old] <- nr_old
  clus_change
}

are_clusters_identical <- function(clus1, clus2) {
  # very inefficient!
  for (i in seq_along(clus1)) {
    if (clus1[i] != clus2[i]) {
      clus1 <- change_nr(clus1, clus2, i)
    }
  }
  all(clus1 == clus2)
}
