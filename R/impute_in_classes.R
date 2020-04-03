
find_classes <- function(ds, class_cols, breaks, donor_limit, type) {
  okay <- FALSE
  while (!okay) {
    split_indices <- get_split_indices(ds, class_cols, breaks)
    okay <- is_split_okay(ds, split_indices, donor_limit, type)
  }
  split_indices
}


get_split_indices <- function(ds, class_cols, breaks = Inf) {
  ds <- as.data.frame(ds) # to get indices by split and easier cutting
  if(!is.null(row.names(ds))) # to get always indices and not names from rows
    row.names(ds) <- NULL
  group_factors <- list()
  for (i in seq_along(class_cols)) {
    if (is.numeric(ds[, class_cols[i]]) && is.finite(breaks)) { # first cut into intervalls
      ds[, class_cols[i]] <- cut(ds[, class_cols[i], drop = TRUE], breaks)
    }
    group_factors[[i]] <- as.factor(ds[, class_cols[i]])
  }
  ds_split <- split(ds, group_factors, drop = TRUE)
  lapply(ds_split, function(x) as.integer(rownames(x)))
}


is_split_okay <- function(ds, split_indices, donor_limit, type) {
  # to be done
}
