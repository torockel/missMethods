# helpers for MD patterns ---------------------------------
find_md_patterns <- function(M) {
  patterns = list()
  pattern_obj = list()
  for(i in seq_len(nrow(M))) {
    pat_nr <- find_md_pattern_nr(M[i, ], patterns)
    if(pat_nr > length(patterns)) { # new pattern
      patterns[[pat_nr]] <- M[i, ]
      pattern_obj[[pat_nr]] <- i
    } else { # existing pattern
      pattern_obj[[pat_nr]] <- c(pattern_obj[[pat_nr]], i)
    }
  }

  pattern_matrix <- matrix(unlist(patterns), byrow = TRUE, ncol = ncol(M))
  list(pattern_matrix = pattern_matrix, pattern_obj = pattern_obj)
}


find_md_pattern_nr <- function(md_pattern, patterns) {
  for (pat_ind in seq_along(patterns)) {
    if (all(md_pattern == patterns[[pat_ind]])) {
      return(pat_ind)
    }
  }
  return(length(patterns) + 1L)
}


find_pot_donor_pattern_nrs <- function(pattern_matrix, recep_pattern) {
  possible_pattern_nrs <- integer()
  for(pat_ind in seq_len(nrow(pattern_matrix))) {
    # possible_pattern: at least in one pattern the variable is observed
    if(!any(pattern_matrix[pat_ind, ] & recep_pattern)) {
      possible_pattern_nrs <- c(possible_pattern_nrs, pat_ind)
    }
  }
  possible_pattern_nrs
}
