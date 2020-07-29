# helpers for MD patterns ---------------------------------

#' Find missing data patterns
#'
#' @param M missing data indicator matrix (normally via: `is.na(ds)`)
#'
#' @return A list with two components:
#' * `pattern_matrix` A matrix of all MD-patterns of `M` (every pattern is
#'    included only once)
#' * `pattern_obj` A numbered list. The entry number `i` contains all objects
#'   that have MD-pattern in row `i` of `pattern_matrix`
#'
#' @noRd
find_md_patterns <- function(M) {
  pattern_matrix <- unique(M)
  pattern_obj <- match(as.data.frame(t(M)), as.data.frame(t(pattern_matrix)))
  pattern_obj <- split(seq_len(nrow(M)), pattern_obj)
  list(pattern_matrix = pattern_matrix, pattern_obj = pattern_obj)
}


find_pot_donor_pattern_nrs <- function(pattern_matrix, recep_pattern) {
  possible_pattern_nrs <- integer()
  for (pat_ind in seq_len(nrow(pattern_matrix))) {
    # possible_pattern: at least in one pattern the variable is observed
    if (!any(pattern_matrix[pat_ind, ] & recep_pattern)) {
      possible_pattern_nrs <- c(possible_pattern_nrs, pat_ind)
    }
  }
  possible_pattern_nrs
}
