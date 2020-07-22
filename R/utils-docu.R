MNAR_documentation <- function(ending) {
  c(
    "@template MNAR",
    "@template delete",
    paste0(
      "@details The functions \\code{delete_MNAR_", ending, "} and ",
      "\\code{\\link{delete_MAR_", ending, "}} are sisters. ",
      "The only difference between these two functions is the column that ",
      "controls the generation of missing values. ",
      "In \\code{\\link{delete_MAR_", ending, "}} a separate column ",
      "\\code{cols_ctrl[i]} controls the generation of missing values in ",
      "\\code{cols_mis[i]}. ",
      "In contrast, in \\code{delete_MNAR_", ending, "} the generation ",
      "of missing values in \\code{cols_mis[i]} is controlled by ",
      "\\code{cols_mis[i]} itself. ",
      "All other aspects are identical for both functions. ",
      "Therefore, further details can be found in ",
      "\\code{\\link{delete_MAR_", ending, "}}. "
    ),
    paste0("@inheritParams delete_MAR_", ending),
    paste0("@seealso \\code{\\link{delete_MAR_", ending, "}}"),
    "@export",
    paste0(
      "@examples ",
      "ds <- data.frame(X = 1:20, Y = 101:120)"
    )
  )
}
