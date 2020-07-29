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


document_LSimpute <- function(ending) {
  c(
    paste0("@title LSimpute_", ending),
    paste0(
      "@description Perform LSimpute_", ending,
      " as described by Bo et al. (2004)"
    ),
    "@template impute",
    paste0(
      "@details This function performs LSimpute_", ending, " as described by ",
      "Bo et al. (2004).The function assumes that the genes are the rows of `ds`."
    ),
    "@export",
    paste0(
      "@references Bo, T. H., Dysvik, B., & Jonassen, I. (2004). LSimpute: ",
      "accurate estimation of missing values in microarray data with least ",
      "squares methods. Nucleic acids research, 32(3), e34"
    ),
    "@family LSimpute functions",
    paste0(
      "@examples ",
      "set.seed(123)\n",
      "ds_mis <- delete_MCAR(mvtnorm::rmvnorm(100, rep(0, 10)), 0.1)\n",
      "ds_imp <- impute_LS_", ending, "(ds_mis)"
    )
  )
}
