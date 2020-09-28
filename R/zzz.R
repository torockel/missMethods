.onLoad <- function(libname, pgkname) {
  op <- options()
  op.missMethods <- list(
    missMethods.warn.too.high.p = TRUE
  )
  toset <- !(names(op.missMethods) %in% names(op))
  if (any(toset)) options(op.missMethods[toset])
}
