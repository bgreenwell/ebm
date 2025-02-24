#' Print model summary
#'
#' Display basic information about a fitted [ebm] object.
#'
#' @param x A fitted [ebm] object.
#'
#' @param digits The number of significant digits to be passed to [format()]
#' when [print]ing.
#'
#' @param ... Additional optional arguments passed [print.default()].
#'
#' @export
print.EBM <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\nCall:\n", paste(deparse(attr(x, which = "call")), sep = "\n",
                         collapse = "\n"), "\n\n", sep = "")
  cat("Python object:\n", reticulate::py_repr(x), "\n\n", sep = "")
  cat("Number of features:", format(x$n_features_in_), "\n")
  cat("Number of terms:", format(length(x$term_names_)), "\n")
  cat("Number of interactions:", format(sum(grepl("&", x = x$term_names_))), "\n")
  cat("Intercept:", format(x$intercept_, digits = digits), "\n")
  cat("Objective:", x$objective, "\n")
  cat("Link function:", x$link_, "\n")
  cat("\n")
  invisible(x)
}
