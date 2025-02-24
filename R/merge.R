#' Merge method for EBM objects
#'
#' Merge multiple EBMs together.
#'
#' @param x,y Fitted [ebm] objects that have been trained on similar data sets
#' that have the same set of features.
#'
#' @param ... Additional [ebm] objects to be merged.
#'
#' @returns A merged [ebm] object.
#'
#' @note As of right now, the `merge()` function produces the following error
#' message:
#' ```
#' Error in py_repr(x) :
#'   AttributeError: 'ExplainableBoostingRegressor' object has no attribute 'cat_smooth'
#' Run `reticulate::py_last_error()` for details.
#' ```
#' This seems to be a bug in the underlying
#' [interpret](https://github.com/interpretml/interpret/)
#' library and does not prevent this function from working. The error message is
#' seemingly just a side effect.
#'
#' @export
merge.EBM <- function(x, y, ...) {
  stopifnot(inherits(x, what = "EBM"), inherits(y, what = "EBM"))
  r <- list(x, y)
  if (length(xtr <- list(...))) {
    if (!all(is.ebm <- vapply(xtr, inherits, NA, what = "EBM"))) {
      xpr <- substitute(c(...))
      nms <- sapply(xpr[-1][!is.ebm], FUN = deparse, nlines = 1L)
      msg <- ngettext(length(nms), "extra argument %s is not of class \"%s\"",
                      "extra arguments %s are not of class \"%s\"s")
      stop(sprintf(msg, paste(nms, collapse = ", "), "EBM"),
           domain = NA)
    }
    r <- c(r, xtr)
    merged <- as.ebm(interpret$glassbox$merge_ebms(r))
    return(merged)
  }
}
