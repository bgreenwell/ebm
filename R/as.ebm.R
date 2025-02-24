#' Coerce to an EBM object
#'
#' If possible, coerces its input to an [ebm] object.
#'
#' @param x An object that inherits from class
#' `"interpret.glassbox._ebm._ebm.ExplainableBoostingClassifier"` or
#' `"interpret.glassbox._ebm._ebm.ExplainableBoostingRegressor"`. For instance,
#' the result of calling `fit$copy()` where `fit` is a fitted [ebm()] object.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @returns An [ebm] object that can be used with the associated methods
#' `plot()` and so forth.
#'
#' @export
as.ebm <- function(x, ...) {
  # FIXME: Create an `is.ebm()` function? If so, it would just be something
  # like `inherits(x, what = "EBM") && !reticulate::py_is_null_xptr(x)`
  if (inherits(x, what = "EBM")) {
    return(x)
  } else if (inherits(x, what = c(
    "interpret.glassbox._ebm._ebm.ExplainableBoostingClassifier",
    "interpret.glassbox._ebm._ebm.ExplainableBoostingRegressor")
  )) {
    class(x) <- c("EBM", class(x))
    # reticulate::py_set_attr(x, name = "call", value = NULL)
    return(x)
  } else {
    stop(gettextf("Cannot coerce class %s to an EBM object",
                  sQuote(deparse(class(x))[1L])), domain = NA)
  }
}
