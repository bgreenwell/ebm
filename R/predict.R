#' Predict method for EBM objects
#'
#' Compute predicted values from a fitted explainable boosting machine.
#'
#' @param object A fitted [ebm] object.
#'
#' @param newdata A data frame in which to look for variables with which to predict.
#'
#' @param type The type of prediction required.
#'
#' @param se.fit Logical indicating whether or not standard errors are required.
#'
#' @param init_score  Optional. Either a model that can generate scores or
#' per-sample initialization score. If samples scores it should be the same
#' length as `newdata`.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @returns For regression (i.e., `EBMReegressor` objects), either a vector of
#' predictions (`se_fit = FALSE`) or a two-column matrix (`se = TRUE`) where the
#' first column gives the prediction and the second column provides the
#' corresponding standard error.
#'
#' For classification (i.e., `EBMClassifier` objects)...TBD.
#'
#' @importFrom stats predict
#'
#' @export
predict.EBM <- function(object, newdata, type = c("response", "link", "class"),
                        se_fit = FALSE, init_score = NULL, ...) {
  type <- match.arg(type)
  out <- if (inherits(object, what = "EBMClassifier")) {
    if (type == "response") {
      object$predict_proba(newdata, init_score = init_score)
    } else if (type == "link") {
      if (isTRUE(se_fit)) {
        object$predict_with_uncertainty(newdata, init_score = init_score)
      } else {
        object$decision_function(newdata, init_score = init_score)
      }
    } else if (type == "class") {
      object$predict(newdata, init_score = init_score)
    }
  } else {
    if (isTRUE(se_fit)) {
      object$predict_with_uncertainty(newdata, init_score = init_score)
    } else {
      object$predict(newdata, init_score = init_score)
    }
  }
  out
}
