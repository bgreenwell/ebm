#' Predict method for EBM objects
#'
#' Compute predicted values from a fitted explainable boosting machine.
#'
#' @param object A fitted [ebm] object.
#'
#' @param newdata A data frame in which to look for variables with which to
#' predict.
#'
#' @param type The type of prediction required. Current options include:
#'
#' * `"response"`: Returns predictions on the scale of the response variable.
#' Thus, for a categorical outcome (i.e., binary or multiclass), a matrix of
#' predicted probabilities is returned.
#'
#' * `"link"`: Returns predictions on the link scale. For a binary outcome with
#' logit link, for example, this results in a vector of logits. For a multiclass
#' outcome, this will return a matrix with one column for each class. Ignored
#' for regression problems.
#'
#' * `"class"`: Returns a vector predicted class label for categorical outcomes.
#'
#' * `"terms"`: Returns a matrix (or list of matrices for multiclass outcomes)
#' of the individual term contributions (e.g., the `f(x)`'s). Note that term
#' contributions are on the link scale, where they are additive.
#'
#' @param se_fit Logical indicating whether or not standard errors are required.
#' Ignored for multiclass outcomes.
#'
#' @param init_score  Optional. Either a model that can generate scores or
#' per-sample initialization score. If samples scores it should be the same
#' length as `newdata`.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @returns Either a vector, matrix, or list of result. See the `type` argument
#' for details.
#'
#' @importFrom stats predict
#'
#' @export
predict.EBM <- function(object, newdata, type = c("response", "link", "class", "terms"),
                        se_fit = FALSE, init_score = NULL, ...) {
  type <- match.arg(type)
  if (type == "terms") {
    return(object$eval_terms(newdata))
  }
  ##############################################################################
  # Classification
  ##############################################################################
  ebc <- "interpret.glassbox._ebm._ebm.ExplainableBoostingClassifier"
  out <- if (inherits(object, what = ebc)) {
    if (type == "response") {
      probs <- object$predict_proba(newdata, init_score = init_score)
      colnames(probs) <- object$classes_  # FIXME: Need to double check this!
      probs
    } else if (type == "link") {
      if (isTRUE(se_fit)) {
        object$predict_with_uncertainty(newdata, init_score = init_score)
      } else {
        object$decision_function(newdata, init_score = init_score)
      }
    } else if (type == "class") {
      object$predict(newdata, init_score = init_score)
    }
  ##############################################################################
  # Regression
  ##############################################################################
  # TODO: Support `type = "link"` using terms. E.g.,
  # .terms <- object$eval_terms(newdata)
  # rowSums(.terms) + object$intercept_
  } else {
    if (isTRUE(se_fit)) {
      object$predict_with_uncertainty(newdata, init_score = init_score)
    } else {
      object$predict(newdata, init_score = init_score)
    }
  }
  return(out)
}
