#' Explainable Boosting Machine (EBM)
#'
#' This function is an R wrapper for the explainable boosting functions in the
#' Python [interpret](https://github.com/interpretml/interpret) library. It
#' trains an Explainable Boosting Machine (EBM) model, which is a tree-based,
#' cyclic gradient boosting generalized additive model with automatic
#' interaction detection. EBMs are often as accurate as state-of-the-art
#' blackbox models while remaining completely interpretable.
#'
#' @param formula A [formula][stats::formula()] of the form `y ~ x1 + x2 + ...`.
#' @param data A data frame containing the variables in the model.
#' @param max_bins Max number of bins per feature for the main effects stage. Default is 1024.
#' @param max_interaction_bins Max number of bins per feature for interaction terms. Default is 64.
#' @param interactions Interaction terms to be included in the model. Default is 0.9. Options are:
#' * Integer (1 <= interactions): Count of interactions to be automatically selected
#' * Percentage (interactions < 1.0): Determine the integer count of interactions by multiplying the number of features by this percentage
#' * List of numeric pairs: The pairs contain the indices of the features within each additive term. In addition to pairs, the interactions parameter accepts higher order interactions. It also accepts univariate terms which will cause the algorithm to boost the main terms at the same time as the interactions. When boosting mains at the same time as interactions, the `exclude` parameter should be set to `"mains"` and currently `max_bins` needs to be equal to `max_interaction_bins`.
#' @param exclude Features or terms to be excluded. Default is NULL.
#' @param validation_size Validation set size. Used for early stopping during boosting, and is needed to create outer bags. Default is 0.15. Options are:
#' * Integer (1 <= `validation_size`): Count of samples to put in the validation sets.
#' * Percentage (`validation_size` < 1.0): Percentage of the data to put in the validation sets.
#' * 0: Turns off early stopping. Outer bags have no utility. Error bounds will be eliminated.
#' @param outer_bags Number of outer bags. Outer bags are used to generate error bounds and help with smoothing the graphs. Default is 16.
#' @param inner_bags Number of inner bags. Default is 0.
#' @param learning_rate Learning rate for boosting. Default is 0.04.
#' @param greedy_ratio The proportion of greedy boosting steps relative to cyclic boosting steps. A value of 0 disables greedy boosting, effectively turning it off. Default is 10.0.
#' @param cyclic_progress This parameter specifies the proportion of the
#' boosting cycles that will actively contribute to improving the model’s
#' performance. It is expressed as a logical or numeric between 0 and 1, with
#' the default set to `TRUE` (1.0), meaning 100% of the cycles are expected to
#' make forward progress. If forward progress is not achieved during a cycle,
#' that cycle will not be wasted; instead, it will be used to update internal
#' gain calculations related to how effective each feature is in predicting the
#' target variable. Setting this parameter to a value less than 1.0 can be
#' useful for preventing overfitting. Default is `FALSE`.
#' @param smoothing_rounds Number of initial highly regularized rounds to set the basic shape of the main effect feature graphs. Default is 500.
#' @param interaction_smoothing_rounds Number of initial highly regularized rounds to set the basic shape of the interaction effect feature graphs during fitting. Default is 100.
#' @param max_rounds Total number of boosting rounds with `n_terms` boosting steps per round. Default is 25000.
#' @param early_stopping_rounds Number of rounds with no improvement to trigger early stopping. 0 turns off early stopping and boosting will occur for exactly `max_rounds`. Default is 100.
#' @param early_stopping_tolerance Tolerance that dictates the smallest delta required to be considered an improvement which prevents the algorithm from early stopping. `early_stopping_tolerance` is expressed as a percentage of the early stopping metric. Negative values indicate that the individual models should be overfit before stopping. EBMs are a bagged ensemble of models. Setting the `early_stopping_tolerance` to zero (or even negative), allows learning to overfit each of the individual models a little, which can improve the accuracy of the ensemble as a whole. Overfitting each of the individual models reduces the bias of each model at the expense of increasing the variance (due to overfitting) of the individual models. But averaging the models in the ensemble reduces variance without much change in bias. Since the goal is to find the optimum bias-variance tradeoff for the ensemble of models---not the individual models---a small amount of overfitting of the individual models can improve the accuracy of the ensemble as a whole. Default is 1e-05.
#' @param min_samples_leaf Minimum number of samples allowed in the leaves. Default is 4.
#' @param min_hessian Minimum hessian required to consider a potential split valid. Default is 0.0.
#' @param reg_alpha L1 regularization. Default is 0.0.
#' @param reg_lambda L2 regularization. Default is 0.0.
#' @param max_delta_step Used to limit the max output of tree leaves. <=0.0 means no constraint. Default is 0.0.
#' @param gain_scale Scale factor to apply to nominal categoricals. A scale factor above 1.0 will cause the algorithm focus more on the nominal categoricals. Default is 5.0.
#' @param min_cat_samples Minimum number of samples in order to treat a category separately. If lower than this threshold the category is combined with other categories that have low numbers of samples. Default is 10.
#' @param cat_smooth Used for the categorical features. This can reduce the effect of noises in categorical features, especially for categories with limited data. Default is 10.0.
#' @param missing Method for handling missing values during boosting. Default is `"separate"`. The placement of the missing value bin can influence the resulting model graphs. For example, placing the bin on the “low” side may cause missing values to affect lower bins, and vice versa. This parameter does not affect the final placement of the missing bin in the model (the missing bin will remain at index 0 in the `term_scores_` attribute). Possible values for missing are:
#' * `"low"`: Place the missing bin on the left side of the graphs.
#' * `"high"`: Place the missing bin on the right side of the graphs.
#' * `"separate"`: Place the missing bin in its own leaf during each boosting step, effectively making it location-agnostic. This can lead to overfitting, especially when the proportion of missing values is small.
#' * `"gain"`: Choose the best leaf for the missing value contribution at each boosting step, based on gain.
#' @param max_leaves Maximum number of leaves allowed in each tree. Default is 2.
#' @param monotone_constraints Default is NULL. This parameter allows you to specify monotonic constraints for each feature's relationship with the target variable during model fitting. However, it is generally recommended to apply monotonic constraints post-fit using the `monotonize()` attribute rather than setting them during the fitting process. This recommendation is based on the observation that, during fitting, the boosting algorithm may compensate for a monotone constraint on one feature by utilizing another correlated feature, potentially obscuring any monotonic violations. If you choose to define monotone constraints, `monotone_constraints` should be a numeric vector with a length equal to the number of features. Each element in the list corresponds to a feature and should take one of the following values:
#' * 0: No monotonic constraint is imposed on the corresponding feature’s partial response.
#' * +1: The partial response of the corresponding feature should be monotonically increasing with respect to the target.
#' * -1: The partial response of the corresponding feature should be monotonically decreasing with respect to the target.
#' @param objective The objective function to optimize. Default is `"log_loss"` for classification and `"rmse"` for regression.
#' @param n_jobs Number of jobs to run in parallel. Default is -1. Negative integers are interpreted as following [joblib](https://github.com/joblib/joblib)'s formula (`n_cpus + 1 + n_jobs`), just like [scikit-learn](https://scikit-learn.org/stable/). For example, `n_jobs = -2` means using all threads except 1.
#' @param random_state Random state. Setting to `NULL` generates non-repeatable sequences.
#' @param ... Additional optional argument. (Currently ignored.)
#' @return A trained EBM model.
#' @importFrom stats model.frame model.response na.pass reformulate terms
#' @examples
#' \dontrun{
#'   data("Hitters", package = "ISLR2")
#'
#'   # Remove rows with missing response values
#'   hitters <- Hitters[!is.na(Hitters$Salary), ]
#'
#'   # Fit a default EBM regressor
#'   fit <- ebm(Salary ~ ., data = hitters, objective = "rmse")
#'
#'   # Generate some predictions
#'   head(predict(fit, newdata = hitters))
#'   head(predict(fit, newdata = hitters, se.fit = TRUE))
#'
#'   # Show global summary and GAM shape functions
#'   plot(fit)
#'   plot(fit, term = "Years")
#'   plot(fit, display = "url")  # can paste the resulting URL in the browser
#'
#'   # Understand an individual prediction
#'   x <- subset(hitters, select = -Salary)[1L, ]  # use first observation
#'   ebm_show(fit, local = TRUE, X = x, y = hitters$Salary[1L])
#' }
#' @export
ebm <- function(
    formula,
    data,
    max_bins = 1024L,
    max_interaction_bins = 64L,
    interactions = 0.9,
    exclude = NULL,
    validation_size = 0.15,
    outer_bags = 16L,
    inner_bags = 0L,
    learning_rate = 0.04,
    greedy_ratio = 10.0,
    cyclic_progress = FALSE,
    smoothing_rounds = 500L,
    interaction_smoothing_rounds = 100L,
    max_rounds = 25000L,
    early_stopping_rounds = 100L,
    early_stopping_tolerance = 1e-05,
    min_samples_leaf = 4L,
    min_hessian = 0.0,
    reg_alpha = 0.0,
    reg_lambda = 0.0,
    max_delta_step = 0.0,
    gain_scale = 5.0,
    min_cat_samples = 10L,
    cat_smooth = 10.0,
    missing = "separate",
    max_leaves = 2L,
    monotone_constraints = NULL,
    objective = c("log_loss", "rmse", "poisson_deviance",
                  "tweedie_deviance:variance_power=1.5", "gamma_deviance",
                  "pseudo_huber:delta=1.0", "rmse_log"),
    n_jobs = -1L,
    random_state = 42L,
    ...
  ) {
  # glassbox <- import("interpret.glassbox")
  obj <- match.arg(objective)
  ebmcc <- if (obj == "log_loss") {  # EBM class constructor
    class_out <- "EBMClassifier"
    interpret$glassbox$ExplainableBoostingClassifier
  } else {
    class_out <- "EBMRegressor"
    interpret$glassbox$ExplainableBoostingRegressor
  }
  mcall <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.pass
  mf[[1]] <- as.name("model.frame")
  m <- mf
  mf <- eval(mf, parent.frame())
  Terms <- attr(mf, "terms")
  # w <- model.weights(mf)
  # offset <- model.offset(mf)
  y <- model.response(mf)
  var.names <- attributes(Terms)$term.labels
  X <- model.frame(terms(reformulate(var.names)), data = data,
                   na.action = na.pass)
  response.name <- as.character(formula[[2L]])

  # Sanity check
  if (obj == "log_loss" && length(unique(y)) != 2) {
    stop("The log loss objective only supports binary outcomes. Please specify",
         " a different objective in the call to `ebm()`", call. = FALSE)
  }

  # Call the EBM class constructor (i.e., EBMRegressor() or EBMClassifier())
  ebm_obj <- ebmcc(
    max_bins = as.integer(max_bins),
    max_interaction_bins = as.integer(max_interaction_bins),
    interactions = interactions,
    exclude = if (is.character(exclude)) as.list(exclude) else exclude,
    validation_size = validation_size,
    outer_bags = as.integer(outer_bags),
    inner_bags = as.integer(inner_bags),
    learning_rate = learning_rate,
    greedy_ratio = greedy_ratio,
    cyclic_progress = cyclic_progress,
    smoothing_rounds = as.integer(smoothing_rounds),
    interaction_smoothing_rounds = as.integer(interaction_smoothing_rounds),
    max_rounds = as.integer(max_rounds),
    early_stopping_rounds = as.integer(early_stopping_rounds),
    early_stopping_tolerance = as.integer(early_stopping_tolerance),
    min_samples_leaf = as.integer(min_samples_leaf),
    min_hessian = min_hessian,
    reg_alpha = reg_alpha,
    reg_lambda = reg_lambda,
    max_delta_step = max_delta_step,
    gain_scale = gain_scale,
    min_cat_samples = as.integer(min_cat_samples),
    cat_smooth = cat_smooth,
    missing = missing,
    max_leaves = as.integer(max_leaves),
    monotone_constraints = monotone_constraints,
    objective = obj,
    n_jobs = as.integer(n_jobs),
    random_state = as.integer(random_state)
  )
  # FIt the model
  ebm_obj$fit(X, y)
  class(ebm_obj) <- c("EBM", class_out, class(ebm_obj))
  return(ebm_obj)
}
