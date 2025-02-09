#' Interpret plots for fitted EBM objects
#'
#' Provides an interactive visualization for a given explanation(s).
#'
#' @param x An EBMClassifier or EBMRegressor object.
#' @param term Character string specifying which term to plot. For interaction
#' effect, you can supply a pair (e.g., `term = c("x1", "x2")`). Default is
#' `NULL` which will just display the overall importance of each term.
#' @param local Logocial indicating whether to display local explanations
#'   (`TRUE`) or global explanations (`FALSE`). Default is `FALSE`.
#' @param X Data frame or matrix of samples. Unless `display = "url"` or
#' `full_dashboard = TRUE`, then `X` can only contain a single row.
#' @param y Optional vector of response values corresponding to `X`.
#' @param init_score Optional. Either a model that can generate scores or
#' per-sample initialization score. If samples scores it should be the same
#' length as `X`.
#' @param display Character string specifying how the results should be
#' displayed. Available options are `"viewer"` (e.g., RStudio viewer browser),
#' `"markdown"` (e.g., for vingettes, Quarto, or Rmarkdown documents), or
#' `"url"` (e.g., to print a URL which can be pasted into a browser). When
#' `display = "url"`, a URL for viewing the entire interpret dashboard is
#' provided (i.e., the `term` and `full_dashboard` arguments are ignored).
#' @param viewer Character string specifying how the results should be viewed.
#' Current choices are `"broswer"`, which calls `utils::browseURL()` to display
#' the results in an HTML browser, or `"rstudio"` for displaying the results
#' within the Viewer pane in an active RStudio session. Also works in VS Code.
#' Default is `"browser"`.
#' @param full_dashboard Logical indicating whether or not to display the full
#' interpret dashboard. Default is `FALSE`. Only works when `display = "viewer"`
#' or `display = "url"` (e.g., paste the resulting URL in your browser).
#' @param ... Additional optional arguments. (Currently ignored.)
#' @export
plot.EBM <- function(x, term = NULL, local = FALSE, X = NULL, y = NULL,
                     init_score = NULL, display = c("viewer", "markdown", "url"),
                     viewer = c("browser", "rstudio"),
                     full_dashboard = FALSE, ...) {
  if (isTRUE(local)) {
    plot_local(
      object = x,
      X = X,
      y = y,
      init_score = init_score,
      display = display,
      viewer = viewer,
      full_dashboard = full_dashboard
    )
  } else {
    plot_global(
      object = x,
      term = term,
      display = display,
      viewer = viewer,
      full_dashboard = full_dashboard
    )
  }
}


#' @keywords internal
#' @noRd
get_term_idx <- function(term_names, x = NULL) {
  # Function to get term idx based on term name (e.g., "x1" or "x1 & x2")
  if (is.null(x)) {
    return(NULL)
  }
  if (length(x) > 1) {
    res <- match(list(x), table = term_names)
    if (is.na(res)) {
      res <- as.integer(match(list(rev(x)), table = term_names))
    }
    return(res - 1L)
  } else {
    as.integer(match(list(x), table = term_names)) - 1L
  }
}


#' @keywords internal
#' @noRd
plot_global <- function(
    object,
    term = NULL,
    display = c("viewer", "markdown", "url"),
    viewer = c("browser", "rstudio"),
    full_dashboard = FALSE
) {

  display <- match.arg(display)

  # Return URL of full dashboard
  if (display == "url" || isTRUE(full_dashboard)) {
    return(interpret$show_link(object$explain_global()))
  }

  # Generate plotly object for specified term
  term_names <- strsplit(object$term_names_, split = " & ")
  idx <- get_term_idx(term_names, x = term)  # idx of associated term in model
  plt <- object$explain_global()$visualize(idx)  # Python plotly object

  # Temporary HTML file hold plotly object
  tmpfile <- tempfile(fileext = ".html")

  ##############################################################################
  # Display plot in specified viewer
  ##############################################################################
  if (display == "viewer") {
    plt$write_html(tmpfile, full_html = TRUE)  # generate full HTML
    viewer <- match.arg(viewer)
    if (viewer == "browser") {
      if (requireNamespace("utils", quietly = TRUE)) {
        utils::browseURL(tmpfile)
      } else {
        stop("Package \"utils\" is required for whenever ",
             "`viewer = \"browser\"`. Please install it.", call. = FALSE)
      }
    } else {
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        if (rstudioapi::isAvailable()) {  # make sure RStudio is running
          rstudioapi::viewer(tmpfile)
        }
      } else {
        stop("Package \"rstudioapi\" is required for whenever ",
             "`viewer = \"rstudio\"`. Please install it.", call. = FALSE)
      }
    }
  ##############################################################################
  # Display in markdown-type document
  ##############################################################################
  } else {
    plt$write_html(tmpfile, full_html = FALSE)  # generate partial HTML
    if (requireNamespace("htmltools", quietly = TRUE)) {
      htmltools::includeHTML(tmpfile)
      # htmltools::tags$iframe(tmpfile)
    } else {
      stop("Package \"htmltools\" is required whenever ",
           "`display = \"markdown\"`. Please install it.", call. = FALSE)
    }
  }
}


#' @keywords internal
#' @noRd
plot_local <- function(
    object,
    X,
    y = NULL,
    init_score = NULL,
    display = c("viewer", "markdown", "url"),
    viewer = c("browser", "rstudio"),
    full_dashboard = FALSE
) {

  display <- match.arg(display)

  # Return URL of full dashboard
  if (display == "url" || isTRUE(full_dashboard)) {
    return(interpret$show_link(object$explain_local(X, y = y, init_score = init_score)))
  }

  if (nrow(X) != 1 || length(y) != 1L) {
    warning("Plotting local explanations currently only works for a single ",
            "observation. Plotting explanations only for the first row of `X`.",
            call. = FALSE)
  }
  plt <- object$explain_local(X, y = y, init_score = init_score)$visualize(0L)  # Python plotly object

  # Temporary HTML file hold plotly object
  tmpfile <- tempfile(fileext = ".html")

  ##############################################################################
  # Display plot in specified viewer
  ##############################################################################
  if (display == "viewer") {
    plt$write_html(tmpfile, full_html = TRUE)  # generate full HTML
    viewer <- match.arg(viewer)
    if (viewer == "browser") {
      if (requireNamespace("utils", quietly = TRUE)) {
        utils::browseURL(tmpfile)
      } else {
        stop("Package \"utils\" is required for whenever ",
             "`viewer = \"browser\"`. Please install it.", call. = FALSE)
      }
    } else {
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        if (rstudioapi::isAvailable()) {  # make sure RStudio is running
          rstudioapi::viewer(tmpfile)
        }
      } else {
        stop("Package \"rstudioapi\" is required for whenever ",
             "`viewer = \"rstudio\"`. Please install it.", call. = FALSE)
      }
    }
    ##############################################################################
    # Display in markdown-type document
    ##############################################################################
  } else {
    plt$write_html(tmpfile, full_html = FALSE)  # generate partial HTML
    if (requireNamespace("htmltools", quietly = TRUE)) {
      htmltools::includeHTML(tmpfile)
      # htmltools::tags$iframe(tmpfile)
    } else {
      stop("Package \"htmltools\" is required whenever ",
           "`display = \"markdown\"`. Please install it.", call. = FALSE)
    }
  }

}
