x <- y <- z <- y_error <- y_lwr <- y_upr <- NULL

#' Interpret plots for fitted EBM objects
#'
#' Provides an interactive visualization for a given explanation(s).
#'
#' @param x An EBMClassifier or EBMRegressor object.
#'
#' @param term Character string specifying which term to plot. For interaction
#' effect, you can supply a pair (e.g., `term = c("x1", "x2")`). Default is
#' `NULL` which will just display the overall importance of each term.
#'
#' @param local Logocial indicating whether to display local explanations
#' (`TRUE`) or global explanations (`FALSE`). Default is `FALSE`.
#'
#' @param X Data frame or matrix of samples. Unless `display = "url"` or
#' `full_dashboard = TRUE`, then `X` can only contain a single row.
#'
#' @param y Optional vector of response values corresponding to `X`.
#'
#' @param init_score Optional. Either a model that can generate scores or
#' per-sample initialization score. If samples scores it should be the same
#' length as `X`.
#'
#' @param interactive Logical indicating whether to produce an interactive plot
#' based on HTML. Default is `FALSE`.
#' (`type = "static"`) or interactive plot (`type = "plotly"`)
#'
#' @param n_features Integer specifying the maximum number of variable
#' importance scores to plot. Default is `NULL` which corresponds to all
#' features.
#'
#' @param geom Character string specifying which type of plot to construct for
#' terms associated with categorical features. Current options are:
#'
#'  * `geom = "col"` uses [geom_col][ggplot2::geom_col] to construct a bar chart
#'  of the scores.
#'
#'  * `geom = "point"` uses [geom_point][ggplot2::geom_point] to construct a
#'  Cleveland dot plot of the term scores.
#'
#' Default is `"point"`.
#'
#' @param mapping Set of aesthetic mappings created by
#' [aes][ggplot2::aes]-related functions and/or tidy eval helpers. See example
#' usage below.
#'
#' @param aesthetics List specifying additional arguments passed on to
#' [layer][ggplot2::layer]. These are often aesthetics, used to set an aesthetic
#' to a fixed value, like`colour = "red"` or `size = 3`. See example usage
#' below.
#'
#' @param horizontal Logical indicating whether or not term plots for
#' categorical features should be flipped horzintally. Default is `FALSE`.
#'
#' @param uncertainty Logical indicating whether or not to also display
#' uncertainty via error bars on the main effect plots. Default is `TRUE`.
#' Not very useful unless `outer_bags > 1` when calling [ebm()].
#'
#' @param width Numeric specifying the width of the error bars displayed in bar/
#' dot plots for categorical features. Default is 0.5.
#'
#' @param alpha Numeric between 0 and 1 specifying the level of transparency to
#' use when displaying uncertainty in plots for continuous features. Default is
#' 0.5.
#'
#' @param fill Character string specifying the fill color to use when displaying
#' uncertainty in plots for continuous features. Default is `"grey"`.
#'
#' @param display Character string specifying how the results should be
#' displayed whenever `interactive = TRUE`. Available options are `"viewer"`
#' (e.g., RStudio viewer browser),
#' `"markdown"` (e.g., for vingettes, Quarto, or Rmarkdown documents), or
#' `"url"` (e.g., to print a URL which can be pasted into a browser). When
#' `display = "url"`, a URL for viewing the entire interpret dashboard is
#' provided (i.e., the `term` and `full_dashboard` arguments are ignored).
#'
#' @param viewer Character string specifying how the results should be viewed.
#' Current choices are `"broswer"`, which calls `utils::browseURL()` to display
#' the results in an HTML browser, or `"rstudio"` for displaying the results
#' within the Viewer pane in an active RStudio session. Also works in VS Code.
#' Default is `"browser"`.
#'
#' @param full_dashboard Logical indicating whether or not to display the full
#' interpret dashboard. Default is `FALSE`. Only works when `display = "viewer"`
#' or `display = "url"` (e.g., paste the resulting URL in your browser).
#'
#' @param ... Additional optional arguments. Currently only passed onto
#' [levelplot()][lattice::levelplot()] for heatmaps of interaction effects.
#'
#' @importFrom graphics dotchart
#' @importFrom lattice levelplot
#' @importFrom stats reorder
#'
#' @export
plot.EBM <- function(
    x,
    term = NULL,
    local = FALSE,
    X = NULL,
    y = NULL,
    init_score = NULL,
    interactive = FALSE,
    n_features = NULL,
    geom = c("point", "col"),
    mapping = NULL,
    aesthetics = list(),
    horizontal = FALSE,
    uncertainty = TRUE,
    width = 0.5,
    alpha = 0.5,
    fill = "grey",
    display = c("viewer", "markdown", "url"),
    viewer = c("browser", "rstudio"),
    full_dashboard = FALSE,
    ...
) {
  if (isTRUE(local)) {
    plot_local(
      object = x,
      X = X,
      y = y,
      init_score = init_score,
      interactive = interactive,
      geom = geom,
      mapping = mapping,
      aesthetics = aesthetics,
      horizontal = horizontal,
      display = display,
      viewer = viewer,
      full_dashboard = full_dashboard,
      ...
    )
  } else {
    plot_global(
      object = x,
      term = term,
      interactive = interactive,
      n_features = n_features,
      geom = geom,
      mapping = mapping,
      aesthetics = aesthetics,
      horizontal = horizontal,
      uncertainty = uncertainty,
      width = width,
      alpha = alpha,
      fill = fill,
      display = display,
      viewer = viewer,
      full_dashboard = full_dashboard,
      ...
    )
  }
}


#' @keywords internal
#' @noRd
get_plot_type <- function(ordered_dict) {
  # Should be one of "scatter", "bar", or "heatmap"
  ordered_dict$data[[1L]]$type
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
    interactive = FALSE,
    n_features = NULL,
    geom = c("point", "col"),
    mapping = NULL,
    aesthetics = list(),
    horizontal = FALSE,
    uncertainty = TRUE,
    width = 0.5,
    alpha = 0.5,
    fill = "grey",
    display = c("viewer", "markdown", "url"),
    viewer = c("browser", "rstudio"),
    full_dashboard = FALSE,
    ...
) {


  ##############################################################################
  # Static graphic
  ##############################################################################
  if (isFALSE(interactive)) {

    if (is.null(term)) {
      # imp <- as.numeric(object$term_importances())
      # tna <- object$term_names_
      # ord <- order(imp, decreasing = FALSE)
      # dotchart(imp[ord], labels = tna[ord],
      #          xlab = "Mean absolute score (weighted)", ...)
      gg_plot_importance(object, n_features = n_features, geom = geom,
                         mapping = mapping, aesthetics = aesthetics,
                         horizontal = horizontal, ...)
    } else {

      # Generate plotly object for specified term
      term_names <- strsplit(object$term_names_, split = " & ")
      idx <- get_term_idx(term_names, x = term)  # idx of associated term in model
      plt <- object$explain_global()$visualize(idx)  # Python plotly object
      ordered_dict <- plt$to_ordered_dict()
      plot_type <- get_plot_type(ordered_dict)
      if (plot_type == "scatter") {
        ggplot_scatter(ordered_dict, mapping = mapping, aesthetics = aesthetics,
                       uncertainty = uncertainty, alpha = alpha, fill = fill, ...)
      } else if (plot_type == "bar") {
        ggplot_bar(ordered_dict, geom = geom, mapping = mapping,
                   aesthetics = aesthetics, horizontal = horizontal,
                   uncertainty = uncertainty, width = width, ...)
      } else {
        ggplot_heatmap(ordered_dict, ...)
      }
    }


  ##############################################################################
  # Plotly graphic
  ##############################################################################
  } else {
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

    ############################################################################
    # Display plot in specified viewer
    ############################################################################
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
    ############################################################################
    # Display in markdown-type document
    ############################################################################
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
}


#' @keywords internal
#' @noRd
plot_local <- function(
    object,
    X,
    y = NULL,
    init_score = NULL,
    interactive = FALSE,
    geom = c("point", "col"),
    mapping = NULL,
    aesthetics = list(),
    horizontal = FALSE,
    display = c("viewer", "markdown", "url"),
    viewer = c("browser", "rstudio"),
    full_dashboard = FALSE,
    ...
) {

  if (nrow(X) != 1 || length(y) != 1L) {
    warning("Plotting local explanations currently only works for a single ",
            "observation. Plotting explanations only for the first row of `X`.",
            call. = FALSE)
  }

  #######
  #
  #######
  if (isFALSE(interactive)) {
    plt <- object$explain_local(X, y = y, init_score = init_score)$visualize(0L)  # Python plotly object
    ordered_dict <- plt$to_ordered_dict()
    gg_plot_explanation(ordered_dict, geom = geom, mapping = mapping,
                        aesthetics = aesthetics, horizontal = horizontal, ...)
  } else {
    display <- match.arg(display)

    # Return URL of full dashboard
    if (display == "url" || isTRUE(full_dashboard)) {
      return(interpret$show_link(object$explain_local(X, y = y, init_score = init_score)))
    }
    plt <- object$explain_local(X, y = y, init_score = init_score)$visualize(0L)  # Python plotly object

    # Temporary HTML file hold plotly object
    tmpfile <- tempfile(fileext = ".html")

    ############################################################################
    # Display plot in specified viewer
    ############################################################################
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
    ############################################################################
    # Display in markdown-type document
    ############################################################################
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


}


################################################################################
# Static plotters
################################################################################

#' @keywords internal
#' @noRd
gg_plot_importance <- function(
    object,
    n_features = NULL,
    geom = c("point", "col"),
    mapping = NULL,
    aesthetics = list(),
    horizontal = FALSE,
    ...
) {
  geom <- match.arg(geom, several.ok = FALSE)
  df <- data.frame(
    "x" = object$term_names_,
    "y" = as.numeric(object$term_importances())  # mean absolute score
  )

  # Determine how many features to include in the plot
  if (!is.null(n_features)) {
    n_features <- as.integer(n_features)[1L]  # make sure n_features is a single integer
    if (n_features > nrow(df) || n_features < 1L) {
      n_features <- nrow(df)
    }
    df <- df[order(df$y, decreasing = TRUE), ]
    df <- df[seq_len(n_features), ]  # only retain num_features variable importance scores
  }

  # Generate plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(x, y), y = y))
  if (geom == "col") {
    p <- p + do.call(
      what = ggplot2::geom_col,
      args = c(list(mapping = mapping), aesthetics)
    )
  }
  if (geom == "point") {
    p <- p + do.call(
      what = ggplot2::geom_point,
      args = c(list(mapping = mapping), aesthetics)
    )
  }

  # Add labels, titles, etc.
  p <- p + ggplot2::xlab("")
  p <- p + ggplot2::ylab("Mean absolute score (weighted)")
  if (isFALSE(horizontal)) {
    p <- p + ggplot2::coord_flip()
  }
  p
}


#' @keywords internal
#' @noRd
ggplot_bar <- function(
    ordered_dict,
    geom = c("point", "col"),
    mapping = NULL,
    aesthetics = list(),
    horizontal = FALSE,
    uncertainty = TRUE,
    width = 0.5
) {
  geom <- match.arg(geom, several.ok = FALSE)
  plotly_data <- ordered_dict$data[[1L]]  # second component is distribution
  df <- data.frame(
    "x" = plotly_data$x,
    "y" = plotly_data$y,
    "y_error" = plotly_data$error_y$array
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
  if (geom == "col") {
    p <- p + do.call(
      what = ggplot2::geom_col,
      args = c(list(mapping = mapping), aesthetics)
    )
  }
  if (geom == "point") {
    p <- p + do.call(
      what = ggplot2::geom_point,
      args = c(list(mapping = mapping), aesthetics)
    )
  }

  # Add error bars
  if (isTRUE(uncertainty)) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(x = x, ymin = y - y_error, ymax = y + y_error), width = width
    )
  }

  # Add labels, titles, etc.
  p <- p + ggplot2::theme(legend.position = "none")
  p <- p + ggplot2::xlab(ordered_dict$layout$title)
  p <- p + ggplot2::ylab("Score")
  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }
  p
}


#' @keywords internal
#' @noRd
ggplot_scatter <- function(
    ordered_dict,
    mapping = NULL,
    aesthetics = list(),
    uncertainty = TRUE,
    alpha = 0.5,
    fill = "grey"
) {
  # geom <- match.arg(geom, several.ok = FALSE)
  # components <- sapply(ordered_dict$data, FUN = function(x) x$name)
  # main <- which(components == "Main")
  odd <- ordered_dict$data
  if (length(odd) == 4L) {
    df <- data.frame(
      "x" = ordered_dict$data[[2L]]$x,
      "y" = ordered_dict$data[[2L]]$y,
      "y_lwr" = ordered_dict$data[[1L]]$y,
      "y_upr" = ordered_dict$data[[3L]]$y
    )
  } else {
    uncertainty <- FALSE
    # No uncertainty (i.e., like after calling `object$monotonize()`)
    df <- data.frame(
      "x" = ordered_dict$data[[1L]]$x,
      "y" = ordered_dict$data[[1L]]$y
    )
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
  if (isTRUE(uncertainty)) {
    p <- p + geom_stepribbon(
      ggplot2::aes(ymin = y_lwr, ymax = y_upr), alpha = alpha, fill = fill
    )
  }
  p <- p + do.call(
    what = ggplot2::geom_step,
    args = c(list(mapping = mapping), aesthetics)
  )
  #p <- p + ggplot2::geom_step(aes(x = x, y = y_lwr))
  #p <- p + ggplot2::geom_step(aes(x = x, y = y_upr))

  # Add labels, titles, etc.
  p <- p + ggplot2::theme(legend.position = "none")
  p <- p + ggplot2::xlab(ordered_dict$layout$title)
  p <- p + ggplot2::ylab("Score")
  p
}


#' @keywords internal
#' @noRd
ggplot_heatmap <- function(ordered_dict, ...) {
  midpoints <- function(x) {
    x[-length(x)] + diff(x) / 2
  }
  plotly_data <- ordered_dict$data[[1L]]  # second component is distribution
  df <- expand.grid(
    "x" = midpoints(plotly_data$x),
    "y" = midpoints(plotly_data$y)
  )
  df$z <- as.numeric(plotly_data$z)
  # p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = z))
  # p <- p + do.call(
  #   # what = ggplot2::geom_contour_filled,
  #   what = ggplot2::geom_tile,
  #   # what = ggplot2::geom_raster,
  #   args = c(list(mapping = mapping), aesthetics)
  # )
  #
  # # Add labels, titles, etc.
  # # p <- p + ggplot2::theme(legend.position = "none")
  # p <- p + ggplot2::xlab(ordered_dict$layout$xaxis$title$text)
  # p <- p + ggplot2::ylab(ordered_dict$layout$yaxis$title$text)
  # p
  lattice::levelplot(
    x = z ~ x*y,
    data = df,
    xlab = ordered_dict$layout$xaxis$title$text,
    ylab = ordered_dict$layout$yaxis$title$text,
    ...
  )
}


#' @keywords internal
#' @noRd
gg_plot_explanation <- function(
    ordered_dict,
    geom = c("point", "col"),
    mapping = NULL,
    aesthetics = list(),
    horizontal = FALSE,
    ...
) {
  geom <- match.arg(geom, several.ok = FALSE)
  df <- data.frame(
    "y" = ordered_dict$data[[1L]]$x,  # contribution to prediction
    "x" = ordered_dict$data[[1L]]$y,
    "z" = "Intercept"
  )
  df[["z"]][df[["y"]] > 0] <- "Positive"
  df[["z"]][df[["y"]] <= 0] <- "Negative"
  df[["z"]][df[["x"]] == "Intercept"] <- "Intercept"
  p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(x, y), y = y, color = z, fill = z))
  if (geom == "col") {
    p <- p + do.call(
      what = ggplot2::geom_col,
      args = c(list(mapping = mapping), aesthetics)
    )
  }
  if (geom == "point") {
    p <- p + do.call(
      what = ggplot2::geom_point,
      args = c(list(mapping = mapping), aesthetics)
    )
  }

  # Add labels, titles, etc.
  p <- p + ggplot2::theme(legend.position = "none")
  p <- p + ggplot2::xlab("")
  p <- p + ggplot2::ylab("Contribution to prediction")
  if (isFALSE(horizontal)) {
    p <- p + ggplot2::coord_flip()
  }
  p
}
