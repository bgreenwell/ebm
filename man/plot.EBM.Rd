% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.EBM}
\alias{plot.EBM}
\title{Interpret plots for fitted EBM objects}
\usage{
\method{plot}{EBM}(
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
)
}
\arguments{
\item{x}{An EBMClassifier or EBMRegressor object.}

\item{term}{Character string specifying which term to plot. For interaction
effect, you can supply a pair (e.g., \code{term = c("x1", "x2")}). Default is
\code{NULL} which will just display the overall importance of each term.}

\item{local}{Logocial indicating whether to display local explanations
(\code{TRUE}) or global explanations (\code{FALSE}). Default is \code{FALSE}.}

\item{X}{Data frame or matrix of samples. Unless \code{display = "url"} or
\code{full_dashboard = TRUE}, then \code{X} can only contain a single row.}

\item{y}{Optional vector of response values corresponding to \code{X}.}

\item{init_score}{Optional. Either a model that can generate scores or
per-sample initialization score. If samples scores it should be the same
length as \code{X}.}

\item{interactive}{Logical indicating whether to produce an interactive plot
based on HTML. Default is \code{FALSE}.
(\code{type = "static"}) or interactive plot (\code{type = "plotly"})}

\item{n_features}{Integer specifying the maximum number of variable
importance scores to plot. Default is \code{NULL} which corresponds to all
features.}

\item{geom}{Character string specifying which type of plot to construct for
terms associated with categorical features. Current options are:
\itemize{
\item \code{geom = "col"} uses \link[ggplot2:geom_bar]{geom_col} to construct a bar chart
of the scores.
\item \code{geom = "point"} uses \link[ggplot2:geom_point]{geom_point} to construct a
Cleveland dot plot of the term scores.
}

Default is \code{"point"}.}

\item{mapping}{Set of aesthetic mappings created by
\link[ggplot2:aes]{aes}-related functions and/or tidy eval helpers. See example
usage below.}

\item{aesthetics}{List specifying additional arguments passed on to
\link[ggplot2:layer]{layer}. These are often aesthetics, used to set an aesthetic
to a fixed value, like\code{colour = "red"} or \code{size = 3}. See example usage
below.}

\item{horizontal}{Logical indicating whether or not term plots for
categorical features should be flipped horzintally. Default is \code{FALSE}.}

\item{uncertainty}{Logical indicating whether or not to also display
uncertainty via error bars on the main effect plots. Default is \code{TRUE}.
Not very useful unless \code{outer_bags > 1} when calling \code{\link[=ebm]{ebm()}}.}

\item{width}{Numeric specifying the width of the error bars displayed in bar/
dot plots for categorical features. Default is 0.5.}

\item{alpha}{Numeric between 0 and 1 specifying the level of transparency to
use when displaying uncertainty in plots for continuous features. Default is
0.5.}

\item{fill}{Character string specifying the fill color to use when displaying
uncertainty in plots for continuous features. Default is \code{"grey"}.}

\item{display}{Character string specifying how the results should be
displayed whenever \code{interactive = TRUE}. Available options are \code{"viewer"}
(e.g., RStudio viewer browser),
\code{"markdown"} (e.g., for vingettes, Quarto, or Rmarkdown documents), or
\code{"url"} (e.g., to print a URL which can be pasted into a browser). When
\code{display = "url"}, a URL for viewing the entire interpret dashboard is
provided (i.e., the \code{term} and \code{full_dashboard} arguments are ignored).}

\item{viewer}{Character string specifying how the results should be viewed.
Current choices are \code{"broswer"}, which calls \code{utils::browseURL()} to display
the results in an HTML browser, or \code{"rstudio"} for displaying the results
within the Viewer pane in an active RStudio session. Also works in VS Code.
Default is \code{"browser"}.}

\item{full_dashboard}{Logical indicating whether or not to display the full
interpret dashboard. Default is \code{FALSE}. Only works when \code{display = "viewer"}
or \code{display = "url"} (e.g., paste the resulting URL in your browser).}

\item{...}{Additional optional arguments. Currently only passed onto
\link[lattice:levelplot]{levelplot()} for heatmaps of interaction effects.}
}
\description{
Provides an interactive visualization for a given explanation(s).
}
