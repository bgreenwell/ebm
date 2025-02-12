#' Step ribbons and area plots
#'
#' A combination of [geom_ribbon()][ggplot2::geom_ribbon] and
#' [geom_step()][ggplot2::geom_step].
#'
#' @inheritParams ggplot2::geom_ribbon
#'
#' @source Taken from [ldatools](https://github.com/adibender/ldatools/tree/master).
#'
#' @importFrom ggplot2 layer GeomRibbon
#'
#' @export
geom_stepribbon <- function(
    mapping     = NULL,
    data        = NULL,
    stat        = "identity",
    position    = "identity",
    na.rm       = FALSE,
    show.legend = NA,
    inherit.aes = TRUE, ...) {

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomStepribbon,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(na.rm = na.rm, ... )
  )

}

#' @rdname geom_stepribbon
#' @format NULL
#' @usage NULL
#' @export
GeomStepribbon <- ggplot2::ggproto(
  "GeomStepribbon", GeomRibbon,

  extra_params = c("na.rm"),

  draw_group = function(data, panel_scales, coord, na.rm = FALSE) {

    if (na.rm) data <- data[complete.cases(data[c("x", "ymin", "ymax")]), ]
    data   <- rbind(data, data)
    data   <- data[order(data$x), ]
    data$x <- c(data$x[2:nrow(data)], NA)
    data   <- data[complete.cases(data["x"]), ]
    GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)

  }

)
