#' Plot confidence interval or uncertainty range around a line.
#'
#' A function to add a band or polygon with a single vector of x values
#' and vectors of minimal and maximal y values to an existing plot.
#'
#' @param x \code{numeric}. A numerical vector of x values.
#' @param ymin \code{numeric}. A numerical vector of y values for the lower boundary
#' of the polygon.
#' @param ymax \code{numeric}. A numerical vector of y values for the upper boundary
#' of the polygon.
#' @param color \code{character}. Single character string for the color of the polygon.
#' Defaults to a transparent grey, \code{rgb(0,0,0,0.25)}. Note that transparency for
#' polygons exceeding the plot area only works with R 4.2.0 or later R versions.
#' @param polygon \code{character}. Single character string for the color of the
#' boundary of the polygon, defaults to \code{NA}.

#' @return A polygon is added to the most recent existing plot.
#' @examples
#' x = 1:25
#' y = sin(seq(0,6,0.25))
#' plot(x = x, y = y, type = "l")
#' conf_band(x = x, ymin = y+0.1, ymax = y-0.1)

conf_band <- function(x,ymin,ymax,color = rgb(0,0,0,0.25), border = NA) {
  polygon( c(x[1], x, x[length(x)], x[length(x)], rev(x), x[1]),
           c((ymin)[1],ymin, (ymin)[length(ymin)], (ymax)[length(ymax)], rev(ymax), (ymax)[1]),
           border = border, col = color)
}
