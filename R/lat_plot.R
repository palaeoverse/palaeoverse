#' Generate generic latitudinal plot
#'
#' A generic function for generating a latitudinal plot for a given variable
#' (e.g. for plotting the latitudinal biodiversity gradient).
#'
#' @param x \code{numeric}. A numeric vector of latitudes (or palaeolatitudes) to use for plotting.
#' @param y \code{numeric}. A numeric vector of respective counts (e.g. species richness) to use for plotting.
#' @param xlim \code{numeric}. A numeric vector of length two indicating the x limits of the plot.
#' @param main \code{character}. The main title for the plot.
#' @param ylab \code{character}. A title for the y-axis.
#' @param xlab \code{character}. A title for the x-axis.
#' @param palaeo \code{logical}. Is the x-axis representing palaeolatitudes?
#' @param type \code{character}. What type of plot should be drawn. See ?plot()
#' @param col \code{character}. The color for points.
#' @param cex \code{numeric}. A numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default.
#' @param tropics \code{logical}. Should lines indicating the tropics be added to the plot?
#' @param equator \code{logical}. Should a line indicating the equator be added to the plot?
#' @param tropics.set \code{numeric}. A numeric vector of length two indicating the tropics' latitudinal boundaries.
#' By default, the tropics' latitudinal limits are set to -23.26º to 23.26º based on the modern geographic definition.
#' @param tropics.col \code{character}. The colour of the lines indicating the tropics.
#' @param equator.col \code{character}. The colour of the line indicating the equator.
#' @param tropics.lty \code{numeric}. The line type for the tropics lines. See ?par()
#' @param equator.lty \code{numeric}. The line type for the equator line. See ?par()
#' @param lwd \code{numeric}. The width of the lines to plot (equator/tropics).
#'
#' @return A latitudinal plot.
#' @section Developer:
#' Lewis A. Jones & Sofía Galván
#' @section Auditor:
#' To be audited
#' @examples
#' #plot data
#' lat_plot(x = c(-20, 0, 20), y = c(10, 15, 10))
#' @export
lat_plot <- function(x, y, xlim = c(-90, 90), ylim = NULL,
                     main = NULL, ylab = "User variable", xlab = "Latitude",
                     tropics = TRUE, equator = FALSE, palaeo = FALSE,
                     tropics.set = c(-23.26, 23.26),
                     tropics.col = "black", equator.col = "black",
                     tropics.lty = 2, equator.lty = 2,
                     type = "o", col = "firebrick4", cex = 1, lwd = 1){

  if(any(x > 90 | x < -90)){
    stop("One or more latitudes is more than or less than 90/-90")
  }

  if(any(tropics.set > 90 | tropics.set < -90)){
    stop("Tropics' boundaries can not be more than or less than 90/-90")
  }

  if(palaeo == TRUE){xlab <- "Palaeolatitude"}

  plot(x = x,
       y = y,
       xlim = xlim,
       ylim = ylim,
       main = main,
       xlab = paste0(xlab, " (\u00B0)"),
       ylab = ylab,
       type = type,
       pch = 21,
       col = "black",
       bg = col,
       cex = cex,
       lwd = lwd)

  if(tropics == TRUE){
    abline(v = tropics.set[1], lty = tropics.lty, col = tropics.col)
    abline(v = tropics.set[2], lty = tropics.lty, col = tropics.col)
  }

  if(equator == TRUE){
    abline(v = 0, lty = equator.lty, col = equator.col)
  }
}
