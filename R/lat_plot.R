#' Generate latitudinal plot
#'
#' A generic function for generating a latitudinal plot (e.g. for plotting the latitudinal biodiversity gradient).
#'
#' @param x \code{numeric}. A numeric vector of latitudes (or palaeolatitudes) to use for plotting.
#' @param y \code{logical}. A numeric vector of respective counts (e.g. species richness) to use for plotting.
#' @param xlim \code{numeric}. A numeric vector of length two to
#' @param main \code{character}.
#' @param ylab \code{character}.
#' @param xlab \code{character}.
#' @param palaeo \code{character}.
#' @param type \code{character}.
#' @param col \code{character}.
#' @param cex \code{numeric}.
#' @param tropics \code{logical}.
#' @param equator \code{logical}.
#' @param tropics.col \code{character}.
#' @param equator.col \code{character}.
#' @param tropics.lty \code{numeric}.
#' @param equator.lty \code{numeric}.
#' @param lwd \code{numeric}.
#'
#' @return A latitudinal plot.
#' @examples
#' #plot data
#' lat_plot(x = c(-20, 0, 20), y = c(10, 15, 10))
#' @export
lat_plot <- function(x, y, xlim = c(-90, 90),
                     main = NULL, ylab = "User variable", xlab = "Latitude",
                     tropics = TRUE, equator = FALSE, palaeo = FALSE,
                     tropics.col = "black", equator.col = "black",
                     tropics.lty = 2, equator.lty = 2,
                     type = "o", col = "firebrick4", cex = 1, lwd = 1){

  if(any(x > 90 | x < -90)){
    stop("One or more latitudes is more than or less than 90/-90")
  }

  if(palaeo == TRUE){xlab <- "Palaeolatitude"}

  plot(x = x,
       y = y,
       xlim = xlim,
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
    abline(v = 23.26, lty = tropics.lty, col = tropics.col)
    abline(v = -23.26, lty = tropics.lty, col = tropics.col)
  }

  if(equator == TRUE){
    abline(v = 0, lty = equator.lty, col = equator.col)
  }
}
