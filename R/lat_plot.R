#' Generate latitudinal plot
#'
#' A generic function for generating a latitudinal plot e.g. for studying the latitudinal biodiversity gradient.
#'
#' @param size \code{numeric}. A single numeric value of more than 0, and less than or equal to 90.
#' @param fit \code{logical}. Should bin size be checked to ensure that the entire latitudinal
#' range is covered (90ºS to 90ºN)? If \code{fit = TRUE}, bin size is set to the nearest integer
#' which is divisible into 180 (the entire latitudinal range).
#' @param plot \code{logical}. Should a plot of the latitudinal bins be generated?
#' @return A \code{data.frame} of latitudinal bins of a given size.
#' @examples
#' Grab some data from the Paleobiology Database
#' x <- read.csv("https://paleobiodb.org/data1.2/colls/list.csv?base_name=Scleractinia&interval=Anisian,Piacenzian")
#'
#' Assign midpoint age of fossil occurrence data for rotation
#' x$age <- (x$max_ma + x$min_ma)/2
#'
#' Rotate the data
#' x <- palaeorotate(x = x)
#'
#' #get latitudinal bins
#' bins <- lat_bins(size = 15)
#'
#' #count number of collections per bin
#'
#'
#' #plot data
#' lat_plot(x = x$p_lat, y = x$lat)
#' @export
lat_plot <- function(x, y, xlim = c(-90, 90),
                     main = NULL, ylab = "User variable", xlab = "Latitude",
                     tropics = TRUE, equator = FALSE, palaeo = FALSE,
                     tropics.col = "black", equator.col = "black",
                     tropics.lty = 2, equator.lty = 2,
                     type = "o", col = "firebrick4", cex = 1, lwd = 1){

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
