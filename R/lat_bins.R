#' Generate latitudinal bins
#'
#' A function to generate latitudinal bins of a given size. If the desired size
#' of the bins is not compatible with the entire latitudinal range
#' (90&deg;S to 90&deg;N ), bin size can be updated to the nearest integer
#' which is divisible into 180 to fit the entire range.
#'
#' @param size \code{numeric}. A single numeric value defining the width of the
#' latitudinal bins. This value must be more than 0, and less than or equal to
#' 90.
#' @param fit \code{logical}. Should bin size be checked to ensure that the
#' entire latitudinal
#' range is covered (90&deg;S to 90&deg;N)? If \code{fit = TRUE}, bin size is
#' set to the nearest integer which is divisible into 180 (the entire
#' latitudinal range). If \code{fit = FALSE}, and bin size is not divisible
#' into 180, part of the Northern Hemisphere latitudinal range will be missing.
#' @param plot \code{logical}. Should a plot of the latitudinal bins be
#' generated?
#' @return A \code{dataframe} of latitudinal bins of user-defined size.
#' @importFrom graphics polygon abline title
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Bethany Allen
#' @export
#' @examples
#' #Generate 20 degrees latitudinal bins
#' lat_bins(size = 20)
#'
#' #Generate latitudinal bins with closest fit to 13 degrees
#' lat_bins(size = 13, fit = TRUE)
#'
lat_bins <- function(size = 10, fit = FALSE, plot = FALSE) {
  #error handling
  if (is.numeric(size) == FALSE) {
    stop("`size` should be a numeric")
  }

  if (size > 90 || size < 0) {
    stop("`size` should be more than 0 and less than or equal to 90")
  }

  if (is.logical(fit) == FALSE) {
    stop("`fit` should be logical (TRUE/FALSE)")
  }

  if (is.logical(plot) == FALSE) {
    stop("`plot` should be logical (TRUE/FALSE)")
  }

  #divide latitudinal range by size of bins
  bins <- 180 / size
  #if fit is set true, generate equal size bins to fit range
  if (fit == TRUE) {
    if (is.integer(bins) == FALSE) {
      int <- 180 / seq(from = 1, to = 90, by = 1)
      int <- which(int %% 1 == 0)
      size <- int[which.min(abs(int - size))]
      bins <- 180 / size
    }
  }
  #generate latitudinal bins
  df <- seq(from = -90, to = 90, by = size)
  min <- df[1:bins]
  max <- df[1:bins] + size
  mid <- (max + min) / 2
  bin <- 1:bins
  df <- cbind(max, mid, min)
  df <- df[order(-max), ]
  df <- cbind.data.frame(bin, df)
  #plot latitudinal bins
  if (plot == TRUE) {
    plot(1, type = "n", xlim = c(-180, 180),
         ylim = c(min(df$min), max(df$max)),
         xlab = "Longitude (\u00B0)", ylab = "Latitude (\u00B0)")
    cols <- rep(c("#feb24c", "#1d91c0"), nrow(df))
    for (i in seq_len(nrow(df))){
      polygon(x = c(-180, -180, 180, 180),
              y = c(df$min[i], df$max[i], df$max[i], df$min[i]),
              col = cols[i],
              border = "black")
    }
    if (fit == TRUE) {
      title(paste0("Bin size set to ", size))
    }
  }

  if (fit == TRUE) {
    message(paste0(
      "Bin size set to ", size, " degrees to fit latitudinal range."))
  }

  return(df)
}
