#' Generate latitudinal bins
#'
#' A function to generate latitudinal bins of a given size. If the desired size
#' of the bins is not compatible with the entire latitudinal range
#' (90&deg;S to 90&deg;N ), bin size can be updated to the nearest integer
#' which is divisible into 180 to fit the entire range. This function also
#' allows the user to easily assign occurrence data to latitudinal bins via
#' the assign argument.
#'
#' @param size \code{numeric}. A single numeric value of more than 0, and less
#' than or equal to 90.
#' @param fit \code{logical}. Should bin size be checked to ensure that the
#' entire latitudinal
#' range is covered (90&deg;S to 90&deg;N)? If \code{fit = TRUE}, bin size is
#' set to the nearest integer which is divisible into 180 (the entire
#' latitudinal range).
#' @param assign \code{numeric}. A numeric vector of latitudes (or
#' palaeolatitudes) to use to assign to bins of a given size. If assign is
#' specified, a numeric vector is returned of the midpoint latitudes of the
#' specified bins.
#' @param plot \code{logical}. Should a plot of the latitudinal bins be
#' generated?
#' @return A \code{dataframe} of latitudinal bins of a given size, or a list
#' with a \code{dataframe} of the latitudinal bins,
#' and a named \code{numeric} vector (bin number) of binned latitudes
#' (midpoint latitude of bin) if assign specified.
#' @importFrom graphics polygon abline title
#' @section Developer:
#' Lewis A. Jones
#' @section Auditor:
#' To be validated
#' @examples
#' #Generate 20 degrees latitudinal bins
#' lat_bins(size = 20)
#'
#' #Generate latitudinal bins with closest fit to 13 degrees
#' lat_bins(size = 13, fit = TRUE)
#'
#' #Assign bins based on given latitudes
#' lat_bins(assign = c(-20, 45, 11, 67))
#' @export
lat_bins <- function(size = 10, fit = FALSE, assign = NULL, plot = FALSE){
  #error handling
  if (is.numeric(size) == FALSE) {
    stop("`size` should be a numeric")
  }

  if (size > 90 | size < 0) {
    stop("`size` should be more than 0 and less than or equal to 90")
  }

  if (is.logical(fit) == FALSE) {
    stop("`fit` should be logical (TRUE/FALSE)")
  }

  if (any(assign > 90 | assign < -90)) {
    stop("Latitudes (`assign`) should be more than -90 and less than 90")
  }

  if (is.logical(plot) == FALSE) {
    stop("`plot` should be logical (TRUE/FALSE)")
  }

  #divide latitudinal range by size of bins
  bins <- 180/size
  #if fit is set true, generate equal size bins to fit range
  if(fit == TRUE){
    if(is.integer(bins) == FALSE){
      int <- 180/seq(from = 1, to = 90, by = 1)
      int <- which(int%%1==0)
      size <- int[which.min(abs(int - size))]
      bins <- 180/size
    }
  }
  #generate latitudinal bins
  df <- seq(from = -90, to = 90, by = size)
  min <- df[1:bins]
  max <- df[1:bins]+size
  mid <- (max+min)/2
  bin <- 1:bins
  df <- cbind(max, mid, min)
  df <- df[order(-max),]
  df <- cbind.data.frame(bin, df)
  #plot latitudinal bins
  if(plot == TRUE){
    plot(1, type = "n", xlim = c(-180, 180), ylim = c(min(df$min), max(df$max)), xlab = "Longitude (\u00B0)", ylab = "Latitude (\u00B0)")
    cols <- rep(c("#2ca25f", "#ccece6"), nrow(df))
    for(i in seq_len(nrow(df))){
      polygon(x = c(-180, -180, 180, 180),
              y = c(df$min[i], df$max[i], df$max[i], df$min[i]),
              col = cols[i],
              border = "black")
    }
    if(fit == TRUE){
      title(paste0("Bin size set to ", size))
    }
  }

  if(fit == TRUE){
    message(paste0(
      "Bin size set to ", size, " degrees to fit latitudinal range."))
  }

  if(!is.null(assign)){
    if(is.numeric(assign)){
    tmp <- assign
      for(i in 1:nrow(df)){
        assign[which(tmp <= df$max[i] & tmp >= df$min[i])] <- df$mid[i]
        names(assign)[which(tmp <= df$max[i] &
                              tmp >= df$min[i])] <- df$bin[i]
      }
    assign <- list(df, assign)
    names(assign) <- c("Bins", "Assignation")
    return(assign)
    }else{stop("assign should be a numeric")}
  }

  return(df)
}
