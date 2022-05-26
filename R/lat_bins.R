#' Generate latitudinal bins
#'
#' A function to generate latitudinal bins of a given size. If the desired size of the bins is not compatible
#' with the entire latitudinal range (90ºS to 90ºN), bin size can be updated to the nearest integer which is
#' divisible into 180 to fit the entire range.
#'
#' @param size \code{numeric}. A single numeric value of more than 0, and less than or equal to 90.
#' @param fit \code{logical}. Should bin size be checked to ensure that the entire latitudinal
#' range is covered (90ºS to 90ºN)? If \code{fit = TRUE}, bin size is set to the nearest integer
#' which is divisible into 180 (the entire latitudinal range).
#' @param assign \code{numeric}.A numeric vector of latitudes (or palaeolatitudes) to use to assign to bins of
#' a given size. If assign is specified, a numeric vector is returned of the midpoint latitudes of the specified bins.
#' @param plot \code{logical}. Should a plot of the latitudinal bins be generated?
#' @return A \code{dataframe} of latitudinal bins of a given size or a list with a \code{dataframe} of latitudinal bins
#' and \code{numeric} vector of binned latitudes (midpoint latitude of bin) if assign specified.
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
lat_bins <- function(size = 10, fit = FALSE, assign = NULL, plot = TRUE){
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
    cols <- rep(c("#2166ac", "#b2182b"), nrow(df))
    for(i in 1:nrow(df)){
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
    message(paste0("Bin size set to ",size," degrees to fit latitudinal range."))
  }

  if(!is.null(assign)){
    if(class(assign) == "numeric"){
      if(any(assign > 90 | assign < -90)){
        stop("One or more latitudes is more than or less than 90/-90")
     }
    tmp <- assign
      for(i in 1:nrow(df)){
        assign[which(tmp <= df$max[i] & tmp >= df$min[i])] <- df$mid[i]
      }
    assign <- list(df, assign)
    names(assign) <- c("Bins", "Assignation")
    return(assign)
    }else{stop("assign should be a numeric")}
  }

  return(df)
}
