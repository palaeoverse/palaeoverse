#' Generate time bins
#'
#' A function to generate time bins for a given study interval. This function is flexible in that either stage-level or near equal-length time bins can be generated
#' by grouping stages together. In this implementation, intervals are grouped together in a way that minimizes the mean and
#' standard deviation between bins based on the user's specified bin size. However, users may also wish to group stages based on subjective reasoning e.g.availability of outcrop.
#'
#' @param interval \code{character or numeric}. Interval name of age available in \code{GTS2020}. If a single interval name is provided, this interval is returned.
#' If two interval names are provided, these intervals and those existing between are returned. If a single interval age is provided, the age matching this interval is returned.
#' If two interval ages are provided, the intervals occurring in the range of these ages are returned. If higher than stage bins are required, these can only be specified using a
#' \code{character} input as the function defaults to using stratigraphic stages for \code{numeric} inputs.
#' @param equal \code{logical}. Should near equal-length time bins be generated?
#' @param size \code{numeric}. If equal == \code{TRUE}, specify the length in millions of years (Myr) of the time bins desired. Defaults to 10 Myr.
#' @param assign \code{numeric}. A numeric vector of age estimates (i.e. midpoint age in specified age range) to use to assign to bins of
#' a given size. If assign is specified, a numeric vector is returned of the midpoint age of the specified bins.
#' @param plot \code{logical}. Should a plot of time bins be generated?
#'
#' @return A \code{dataframe} of time bins for a specified interval or a list with a \code{dataframe} of time bins and \code{numeric} of binned age estimates (midpoint of specified
#' bins) if assign specified.
#'
#' @details This function uses the Geological Timescale 2020. Age data were compiled from: \url{https://stratigraphy.org/timescale/}.
#' Available intervals names are accessible via GTS2020$interval_name.
#' @section Developer:
#' Lewis A. Jones
#' @section Auditor:
#' To be validated
#' @examples
#' #Using interval midpoint age
#' time_bins(interval = 10, equal = FALSE, plot = TRUE)
#'
#' #Using interval age range
#' time_bins(interval = c(50, 100), equal = FALSE, plot = TRUE)
#'
#' #Using a single interval name
#' time_bins(interval = c("Maastrichtian"), equal = FALSE, plot = TRUE)
#'
#' #Using a range of intervals defined by two named intervals and equal duration bins
#' time_bins(interval = c("Fortunian", "Meghalayan"), equal = TRUE, size = 10, plot = TRUE)
#'
#' #Assign bins based on given age estimates
#' time_bins(interval = c("Fortunian", "Meghalayan"), assign = c(232, 167, 33), plot = TRUE)
#' @export
time_bins <- function(interval = c("Fortunian", "Meghalayan"), equal = FALSE, size = 10, assign = NULL, plot = TRUE){
  #download data
  df <- palaeoverse::GTS2020

  #which rank is required? Non-stage level is only available for character string.
  if(is.character(interval)){
    rnk <- df[which(df$interval_name %in% interval), c("rank")]

    if(length(rnk) != length(interval)){stop(paste0("Checking spelling of specified intervals.
    Available intervals are accessible via GTS2020$interval_name."))}

    rnk <- unique(rnk)}else{rnk <- "Stage"}

  if(length(rnk) != 1){stop("Interval ranks should be the same, e.g. specifiy only stage or period names, not a mixture")}

  #subset to stages
  df <- subset(df, rank == rnk)

  #character string entered
  if(is.character(interval)){
    if(length(interval) == 1){
      w <- which(df$interval_name %in% interval)
      if(length(w) != length(interval)){stop(paste0("Checking spelling of specified intervals.
    Available intervals are accessible via GTS2020$interval_name."))}
      df <- df[w,]
    }
    if(length(interval) == 2){
      w <- which(df$interval_name %in% interval)
      df <- df[w[1]:w[2],]
    }
  }
  #numeric ages entered
  if(is.numeric(interval)){
    if(length(interval) == 1){
      if(interval > max(df$max_ma) | interval < min(df$min_ma)){
        stop("Value does not appear in range of available intervals: 0 to 541")
      }
      df <- df[which.min(abs(df$mid_ma - interval)),]
    }

    if(length(interval) == 2){
      max_int <- max(interval)
      min_int <- min(interval)

      if(max_int > max(df$max_ma) | min_int < min(df$min_ma)){
        stop("Values do not appear in range of available intervals: 0 to 541")
      }

      df <- df[which.min(abs(df$min_ma - min_int)):which.min(abs(df$max_ma - max_int)),]
    }
  }

  if(length(interval) > 2){
    stop("interval must be a character vector or a numeric vector of length 1 or 2")
  }

  #are equal length time bins required?
  if(equal == TRUE){
    #track cumulative sum
    tracker <- list()
    for(i in 1:nrow(df)){
      tracker[[i]] <- rep(NA, length.out = nrow(df))
      tracker[[i]][i:nrow(df)] <- abs(cumsum(df$duration_myr[i:nrow(df)]) - size)
    }

    #calculate upper and lower limits for each bin
    lower <- NULL
    upper <- NULL
    count <- 1
    while(count <= nrow(df)){
      upper <- append(upper, which.min(tracker[[count]]))
      lower <- append(lower, (count))
      count <- which.min(tracker[[count]]) + 1
    }

    #generate bin information
    bin <- length(upper):1
    max_ma <- df[upper,c("max_ma")]
    min_ma <- df[lower,c("min_ma")]
    mid_ma <- (max_ma + min_ma)/2
    duration_myr <- max_ma - min_ma
    intervals <- vector("character")
    #get interval names
    for(i in 1:length(upper)){
      intervals[i] <- toString(df[lower[i]:upper[i], c("interval_name")])
    }
    #generate dataframe
    df <- cbind.data.frame(bin, max_ma, mid_ma, min_ma, duration_myr, intervals)
    #message user
    message(paste0("Target equal length time bins was set to ", size,
                   " Myr. \nGenerated time bins have a mean length of ", round(mean(df$duration_myr), digits = 2),
                   " Myr and a standard deviation of ", round(sd(df$duration_myr), digits = 2), " Myr."))
  }

  if(plot == TRUE){
    if(equal == TRUE){df$colour <- c("#4292c6")}
    plot(1, type = "n", xlim = c(max(df$max_ma), min(df$min_ma)), ylim = c(0, max(df$duration_myr)), xlab = "Time (Ma)", ylab = "Duration (Myr)")
      for(i in 1:nrow(df)){
        polygon(x = c(df$min_ma[i], df$max_ma[i], df$max_ma[i], df$min_ma[i]),
                y = c(0, 0, df$duration_myr[i], df$duration_myr[i]),
                col = df$colour[i])
      }
    if(equal == TRUE){
      title(paste0("Mean bin length = ", round(mean(df$duration_myr), digits = 2),
                   " (standard deviation = ", round(sd(df$duration_myr), digits = 2), ")"))
      df <- df[,-which(colnames(df) == "colour")]
    }

    if(equal == FALSE){
      df$bin <- nrow(df):1
      df <- df[,c("bin","interval_name", "max_ma", "mid_ma", "min_ma", "duration_myr")]
    }
    df <- df[order(df$bin, decreasing = FALSE),]

  }

  if(!is.null(assign)){
    if(class(assign) == "numeric"){
      if(any(assign > max(df$max_ma) | assign < min(df$min_ma))){
        stop("One or more ages is more than or less than the specified time interval range")
      }
      tmp <- assign
      for(i in 1:nrow(df)){
        assign[which(tmp <= df$max_ma[i] & tmp >= df$min_ma[i])] <- df$mid_ma[i]
      }
      assign <- list(df, assign)
      names(assign) <- c("Bins", "Assignation")
      return(assign)
    }else{stop("assign should be a numeric")}
  }

  return(df)
}

