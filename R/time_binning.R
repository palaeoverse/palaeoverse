#' Add occurrences to time bins
#'
#' A function to
#'
#' @param
#'
#' @return A \code{dataframe} of time bins for a specified interval or a list with a \code{dataframe} of time bins and \code{numeric} of binned age estimates (midpoint of specified
#' bins) if assign specified.
#'
#' @details
#' @section Developer:
#' Christopher D. Dean & Lewis A. Jones
#' @section Auditor:
#' To be validated
#' @examples

#' @export

# Test dataset 1
occdf <- data.frame(occname = c("occ1", "occ2", "occ3", "occ4"),
                    max_ma = c("Jurassic", "Campanian", "Cenomanian", "Mesozoic"),
                    min_ma = c("Cretaceous", "Maastrichtian", "Coniacian", "Cenozoic")
)

# Test dataset 2
occdf <- data.frame(occname = c("occ1", "occ2", "occ3", "occ4", "occ5"),
                    max_ma = c(43.1, 69.5, 145.2, 238, 177),
                    min_ma = c(38.1, 66.5, 144.5, 220.7, 166)
)

time_binning <- function(occdf, bins, method = "mid", threshold = 2, scale = "GTS2020"){

  #=== Handling errors ===
  if (is.data.frame(occdf) == FALSE) {
    stop("Occdf should be a dataframe")
  }
  if (is.data.frame(bins) == FALSE) {
    stop("Bins should be a dataframe")
  }
  if (is.numeric(threshold) == FALSE) {
    stop("Threshold should be numeric")
  }
  TYPE <- c("all", "majority", "random", "dist", "mid")
  if (is.na(pmatch(method, TYPE))){
    stop("Invalid drawing method. Choose either 'all', 'majority', 'random', 'dist', or 'mid'.") # If the user has entered a non-valid term for the "method" argument, generate an error and warn the user.
  }

  #=== Sorting non-numeric age designations ===
  if(is.character(occdf$max_ma)){ # If entered value for max_ma is character rather than numeric:

    # which geological timescale to use?
    if(scale == "GTS2020"){df <- palaeoverse::GTS2020}
    if(scale == "GTS2012"){df <- palaeoverse::GTS2012}

    # Merge dataframes
    occdf <- merge(occdf, df[,c(5,7)], by.x = "max_ma", by.y = "interval_name", all.x = TRUE, ) # Merge dataframes by interval name.

    # re-name columns to work with rest of function
    names(occdf)[names(occdf) == "max_ma"] <- "max_interval"
    names(occdf)[names(occdf) == "max_ma.y"] <- "max_ma"

    # Merge dataframes
    occdf <- merge(occdf, df[,c(5,9)], by.x = "min_ma", by.y = "interval_name", all.x = TRUE)

    # re-name columns to work with rest of function
    names(occdf)[names(occdf) == "min_ma"] <- "min_interval"
    names(occdf)[names(occdf) == "min_ma.y"] <- "min_ma"

    if(sum(is.na(occdf$min_ma)) > 0 | sum(is.na(occdf$max_ma)) > 0){ # If not all intervals can be matched, produce error report and message to fix spellings.
      error_df <- occdf[rowSums(is.na(occdf)) > 0,]
      stop(paste(c("Unable to match interval to numeric value for all occurrences. Check interval spelling for occurrences listed below.
      Intervals names are accessible via GTS2020$interval_name and GTS2012$interval_name.
     ",
                   capture.output(print(error_df, row.names = FALSE))),
                 collapse = "\n"))
    }
  }
  #=== Methods ===

  # Method 1: Midpoint
  if(method == "mid"){
    if(("mid_ma" %in% colnames(occdf)) == 0){
      occdf$mid_ma <- (occdf$max_ma + occdf$min_ma) / 2
    }
    occdf$newbin <- cut(occdf$mid_ma, (c(bins[,2], bins[nrow(bins), 4])), rev(bins[,1]))
    return(occdf)
  }

  # All other methods - make list of which bins occurrences are in
  test_list <- list()
  test_list <- sapply(occdf$occname,function(x) NULL)
  for(o in 1:nrow(occdf)){
    tracker <- 0
    for(b in 1:nrow(bins)){
      if(occdf$max_ma[o] > bins$min_ma[b] &&
         occdf$min_ma[o] < bins$max_ma[b]){
        tracker <- tracker + 1
        test_list[[o]][tracker] <- bins$bin[b]
      }
    }
  }
  for(o in 1:length(test_list)){ # if occurrence is present in only one bin
    if(length(test_list[[o]]) == 1){
      occdf$newbin[[o]] <- test_list[[o]]
    }
    else if(method = "all"){

    }
    else if(method = "majority"){
      tmpbin <- bins[bins$bin %in% test_list[[o]], ]
      tmpocc <- occdf[o,c('occname', 'min_ma', 'max_ma')]
      colnames(tmpocc)[1] <- 'bin'
      df <- rbind(tmpocc, tmpbin[,c('bin', 'min_ma','max_ma')])
      out <- 100 * with(df, t((outer(max_ma, max_ma, pmin) - outer(min_ma, min_ma, pmax)) / (max_ma - min_ma)))
      dimnames(out) <- list(df$bin, df$bin)
      out <- out[-1,]
      occdf$newbin[[o]] <- as.numeric(rownames(out)[which.max(out[,1])])

    }
    else if(method = "random"){
      occdf$newbin[[o]] <- sample(test_list[[o]], 1)
    }
    else if(method = "dist"){

    }
  }
}
