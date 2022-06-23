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

    # Merge dataframes.
    occdf <- merge(occdf, df[,c(5,7)], by.x = "max_ma", by.y = "interval_name", all.x = TRUE, ) # Merge dataframes by interval name.

    # re-name columns to work with rest of function.
    names(occdf)[names(occdf) == "max_ma"] <- "max_interval"
    names(occdf)[names(occdf) == "max_ma.y"] <- "max_ma"

    # Merge dataframes.
    occdf <- merge(occdf, df[,c(5,9)], by.x = "min_ma", by.y = "interval_name", all.x = TRUE)

    # re-name columns to work with rest of function.
    names(occdf)[names(occdf) == "min_ma"] <- "min_interval"
    names(occdf)[names(occdf) == "min_ma.y"] <- "min_ma"

    # If not all intervals can be matched, produce error report and message to fix spellings.
    if(sum(is.na(occdf$min_ma)) > 0 | sum(is.na(occdf$max_ma)) > 0){
      error_df <- occdf[rowSums(is.na(occdf)) > 0,]
      stop(paste(c("Unable to match interval to numeric value for all occurrences. Check interval spelling for occurrences listed below.
      Intervals names are accessible via GTS2020$interval_name and GTS2012$interval_name.
     ",
                   capture.output(print(error_df, row.names = FALSE))),
                 collapse = "\n"))
    }
  }


  #=== Reporting Info ===

  # Make an empty list that's the length of the occurrence dataframe.
  test_list <- list()
  test_list <- sapply(occdf$occname,function(x) NULL)

  # For each occurrence, find all the bins that it is present within, and add as elements to that part of the list.
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

  # Generate temporary id column for data (this is for tracking duplicate rows).
  id <- 1:nrow(occdf)
  occdf$id <- id

  # Generate empty column for recording number of bins an occurrence appears in.
  occdf$n_bins <- NA

  # Assign number of bins per occurrence.
  occdf$n_bins <- lengths(test_list)

  # Generate empty column for new bin.
  occdf$newbin <- NA

  #=== Methods ===

  #--- Method 1: Midpoint ---
  if(method == "mid"){

    # If no mid point is present for occurrence age range, add one in a new column.
    if(("mid_ma" %in% colnames(occdf)) == 0){
      occdf$mid_ma <- (occdf$max_ma + occdf$min_ma) / 2
    }

    # Cut the midpoints according to the chosen bins, and add to the occurrence bin column.
    occdf$newbin <- cut(occdf$mid_ma, (c(bins[,2], bins[nrow(bins), 4])), rev(bins[,1]))

    # Return the dataframe and end the function.
    return(occdf)
  }

  #--- Method 2: All ---
  if(method == "all"){

    # Duplicate rows by number of bins.
    occdf <- occdf[rep(seq_len(dim(occdf)[1]), occdf$n_bins),]

    # Use id to track unique rows and update bin numbers.
    for(i in id){
      id_vec <- which(occdf$id == i)
      vec <- test_list[[i]]
      occdf$newbin[id_vec] <- bins$bin[vec]
    }

    # Return the dataframe and end the function.
    return(occdf)
  }

  else{
    # Run through occurrences to assign bins under methods "majority", "random" or "dist".
    for(o in 1:length(test_list)){

      #--- Method 3: Majority ---
      if(method == "majority"){

        # Find the bins that current occurrence appears in and max/min ma of the occurrence.
        tmpbin <- bins[bins$bin %in% test_list[[o]], ]
        tmpocc <- occdf[o,c('occname', 'min_ma', 'max_ma')]

        # Change column name to allow bind of dataframes and then add the occurrence to the bins that it appears in.
        colnames(tmpocc)[1] <- 'bin'
        df <- rbind(tmpocc, tmpbin[,c('bin', 'min_ma','max_ma')])

        # Produce table of percentages of overlap between bins and occurrence. First column shows percentage overlap between occurrence range and bin ranges.
        out <- 100 * with(df, t((outer(max_ma, max_ma, pmin) - outer(min_ma, min_ma, pmax)) / (max_ma - min_ma)))

        # Add col/row names and remove first row which records the occurrence (and so always has the highest overlap).
        # Then find the bin with the largest overlap, and add to the bin column for the occurrence.
        dimnames(out) <- list(df$bin, df$bin)
        if(nrow(out) == 2){
          occdf$newbin[[o]] <- as.numeric(rownames(out))[2]
        }
        else{
          out <- out[-1,]
          occdf$newbin[[o]] <- as.numeric(rownames(out)[which.max(out[,1])])
        }
      }

      #--- Method 4: Random ---
      else if(method == "random"){
        # Randomly sample from the list of bins that occurrence appears in, and add to the bin column for the occurrence.
        occdf$newbin[[o]] <- sample(test_list[[o]], 1)
      }

      #--- Method 5: Distribution
      else if(method == "dist"){




      }
    }
  }
}
