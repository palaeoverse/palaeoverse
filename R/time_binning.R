#' Assign fossil occurrences to time bins
#'
#' A function to assign fossil occurrences to specified time bins based on
#' different approaches commonly applied in palaeobiology.
#'
#' @param occdf \code{dataframe}. A dataframe of the fossil occurrences you
#' wish to bin. This dataframe should contain  the following named columns:
#' "max_ma" and "min_ma". These columns may be either `numeric` or `character`
#' values. Ages given in `numeric` form are preferred for bin assignment.
#' However, if interval names are provided, the function will try to match
#' names to the Geological Timescale 2012/2020 (depending on user
#' specification) to generate `numeric` ages for occurrences (see
#' "return_error" for additional information).
#' @param bins \code{dataframe}. A dataframe of the bins that you wish to
#' allocate fossil occurrences to such as that returned by
#' \code{\link[palaeoverse:time_bins]{time_bins()}}. This dataframe must
#' contain at least the following named columns: "bin", "max_ma" and "min_ma".
#' Columns "max_ma" and "min_ma" must be `numeric` values.
#' @param method \code{character}. The method desired for binning fossil
#' occurrences. Currently, five methods exist in this function: "mid",
#' "majority", "all", "random", and "point". See Details for a description of
#' each.
#' @param reps \code{numeric}. A non-negative `numeric` specifying the number
#' of replications for sampling. This argument is only useful in the case of
#' the "random" or "point" method being specified in the `method` argument.
#' Defaults to 100.
#' @param scale \code{character}. Specify the desired geological timescale to
#' be used ("GTS2020" or "GTS2012"). This argument is only relevant if "min_ma"
#' and "max_ma" columns are `character` values of interval names. Available
#' interval names can be accessed via the call \code{GTS2020$interval_name} or
#' \code{GTS2012$interval_name}. "GTS2020" is the default option.
#' @param return_error \code{logical}. Should a vector of numbers be returned
#' to flag the rows of the `occdf` that cannot be matched to `character`
#' interval names? This is only relevant if `occdf$max_ma` and `occdf$min_ma`
#' are `character` values.
#'
#' @return A \code{dataframe} of the original input `occdf` with the following
#' appended columns: occurrence id (`id`), number of bins the occurrence age
#' range covers (`n_bins`), bin assignment (`bin_assignment`), and bin midpoint
#' (`bin_midpoint`). In the case of the "random" method, a \code{list} is
#' appended for the bin assignment and midpoint columns. If `method` is
#' specified as "point", only point age estimates are appended
#' (`point_estimates`) as a \code{list} in the dataframe.
#'
#' @details Five approaches (methods) exist in the `time_binning()` function
#' for assigning occurrences to time bins:
#' - Midpoint: The "mid" method is the simplest approach and uses the midpoint
#' of the fossil occurrence age range to bin the occurrence.
#' - Majority: The "majority" method bins an occurrence into the bin which it
#' most overlaps with.
#' As part of this implementation, the majority percentage overlap of the
#' occurrence is also calculated and returned as an additional column in
#' `occdf`. If desired, these percentages can be used to further filter an
#' occurrence dataset.
#' - All: The "all" method bins an occurrence into every bin its age range
#' covers. For occurrences with age ranges of more than one bin, the occurrence
#' row is duplicated. Each occurrence is assigned an ID in the column
#' `occdf$id` so that duplicates can be tracked. Additionally, `occdf$n_bins`
#' records the number of bins each occurrence appears within.
#' - Random: The "random" method randomly samples X amount of bins (with
#' replacement) from the bins that the fossil occurrence age range covers.
#' The `reps` argument determines the number of times the sample process is
#' repeated. All replications for each occurrence are stored as a list within
#' the `occdf$bin_assignment` and `occdf$bin_midpoint` columns.
#' - Point: The "point" method randomly samples X amount of point age estimates
#' from the age range of the fossil occurrence. Sampling is weighted by a
#' normal probability distribution defined by the age range of the fossil
#' occurrence. The `reps` argument determines the number of times the sample
#' process is repeated. All replications for each occurrence are stored as a
#' list within the `occdf$point_estimates` column.
#'
#' @section Developer(s):
#' Christopher D. Dean & Lewis A. Jones
#' @section Reviewer(s):
#' To be reviewed
#' @importFrom stats dnorm sd
#' @importFrom utils capture.output
#' @examples
#' #Grab some data from the Paleobiology Database
#' occdf <- read.csv(
#' "https://paleobiodb.org/data1.2/colls/list.csv?base_name=Scleractinia")
#' bins <- data.frame(bin = 1:30,
#' max_ma = seq(10, 300, 10), min_ma = seq(0, 290, 10))
#'
#' #Assign via midpoint age of fossil occurrence data
#' time_binning(occdf = occdf, bins = bins, method = "mid")
#'
#' #Assign to all bins that age range covers
#' time_binning(occdf = occdf, bins = bins, method = "all")
#'
#' #Assign via majority overlap based on fossil occurrence age range
#' time_binning(occdf = occdf, bins = bins, method = "majority")
#'
#' #Assign randomly to overlapping bins based on fossil occurrence age range
#' time_binning(occdf = occdf, bins = bins, method = "random", reps = 100)
#'
#' #Assign point estimates based on fossil occurrence age range
#' time_binning(occdf = occdf, bins = bins, method = "point", reps = 100)
#' @export
time_binning <-
  function(occdf,
           bins,
           method = "mid",
           reps = 100,
           scale = "GTS2020",
           return_error = FALSE) {
    #=== Handling errors ===
    if (is.data.frame(occdf) == FALSE) {
      stop("`occdf` should be a dataframe.")
    }
    if (is.data.frame(bins) == FALSE) {
      stop("`bins` should be a dataframe.")
    }
    if (method %in% c("all", "majority", "random", "point", "mid") == FALSE) {
      # If the user has entered a non-valid term for the "method" argument,
      # generate an error and warn the user.
      stop("Invalid `method`. Choose either:
           'all', 'majority', 'random', 'point', or 'mid'.")
    }
    if (scale %in% c("GTS2020", "GTS2012") == FALSE) {
      stop("Invalid `scale`. Choose either 'GTS2020' or 'GTS2012'")
    }
    if (is.numeric(reps) == FALSE) {
      stop("Invalid `reps`. Choose an numeric value.")
    }
    if (is.logical(return_error) == FALSE) {
      stop("Invalid `return_error`.
           Choose a logical value (i.e. TRUE or FALSE).")
    }
    if (class(occdf$max_ma) != class(occdf$min_ma)) {
      stop("Invalid occdf$max_ma or occdf$min_ma.
           Columns should be of the same class.")
    }

    if (is.numeric(occdf$max_ma) &
        max(occdf$max_ma) > max(bins$max_ma)) {
      stop("Maximum age of occurrence data surpasses maximum age of bins")
    }

    #=== Sorting non-numeric age designations ===
    if (is.character(occdf$max_ma)) {
      # If entered value for max_ma is character rather than numeric:

      # which geological timescale to use?
      if (scale == "GTS2020") {
        df <- palaeoverse::GTS2020
      }
      if (scale == "GTS2012") {
        df <- palaeoverse::GTS2012
      }

      # Re-name columns to work with rest of function.
      occdf$tmp_bin <- 1:nrow(occdf)
      names(occdf)[names(occdf) == "max_ma"] <- "max_interval"
      names(occdf)[names(occdf) == "min_ma"] <- "min_interval"

      # Merge dataframes (max ma)
      occdf <- merge(
        x = occdf,
        y = df[, c("interval_name", "max_ma")],
        by.x = "max_interval",
        by.y = "interval_name",
        all.x = TRUE
      )

      # Merge dataframes (min ma)
      occdf <- merge(
        x = occdf,
        y = df[, c("interval_name", "min_ma")],
        by.x = "min_interval",
        by.y = "interval_name",
        all.x = TRUE
      )

      # Ensure order of dataframe is maintained after merge
      occdf <- occdf[order(occdf$tmp_bin), ]

      occdf <- occdf[, -which(colnames(occdf) == "tmp_bin")]

      # If not all intervals can be matched, produce error report
      # and message to fix spellings.
      if (any(is.na(occdf$min_ma)) == TRUE |
          any(is.na(occdf$max_ma)) == TRUE) {
        # Generate error vector
        error_vec <- which(is.na(occdf$min_ma) | is.na(occdf$max_ma))
        # Should an error vector be returned to the user?
        if (return_error == TRUE) {
          return(error_vec)
        } else{
          # return error message
          stop(paste(c(
  "Unable to match interval to numeric value for all occurrences. Available
  intervals names are accessible via GTS2020 and GTS2012. Please check interval
  spelling for the following rows in `occdf` (note: an error vector can be
  returned with the `return_error` argument):",
              capture.output(print(error_vec))
            ),
            collapse = "\n"
          ))
        }
      }
    }

    #=== Reporting Info ===

    # Make an empty list that's the length of the occurrence dataframe.
    bin_list <- list()
    bin_list <- sapply(1:nrow(occdf), function(x)
      NULL)

    # For each occurrence, find all the bins that it is present within, and
    # add as elements to that part of the list.
    for (i in 1:nrow(bins)) {
      v <-
        which(occdf$max_ma > bins$min_ma[i] &
                occdf$min_ma < bins$max_ma[i])
      for (j in v) {
        bin_list[[j]] <- append(bin_list[[j]], bins$bin[i])
      }
    }

    # Generate id column for data (this is for tracking duplicate rows).
    id <- 1:nrow(occdf)
    occdf$id <- id

    # Generate empty column for recording the number of bins an occurrence
    # appears in, and empty columns for the new bin allocation and midpoint.
    occdf$n_bins <- NA
    occdf$bin_assignment <- NA
    occdf$bin_midpoint <- NA

    # Assign number of bins per occurrence.
    occdf$n_bins <- lengths(bin_list)

    # Generate midpoint ages of bins
    bins$mid_ma <- (bins$max_ma + bins$min_ma) / 2

    #=== Methods ===

    #--- Method 1: Midpoint ---
    if (method == "mid") {
      # If no mid point is present for occurrence age range, add one in a
      # new column.
      rmcol <- FALSE
      if (("mid_ma" %in% colnames(occdf)) == FALSE) {
        occdf$mid_ma <- (occdf$max_ma + occdf$min_ma) / 2
        rmcol <- TRUE
      }

      # Assign bin based on midpoint age of the age range
      for (i in 1:nrow(bins)) {
        v <-
          which(occdf$mid_ma > bins$min_ma[i] &
                  occdf$mid_ma < bins$max_ma[i])
        occdf$bin_assignment[v] <- bins$bin[i]
        occdf$bin_midpoint[v] <- bins$mid_ma[i]
      }

      # Remove mid_ma for fossil occurrences (if not already present as input)
      if (rmcol == TRUE) {
        occdf <- occdf[, -which(colnames(occdf) == "mid_ma")]
      }

      # Return the dataframe and end the function.
      return(occdf)
    }

    #--- Method 2: Point estimates ---
    if (method == "point") {
      # Add column for point estimates
      occdf$point_estimates <- NA

      # For each occurrence max/min age, make probability distribution and
      # sample from it. Record that with each occurrence.
      for (i in 1:nrow(occdf)) {
        occ_seq <- seq(occdf[i, "min_ma"], occdf[i, "max_ma"], by = 0.01)
        prob <- dnorm(occ_seq,
                      mean = mean(occ_seq),
                      sd = sd(occ_seq))
        estimates <-
          sample(
            x = occ_seq,
            size = reps,
            replace = TRUE,
            prob = prob
          )
        if (reps == 1) {
          occdf$point_estimates[i] <- estimates
        }
        else{
          occdf$point_estimates[i] <- list(estimates)
        }
      }

      #drop cols that are not needed
      occdf <- occdf[, -which(colnames(occdf) == "bin_assignment")]
      occdf <- occdf[, -which(colnames(occdf) == "bin_midpoint")]

      return(occdf)
    }


    #--- Method 3: All ---
    if (method == "all") {
      # Duplicate rows by number of bins.
      occdf <- occdf[rep(seq_len(dim(occdf)[1]), occdf$n_bins), ]

      # Use id to track unique rows and update bin numbers.
      for (i in id) {
        id_vec <- which(occdf$id == i)
        occdf$bin_assignment[id_vec] <- bin_list[[i]]
      }
      # Add bin midpoints to dataframe
      for (i in 1:nrow(bins)) {
        vec <- which(occdf$bin_assignment == bins$bin[i])
        occdf$bin_midpoint[vec] <- bins$mid_ma[i]
      }

      rownames(occdf) <- 1:nrow(occdf)

      # Return the dataframe and end the function.
      return(occdf)
    }

    #--- Method 4: Majority ---
    if (method == "majority") {
      # Setup column for calculating overlap of age range with bin
      occdf$overlap_percentage <- NA

      # Run across bin list
      for (i in 1:length(bin_list)) {
        # Dataframe of bins occurrence known to occur in
        tmpbin <- bins[bins$bin %in% bin_list[[i]], ]

        # Generate sequence of length 10000 for percentage calculations
        occ_seq <-
          seq(occdf[i, "min_ma"], occdf[i, "max_ma"], length.out = 10000)

        # Calculate overlap across known bins
        percentage <- vector()
        for (j in 1:nrow(tmpbin)) {
          percentage[j] <-
            (length(
              which(occ_seq >= tmpbin$min_ma[j] &
                      occ_seq <= tmpbin$max_ma[j])
            ) / 10000) * 100
        }

        # Assign bins, bin midpoints and overlap percentage
        occdf[i, "bin_assignment"] <-
          tmpbin$bin[which.max(percentage)]
        occdf[i, "bin_midpoint"] <-
          tmpbin$mid_ma[which.max(percentage)]
        occdf[i, "overlap_percentage"] <-
          percentage[which.max(percentage)]
      }
      return(occdf)
    }

    #--- Method 5: Random ---
    if (method == "random") {
      # Randomly sample from the list of bins that occurrence appears in, and
      # add to the bin column for the occurrence.
      for (i in 1:length(bin_list)) {
        # Dataframe of bins occurrence known to occur in
        tmpbin <- bins[bins$bin %in% bin_list[[i]],]

        # If occurrence only appears in one bin, assign bin
        if (length(bin_list[[i]]) == 1) {
          # If there's only one rep
          if (reps == 1) {
            occdf$bin_assignment[i] <- bin_list[[i]]
          }

          # Assign bin and midpoint without sampling, and add to correct part
          # of list
          else{
            rsamp <- bin_list[[i]]
            occdf$bin_assignment[i] <- list(rep(x = rsamp,
                                                times = reps))
            occdf$bin_midpoint[i] <-
              list(rep(x = bins$mid_ma[which(bins$bin == rsamp)],
                       times = reps))
          }
        } else {
          # Otherwise:

          # Randomly sample from possible bins
          rsamp <- sample(x = tmpbin$bin,
                          size = reps,
                          replace = TRUE)

          # Assign bin and bin midpoint
          if (reps == 1) {
            occdf$bin_assignment[i] <- rsamp
            occdf$bin_midpoint[i] <-
              bins$mid_ma[which(bins$bin == rsamp)]
          }
          else{
            occdf$bin_assignment[i] <- list(rsamp)
            occdf$bin_midpoint[i] <-
              list(bins$mid_ma[sapply(rsamp, function(x) {
                which(bins$bin == x)
              })])
          }
        }
      }
      return(occdf)
    }
  }
