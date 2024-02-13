#' Assign fossil occurrences to time bins
#'
#' A function to assign fossil occurrences to specified time bins based on
#' different approaches commonly applied in palaeobiology.
#'
#' @param occdf \code{dataframe}. A dataframe of the fossil occurrences you
#'   wish to bin. This dataframe should contain at least two columns with
#'   `numeric` values: maximum age of occurrence and minimum age of
#'   occurrence (see `max_ma`, `min_ma`). If required, `numeric` ages can be
#'   generated from interval names via the
#'   \code{\link[palaeoverse:look_up]{look_up()}} function.
#' @param min_ma \code{character}. The name of the column you wish to be
#'   treated as the minimum age for `occdf` and `bins`, e.g. "min_ma"
#'   (default).
#' @param max_ma \code{character}. The name of the column you wish to be
#'   treated as the maximum age for `occdf` and `bins`, e.g. "max_ma"
#'   (default).
#' @param bins \code{dataframe}. A dataframe of the bins that you wish to
#'   allocate fossil occurrences to such as that returned by
#'   \code{\link[palaeoverse:time_bins]{time_bins()}}. This dataframe must
#'   contain at least the following named columns: "bin" and those specified
#'   to `max_ma` (default: "max_ma") and `min_ma` (default: "min_ma").
#'   Columns `max_ma` and `min_ma` must be `numeric` values.
#' @param method \code{character}. The method desired for binning fossil
#'   occurrences. Currently, five methods exist in this function: "mid",
#'   "majority", "all", "random", and "point". See Details for a description
#'   of each.
#' @param reps \code{numeric}. A non-negative `numeric` specifying the number
#'   of replications for sampling. This argument is only useful in the case of
#'   the "random" or "point" method being specified in the `method` argument.
#'   Defaults to 100.
#' @param fun \code{function}. A probability density function from the
#'   stats package such as \link[stats]{dunif} or \link[stats]{dnorm}.
#'   This argument is only useful if the "point" method is specified in the
#'   `method` argument.
#' @param ... Additional arguments available in the called function (`fun`).
#'   These arguments may be required for function arguments without default
#'   values, or if you wish to overwrite the default argument value (see
#'   example). `x` input values are generated internally based
#'   on the age range of the fossil occurrence and should not be manually
#'   provided. Note that `x` input values range between 0 and 1, and
#'   function arguments should therefore be scaled to be within these bounds.
#'
#' @return For methods "mid", "majority" and "all", a \code{dataframe} of the
#'   original input `occdf` with the following appended columns is returned:
#'   occurrence id (`id`), number of bins that the occurrence age range covers
#'   (`n_bins`), bin assignment (`bin_assignment`), and bin midpoint
#'   (`bin_midpoint`). In the case of the "majority" method, an additional
#'   column of the majority percentage overlap (`overlap_percentage`) is also
#'   appended. For the "random" and "point" method, a \code{list} is returned
#'   (of length reps) with each element a copy of the `occdf` and appended
#'   columns (random: `bin_assignment` and `bin_midpoint`; point:
#'   `bin_assignment` and `point_estimates`).
#'
#' @details Five approaches (methods) exist in the `bin_time()` function for
#'   assigning occurrences to time bins:
#' - Midpoint: The "mid" method is the simplest approach and uses the midpoint
#'   of the fossil occurrence age range to bin the occurrence.
#' - Majority: The "majority" method bins an occurrence into the bin which it
#'   most overlaps with. As part of this implementation, the majority
#'   percentage overlap of the occurrence is also calculated and returned as
#'   an additional column in `occdf`. If desired, these percentages can be
#'   used to further filter an occurrence dataset.
#' - All: The "all" method bins an occurrence into every bin its age range
#'   covers. For occurrences with age ranges of more than one bin, the
#'   occurrence row is duplicated. Each occurrence is assigned an ID in the
#'   column `occdf$id` so that duplicates can be tracked. Additionally,
#'   `occdf$n_bins` records the number of bins each occurrence appears within.
#' - Random: The "random" method randomly samples X amount of bins (with
#'   replacement) from the bins that the fossil occurrence age range covers
#'   with equal probability regardless of bin length. The `reps` argument
#'   determines the number of times the sample process is repeated. All
#'   replications are stored as individual elements within the returned list
#'   with an appended `bin_assignment` and `bin_midpoint` column to the
#'   original input `occdf`. If desired, users can easily bind this list using
#'   \code{do.call(rbind, x)}.
#' - Point: The "point" method randomly samples X (`reps`) amount of point age
#'   estimates from the age range of the fossil occurrence. Sampling follows a
#'   user-input probability density function such
#'   as \link[stats]{dnorm} (see example 5). Users should also provide any
#'   additional arguments for the probability density function (see `...`).
#'   However, `x` (vector of quantiles) values should not be provided as these
#'   values are input from the age range of each occurrence. These
#'   values range between 0 and 1, and therefore function arguments should be
#'   scaled to be within these bounds. The `reps` argument determines the
#'   number of times the sample process is repeated. All replications are
#'   stored as individual elements within the returned list with an appended
#'   `bin_assignment` and `point_estimates` column to the original input
#'   `occdf`. If desired, users can easily bind this list using
#'   \code{do.call(rbind, x)}.
#'
#' @section Developer(s): Christopher D. Dean & Lewis A. Jones
#' @section Reviewer(s): William Gearty
#' @importFrom stats dunif
#' @examples
#' #Grab internal tetrapod data
#' occdf <- tetrapods[1:100, ]
#' bins <- time_bins()
#'
#' #Assign via midpoint age of fossil occurrence data
#' ex1 <- bin_time(occdf = occdf, bins = bins, method = "mid")
#'
#' #Assign to all bins that age range covers
#' ex2 <- bin_time(occdf = occdf, bins = bins, method = "all")
#'
#' #Assign via majority overlap based on fossil occurrence age range
#' ex3 <- bin_time(occdf = occdf, bins = bins, method = "majority")
#'
#' #Assign randomly to overlapping bins based on fossil occurrence age range
#' ex4 <- bin_time(occdf = occdf, bins = bins, method = "random", reps = 5)
#'
#' #Assign point estimates following a normal distribution
#' ex5 <- bin_time(occdf = occdf, bins = bins, method = "point", reps = 5,
#'                 fun = dnorm, mean = 0.5, sd = 0.25)
#' @export
bin_time <- function(occdf, min_ma = "min_ma", max_ma = "max_ma",
                     bins, method = "mid", reps = 100,
                     fun = dunif, ...) {
    #=== Handling errors ===
    if (is.data.frame(occdf) == FALSE) {
      stop("`occdf` should be a dataframe.")
    }
    if (is.data.frame(bins) == FALSE) {
      stop("`bins` should be a dataframe.")
    }
    if (any(is.na(occdf[, max_ma])) || any(is.na(occdf[, min_ma]))) {
      stop(paste("NA values detected in", max_ma, "or", min_ma))
    }

    possible_methods <- c("all", "majority", "random", "point", "mid")
    method_match <- charmatch(method, possible_methods)

    if (is.na(method_match) == TRUE) {
      # If the user has entered a non-valid term for the "method" argument,
      # generate an error and warn the user.
      stop(paste("Invalid `method`. Choose either: \n",
                 "'all', 'majority', 'random', 'point', or 'mid'."))
    } else {
      method <- possible_methods[method_match]
    }

    if (is.numeric(reps) == FALSE) {
      stop("Invalid `reps`. Choose a numeric value.")
    }

    if (!all(c("bin", max_ma, min_ma) %in% colnames(bins))) {
      stop(paste0("Either: bin, ", max_ma, ", or ", min_ma,
                  " column(s) do not exist in `bins`."))
    }

    if (is.numeric(occdf[, max_ma]) &&
        max(occdf[, max_ma]) > max(bins[, max_ma])) {
      stop("Maximum age of occurrence data surpasses maximum age of bins.")
    }

    if (is.numeric(occdf[, min_ma]) &&
        min(occdf[, min_ma]) < min(bins[, min_ma])) {
      stop("Minimum age of occurrence data is less than minimum age of bins.")
    }

    if (method == "point" && !is.function(fun)) {
      stop('`fun` is not a function.')
    }

    #=== Reporting Info ===

    # Make an empty list that's the length of the occurrence dataframe.
    bin_list <- list()
    bin_list <- sapply(seq_len(nrow(occdf)), function(x) NULL)

    # For each occurrence, find all the bins that it is present within, and
    # add as elements to that part of the list.
    for (i in seq_len(nrow(bins))) {
      v <- which(occdf[, max_ma] > bins[i, min_ma] &
                 occdf[, min_ma] < bins[i, max_ma])
      for (j in v) {
        bin_list[[j]] <- append(bin_list[[j]], bins$bin[i])
      }
    }

    # Generate id column for data (this is for tracking duplicate rows).
    id <- seq_len(nrow(occdf))
    occdf$id <- id

    # Generate empty column for recording the number of bins an occurrence
    # appears in, and empty columns for the new bin allocation and midpoint.
    occdf$n_bins <- NA
    occdf$bin_assignment <- NA
    occdf$bin_midpoint <- NA

    # Assign number of bins per occurrence.
    occdf$n_bins <- lengths(bin_list)

    # Generate midpoint ages of bins
    bins$mid_ma <- (bins[, max_ma] + bins[, min_ma]) / 2

    #=== Methods ===

    #--- Method 1: Midpoint ---
    if (method == "mid") {
      # If no mid point is present for occurrence age range, add one in a
      # new column.
      rmcol <- FALSE
      if (("mid_ma" %in% colnames(occdf)) == FALSE) {
        occdf$mid_ma <- (occdf[, max_ma] + occdf[, min_ma]) / 2
        rmcol <- TRUE
      }

      # Assign bin based on midpoint age of the age range
      for (i in seq_len(nrow(bins))) {
        v <- which(occdf$mid_ma > bins[i, min_ma] &
                   occdf$mid_ma < bins[i, max_ma])
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
      # Check for errors in inputs
      supp_args <- list(...)
      if (!("..." %in% names(formals(fun)))) {
        indx <- which(!(names(supp_args) %in% names(formals(fun))))
        if (length(indx) > 1) {
          stop(paste(
            paste0("`", names(supp_args)[indx], "`", collapse = "/"),
            "are not valid arguments for the specified function"
          ))
        } else if (length(indx) == 1) {
          stop(paste0(
            "`",
            names(supp_args)[indx],
            "`",
            " is not a valid argument for the specified function"
          ))
        } else if ("x" %in% names(supp_args)) {
          stop("`x` should not be specified. This is generated internally.")
        }
      }
      # make occurrence list for filling with reps
      occ_list <- list()
      occ_list <- sapply(seq_len(nrow(occdf)), function(x) NULL)

      # For each occurrence max/min age, make probability distribution and
      # sample from it. Record that with each occurrence.
      for (i in seq_len(nrow(occdf))) {
        #generate occurrence sequence for sampling
        occ_seq <- seq(from = occdf[i, min_ma],
                       to = occdf[i, max_ma],
                       by = 0.001)
        #generate x for input probability function
        x_prob <- seq(from = 0, to = 1, length.out = length(occ_seq))
        # Generate probabilities
        prob <- fun(x_prob, ...)
        #if max/min ages are the same replicate age
        if (length(unique(occ_seq)) == 1) {
          occ_list[[i]] <- rep(occ_seq, times = reps)
          next
        } else {
        estimates <- sample(x = occ_seq, size = reps,
                            replace = TRUE, prob = prob)
        occ_list[[i]] <- estimates
        }
      }

      occdf$point_estimates <- NA
      #drop cols that are not needed
      occdf <- occdf[, -which(colnames(occdf) == "bin_midpoint")]

      occ_df_list <- list()
      occ_df_list <- sapply(seq_len(reps), function(x) NULL)

      #add point estimates to each dataframe
      for (i in seq_len(reps)) {
        occdf$point_estimates <- do.call(rbind, occ_list)[, i]
          for (j in seq_len(nrow(bins))){
            vec <- which(occdf$point_estimates <= bins[j, max_ma] &
                    occdf$point_estimates >= bins[j, min_ma])
            occdf$bin_assignment[vec] <- bins$bin[j]
          }
        occ_df_list[[i]] <- occdf
      }

      #return list of data
      return(occ_df_list)
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
      for (i in seq_len(nrow(bins))) {
        vec <- which(occdf$bin_assignment == bins$bin[i])
        occdf$bin_midpoint[vec] <- bins$mid_ma[i]
      }

      rownames(occdf) <- seq_len(nrow(occdf))

      # Return the dataframe and end the function.
      return(occdf)
    }

    #--- Method 4: Majority ---
    if (method == "majority") {
      # Setup column for calculating overlap of age range with bin
      occdf$overlap_percentage <- NA

      # Run across bin list
      for (i in seq_along(bin_list)) {
        # Dataframe of bins occurrence known to occur in
        tmpbin <- bins[bins$bin %in% bin_list[[i]], ]

        # Generate sequence of length 10000 for percentage calculations
        occ_seq <- seq(occdf[i, min_ma], occdf[i, max_ma], length.out = 10000)

        # Calculate overlap across known bins
        percentage <- vector()
        for (j in seq_len(nrow(tmpbin))) {
          percentage[j] <- (length(which(occ_seq >= tmpbin[j, min_ma] &
                      occ_seq <= tmpbin[j, max_ma])) / 10000) * 100
        }

        # Assign bins, bin midpoints and overlap percentage
        occdf[i, "bin_assignment"] <- tmpbin$bin[which.max(percentage)]
        occdf[i, "bin_midpoint"] <- tmpbin$mid_ma[which.max(percentage)]
        occdf[i, "overlap_percentage"] <- percentage[which.max(percentage)]
      }
      return(occdf)
    }

    #--- Method 5: Random ---
    if (method == "random") {
      # Generate empty lists for populating
      occ_list <- list()
      occ_list <- sapply(seq_len(nrow(occdf)), function(x) NULL)
      occ_df_list <- list()
      occ_df_list <- sapply(seq_len(reps), function(x) NULL)

      # Randomly sample from the list of bins that occurrence appears in, and
      # add to the bin column for the occurrence.
      for (i in seq_along(bin_list)) {
        # Dataframe of bins occurrence known to occur in
        tmpbin <- bins[bins$bin %in% bin_list[[i]], ]

        # If occurrence only appears in one bin, assign bin
        if (length(bin_list[[i]]) == 1) {
          occ_list[[i]] <- rep(x = bin_list[[i]], times = reps)
          next
        } else {
          # Randomly sample from possible bins
          occ_list[[i]] <- sample(x = tmpbin$bin,
                                  size = reps,
                                  replace = TRUE)
        }
      }

        #add point estimates to each dataframe
        for (i in 1:reps) {
          occdf$bin_assignment <- do.call(rbind, occ_list)[, i]
          occdf$bin_midpoint <- bins$mid_ma[
            sapply(occdf$bin_assignment, function(x) {
              which(bins$bin == x)}, simplify = TRUE)]
          occ_df_list[[i]] <- occdf
        }
      return(occ_df_list)
    }
  }
