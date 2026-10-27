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
#' @param method The method desired for binning fossil occurrences. This must
#'   be a call to one of [bin_time_mid()] (default), [bin_time_majority()],
#'   [bin_time_all()], [bin_time_random()], or [bin_time_point()], placed
#'   directly in this argument. See Details for a description of each.
#' @param reps,fun,... `r lifecycle::badge("deprecated")` Use the arguments
#'   of [bin_time_random()] or [bin_time_point()] instead.
#'
#' @return For [bin_time_mid()], [bin_time_majority()] and [bin_time_all()], a
#'   \code{dataframe} of the original input `occdf` with the following
#'   appended columns is returned: occurrence id (`id`), number of bins that
#'   the occurrence age range covers (`n_bins`), bin assignment
#'   (`bin_assignment`), and bin midpoint (`bin_midpoint`). In the case of
#'   [bin_time_majority()], an additional column of the majority percentage
#'   overlap (`overlap_percentage`) is also appended. For [bin_time_random()]
#'   and [bin_time_point()], a \code{list} is returned
#'   (of length reps) with each element a copy of the `occdf` and appended
#'   columns (random: `bin_assignment` and `bin_midpoint`; point:
#'   `bin_assignment` and `point_estimates`).
#'
#' @details Five approaches (methods) exist in the `bin_time()` function for
#'   assigning occurrences to time bins:
#' - Midpoint: [bin_time_mid()] is the simplest approach and uses the midpoint
#'   of the fossil occurrence age range to bin the occurrence.
#' - Majority: [bin_time_majority()] bins an occurrence into the bin which it
#'   most overlaps with. As part of this implementation, the majority
#'   percentage overlap of the occurrence is also calculated and returned as
#'   an additional column in `occdf`. If desired, these percentages can be
#'   used to further filter an occurrence dataset.
#' - All: [bin_time_all()] bins an occurrence into every bin its age range
#'   covers. For occurrences with age ranges of more than one bin, the
#'   occurrence row is duplicated. Each occurrence is assigned an ID in the
#'   column `occdf$id` so that duplicates can be tracked. Additionally,
#'   `occdf$n_bins` records the number of bins each occurrence appears within.
#' - Random: [bin_time_random()] randomly samples X amount of bins (with
#'   replacement) from the bins that the fossil occurrence age range covers
#'   with equal probability regardless of bin length. Its `reps` argument
#'   determines the number of times the sample process is repeated. All
#'   replications are stored as individual elements within the returned list
#'   with an appended `bin_assignment` and `bin_midpoint` column to the
#'   original input `occdf`. If desired, users can easily bind this list using
#'   \code{do.call(rbind, x)}.
#' - Point: [bin_time_point()] randomly samples X (`reps`) amount of point age
#'   estimates from the age range of the fossil occurrence. Sampling follows a
#'   user-input probability density function such
#'   as \link[stats]{dnorm} (see example 5). Users should also provide any
#'   additional arguments for the probability density function in `...`.
#'   However, `x` (vector of quantiles) values should not be provided as these
#'   values are input from the age range of each occurrence. These values
#'   range between 0 and 1, and therefore function arguments should be scaled
#'   to be within these bounds. Its `reps` argument determines the
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
#' ex1 <- bin_time(occdf = occdf, bins = bins, method = bin_time_mid())
#'
#' #Assign to all bins that age range covers
#' ex2 <- bin_time(occdf = occdf, bins = bins, method = bin_time_all())
#'
#' #Assign via majority overlap based on fossil occurrence age range
#' ex3 <- bin_time(occdf = occdf, bins = bins, method = bin_time_majority())
#'
#' #Assign randomly to overlapping bins based on fossil occurrence age range
#' ex4 <- bin_time(occdf = occdf, bins = bins,
#'                 method = bin_time_random(reps = 5))
#'
#' #Assign point estimates following a normal distribution
#' ex5 <- bin_time(occdf = occdf, bins = bins,
#'                 method = bin_time_point(reps = 5, fun = dnorm,
#'                                         mean = 0.5, sd = 0.25))
#' @export
bin_time <- function(
  occdf,
  min_ma = "min_ma",
  max_ma = "max_ma",
  bins,
  method = bin_time_mid(),
  reps = deprecated(),
  fun = deprecated(),
  ...
) {
  ensure_args_are_named(exceptions = "occdf")

  if (lifecycle::is_present(reps)) {
    lifecycle::deprecate_stop(
      "2.0.0",
      "bin_time(reps)",
      details = "Use the `reps` argument of `bin_time_random()` or `bin_time_point()` instead."
    )
  }
  if (lifecycle::is_present(fun)) {
    lifecycle::deprecate_stop("2.0.0", "bin_time(fun)", "bin_time_point(fun)")
  }
  if (...length() > 0) {
    lifecycle::deprecate_stop("2.0.0", "bin_time(...)", "bin_time_point(...)")
  }

  check_data_frame(occdf)
  check_data_frame(bins)

  check_column_presence(occdf, min_ma)
  check_column_presence(occdf, max_ma)
  check_column_presence(bins, min_ma)
  check_column_presence(bins, max_ma)
  check_column_presence(bins, "bin")

  occdf_min_ma_vals <- occdf[[min_ma]]
  occdf_max_ma_vals <- occdf[[max_ma]]
  check_na(occdf, min_ma)
  check_na(occdf, max_ma)
  check_class(occdf, min_ma, "numeric")
  check_class(occdf, max_ma, "numeric")
  check_min_lower_than_max(occdf, min_ma, max_ma)

  # The bin_time_*() constructors error if they are not evaluated while this
  # flag is set, i.e. when `method` is forced here.
  in_bin_time <- the$in_bin_time
  the$in_bin_time <- TRUE
  on.exit(the$in_bin_time <- in_bin_time, add = TRUE)
  force(method)
  the$in_bin_time <- in_bin_time

  if (!inherits(method, "palaeoverse_bin_time_method")) {
    cli::cli_abort(
      c(
        "{.arg method} must be a call to one of {.fn bin_time_mid}, {.fn bin_time_majority}, {.fn bin_time_all}, {.fn bin_time_random}, or {.fn bin_time_point}, not {obj_type_friendly(method)}.",
        "i" = if (
          rlang::is_string(method) &&
            method %in% c("mid", "majority", "all", "random", "point")
        ) {
          "Use {.code method = bin_time_{method}()} instead."
        }
      )
    )
  }
  reps <- method$reps
  fun <- method$fun
  supp_args <- method$args
  method <- method$method

  bins_min_ma_vals <- bins[[min_ma]]
  bins_max_ma_vals <- bins[[max_ma]]
  check_min_lower_than_max(bins, min_ma, max_ma)

  if (max(occdf_max_ma_vals) > max(bins_max_ma_vals)) {
    cli::cli_abort(
      "Maximum age of occurrence data ({.val {max(occdf_max_ma_vals)}}) surpasses maximum age of bins ({.val {max(bins_max_ma_vals)}})."
    )
  }

  if (min(occdf_min_ma_vals) < min(bins_min_ma_vals)) {
    cli::cli_abort(
      "Minimum age of occurrence data ({.val {min(occdf_min_ma_vals)}}) is less than minimum age of bins ({.val {min(bins_min_ma_vals)}})."
    )
  }

  #=== Reporting Info ===

  # Make an empty list that's the length of the occurrence dataframe.
  bin_list <- vector("list", length = nrow(occdf))

  # For each occurrence, find all the bins that it is present within, and
  # add as elements to that part of the list.
  for (i in seq_len(nrow(bins))) {
    v <- which(
      occdf[, max_ma, drop = TRUE] > bins[i, min_ma, drop = TRUE] &
        occdf[, min_ma, drop = TRUE] < bins[i, max_ma, drop = TRUE]
    )
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
  bins$mid_ma <- (bins[, max_ma, drop = TRUE] +
    bins[, min_ma, drop = TRUE]) /
    2

  #=== Methods ===

  #--- Method 1: Midpoint ---
  if (method == "mid") {
    # If no mid point is present for occurrence age range, add one in a
    # new column.
    rmcol <- FALSE
    if (!("mid_ma" %in% colnames(occdf))) {
      occdf$mid_ma <- (occdf[, max_ma, drop = TRUE] +
        occdf[, min_ma, drop = TRUE]) /
        2
      rmcol <- TRUE
    }
    # Check if mid_ma equivalent to any bin boundaries
    if (any(occdf$mid_ma %in% bins$mid_ma)) {
      warning(paste(
        "One or more occurrences have a midpoint age",
        "equivalent to a bin boundary. Binning skipped for",
        "these occurrences.",
        "Hint: `which(is.na(occdf$bin_assignment))`."
      ))
    }

    # Assign bin based on midpoint age of the age range
    for (i in seq_len(nrow(bins))) {
      v <- which(
        occdf$mid_ma > bins[i, min_ma, drop = TRUE] &
          occdf$mid_ma < bins[i, max_ma, drop = TRUE]
      )
      occdf$bin_assignment[v] <- bins$bin[i]
      occdf$bin_midpoint[v] <- bins$mid_ma[i]
    }

    # Remove mid_ma for fossil occurrences (if not already present as input)
    if (rmcol) {
      occdf <- occdf[, -which(colnames(occdf) == "mid_ma")]
    }

    # Return the dataframe and end the function.
    return(occdf)
  }

  #--- Method 2: Point estimates ---
  if (method == "point") {
    # make occurrence list for filling with reps
    occ_list <- vector("list", length = nrow(occdf))

    # For each occurrence max/min age, make probability distribution and
    # sample from it. Record that with each occurrence.
    for (i in seq_len(nrow(occdf))) {
      #generate occurrence sequence for sampling
      occ_seq <- seq(
        from = occdf[i, min_ma, drop = TRUE],
        to = occdf[i, max_ma, drop = TRUE],
        by = 0.001
      )
      #generate x for input probability function
      x_prob <- seq(from = 0, to = 1, length.out = length(occ_seq))
      # Generate probabilities
      prob <- do.call(fun, c(list(x_prob), supp_args))
      #if max/min ages are the same replicate age
      if (length(unique(occ_seq)) == 1) {
        occ_list[[i]] <- rep(occ_seq, times = reps)
        next
      } else {
        estimates <- sample(
          x = occ_seq,
          size = reps,
          replace = TRUE,
          prob = prob
        )
        occ_list[[i]] <- estimates
      }
    }

    occdf$point_estimates <- NA
    #drop cols that are not needed
    occdf <- occdf[, -which(colnames(occdf) == "bin_midpoint")]

    occ_df_list <- vector("list", length = reps)

    #add point estimates to each dataframe
    for (i in seq_len(reps)) {
      occdf$point_estimates <- do.call(rbind, occ_list)[, i]
      for (j in seq_len(nrow(bins))) {
        vec <- which(
          occdf$point_estimates <= bins[j, max_ma, drop = TRUE] &
            occdf$point_estimates >= bins[j, min_ma, drop = TRUE]
        )
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

    if (!inherits(occdf, "tbl")) {
      rownames(occdf) <- seq_len(nrow(occdf))
    }

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
      occ_seq <- seq(
        occdf[i, min_ma, drop = TRUE],
        occdf[i, max_ma, drop = TRUE],
        length.out = 10000
      )

      # Calculate overlap across known bins
      percentage <- vector()
      for (j in seq_len(nrow(tmpbin))) {
        percentage[j] <- (length(which(
          occ_seq >= tmpbin[j, min_ma] &
            occ_seq <= tmpbin[j, max_ma]
        )) /
          10000) *
          100
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
    occ_list <- vector("list", length = nrow(occdf))
    occ_df_list <- vector("list", length = reps)

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
        occ_list[[i]] <- sample(x = tmpbin$bin, size = reps, replace = TRUE)
      }
    }

    #add point estimates to each dataframe
    for (i in 1:reps) {
      occdf$bin_assignment <- do.call(rbind, occ_list)[, i]
      occdf$bin_midpoint <- bins$mid_ma[
        sapply(
          occdf$bin_assignment,
          function(x) {
            which(bins$bin == x)
          },
          simplify = TRUE
        )
      ]
      occ_df_list[[i]] <- occdf
    }
    return(occ_df_list)
  }
}

the <- new.env(parent = emptyenv())
the$in_bin_time <- FALSE

#' Methods to assign fossil occurrences to time bins
#'
#' These functions specify the method used by [bin_time()] to assign fossil
#' occurrences to time bins. They must be called directly in the `method`
#' argument of [bin_time()] and error if they are used anywhere else. See the
#' Details section of [bin_time()] for a description of each method.
#'
#' @param ... For `bin_time_point()`, additional arguments passed to `fun`.
#'   These arguments may be required for function arguments without default
#'   values, or if you wish to overwrite the default argument value (see
#'   examples). `x` input values are generated internally based on the age
#'   range of the fossil occurrence and should not be manually provided. Note
#'   that `x` input values range between 0 and 1, and function arguments
#'   should therefore be scaled to be within these bounds. For
#'   `bin_time_random()`, these dots must be empty.
#' @param reps \code{numeric}. A positive whole number specifying the number
#'   of replications for sampling. Defaults to 100.
#' @param fun \code{function}. A probability density function from the
#'   stats package such as \link[stats]{dunif} or \link[stats]{dnorm}.
#'
#' @return An object of class `palaeoverse_bin_time_method`, only meant to be
#'   used by [bin_time()].
#'
#' @name bin_time_methods
#' @examples
#' occdf <- tetrapods[1:100, ]
#' bins <- time_bins()
#'
#' ex1 <- bin_time(occdf = occdf, bins = bins, method = bin_time_majority())
#' ex2 <- bin_time(occdf = occdf, bins = bins,
#'                 method = bin_time_point(reps = 5, fun = dnorm,
#'                                         mean = 0.5, sd = 0.25))
NULL

#' @rdname bin_time_methods
#' @export
bin_time_mid <- function() {
  bin_time_method("mid")
}

#' @rdname bin_time_methods
#' @export
bin_time_majority <- function() {
  bin_time_method("majority")
}

#' @rdname bin_time_methods
#' @export
bin_time_all <- function() {
  bin_time_method("all")
}

#' @rdname bin_time_methods
#' @export
bin_time_random <- function(..., reps = 100) {
  out <- bin_time_method("random", reps = reps)
  rlang::check_dots_empty()
  rlang::check_number_whole(reps, min = 1)
  out
}

#' @rdname bin_time_methods
#' @export
bin_time_point <- function(..., reps = 100, fun = dunif) {
  # Arguments are only forced once the context is checked.
  out <- bin_time_method("point", reps = reps, fun = fun, args = list(...))
  rlang::check_number_whole(reps, min = 1)
  check_function(fun)

  supp_args <- out$args
  if (!("..." %in% names(formals(fun)))) {
    indx <- which(!(names(supp_args) %in% names(formals(fun))))
    if (length(indx) >= 1) {
      to_report <- cli::cli_vec(
        names(supp_args)[indx],
        list(`vec-last` = ", and ")
      )
      cli::cli_abort(
        "{.code {to_report}} {?is/are} not {?a/} valid argument{?s} for {.arg fun}."
      )
    }
  }
  if ("x" %in% names(supp_args)) {
    cli::cli_abort(
      "{.arg x} must not be specified as it is generated internally."
    )
  }

  out
}

bin_time_method <- function(method, reps = NULL, fun = NULL, args = NULL) {
  call <- rlang::caller_env()
  if (!isTRUE(the$in_bin_time)) {
    cli::cli_abort(
      "{.fn {rlang::call_name(rlang::frame_call(call))}} must be used directly in the {.arg method} argument of {.fn bin_time}.",
      call = call
    )
  }
  structure(
    list(method = method, reps = reps, fun = fun, args = args),
    class = "palaeoverse_bin_time_method"
  )
}
