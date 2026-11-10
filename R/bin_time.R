#' Assign fossil occurrences to time bins
#'
#' @description
#' These functions assign fossil occurrences to specified time bins based on
#' different approaches commonly applied in palaeobiology (see Details).
#'
#' `r lifecycle::badge("deprecated")` `bin_time()` is deprecated, use one of
#' the other `bin_time_*()` functions instead.
#'
#' @param data \code{dataframe}. A dataframe of the fossil occurrences you
#'   wish to bin. This dataframe should contain at least two columns with
#'   `numeric` values: maximum age of occurrence and minimum age of
#'   occurrence (see `max_ma`, `min_ma`). If required, `numeric` ages can be
#'   generated from interval names via the
#'   \code{\link[palaeoverse:look_up]{look_up()}} function.
#' @param min_ma \code{character}. The name of the column you wish to be
#'   treated as the minimum age for `data` and `bins`, e.g. "min_ma"
#'   (default).
#' @param max_ma \code{character}. The name of the column you wish to be
#'   treated as the maximum age for `data` and `bins`, e.g. "max_ma"
#'   (default).
#' @param bins \code{dataframe}. A dataframe of the bins that you wish to
#'   allocate fossil occurrences to such as that returned by
#'   \code{\link[palaeoverse:time_bins]{time_bins()}}. This dataframe must
#'   contain at least the following named columns: "bin" and those specified
#'   to `max_ma` (default: "max_ma") and `min_ma` (default: "min_ma").
#'   Columns `max_ma` and `min_ma` must be `numeric` values.
#' @param method \code{character}. The method desired for binning fossil
#'   occurrences: "mid", "majority", "all", "random", or "point". This
#'   calls the corresponding `bin_time_*()` function.
#' @param reps \code{numeric}. A positive whole number specifying the number
#'   of replications for sampling. Defaults to 100.
#' @param fun \code{function}. A probability density function from the
#'   stats package such as \link[stats]{dunif} or \link[stats]{dnorm}.
#' @param ... Additional arguments passed to `fun`. These arguments may be
#'   required for function arguments without default values, or if you wish
#'   to overwrite the default argument value (see examples). `x` input values
#'   are generated internally based on the age range of the fossil occurrence
#'   and should not be manually provided. Note that `x` input values range
#'   between 0 and 1, and function arguments should therefore be scaled to be
#'   within these bounds.
#'
#' @return
#' For `bin_time_mid()`, `bin_time_majority()` and `bin_time_all()`, a \code{dataframe}
#' of the original input `data` with the following appended columns is returned:
#' occurrence id (`id`), number of bins that the occurrence age range covers (`n_bins`),
#' bin assignment (`bin_assignment`), and bin midpoint (`bin_midpoint`).
#'
#' For `bin_time_majority()`, an additional column of the majority percentage overlap
#' (`overlap_percentage`) is also appended.
#'
#' For `bin_time_random()` and `bin_time_point()`, a list of length \code{reps}
#' is returned with each element a copy of the `data` and appended columns (random:
#' `bin_assignment` and `bin_midpoint`; point: `bin_assignment` and `point_estimates`).
#'
#' @details Five approaches (methods) exist for assigning occurrences to time
#'   bins:
#' - Midpoint: `bin_time_mid()` is the simplest approach and uses the midpoint
#'   of the fossil occurrence age range to bin the occurrence.
#' - Majority: `bin_time_majority()` bins an occurrence into the bin which it
#'   most overlaps with. As part of this implementation, the majority
#'   percentage overlap of the occurrence is also calculated and returned as
#'   an additional column in `data`. If desired, these percentages can be
#'   used to further filter an occurrence dataset.
#' - All: `bin_time_all()` bins an occurrence into every bin its age range
#'   covers. For occurrences with age ranges of more than one bin, the
#'   occurrence row is duplicated. Each occurrence is assigned an ID in the
#'   column `data$id` so that duplicates can be tracked. Additionally,
#'   `data$n_bins` records the number of bins each occurrence appears within.
#' - Random: `bin_time_random()` randomly samples X amount of bins (with
#'   replacement) from the bins that the fossil occurrence age range covers
#'   with equal probability regardless of bin length. Its `reps` argument
#'   determines the number of times the sample process is repeated. All
#'   replications are stored as individual elements within the returned list
#'   with an appended `bin_assignment` and `bin_midpoint` column to the
#'   original input `data`. If desired, users can easily bind this list using
#'   \code{do.call(rbind, x)}.
#' - Point: `bin_time_point()` randomly samples X (`reps`) amount of point age
#'   estimates from the age range of the fossil occurrence. Sampling follows a
#'   user-input probability density function such
#'   as \link[stats]{dnorm} (see example 5). Users should also provide any
#'   additional arguments for the probability density function (see `...`).
#'   However, `x` (vector of quantiles) values should not be provided as these
#'   values are input from the age range of each occurrence. These values
#'   range between 0 and 1, and therefore function arguments should be scaled
#'   to be within these bounds. Its `reps` argument determines the
#'   number of times the sample process is repeated. All replications are
#'   stored as individual elements within the returned list with an appended
#'   `bin_assignment` and `point_estimates` column to the original input
#'   `data`. If desired, users can easily bind this list using
#'   \code{do.call(rbind, x)}.
#'
#' @section Developer(s): Christopher D. Dean & Lewis A. Jones
#' @section Reviewer(s): William Gearty
#' @importFrom stats dunif
#' @examples
#' #Grab internal tetrapod data
#' data <- tetrapods[1:100, ]
#' bins <- time_bins()
#'
#' #Assign via midpoint age of fossil occurrence data
#' ex1 <- bin_time_mid(data = data, bins = bins)
#'
#' #Assign to all bins that age range covers
#' ex2 <- bin_time_all(data = data, bins = bins)
#'
#' #Assign via majority overlap based on fossil occurrence age range
#' ex3 <- bin_time_majority(data = data, bins = bins)
#'
#' #Assign randomly to overlapping bins based on fossil occurrence age range
#' ex4 <- bin_time_random(data = data, bins = bins, reps = 5)
#'
#' #Assign point estimates following a normal distribution
#' ex5 <- bin_time_point(data = data, bins = bins, reps = 5, fun = dnorm,
#'                       mean = 0.5, sd = 0.25)
#' @name bin_time
NULL

#' @rdname bin_time
#' @export
bin_time_mid <- function(data, min_ma = "min_ma", max_ma = "max_ma", bins) {
  ensure_args_are_named(exceptions = "data")
  check_bin_time_args(data, min_ma, max_ma, bins)

  rmcol <- FALSE
  if (!("mid_ma" %in% colnames(data))) {
    data$mid_ma <- (data[[max_ma]] + data[[min_ma]]) / 2
    rmcol <- TRUE
  }
  if (!("mid_ma" %in% colnames(bins))) {
    bins$mid_ma <- (bins[[max_ma]] + bins[[min_ma]]) / 2
  }

  # Check if mid_ma equivalent to any bin boundaries
  if (any(data$mid_ma %in% c(bins[[min_ma]], bins[[max_ma]]))) {
    cli::cli_warn(c(
      "!" = "One or more occurrences have a midpoint age equivalent to a bin boundary.",
      "x" = "Binning is skipped for these occurrences.",
      "i" = "Hint: {.code which(is.na(data$bin_assignment))}."
    ))
  }

  # Generate id column for data (this is for tracking duplicate rows).
  data$id <- seq_len(nrow(data))
  data$n_bins <- lengths(compute_overlaps(data, bins, min_ma, max_ma))
  data$bin_assignment <- NA
  data$bin_midpoint <- NA

  for (i in seq_len(nrow(bins))) {
    v <- which(
      data$mid_ma > bins[i, min_ma, drop = TRUE] &
        data$mid_ma < bins[i, max_ma, drop = TRUE]
    )
    data$bin_assignment[v] <- bins$bin[i]
    data$bin_midpoint[v] <- bins$mid_ma[i]
  }

  # Remove mid_ma for fossil occurrences (if not already present as input)
  if (rmcol) {
    data[["mid_ma"]] <- NULL
  }

  data
}

#' @rdname bin_time
#' @export
bin_time_majority <- function(
  data,
  min_ma = "min_ma",
  max_ma = "max_ma",
  bins
) {
  ensure_args_are_named(exceptions = "data")
  check_bin_time_args(data, min_ma, max_ma, bins)

  # For each observation in occurrence data, get the indices of bins that completely
  # fall in the `min_ma` - `max_ma` interval of this observation.
  overlaps <- compute_overlaps(data, bins, min_ma, max_ma)
  bins$mid_ma <- (bins[[max_ma]] + bins[[min_ma]]) / 2

  data$id <- seq_len(nrow(data))
  data$n_bins <- lengths(overlaps)
  data$bin_assignment <- NA
  data$bin_midpoint <- NA
  data$overlap_percentage <- NA

  for (i in seq_along(overlaps)) {
    # Dataframe of bins occurrence known to occur in
    tmpbin <- bins[bins$bin %in% overlaps[[i]], ]

    # Generate sequence of length 10000 for percentage calculations
    occ_seq <- seq(
      data[i, min_ma, drop = TRUE],
      data[i, max_ma, drop = TRUE],
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
    data[i, "bin_assignment"] <- tmpbin$bin[which.max(percentage)]
    data[i, "bin_midpoint"] <- tmpbin$mid_ma[which.max(percentage)]
    data[i, "overlap_percentage"] <- percentage[which.max(percentage)]
  }
  data
}

#' @rdname bin_time
#' @export
bin_time_all <- function(data, min_ma = "min_ma", max_ma = "max_ma", bins) {
  ensure_args_are_named(exceptions = "data")
  check_bin_time_args(data, min_ma, max_ma, bins)

  # For each observation in occurrence data, get the indices of bins that completely
  # fall in the `min_ma` - `max_ma` interval.
  overlaps <- compute_overlaps(data, bins, min_ma, max_ma)
  bins$mid_ma <- (bins[[max_ma]] + bins[[min_ma]]) / 2

  data$id <- seq_len(nrow(data))
  data$n_bins <- lengths(overlaps)
  data$bin_assignment <- NA
  data$bin_midpoint <- NA

  # Duplicate rows by number of bins.
  data <- data[rep(seq_len(dim(data)[1]), data$n_bins), ]

  # Use id to track unique rows and update bin numbers.
  for (i in seq_along(overlaps)) {
    id_vec <- which(data$id == i)
    data$bin_assignment[id_vec] <- overlaps[[i]]
  }
  # Add bin midpoints to dataframe
  for (i in seq_len(nrow(bins))) {
    vec <- which(data$bin_assignment == bins$bin[i])
    data$bin_midpoint[vec] <- bins$mid_ma[i]
  }

  if (!inherits(data, "tbl")) {
    rownames(data) <- seq_len(nrow(data))
  }

  data
}

#' @rdname bin_time
#' @export
bin_time_random <- function(
  data,
  min_ma = "min_ma",
  max_ma = "max_ma",
  bins,
  reps = 100
) {
  ensure_args_are_named(exceptions = "data")
  rlang::check_number_whole(reps, min = 1)
  check_bin_time_args(data, min_ma, max_ma, bins)

  # For each observation in occurrence data, get the indices of bins that completely
  # fall in the `min_ma` - `max_ma` interval.
  overlaps <- compute_overlaps(data, bins, min_ma, max_ma)
  bins$mid_ma <- (bins[[max_ma]] + bins[[min_ma]]) / 2

  data$id <- seq_len(nrow(data))
  data$n_bins <- lengths(overlaps)
  data$bin_assignment <- NA
  data$bin_midpoint <- NA

  # Generate empty lists for populating
  occ_list <- vector("list", length = nrow(data))
  data_list <- vector("list", length = reps)

  # Randomly sample from the list of bins that occurrence appears in, and
  # add to the bin column for the occurrence.
  for (i in seq_along(overlaps)) {
    # Dataframe of bins occurrence known to occur in
    tmpbin <- bins[bins$bin %in% overlaps[[i]], ]

    # If occurrence only appears in one bin, assign bin
    if (length(overlaps[[i]]) == 1) {
      occ_list[[i]] <- rep(x = overlaps[[i]], times = reps)
      next
    } else {
      # Randomly sample from possible bins
      occ_list[[i]] <- sample(x = tmpbin$bin, size = reps, replace = TRUE)
    }
  }

  #add point estimates to each dataframe
  for (i in 1:reps) {
    data$bin_assignment <- do.call(rbind, occ_list)[, i]
    data$bin_midpoint <- bins$mid_ma[
      sapply(
        data$bin_assignment,
        function(x) {
          which(bins$bin == x)
        },
        simplify = TRUE
      )
    ]
    data_list[[i]] <- data
  }
  data_list
}

#' @rdname bin_time
#' @export
bin_time_point <- function(
  data,
  min_ma = "min_ma",
  max_ma = "max_ma",
  bins,
  reps = 100,
  fun = dunif,
  ...
) {
  ensure_args_are_named(exceptions = "data")
  rlang::check_number_whole(reps, min = 1)
  check_function(fun)

  supp_args <- list(...)
  # If `fun` doesn't have `...` then we can properly check that arguments passed in our
  # `...` exist in the definition of `fun`.
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
      "{.arg x} must not be specified as it is already defined internally."
    )
  }

  check_bin_time_args(data, min_ma, max_ma, bins)

  # For each observation in occurrence data, get the indices of bins that completely
  # fall in the `min_ma` - `max_ma` interval.
  n_bins <- vapply(
    seq_len(nrow(data)),
    function(i) {
      sum(
        data[[max_ma]][i] > bins[[min_ma]] &
          data[[min_ma]][i] < bins[[max_ma]]
      )
    },
    integer(1)
  )
  data$id <- seq_len(nrow(data))
  data$n_bins <- n_bins
  data$bin_assignment <- NA

  # make occurrence list for filling with reps
  occ_list <- vector("list", length = nrow(data))

  # For each occurrence max/min age, make probability distribution and
  # sample from it. Record that with each occurrence.
  for (i in seq_len(nrow(data))) {
    #generate occurrence sequence for sampling
    occ_seq <- seq(
      from = data[i, min_ma, drop = TRUE],
      to = data[i, max_ma, drop = TRUE],
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

  data$point_estimates <- NA

  data_list <- vector("list", length = reps)

  #add point estimates to each dataframe
  for (i in seq_len(reps)) {
    data$point_estimates <- do.call(rbind, occ_list)[, i]
    for (j in seq_len(nrow(bins))) {
      vec <- which(
        data$point_estimates <= bins[j, max_ma, drop = TRUE] &
          data$point_estimates >= bins[j, min_ma, drop = TRUE]
      )
      data$bin_assignment[vec] <- bins$bin[j]
    }
    data_list[[i]] <- data
  }

  data_list
}

#' @rdname bin_time
#' @export
bin_time <- function(
  data,
  min_ma = "min_ma",
  max_ma = "max_ma",
  bins,
  method = "mid",
  reps = 100,
  fun = dunif,
  ...
) {
  ensure_args_are_named(exceptions = "data")
  lifecycle::deprecate_warn(
    "2.0.0",
    "bin_time()",
    details = "Use one of `bin_time_mid()`, `bin_time_majority()`, `bin_time_all()`, `bin_time_random()`, or `bin_time_point()` instead.",
    always = TRUE
  )

  method <- rlang::arg_match(
    method,
    values = c("mid", "majority", "all", "random", "point")
  )

  switch(
    method,
    mid = bin_time_mid(data, min_ma = min_ma, max_ma = max_ma, bins = bins),
    majority = bin_time_majority(
      data,
      min_ma = min_ma,
      max_ma = max_ma,
      bins = bins
    ),
    all = bin_time_all(data, min_ma = min_ma, max_ma = max_ma, bins = bins),
    random = bin_time_random(
      data,
      min_ma = min_ma,
      max_ma = max_ma,
      bins = bins,
      reps = reps
    ),
    point = bin_time_point(
      data,
      min_ma = min_ma,
      max_ma = max_ma,
      bins = bins,
      reps = reps,
      fun = fun,
      ...
    )
  )
}

# Input checks shared by all "bin_time" methods.
check_bin_time_args <- function(
  data,
  min_ma,
  max_ma,
  bins,
  call = rlang::caller_env()
) {
  # Errors are reported as coming from the exported function. Without this line,
  # error messages would report `check_bin_time()`.
  rlang::local_error_call(call)

  check_data_frame(data)
  check_data_frame(bins)

  check_column_presence(data, min_ma)
  check_column_presence(data, max_ma)
  check_column_presence(bins, min_ma)
  check_column_presence(bins, max_ma)
  check_column_presence(bins, "bin")

  data_min_ma_vals <- data[[min_ma]]
  data_max_ma_vals <- data[[max_ma]]
  check_na(data, min_ma)
  check_na(data, max_ma)
  check_class(data, min_ma, "numeric")
  check_class(data, max_ma, "numeric")
  check_min_lower_than_max(data, min_ma, max_ma)

  bins_min_ma_vals <- bins[[min_ma]]
  bins_max_ma_vals <- bins[[max_ma]]
  check_min_lower_than_max(bins, min_ma, max_ma)

  if (max(data_max_ma_vals) > max(bins_max_ma_vals)) {
    cli::cli_abort(
      "Maximum age of occurrence data ({.val {max(data_max_ma_vals)}}) surpasses maximum age of bins ({.val {max(bins_max_ma_vals)}})."
    )
  }

  if (min(data_min_ma_vals) < min(bins_min_ma_vals)) {
    cli::cli_abort(
      "Minimum age of occurrence data ({.val {min(data_min_ma_vals)}}) is less than minimum age of bins ({.val {min(bins_min_ma_vals)}})."
    )
  }
}

# Create a list of overlaps between occurrence data and bins. For each observation in the
# occurrence data, we get the indices of the bins that fall completely between the min and
# max age of the occurrence.
# In other words, each list element records the bins each occurrence appears within.
compute_overlaps <- function(data, bins, min_ma, max_ma) {
  lapply(seq_len(nrow(data)), function(i) {
    bins$bin[
      data[[max_ma]][i] > bins[[min_ma]] & data[[min_ma]][i] < bins[[max_ma]]
    ]
  })
}
