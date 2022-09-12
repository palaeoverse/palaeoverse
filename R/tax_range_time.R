#' Calculate the temporal range of fossil taxa
#'
#' A function to calculate the temporal range of fossil taxa from occurrence
#' data.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences. The
#' dataframe should contain at least three columns: names of taxa (`character`)
#' and the maximum (`numeric` or `character`) and minimum
#' (`numeric`  or `character`) age which constrain the
#' age range (in millions of years if `numeric`) of the fossil occurrence.
#' If the age range columns are provided as (`character`) values
#' (i.e., stage names), the function will attempt to extract numeric age data
#' from the Geological Timescale by matching the provided character strings to
#' the stage names.
#' @param name \code{character}. The name of the column you wish to be treated
#' as the input names (e.g., "species" or "genus"). NA data should be removed
#' prior to function call.
#' @param min_ma \code{character}. The name of the column you wish to be treated
#' as the minimum limit of the age range (e.g., "min_ma").
#' @param max_ma \code{character}. The name of the column you wish to be treated
#' as the maximum limit of the age range (e.g., "max_ma").
#' @param by \code{character}. How should the output (and plot) be sorted?
#' Either: "FAD" (first-appearance date), "LAD" (last-appearance data), "name"
#' (alphabetically by taxon names).
#' @param scale \code{character}. Specify the desired geological timescale to
#' be used, either "GTS2020" or "GTS2012". This argument is only useful if the
#' supplied "max_ma" and "min_ma" columns are of class \code{character}.
#' "GTS2020" is the default. For similar advanced functionality, see
#' palaeoverse::look_up.
#' @param plot \code{logical}. Should a plot of the ranges be generated?
#' @param return_error \code{logical}. Should a numeric vector be returned
#' to flag the rows of the `occdf` that cannot be matched to `character`
#' interval names? This is only relevant if `max_ma` and `min_ma`
#' are `character` values.
#'
#' @return A \code{dataframe} containing the following columns:
#' unique taxa (`taxa`), taxa ID (`taxon_id`), first appearance of taxon
#' (`FAD_ma`), last appearance of taxon (`LAD_ma`), duration of temporal
#' range (`range_myr`), and number of occurrences per taxon (`n_occ`) is
#' returned.
#'
#' @details The temporal range(s) of taxa are calculated by extracting all
#' unique taxa (`name` column) from the input `occdf`, and checking their first
#' and last appearance. The temporal duration of each taxon is also calculated.
#' A plot of the temporal range of each taxa is also returned if `plot = TRUE`.
#' If `return_error == TRUE`, a numeric vector flagging rows that cannot be
#' matched to interval names is returned.
#'
#' Note: this function provides output based solely on the user input data. The
#' true duration of a taxon is likely confounded by uncertainty in dating
#' occurrences and incomplete sampling and preservation.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Bethany Allen
#' @importFrom graphics points
#' @examples
#' # Grab internal data
#' occdf <- tetrapods
#' # Remove NAs
#' occdf <- subset(occdf, !is.na(genus))
#' # Temporal range
#' tax_range_time(occdf = occdf, name = "genus", plot = TRUE)
#'
#' @export
tax_range_time <- function(occdf,
                           name = "name",
                           min_ma = "min_ma",
                           max_ma = "max_ma",
                           by = "FAD",
                           scale = "GTS2020",
                           plot = FALSE,
                           return_error = FALSE) {

  #=== Handling errors ===
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe")
  }

  if (is.logical(plot) == FALSE) {
    stop("`plot` should be logical (TRUE/FALSE)")
  }

  if (class(occdf[, max_ma]) != class(occdf[, min_ma])) {
    stop("The class of max_ma and min_ma must be the same.
Either numeric or character, but not both.")
  }

  if (any(c(name, min_ma, max_ma) %in% colnames(occdf) == FALSE)) {
    stop("Either `name`, `min_ma`, or `max_ma`, is not a named column in
`occdf`")
  }

  if (any(is.na(occdf[, name]))) {
    stop("The `name` column contains NA values")
  }

  if (any(is.na(occdf[, min_ma])) || any(is.na(occdf[, max_ma]))) {
    stop("`min_ma` and/or `max_ma` columns contain NA values")
  }

  if (!by %in% c("name", "FAD", "LAD")) {
    stop('`by` must be either "FAD", "LAD", or "name"')
  }

  #=== Set-up ===
  unique_taxa <- unique(occdf[, name])
  # Order taxa
  unique_taxa <- unique_taxa[order(unique_taxa)]

  # Character string entered
  if (is.character(occdf[, min_ma]) && is.character(occdf[, max_ma])) {
    # Which GTS should be used?
    if (scale == "GTS2020") {
      df <- palaeoverse::GTS2020
    }
    if (scale == "GTS2012") {
      df <- palaeoverse::GTS2012
    }
    df <- subset(df, rank == "stage")
    # Which intervals are present?
    unique_intervals <- unique(c(occdf[, max_ma], occdf[, min_ma]))
    # Which of these intervals are present in the GTS
    vec <- which(df$interval_name %in% unique_intervals)
    # If not available, stop and return error
    if (length(vec) != length(unique_intervals)) {
      error_vec <- which(!unique_intervals %in% df$interval_name)
      if (return_error == TRUE) {
        return(error_vec)
      } else {
      stop(
        paste(c(
          "Check spelling of specified max_ma and min_ma intervals.
  Available interval names are accessible via GTS2020 and GTS2012.
  Check rows:",
          capture.output(print(error_vec))
        ),
        collapse = "\n"
      ))
      }
    }
    # Convert character to numeric ages
    for (i in seq_along(df$interval_name)) {
      occdf[which(occdf[, max_ma] == df$interval_name[i]), max_ma] <-
        df$max_ma[i]
      occdf[which(occdf[, min_ma] == df$interval_name[i]), min_ma] <-
        df$min_ma[i]
    }
    occdf[, max_ma] <- as.numeric(occdf[, max_ma])
    occdf[, min_ma] <- as.numeric(occdf[, min_ma])
  }
  #=== Temporal range ===
  # Generate dataframe for population
  temp_df <- data.frame(taxa = unique_taxa,
                          taxon_id = seq(1, length(unique_taxa), 1),
                          FAD_ma = rep(NA, length(unique_taxa)),
                          LAD_ma = rep(NA, length(unique_taxa)),
                          range_myr = rep(NA, length(unique_taxa)),
                          n_occ = rep(NA, length(unique_taxa)))
    # Run for loop across unique taxa
    for(i in seq_along(unique_taxa)){
      vec <- which(occdf[, name] == unique_taxa[i])
      temp_df$FAD_ma[i] <- max(occdf[vec, max_ma])
      temp_df$LAD_ma[i] <- min(occdf[vec, min_ma])
      temp_df$range_myr[i] <- temp_df$FAD_ma[i] - temp_df$LAD_ma[i]
      temp_df$n_occ[i] <- length(vec)
    }
    # Remove row names
    row.names(temp_df) <- NULL
    # Round off values
    temp_df[, c("FAD_ma", "LAD_ma", "range_myr")] <- round(
      x = temp_df[, c("FAD_ma", "LAD_ma", "range_myr")], digits = 3)

    # Should data be ordered by FAD or LAD?
    if (by == "FAD") {
      temp_df <- temp_df[order(temp_df$FAD_ma), ]
      temp_df$taxon_id <- 1:nrow(temp_df)
    }

    if (by == "LAD") {
      temp_df <- temp_df[order(temp_df$LAD_ma), ]
      temp_df$taxon_id <- 1:nrow(temp_df)
      }

    # Plot data?
    if (plot == TRUE){
      x_range <- c(max(temp_df$FAD_ma), min(temp_df$LAD_ma))
      y_range <- c(0, nrow(temp_df))
      plot(x = NA,
           y = NA,
           xlim = x_range,
           ylim = y_range,
           axes = TRUE,
           xaxt = "n",
           xlab = NA,
           ylab = "Taxa ID",
           main = "Temporal range of taxa")
      segments(x0 = temp_df$FAD_ma,
               x1 = temp_df$LAD_ma,
               y0 = temp_df$taxon_id,
               col = temp_df$taxon_id)
      points(x = temp_df$FAD_ma,
             y = temp_df$taxon_id,
             pch = 20,
             col = temp_df$taxon_id)
      points(x = temp_df$LAD_ma,
             y = temp_df$taxon_id,
             pch = 20,
             col = temp_df$taxon_id)
      axis_geo(side = 1, intervals = "periods")
      title(xlab = "Time (Ma)", line = 4)
    }

    # Return dataframe
    return(temp_df)
  }
