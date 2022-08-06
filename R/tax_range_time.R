#' Calculate the temporal range of fossil taxa
#'
#' A function to calculate the temporal range of fossil taxa.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences. The
#' dataframe should contain at least three columns: names of taxa (`character`)
#' and the maximum (`numeric`) and minimum (`numeric`) age which constrain the
#' age range (in millions of years) of the fossil occurrence. If the age range
#' columns are provided as (`character`) values (i.e., stage names), the
#' function will attempt to extract numeric age data from the
#' Geological Timescale by linking stage names.
#' @param name \code{character}. The name of the column you wish to be treated
#' as the input names (e.g., "species" or "genus").
#' @param min_ma \code{character}. The name of the column you wish to be treated
#' as the input minimum age range (e.g., "min_ma").
#' @param max_ma \code{character}. The name of the column you wish to be treated
#' as the input maximum age range (e.g., "max_ma").
#' @param scale \code{character}. Specify the desired geological timescale to
#' be used "GTS2020" or "GTS2012". This argument is only useful if the supplied
#' "max_ma" and "min_ma" columns are of class \code{character}.
#' "GTS2020" is the default.
#' @param plot \code{logical}. Should a plot of the ranges be generated?
#' @param return_error \code{logical}. Should a vector of numbers be returned
#' to flag the rows of the `occdf` that cannot be matched to `character`
#' interval names? This is only relevant if `max_ma` and `min_ma`
#' are `character` values.
#'
#' @return A \code{dataframe} containing the following columns:
#' unique taxa (`taxa`), taxa ID (`taxa_id`), first appearnce of taxa
#' (`FAD_ma`), last appearance of taxa (`LAD_ma`), and temporal range duration
#' (`range_myr`) is returned.
#'
#' @details The temporal range(s) of taxa are calculated by extracting all
#' unique taxa (`name` column) from the input `occdf`, and checking their first
#' and last appearance. The temporal duration of each taxa is also calculated.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' To be reviewed
#' @importFrom graphics points
#' @examples
#' # Grab internal data
#' occdf <- tetrapods
#' # Temporal range
#' tax_range_time(occdf = occdf, name = "accepted_name", plot = TRUE)
#'
#' @export
tax_range_time <- function(occdf,
                           name = "name",
                           min_ma = "min_ma",
                           max_ma = "max_ma",
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
    stop("The class of max_ma and min_ma must be the same")
  }

  if (any(c(name, min_ma, max_ma) %in% colnames(occdf) == FALSE)) {
    stop("Either `name`, `min_ma`, or `max_ma`, do not exist in `occdf`")
  }

  if (any(is.na(occdf[, name]))) {
    stop("The `name` column contains NA values")
  }

  if (any(is.na(occdf[, min_ma])) || any(is.na(occdf[, max_ma]))) {
    stop("`min_ma` and/or `max_ma` columns contain NA values")
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
                          taxa_id = seq(1, length(unique_taxa), 1),
                          FAD_ma = rep(NA, length(unique_taxa)),
                          LAD_ma = rep(NA, length(unique_taxa)),
                          range_myr = rep(NA, length(unique_taxa)))
    # Run for loop across unique taxa
    for(i in seq_along(unique_taxa)){
      vec <- which(occdf[, name] == unique_taxa[i])
      temp_df$FAD_ma[i] <- max(occdf[vec, max_ma])
      temp_df$LAD_ma[i] <- min(occdf[vec, min_ma])
      temp_df$range_myr[i] <- temp_df$FAD_ma[i] - temp_df$LAD_ma[i]
    }
    # Remove row names
    row.names(temp_df) <- NULL
    # Round off values
    temp_df[, c("FAD_ma", "LAD_ma", "range_myr")] <- round(
      x = temp_df[, c("FAD_ma", "LAD_ma", "range_myr")], digits = 3)
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
               y0 = temp_df$taxa_id,
               col = temp_df$taxa_id)
      points(x = temp_df$FAD_ma,
             y = temp_df$taxa_id,
             pch = 20,
             col = temp_df$taxa_id)
      points(x = temp_df$LAD_ma,
             y = temp_df$taxa_id,
             pch = 20,
             col = temp_df$taxa_id)
      axis_geo(side = 1, intervals = "periods")
      title(xlab = "Time (Ma)", line = 4)
    }

    # Return dataframe
    return(temp_df)
  }
