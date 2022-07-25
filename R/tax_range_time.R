#' Calculate the temporal range of fossil taxa
#'
#' A function to calculate the temporal range of fossil taxa.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences. The
#' dataframe should contain the following named columns: "name"
#' (e.g., species name), "max_ma", and "min_ma". The columns "max_ma" and
#' "min_ma" should be of class \code{numeric} and provide the age range of the
#' fossil occurrence in millions of years before present. However, if they are
#' provided as \code{character} (i.e. stage names), the function will try to
#' extract numeric age data from the Geological Timescale by linking stage
#' names.
#' @param scale \code{character}. Specify the desired geological timescale to
#' be used "GTS2020" or "GTS2012". This argument is only useful if the supplied
#' "max_ma" and "min_ma" columns are of class \code{character}.
#' "GTS2020" is the default.
#' @param plot \code{logical}. Should a plot of the ranges be generated?
#'
#' @return A \code{dataframe} containing the following columns:
#' unique taxa (`name`), taxa ID (`taxa_id`), first appearnce of taxa
#' (`FAD_ma`), last appearance of taxa (`LAD_ma`), and range duration
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
#' # Add name column
#' occdf$name <- occdf$accepted_name
#' # Temporal range
#' tax_range_time(occdf = occdf, plot = TRUE)
#'
#' @export
tax_range_time <- function(occdf, scale = "GTS2020", plot = FALSE) {

  #=== Handling errors ===
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe.")
  }

  if (is.logical(plot) == FALSE) {
    stop("`plot` should be logical (TRUE/FALSE).")
  }

  if (class(occdf$max_ma) != class(occdf$min_ma)) {
    stop("The class of max_ma and min_ma should be the same.")
  }

  # Method specific error handling
  if (sum(c("name", "min_ma", "max_ma") %in% colnames(occdf)) != 3) {
    stop("The 'temporal' approach in `method` requires the columns:
         'name', 'min_ma', 'max_ma'")
    }

  #=== Set-up ===
  unique_taxa <- unique(occdf$name)
  # Order taxa
  unique_taxa <- unique_taxa[order(unique_taxa)]

  # Character string entered
  if (is.character(occdf$max_ma) && is.character(occdf$min_ma)) {
    # Which GTS should be used?
    if (scale == "GTS2020") {
      df <- palaeoverse::GTS2020
    }
    if (scale == "GTS2012") {
      df <- palaeoverse::GTS2012
    }
    df <- subset(df, rank == "stage")
    # Which intervals are present?
    unique_intervals <- unique(c(occdf$max_ma, occdf$min_ma))
    # Which of these intervals are present in the GTS
    vec <- which(df$interval_name %in% unique_intervals)
    # If not available, stop and return error
    if (length(vec) != length(unique_intervals)) {
      stop(
        paste0(
          "Check spelling of specified max_ma and min_ma intervals.
  Available interval names are accessible via GTS2020 and GTS2012."
        )
      )
    }
    # Convert character to numeric ages
    for (i in seq_along(df$interval_name)) {
      occdf[which(occdf$max_ma == df$interval_name[i]), "max_ma"] <-
        df$max_ma[i]
      occdf[which(occdf$min_ma == df$interval_name[i]), "min_ma"] <-
        df$min_ma[i]
    }
    occdf$max_ma <- as.numeric(occdf$max_ma)
    occdf$min_ma <- as.numeric(occdf$min_ma)
  }
  #=== Temporal range ===
  # Generate dataframe for population
  temp_df <- data.frame(name = unique_taxa,
                          taxa_id = seq(1, length(unique_taxa), 1),
                          FAD_ma = rep(NA, length(unique_taxa)),
                          LAD_ma = rep(NA, length(unique_taxa)),
                          range_myr = rep(NA, length(unique_taxa)))
    # Run for loop across unique taxa
    for(i in seq_along(unique_taxa)){
      vec <- which(occdf$name == unique_taxa[i])
      temp_df$FAD_ma[i] <- max(occdf$max_ma[vec])
      temp_df$LAD_ma[i] <- min(occdf$min_ma[vec])
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
