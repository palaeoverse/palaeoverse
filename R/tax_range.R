#' Calculate the range of fossil taxa
#'
#' A function to calculate various types of ranges for fossil taxa. The
#' function can calculate either temporal, (palaeo-)latitudinal, or the spatial
#' range of taxa.
#'
#' @param occdf \code{dataframe}. A dataframe of the fossil occurrences you
#' wish to calculate range metrics for. For the "temporal" method, the
#' dataframe should contain at least the following columns: "name"
#' (e.g., species name), "max_ma", and "min_ma". For the "latitudinal" method,
#' the dataframe should contain at least the following columns: "name" and
#' "p_lat" (i.e., palaeolatitude). For the "spatial" method, the dataframe
#' should contain at least the following columns: "name", "p_lng" and "p_lat".
#' @param method \code{character}. The type of ranges you wish to calculate
#' for each unique taxa in the occurrence dataframe. Three options exist in
#' this function: "temporal", "latitudinal", and "spatial". See Details for a
#' description of each.
#' @param plot \code{logical}. Should a plot of the ranges be generated?
#'
#' @return A \code{dataframe} with method specific columns. For the "temporal"
#' method, a \code{dataframe} with unique taxa (`name`), taxa ID (`taxa_id`),
#' maximum age of occurrence (`max_ma`), minimum age of occurrence
#' (`min_ma`) and range duration (`range_myr`) is returned. For the
#' "latitudinal" method, a \code{dataframe} with unique taxa (`name`),
#' taxa ID (`taxa_id`), maximum palaeolatitude of occurrence (`max_p_lat`),
#' minimum palaeolatitude of occurrence (`min_p_lat`) and palaeolatitudinal
#' range (`range_p_lat`) is returned.
#'
#' @details Three approaches (methods) exist in the `tax_range` function for
#' calculating ranges:
#' - Temporal: the "temporal" method calculates the temporal range of a taxa.
#' It does so by extracting all unique taxa from the input `occdf`, and
#' checking their first and last appearance. Subsequently, the temporal range
#' of each taxa is calculated.
#' - Latitudinal: the "latitudinal" method calculates the (palaeo-)latitudinal
#' range of a taxa. It does so by extracting all unique taxa from the input
#' `occdf` and checking their most notherly and southerly occurrence.
#' Subsequently, the latitudinal range of each taxa is calculated.
#' - Spatial:
#'
#' @section Developer(s):
#' Lewis A. Jones, Bethany Allen & Christopher D. Dean
#' @section Reviewer(s):
#' To be reviewed
#'
#' @examples
#' # Grab internal data
#' occdf <- tetrapods
#' # Add name column
#' occdf$name <- occdf$accepted_name
#' # Temporal range
#' tax_range(occdf = occdf, method = "temporal", plot = TRUE)
#' # Latitudinal range
#' occdf$p_lat <- occdf$lat
#' tax_range(occdf = occdf, method = "latitudinal", plot = TRUE)
#'
#' @export
tax_range <- function(occdf, method = "temporal", plot = FALSE) {

  #=== Handling errors ===
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe.")
  }

  if (is.character(method) == FALSE) {
    stop("`method` should be of character class")
  }

  possible_methods <- c("temporal", "latitudinal", "spatial")
  method_match <- charmatch(method, possible_methods)

  if (is.na(method_match) == TRUE) {
    # If the user has entered a non-valid term for the "method" argument,
    # generate an error and warn the user.
    stop("Invalid `method`. Choose either:
  'temporal', 'latitudinal', or 'spatial'.")
  } else {
    method <- possible_methods[method_match]
  }

  if (method == "temporal") {
    if (sum(c("name", "min_ma", "max_ma") %in% colnames(occdf)) != 3) {
      stop("The 'temporal' approach in `method` requires the columns:
         'name', 'min_ma', 'max_ma'")
    }
    if (!is.numeric(occdf$max_ma) || !is.numeric(occdf$min_ma)) {
      stop(
        "'max_ma' and 'min_ma' columns in `occdf` must be of class numeric.")
    }
  }

  if (method == "latitudinal") {
    if (sum(c("name", "p_lat") %in% colnames(occdf)) != 2) {
      stop("The 'latitudinal' approach in `method` requires the columns:
         'name', 'p_lat'")
    }
    if (!is.numeric(occdf$p_lat)) {
      stop(
        "'p_lat' in `occdf` must be of class numeric.")
    }
  }


  #=== Set-up ===
  unique_taxa <- unique(occdf$name)
  # Order taxa
  unique_taxa <- unique_taxa[order(unique_taxa)]

  #=== Temporal range ===
  if (method == "temporal") {
    # Generate dataframe for population
    temp_df <- data.frame(name = unique_taxa,
                          taxa_id = seq(1, length(unique_taxa), 1),
                          max_ma = rep(NA, length(unique_taxa)),
                          min_ma = rep(NA, length(unique_taxa)),
                          range_myr = rep(NA, length(unique_taxa)))
    # Run for loop across unique taxa
    for(i in seq_along(unique_taxa)){
      vec <- which(occdf$name == unique_taxa[i])
      temp_df$max_ma[i] <- max(occdf$max_ma[vec])
      temp_df$min_ma[i] <- min(occdf$min_ma[vec])
      temp_df$range_myr[i] <- temp_df$max_ma[i] - temp_df$min_ma[i]
    }
    # Remove row names
    row.names(temp_df) <- NULL
    #round off values
    temp_df[, c("max_ma", "min_ma", "range_myr")] <- round(
      x = temp_df[, c("max_ma", "min_ma", "range_myr")], digits = 3)
    # plot data?
    if (plot == TRUE){
      x_range <- c(max(temp_df$max_ma), min(temp_df$min_ma))
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
      segments(x0 = temp_df$max_ma,
               x1 = temp_df$min_ma,
               y0 = temp_df$taxa_id,
               col = temp_df$taxa_id)
      points(x = temp_df$max_ma,
             y = temp_df$taxa_id,
             pch = 20,
             col = temp_df$taxa_id)
      points(x = temp_df$min_ma,
             y = temp_df$taxa_id,
             pch = 20,
             col = temp_df$taxa_id)
      axis_geo(side = 1, intervals = "periods")
      title(xlab = "Time (Ma)", line = 4)
    }

    # Return dataframe
    return(temp_df)
  }

  #=== Latitudinal range ===
  if (method == "latitudinal") {
    # Generate dataframe for population
    lat_df <- data.frame(name = unique_taxa,
                         taxa_id = seq(1, length(unique_taxa), 1),
                         max_p_lat = rep(NA, length(unique_taxa)),
                         min_p_lat = rep(NA, length(unique_taxa)),
                         range_p_lat = rep(NA, length(unique_taxa)))
    # Run for loop across unique taxa
    for(i in seq_along(unique_taxa)){
      vec <- which(occdf$name == unique_taxa[i])
      lat_df$max_p_lat[i] <- max(occdf$p_lat[vec])
      lat_df$min_p_lat[i] <- min(occdf$p_lat[vec])
      lat_df$range_p_lat[i] <- lat_df$max_p_lat[i] - lat_df$min_p_lat[i]
    }
    # Remove row names
    row.names(lat_df) <- NULL
    # Add taxa id

    #round off values
    lat_df[, c("max_p_lat", "min_p_lat", "range_p_lat")] <- round(
      x = lat_df[, c("max_p_lat", "min_p_lat", "range_p_lat")], digits = 3)
    if (plot == TRUE){
      y_range <- c(min(lat_df$min_p_lat), max(lat_df$max_p_lat))
      x_range <- c(0, nrow(lat_df))
      plot(x = NA,
           y = NA,
           xlim = x_range,
           ylim = y_range,
           axes = TRUE,
           ylab = "Palaeolatitude (\u00B0)",
           xlab = "Taxa ID",
           main = "Palaeolatitudinal range of taxa")
      segments(y0 = lat_df$max_p_lat,
               y1 = lat_df$min_p_lat,
               x0 = lat_df$taxa_id,
               col = lat_df$taxa_id)
      points(y = lat_df$max_p_lat,
             x = lat_df$taxa_id,
             pch = 20,
             col = lat_df$taxa_id)
      points(y = lat_df$min_p_lat,
             x = lat_df$taxa_id,
             pch = 20,
             col = lat_df$taxa_id)
    }
    # Return dataframe
    return(lat_df)
  }
}
