#' Calculate the range of taxa
#'
#' A function to calculate various types of ranges for taxa. The function can
#' calculate either temporal, latitudinal, or the spatial range of taxa.
#'
#' @param occdf \code{dataframe}.
#' @param method \code{character}.
#' @param plot \code{character}.
#'
#' @return
#'
#' @details
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
      stop("The 'temporal' approach in `method` requires the following columns:
         'name', 'min_ma', 'max_ma'")
    }
    if (!is.numeric(occdf$max_ma) || !is.numeric(occdf$min_ma)) {
      stop(
        "'max_ma' and 'min_ma' columns in `occdf` must be of class numeric.")
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
               y0 = seq_len(nrow(temp_df)))
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
                         max_lat = rep(NA, length(unique_taxa)),
                         min_lat = rep(NA, length(unique_taxa)),
                         range_lat = rep(NA, length(unique_taxa)))
    # Run for loop across unique taxa
    for(i in seq_along(unique_taxa)){
      vec <- which(occdf$name == unique_taxa[i])
      lat_df$max_lat[i] <- max(occdf$p_lat[vec])
      lat_df$min_lat[i] <- min(occdf$p_lat[vec])
      lat_df$range_lat[i] <- lat_df$max_lat[i] - lat_df$min_lat[i]
    }
    # Remove row names
    row.names(lat_df) <- NULL
    # Add taxa id

    #round off values
    lat_df[, c("max_lat", "min_lat", "range_lat")] <- round(
      x = lat_df[, c("max_lat", "min_lat", "range_lat")], digits = 3)
    if (plot == TRUE){
      y_range <- c(min(lat_df$min_lat), max(lat_df$max_lat))
      x_range <- c(0, nrow(lat_df))
      plot(x = NA,
           y = NA,
           xlim = x_range,
           ylim = y_range,
           axes = TRUE,
           ylab = "Latitude (\u00B0)",
           xlab = "Taxa ID",
           main = "Latitudinal range of taxa")
      segments(y0 = lat_df$max_lat,
               y1 = lat_df$min_lat,
               x0 = seq_len(nrow(lat_df)))
    }
    # Return dataframe
    return(lat_df)
  }
}
