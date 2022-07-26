#' Calculate the geographic range of fossil taxa
#'
#' A function to calculate the geographic range of fossil taxa. The function
#' can calculate geographic range in four ways: convex hull, latitudinal range,
#' maximum great circle distance, and the number of occupied equal-area
#' hexagonal grid cells.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences.
#' The dataframe should contain the following named columns: "name"
#' (e.g., species name), "p_lng", and "p_lat" (i.e., palaeolongitude and
#' palaeolatitude). Only "name" and "p_lat" are required for the "lat" method.
#' @param method \code{character}. How should geographic range be calculated
#' for each taxa in `occdf`? Four options exist in this function:
#' "con", "lat", "gcd", and "occ". See Details for a description of each.
#' @param spacing \code{numeric}. The desired spacing (in km) between the
#' center of adjacent grid cells. Only useful if the method argument is set to
#' "occ".
#' @param plot \code{logical}. Should a plot of the ranges be generated?
#'
#' @return A \code{dataframe} with method specific columns:
#' - For the "con" method, a \code{dataframe} with unique taxa (`name`),
#' taxa ID (`taxa_id`), convex hull coordinates (`p_lng` & `p_lat`), and area
#' (`area`) in km^2 is returned.
#' - For the "lat" method, a \code{dataframe} with unique taxa (`name`),
#' taxa ID (`taxa_id`), maximum palaeolatitude of occurrence (`max_p_lat`),
#' minimum palaeolatitude of occurrence (`min_p_lat`), and palaeolatitudinal
#' range (`range_p_lat`) is returned.
#' - For the "gcd" method, a \code{dataframe} with unique taxa (`name`), taxa
#' ID (`taxa_id`), coordinates of the two most distant points
#' (`p_lng` & `p_lat`), and the 'Great Circle Distance' between these points in
#' km is returned.
#' - For the "occ" method, a \code{dataframe} with unique taxa (`name`), taxa
#' ID (`taxa_id`), the number of occupied cells (`cells`), and the spacing
#' between cells (`spacing`) is returned.
#'
#' @details Four approaches (methods) exist in the `tax_range_geo` function for
#' calculating ranges:
#' - Convex hull: the "con" method calculates the geographic range of a taxa
#' using a convex hull for each taxa in `occdf`, and calculates the area of
#' the convex hull (in km^2) using
#' \code{\link[geosphere:areaPolygon]{geosphere::areaPolygon()}}.
#' - Latitudinal: the "lat" method calculates the palaeolatitudinal
#' range of a taxa. It does so for each taxa in `occdf` by finding their most
#' northern and southern occurrence. The palaeolatitudinal range of each taxa
#' is also calculated.
#' - Maximum Great Circle Distance: the "gcd" method calculates the maximum
#' Great Circle Distance between occurrences for each taxa in `occdf`. It does
#' so using \code{\link[fields:rdist.earth]{fields::rdist.earth()}}. This
#' function calculates Great Circle Distance using the Haversine method with
#' the radius of the Earth set to the 6378.388 km (equatorial radius).
#' - Occupied cells: the "occ" method calculates the number of occupied
#' grid cells. It does so by creating an equal-area hexagonal grid using the
#' \code{\link[dggridR:dgconstruct]{dggridR::dgconstruct()}} function. The size
#' of the cells are defined by the spacing between cells (the distance between
#' the center of adjacent cells).
#'
#' @section Developer(s):
#' Lewis A. Jones, Bethany Allen, & Christopher D. Dean
#' @section Reviewer(s):
#' To be reviewed
#' @importFrom geosphere areaPolygon
#' @importFrom grDevices chull rgb
#' @importFrom graphics points
#' @importFrom fields rdist.earth
#' @importFrom dggridR dgconstruct dgGEO_to_SEQNUM dgcellstogrid
#' @importFrom terra plot
#' @examples
#' # Grab internal data and set-up
#' occdf <- tetrapods
#' occdf$p_lng <- tetrapods$lng
#' occdf$p_lat <- tetrapods$lat
#' occdf$name <- occdf$accepted_name
#' # Convex hull
#' tax_range_geo(occdf = occdf, method = "con", plot = TRUE)
#' # Latitudinal range
#' tax_range_geo(occdf = occdf, method = "lat", plot = TRUE)
#' # Great Circle Distance
#' tax_range_geo(occdf = occdf, method = "gcd", plot = FALSE)
#' # Occupied grid cells
#' tax_range_geo(occdf = occdf, method = "occ", plot = FALSE)
#' @export
tax_range_geo <- function(occdf, method = "lat", spacing = 1000, plot = FALSE) {

  #=== Handling errors ===
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe.")
  }

  if (is.character(method) == FALSE) {
    stop("`method` should be of character class")
  }

  if (is.logical(plot) == FALSE) {
    stop("`plot` should be logical (TRUE/FALSE)")
  }

  # Possible methods specified?
  possible_methods <- c("con", "lat", "gcd", "occ")
  method_match <- charmatch(method, possible_methods)

  if (is.na(method_match) == TRUE) {
    # If the user has entered a non-valid term for the "method" argument,
    # generate an error and warn the user.
    stop("Invalid `method`. Choose either:
  'con', 'lat', 'gcd', and 'occ'.")
  } else {
    method <- possible_methods[method_match]
  }

  # Method specific error handling
  if (method == "con" | method == "gcd" | method == "occ") {
    if (sum(c("name", "p_lng", "p_lat") %in% colnames(occdf)) != 3) {
      stop("The 'con', 'gcd', and 'occ' method requires the columns:
         'name', 'p_lng', 'p_lat'")
    }
    if (!is.numeric(occdf$p_lat)) {
      stop(
        "'p_lng' and 'p_lat' in `occdf` must be of class numeric.")
    }
    if (any(is.na(occdf$p_lng)) || any(is.na(occdf$p_lat))) {
      stop(
        "NA values present in 'p_lng' and 'p_lat'."
      )
    }
  }

  if (method == "lat") {
    if (sum(c("name", "p_lat") %in% colnames(occdf)) != 2) {
      stop("The 'lat' method requires the columns:
         'name', 'p_lat'")
    }
    if (!is.numeric(occdf$p_lat)) {
      stop(
        "'p_lat' in `occdf` must be of class numeric.")
    }
    if (any(is.na(occdf$p_lat))) {
      stop(
        "NA values present in 'p_lat'."
      )
    }
  }

  #=== Set-up ===
  unique_taxa <- unique(occdf$name)
  # Order taxa
  unique_taxa <- unique_taxa[order(unique_taxa)]

  #=== convex hull  ===
  if (method == "con") {
    # Generate dataframe for population
    spat_df <- data.frame()
    # Run for loop across unique taxa
    for (i in seq_along(unique_taxa)) {
      name <- unique_taxa[i]
      taxa_id <- i
      # Subset taxa
      tmp <- occdf[which(occdf$name == unique_taxa[i]), ]
      # Calculate convex hull
      tmp <- tmp[chull(x = tmp$p_lng, y = tmp$p_lat),
                 c("p_lng", "p_lat")]
      # Calculate area of convex hull and convert to km^2
      area <- geosphere::areaPolygon(tmp) / 1e+6
      tmp <- cbind.data.frame(name, taxa_id, tmp, area)
      spat_df <- rbind.data.frame(spat_df, tmp)
    }
    # Plot data?
    if (plot == TRUE) {
      y_range <- c(min(spat_df$p_lat), max(spat_df$p_lat))
      x_range <- c(min(spat_df$p_lng), max(spat_df$p_lng))
      plot(x = NA,
           y = NA,
           xlim = x_range,
           ylim = y_range,
           axes = TRUE,
           ylab = "Palaeolatitude (\u00B0)",
           xlab = "Palaeolongitude (\u00B0)",
           main = "Palaeogeographic range of taxa")
      for (i in seq_along(unique_taxa)) {
        tmp <- spat_df[which(spat_df$name == unique_taxa[i]), ]
        polygon(x = tmp$p_lng, y = tmp$p_lat,
                col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
        points(tmp[,c("p_lng", "p_lat")], pch = 20)
      }
    }
    # Remove row names
    row.names(spat_df) <- NULL
    return(spat_df)
  }

  #=== Latitudinal range ===
  if (method == "lat") {
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

    # Round off values
    lat_df[, c("max_p_lat", "min_p_lat", "range_p_lat")] <- round(
      x = lat_df[, c("max_p_lat", "min_p_lat", "range_p_lat")], digits = 3)

    # Plot data?
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

  #=== Great Circle Distance  ===
  if (method == "gcd") {
    # Generate dataframe for population
    gcd_df <- data.frame()
    # Run for loop across unique taxa
    for(i in seq_along(unique_taxa)){
      # Unique taxa name
      name <- unique_taxa[i]
      # Taxa id
      taxa_id <- i
      # Subset df
      tmp <- subset(occdf, name == unique_taxa[i])
      # Calculate GCD matrix using the Haversine method with a radius of
      # 6378.388 km by default
      vals <- fields::rdist.earth(x1 = tmp[,c("p_lng", "p_lat")],
                          miles = FALSE)
      # Extract location of points with max GCD
      loc <- which(vals == max(vals), arr.ind = TRUE)
      # Get maximum GCD in km
      GCD <- as.numeric(max(vals))
      # Extract coordinates of points
      coords <- data.frame(tmp[loc[1, 1:2], c("p_lng", "p_lat")])
      # Build dataframe
      tmp <- cbind.data.frame(name, taxa_id, coords, GCD)
      gcd_df <- rbind.data.frame(gcd_df, tmp)
    }

    # Remove row names
    row.names(gcd_df) <- NULL

    # Round off values
    gcd_df[, c("p_lng", "p_lat", "GCD")] <- round(
      x = gcd_df[, c("p_lng", "p_lat", "GCD")], digits = 3)

    # Plot data?
    if (plot == TRUE) {
      y_range <- c(min(gcd_df$p_lat), max(gcd_df$p_lat))
      x_range <- c(min(gcd_df$p_lng), max(gcd_df$p_lng))
      plot(x = NA,
           y = NA,
           xlim = x_range,
           ylim = y_range,
           axes = TRUE,
           ylab = "Palaeolatitude (\u00B0)",
           xlab = "Palaeolongitude (\u00B0)",
           main = "Maximum Great Circle Distance")
      for (i in seq_along(unique_taxa)) {
        tmp <- gcd_df[which(gcd_df$name == unique_taxa[i]), ]
        segments(y0 = tmp$p_lat[1],
                 y1 = tmp$p_lat[2],
                 x0 = tmp$p_lng[1],
                 x1 = tmp$p_lng[2],
                 col = tmp$taxa_id)
        points(x = tmp$p_lng,
               y = tmp$p_lat,
               pch = 20,
               col = tmp$taxa_id)
      }
    }
    # Return dataframe
    return(gcd_df)
  }
  #=== Occupied grid cells  ===
  if (method == "occ") {
    # Generate dataframe for population
    oc_df <- data.frame(name = unique_taxa,
                         taxa_id = seq(1, length(unique_taxa), 1),
                         cells = rep(NA, length(unique_taxa)),
                         spacing = rep(spacing, length(unique_taxa)))
    # Generate equal area hexagonal grid
    dggs <- dggridR::dgconstruct(spacing = spacing,
                                 metric = TRUE,
                                 resround = "nearest")
    # Run for loop over all unique taxa
    for (i in seq_along(unique_taxa)) {
      # Subset df
      tmp <- subset(occdf, name == unique_taxa[i])
      # Extract cells
      cells <- dggridR::dgGEO_to_SEQNUM(dggs = dggs,
                                   in_lon_deg = tmp$p_lng,
                                   in_lat_deg = tmp$p_lat)$seqnum
      # Calculate number of unique cells occupied
      oc_df$cells[i] <- length(unique(cells))
    }

    if (plot == TRUE) {
      # Get occupied cells for all occurrences
      cells <- dggridR::dgGEO_to_SEQNUM(dggs = dggs,
                                        in_lon_deg = occdf$p_lng,
                                        in_lat_deg = occdf$p_lat)$seqnum

      # Get grid
      grid <- dggridR::dgcellstogrid(dggs = dggs,
                                     cells =cells,
                                     frame = FALSE)

      # Plot data
      terra::plot(grid,
           ylab = "Palaeolatitude (\u00B0)",
           xlab = "Palaeolongitude (\u00B0)",
           main = "Occuppied cells (equal-area hexagonal grid)",
           axes = TRUE,
           col = "blue")
    }
    # Return data
    return(oc_df)
  }
}
