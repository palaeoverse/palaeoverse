#' Calculate the geographic range of fossil taxa
#'
#' A function to calculate the geographic range of fossil taxa. The function
#' can calculate geographic range in four ways: convex hull, latitudinal range,
#' maximum great circle distance, and the number of occupied equal-area
#' hexagonal grid cells.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences.
#' This dataframe should contain at least three columns: names of taxa
#' (`character`), longitude (`numeric`) and latitude (`numeric`).
#' @param name \code{character}. The name of the column you wish to be treated
#' as the input names (e.g., "species" or "genus").
#' @param lng \code{character}. The name of the column you wish to be treated
#' as the input longitude (e.g., "lng" or "p_lng").
#' @param lat \code{character}. The name of the column you wish to be treated
#' as the input latitude (e.g., "lat" or "p_lat").
#' @param method \code{character}. How should geographic range be calculated
#' for each taxa in `occdf`? Four options exist in this function:
#' "con", "lat", "gcd", and "occ". See Details for a description of each.
#' @param spacing \code{numeric}. The desired spacing (in km) between the
#' center of adjacent grid cells. Only useful if the `method` argument is set
#' to "occ". The default is 100.
#'
#' @return A \code{dataframe} with method specific columns:
#' - For the "con" method, a \code{dataframe} with unique taxa (`taxa`),
#' taxa ID (`taxa_id`), convex hull coordinates (`lng` & `lat`), and area
#' (`area`) in km\ifelse{html}{\out{<sup>2</sup>}}{\eqn{^2}} is returned.
#' - For the "lat" method, a \code{dataframe} with unique taxa (`taxa`),
#' taxa ID (`taxa_id`), maximum latitude of occurrence (`max_lat`),
#' minimum latitude of occurrence (`min_lat`), and latitudinal
#' range (`range_lat`) is returned.
#' - For the "gcd" method, a \code{dataframe} with unique taxa (`taxa`), taxa
#' ID (`taxa_id`), coordinates of the two most distant points
#' (`lng` & `lat`), and the 'Great Circle Distance' (`GCD`) between these
#' points in km is returned.
#' - For the "occ" method, a \code{dataframe} with unique taxa (`taxa`), taxa
#' ID (`taxa_id`), the number of occupied cells (`cells`), and the spacing
#' between cells (`spacing`) in km is returned.
#'
#' @details Four approaches (methods) exist in the `tax_range_geo` function for
#' calculating ranges:
#' - Convex hull: the "con" method calculates the geographic range of a taxa
#' using a convex hull for each taxa in `occdf`, and calculates the area of
#' the convex hull (in km\ifelse{html}{\out{<sup>2</sup>}}{\eqn{^2}}) using
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
#' grid cells. It does so using discrete hexagonal grids via the
#' \code{\link[h3jsr]{h3jsr}} package. This package relies on Uber's H3 library,
#' a geospatial indexing system that partitions the world into hexagonal cells:
#' \url{https://h3geo.org/docs}. In H3, 16 different resolutions are available:
#' \url{https://h3geo.org/docs/core-library/restable}. In the implementation of
#' the `tax_range_geo()` function, the resolution is defined by the user-input
#' `spacing` which represents the distance between the centroid of adjacent
#' cells. Using this distance, the function identifies which resolution is most
#' similar to the input `spacing`, and uses this resolution.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' To be reviewed
#' @importFrom geosphere areaPolygon
#' @importFrom grDevices chull
#' @importFrom fields rdist.earth
#' @importFrom h3jsr point_to_h3
#' @examples
#' # Grab internal data and set-up
#' occdf <- tetrapods
#' # Convex hull
#' tax_range_geo(occdf = occdf, name = "accepted_name", method = "con")
#' # Latitudinal range
#' tax_range_geo(occdf = occdf, name = "accepted_name", method = "lat")
#' # Great Circle Distance
#' tax_range_geo(occdf = occdf, name = "accepted_name", method = "gcd")
#' # Occupied grid cells
#' tax_range_geo(occdf = occdf, name = "accepted_name", method = "occ")
#' @export
tax_range_geo <- function(occdf,
                          name = "name",
                          lng = "lng",
                          lat = "lat",
                          method = "lat",
                          spacing = 100) {

  #=== Handling errors ===
  if (!is.data.frame(occdf)) {
    stop("`occdf` should be a dataframe")
  }

  if (any(c(name, lng, lat) %in% colnames(occdf) == FALSE)) {
    stop("Either `name`, `lng`, or `lat`, do not exist in `occdf`")
  }

  if (!is.character(method)) {
    stop("`method` is not of character class")
  }

  if (!is.numeric(occdf[,lat]) || !is.numeric(occdf[,lng])) {
    stop("`lng` and/or `lat` columns are not of numeric class")
  }

  if (any(is.na(occdf[,name]))) {
    stop("The `name` column contains NA values")
  }

  if (any(is.na(occdf[,lat])) || any(is.na(occdf[,lng]))) {
    stop("`lng` and/or `lat` columns contain NA values")
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

  #=== Set-up ===
  unique_taxa <- unique(occdf[,name])
  # Order taxa
  unique_taxa <- unique_taxa[order(unique_taxa)]

  #=== convex hull  ===
  if (method == "con") {
    # Generate dataframe for population
    spat_df <- data.frame()
    # Run for loop across unique taxa
    for (i in seq_along(unique_taxa)) {
      taxa <- unique_taxa[i]
      taxa_id <- i
      # Subset taxa
      tmp <- occdf[which(occdf[, name] == unique_taxa[i]), ]
      # Calculate convex hull
      tmp <- tmp[chull(x = tmp[,lng], y = tmp[,lat]), c(lng, lat)]
      # Calculate area of convex hull and convert to km^2
      area <- geosphere::areaPolygon(tmp) / 1e+6
      # Round to three decimal places
      area <- round(area, digits = 3)
      tmp <- cbind.data.frame(taxa, taxa_id, tmp, area)
      spat_df <- rbind.data.frame(spat_df, tmp)
    }
    # Remove row names
    row.names(spat_df) <- NULL
    return(spat_df)
  }

  #=== Latitudinal range ===
  if (method == "lat") {
    # Generate dataframe for population
    lat_df <- data.frame(taxa = unique_taxa,
                         taxa_id = seq(1, length(unique_taxa), 1),
                         max_lat = rep(NA, length(unique_taxa)),
                         min_lat = rep(NA, length(unique_taxa)),
                         range_lat = rep(NA, length(unique_taxa)))
    # Run for loop across unique taxa
    for(i in seq_along(unique_taxa)){
      vec <- which(occdf[, name] == unique_taxa[i])
      lat_df$max_lat[i] <- max(occdf[vec, lat])
      lat_df$min_lat[i] <- min(occdf[vec, lat])
      lat_df$range_lat[i] <- lat_df$max_lat[i] - lat_df$min_lat[i]
    }
    # Remove row names
    row.names(lat_df) <- NULL

    # Round off values
    lat_df[, c("max_lat", "min_lat", "range_lat")] <- round(
      x = lat_df[, c("max_lat", "min_lat", "range_lat")], digits = 3)

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
      taxa <- unique_taxa[i]
      # Taxa id
      taxa_id <- i
      # Subset df
      tmp <- occdf[which(occdf[, name] == unique_taxa[i]), ]
      # Calculate GCD matrix using the Haversine method with a radius of
      # 6378.388 km by default
      vals <- fields::rdist.earth(x1 = tmp[,c(lng, lat)], miles = FALSE)
      # Extract location of points with max GCD
      loc <- which(vals == max(vals), arr.ind = TRUE)
      # Get maximum GCD in km
      GCD <- as.numeric(max(vals))
      # Extract coordinates of points
      coords <- data.frame(tmp[loc[1, 1:2], c(lng, lat)])
      # Build dataframe
      tmp <- cbind.data.frame(taxa, taxa_id, coords, GCD)
      gcd_df <- rbind.data.frame(gcd_df, tmp)
    }

    # Remove row names
    row.names(gcd_df) <- NULL

    # Round off values
    gcd_df[, c(lng, lat, "GCD")] <- round(
      x = gcd_df[, c(lng, lat, "GCD")], digits = 3)

    # Return dataframe
    return(gcd_df)
  }
  #=== Occupied grid cells  ===
  if (method == "occ") {
    # Generate dataframe for population
    oc_df <- data.frame(taxa = unique_taxa,
                         taxa_id = seq(1, length(unique_taxa), 1),
                         cells = rep(NA, length(unique_taxa)),
                         spacing = rep(NA, length(unique_taxa)))
    # Generate equal area hexagonal grid
    # Which resolution should be used based on input distance/spacing?
    # Use the h3jsr::h3_info_table to calculate resolution (however, this
    # table is not exported in their package, added into palaeoverse)
    grid <- h3_info_table[
      which.min(abs(h3_info_table$avg_cendist_km - spacing)), ]
    # Add resolution spacing
    oc_df$spacing <- grid$avg_cendist_km
    # Run for loop over all unique taxa
    for (i in seq_along(unique_taxa)) {
      # Subset df
      tmp <- occdf[which(occdf[, name] == unique_taxa[i]),]
      # Extract cell ID
      cells <- suppressMessages(
        h3jsr::point_to_h3(tmp[,c(lng, lat)], res = grid$h3_resolution)
      )
      # Calculate number of unique cells occupied
      oc_df$cells[i] <- length(unique(cells))
    }
    # Return data
    return(oc_df)
  }
}
