#' Calculate the geographic range of fossil taxa
#'
#' A function to calculate the geographic range of fossil taxa from occurrence
#' data. The function can calculate geographic range in four ways: convex hull,
#' latitudinal range, maximum great circle distance, and the number of
#' occupied equal-area hexagonal grid cells.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences.
#' This dataframe should contain at least three columns: names of taxa,
#' longitude and latitude (see `name`, `lng`, and `lat` arguments).
#' @param name \code{character}. The name of the column you wish to be treated
#' as the input names (e.g., "species" or "genus"). NA data should be removed
#' prior to function call.
#' @param lng \code{character}. The name of the column you wish to be treated
#' as the input longitude (e.g., "lng" or "p_lng"). NA data should be removed
#' prior to function call.
#' @param lat \code{character}. The name of the column you wish to be treated
#' as the input latitude (e.g., "lat" or "p_lat"). NA data should be removed
#' prior to function call.
#' @param method \code{character}. How should geographic range be calculated
#' for each taxon in `occdf`? Four options exist in this function:
#' "con", "lat", "gcd", and "occ". See Details for a description of each.
#' @param spacing \code{numeric}. The desired spacing (in km) between the
#' center of adjacent grid cells. Only required if the `method` argument is set
#' to "occ". The default is 100.
#'
#' @return A \code{dataframe} with method-specific columns:
#' - For the "con" method, a \code{dataframe} with each unique taxa (`taxa`) and
#' taxon ID (`taxon_id`) by convex hull coordinate (`lng` & `lat`) combination,
#' and area (`area`) in km\ifelse{html}{\out{<sup>2</sup>}}{\eqn{^2}} is
#' returned.
#' - For the "lat" method, a \code{dataframe} with unique taxa (`taxa`),
#' taxon ID (`taxon_id`), maximum latitude of occurrence (`max_lat`),
#' minimum latitude of occurrence (`min_lat`), and latitudinal
#' range (`range_lat`) is returned.
#' - For the "gcd" method, a \code{dataframe} with each unique taxa (`taxa`) and
#' taxon ID (`taxon_id`) by coordinate combination (`lng` & `lat`) of the two
#' most distant points, and the 'Great Circle Distance' (`GCD`) between these
#' points in km is returned.
#' - For the "occ" method, a \code{dataframe} with unique taxa (`taxa`), taxon
#' ID (`taxon_id`), the number of occupied cells (`cells`), proportion of
#' occupied cells from all occupied by occurrences (`proportional_occ`),
#' and the spacing between cells (`spacing`) in km is returned. Note: the number
#' of occupied cells and proportion of occupied cells is highly dependent on
#' the user-defined `spacing.`
#' For the "con", "lat" and "gcd" method, values of zero indicate that the
#' respective taxon is a singleton (i.e. represented by only one occurrence).
#'
#' @details Four commonly applied approaches (Darroch et al., 2020)
#' are available using the `tax_range_space` function for calculating ranges:
#' - Convex hull: the "con" method calculates the geographic range of taxa
#' using a convex hull for each taxon in `occdf`, and calculates the area of
#' the convex hull (in km\ifelse{html}{\out{<sup>2</sup>}}{\eqn{^2}}) using
#' \code{\link[geosphere:areaPolygon]{geosphere::areaPolygon()}}. The
#' convex hull method works by creating a polygon that encompasses all
#' occurrence points of the taxon.
#' - Latitudinal: the "lat" method calculates the palaeolatitudinal
#' range of a taxon. It does so for each taxon in `occdf` by finding their
#' maximum and minimum latitudinal occurrence (from input `lat`).
#' The palaeolatitudinal range of each taxon is also calculated (i.e. the
#' difference between the minimum and maximum latitude).
#' - Maximum Great Circle Distance: the "gcd" method calculates the maximum
#' Great Circle Distance between occurrences for each taxon in `occdf`. It does
#' so using \code{\link[geosphere:distHaversine]{geosphere::distHaversine()}}.
#' This function calculates Great Circle Distance using the Haversine method
#' with the radius of the Earth set to the 6378.137 km.
#' Great Circle Distance represents the shortest distance between two
#' points on the surface of a sphere. This is different from Euclidean Distance,
#' which represents the distance between two points on a plane.
#' - Occupied cells: the "occ" method calculates the number and proportion of
#' occupied equal-area grid cells. It does so using discrete hexagonal grids
#' via the \code{\link[h3jsr]{h3jsr}} package. This package relies on
#' [Uber's H3](https://h3geo.org/docs) library, a geospatial indexing system
#' that partitions the world into hexagonal cells. In H3, 16 different
#' resolutions are available
#' ([see here](https://h3geo.org/docs/core-library/restable)).
#' In the implementation of the `tax_range_space()` function, the resolution is
#' defined by the user-input `spacing` which represents the distance between
#' the centroid of adjacent cells. Using this distance, the function identifies
#' which resolution is most similar to the input `spacing`, and uses this
#' resolution.
#'
#' @section Reference(s):
#' Darroch, S. A., Casey, M. M., Antell, G. S., Sweeney, A., & Saupe, E. E.
#' (2020). High preservation potential of paleogeographic range size
#' distributions in deep time. The American Naturalist, 196(4), 454-471.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Bethany Allen & Christopher D. Dean
#' @importFrom geosphere areaPolygon
#' @importFrom grDevices chull
#' @importFrom geosphere distm distHaversine
#' @importFrom h3jsr point_to_cell
#' @examples
#' # Grab internal data
#' occdf <- tetrapods
#' # Remove NAs
#' occdf <- subset(occdf, !is.na(genus))
#' # Convex hull
#' ex1 <- tax_range_space(occdf = occdf, name = "genus", method = "con")
#' # Latitudinal range
#' ex2 <- tax_range_space(occdf = occdf, name = "genus", method = "lat")
#' # Great Circle Distance
#' ex3 <- tax_range_space(occdf = occdf, name = "genus", method = "gcd")
#' # Occupied grid cells
#' ex4 <- tax_range_space(occdf = occdf, name = "genus", method = "occ")
#' @export
tax_range_space <- function(occdf,
                          name = "genus",
                          lng = "lng",
                          lat = "lat",
                          method = "lat",
                          spacing = 100) {

  #=== Handling errors ===
  if (!is.data.frame(occdf)) {
    stop("`occdf` should be a dataframe")
  }

  if (any(c(name, lng, lat) %in% colnames(occdf) == FALSE)) {
    stop("Either `name`, `lng`, or `lat`, is not a named column
in `occdf`")
  }

  if (!is.character(method)) {
    stop("`method` is not of character class")
  }

  if (!is.numeric(occdf[, lat]) || !is.numeric(occdf[, lng])) {
    stop("`lng` and/or `lat` columns are not of numeric class")
  }

  if (any(is.na(occdf[, name]))) {
    stop("The `name` column contains NA values")
  }

  if (any(is.na(occdf[, lat])) || any(is.na(occdf[, lng]))) {
    stop("`lng` and/or `lat` columns contain NA values")
  }

  # Possible methods specified?
  possible_methods <- c("con", "lat", "gcd", "occ")
  method_match <- charmatch(method, possible_methods)

  if (is.na(method_match) == TRUE) {
    # If the user has entered a non-valid term for the "method" argument,
    # generate an error and warn the user.
    stop("Invalid `method`. Choose either:
  'con', 'lat', 'gcd', or 'occ'.")
  } else {
    method <- possible_methods[method_match]
  }

  #=== Set-up ===
  unique_taxa <- unique(occdf[, name])
  # Order taxa
  unique_taxa <- unique_taxa[order(unique_taxa)]

  #=== convex hull  ===
  if (method == "con") {
    # Generate dataframe for population
    spat_df <- data.frame()
    # Run for loop across unique taxa
    for (i in seq_along(unique_taxa)) {
      taxa <- unique_taxa[i]
      taxon_id <- i
      # Subset taxa
      tmp <- occdf[which(occdf[, name] == unique_taxa[i]), ]
      # Calculate convex hull
      tmp <- tmp[chull(x = tmp[, lng], y = tmp[, lat]), c(lng, lat)]
      # Calculate area of convex hull and convert to km^2
      area <- geosphere::areaPolygon(tmp) / 1e+6
      # Round to three decimal places
      area <- round(area, digits = 3)
      tmp <- cbind.data.frame(taxa, taxon_id, tmp, area)
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
                         taxon_id = seq(1, length(unique_taxa), 1),
                         max_lat = rep(NA, length(unique_taxa)),
                         min_lat = rep(NA, length(unique_taxa)),
                         range_lat = rep(NA, length(unique_taxa)))
    # Run for loop across unique taxa
    for (i in seq_along(unique_taxa)) {
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
    for (i in seq_along(unique_taxa)) {
      # Unique taxa name
      taxa <- unique_taxa[i]
      # taxon id
      taxon_id <- i
      # Subset df
      tmp <- occdf[which(occdf[, name] == unique_taxa[i]), ]
      # Calculate GCD matrix using the Haversine method
      vals <- geosphere::distm(x = tmp[ ,c(lng, lat)],
                               fun = geosphere::distHaversine)
      # Convert to km
      vals <- vals / 10^3
      # Extract location of points with max GCD
      loc <- which(vals == max(vals), arr.ind = TRUE)
      # Get maximum GCD in km
      GCD <- as.numeric(max(vals))
      # Extract coordinates of points
      coords <- data.frame(tmp[loc[1, 1:2], c(lng, lat)])
      # Build dataframe
      tmp <- cbind.data.frame(taxa, taxon_id, coords, GCD)
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
                         taxon_id = seq(1, length(unique_taxa), 1),
                         cells = rep(NA, length(unique_taxa)),
                         proportional_occ = rep(NA, length(unique_taxa)),
                         spacing = rep(NA, length(unique_taxa)))
    # Generate equal area hexagonal grid
    # Which resolution should be used based on input distance/spacing?
    # Use the h3jsr::h3_info_table to calculate resolution
    grid <- h3jsr::h3_info_table[
      which.min(abs(h3jsr::h3_info_table$avg_cendist_km - spacing)), ]
    # Add resolution spacing
    oc_df$spacing <- grid$avg_cendist_km
    # Track occupied cells
    tracker <- vector()
    # Run for loop over all unique taxa
    for (i in seq_along(unique_taxa)) {
      # Subset df
      tmp <- occdf[which(occdf[, name] == unique_taxa[i]), ]
      # Extract cell ID
      cells <- suppressMessages(
        h3jsr::point_to_cell(tmp[, c(lng, lat)], res = grid$h3_resolution)
      )
      # Calculate number of unique cells occupied
      oc_df$cells[i] <- length(unique(cells))
      # Append cells
      tracker <- append(tracker, cells)
    }
    # Get proportional occupancy
    oc_df$proportional_occ <- round(oc_df$cells / length(unique(tracker)),
                                    digits = 3)
    # Return data
    return(oc_df)
  }
}
