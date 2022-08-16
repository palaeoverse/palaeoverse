#' Assign fossil occurrences to spatial bins
#'
#' A function to assign fossil occurrences (or localities) to spatial
#' bins/samples using a hexagonal equal-area grid, or a distance-based approach.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences you
#' wish to bin. This dataframe should contain the decimal degree coordinates of
#' your occurrences, and they should be of class `numeric`.
#' @param lng \code{character}. The name of the column you wish to be treated
#' as the input longitude (e.g., "lng" or "p_lng").
#' @param lat \code{character}. The name of the column you wish to be treated
#' as the input latitude (e.g., "lat" or "p_lat").
#' @param method \code{character}. The desired method for spatially binning
#' occurrence data. Either: "grid" or "dist". The "grid" method (default)
#' spatially bins data into equal-area hexagonal grid cells based on a
#' user-defined spacing between the center of adjacent cells. The "dist" method
#' generates spatial samples for each locality (i.e., unique coordinate pairs)
#' based on a user-defined distance. See details for further description.
#' @param dist \code{numeric}. Under the "grid" method, this value indicates
#' the desired spacing between the center of adjacent cells. Under the "dist"
#' method, this is the distance threshold for defining spatial samples of
#' occurrences. Note: `dist` should be provided in kilometres.
#' This argument allows the user to modify the definition of unique
#' localities to draw from a larger area than point coordinates. This might be
#' desirable if a high density of occurrences are in close proximity, but differ
#' slightly in their coordinates (e.g., < 1 kilometre).
#' @param reps \code{numeric}.
#' @param size \code{numeric}.
#' Note: `buffer` should be provided in kilometres. The default value is `NULL`,
#' which skips the buffer implementation.
#' @param return \code{logical}. Should the equal-area grid information be
#'  returned? Only useful if the method argument is set to "grid".
#'  The default is `FALSE.`
#'
#' @return If the `method` argument is specified as "grid" and `return` as
#' `FALSE`, a dataframe is returned of the original input `occdf` with
#' cell information: ID number (`cell_ID`) and cell centroid coordinates
#' (`cell_centroid_lng` and `cell_centroid_lat`). If `return` is
#' set to `TRUE`, a list is returned with both the input `occdf` and grid
#' information.
#' If the `method` argument is specified as "dist", a list is returned of
#' spatial samples drawn around unique localities based on the
#' user-defined `dist`. Each element of the list contains occurrences within
#' the geographic distance of the reference locality defined by the user.
#'
#' @details Two approaches (methods) exist in the `bin_spatial()` function for
#' assigning occurrences to bins/samples:
#' - Equal-area grid: The "grid" method bins fossil occurrence data into
#' equal-area grid cells using discrete hexagonal grids via the
#' \code{\link[h3jsr]{h3jsr}} package. This package relies on Uber's H3 library,
#' a geospatial indexing system that partitions the world into hexagonal cells:
#' \url{https://h3geo.org/docs}. In H3, 16 different resolutions are available:
#' \url{https://h3geo.org/docs/core-library/restable}. In the implementation of
#' the `bin_spatial()` function, the resolution is defined by the user-input
#' `dist` which represents the distance between the centroid of adjacent cells.
#' Using this distance, the function identifies which resolution is most
#' similar to the input `dist`, and uses this resolution.
#' - Distance: The "dist" method identifies unique localities in the input
#' `occdf` (i.e., unique pairs of coordinates) and generates a spatial sample
#' for each locality based on a user-defined distance. All occurrences within
#' the specified distance from the reference locality are drawn as a single
#' spatial sample. As high density of occurrences might result in numerous
#' samples for the same area, the `buffer` argument has been implemented to
#' allow users to generalise the definition of
#' a unique locality to incorporate a broader geographic area. The
#' functionality of this approach is heavily
#' dependent on the
#' \code{\link[sf:st_is_within_distance]{sf::st_is_within_distance()}} function.
#'
#' Note: prior to implementation of either method, the coordinate reference
#' system (CRS) for input data is defined as EPSG:4326 (World Geodetic System
#' 1984). The user might wish to update their data accordingly if this is
#' not appropriate. If you are unfamiliar with working with geographic data,
#' we highly recommend checking out Geocomputation with R
#' \url{https://geocompr.robinlovelace.net/index.html}.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' To be reviewed
#' @importFrom sf st_as_sf st_is_within_distance st_drop_geometry
#' @importFrom h3jsr point_to_h3 h3_to_point
#' @examples
#' # Get internal data
#' data("tetrapods")
#' # Smaller dataframe for examples
#' occdf <- tetrapods[1:500, ]
#' # Bin data using a hexagonal equal-area grid
#' bin_spatial(occdf = occdf, method = "grid", dist = 250)
#' # Bin data based on distance for each unique location
#' bin_spatial(occdf = occdf, method = "dist", dist = 250)
#' # Bin data based on distance with a buffer to define each unique location
#' bin_spatial(occdf = occdf, method = "dist", dist = 250, buffer = 100)
#' @export
bin_spatial <- function(occdf,
                        lng = "lng",
                        lat = "lat",
                        method = "grid",
                        dist = 100,
                        reps = 100,
                        size = NULL,
                        return = FALSE,
                        plot = FALSE) {

  #=== Error handling ===
  if (!is.data.frame(occdf)) {
    stop("occdf should be of class dataframe")
  }

  if (lng %in% colnames(occdf) == FALSE ||
      lat %in% colnames(occdf) == FALSE) {
    stop("input column names do not exist in `occdf")
  }

  if (!is.numeric(occdf[, lng]) || !is.numeric(occdf[, lat])) {
    stop("input coordinates are not of class numeric")
  }

  if (any(occdf[, lat] > 90) || any(occdf[, lat] < -90)) {
    stop("Latitudinal coordinates should be more than -90 and less than 90")
  }

  if (any(occdf[, lng] > 180) || any(occdf[, lng] < -180)) {
    stop("Longitudinal coordinates should be more than -180 and less than 180")
  }

  if (!is.character(method)) {
    stop("`method` should be of class character")
  }

  if (method != "grid" && method != "dist") {
    stop("`method` should be either 'grid' or 'dist'")
  }

  if (!is.numeric(dist)) {
    stop("`dist` should be of class numeric")
  }

  if (!is.null(size) && !is.numeric(size)) {
    stop("`size` should be NULL or of class numeric")
  }

  if (is.logical(return) == FALSE) {
    stop("`return` should be logical (TRUE/FALSE)")
  }

  #=== Set-up ===
  # Convert to sf object and add CRS
  occdf <- sf::st_as_sf(occdf,coords=c(lng, lat),
                        remove = FALSE,
                        crs = "EPSG:4326")

  #=== Grid binning  ===
  if (method == "grid") {
    # Generate equal area hexagonal grid

    # Which resolution should be used based on input distance/spacing?
    # Use the h3jsr::h3_info_table to calculate resolution (however, this
    # table is not exported in their package, added into palaeoverse)
    grid <- h3_info_table[
      which.min(abs(h3_info_table$avg_cendist_km - dist)), ]

    # Extract cell ID
    occdf$cell_ID <- h3jsr::point_to_h3(occdf, res = grid$h3_resolution)

    # Extract cell centroids
    occdf$cell_centroid_lng <- sf::st_coordinates(
      h3jsr::h3_to_point(h3_address = occdf$cell_ID))[,c("X")]
    occdf$cell_centroid_lat <- sf::st_coordinates(
      h3jsr::h3_to_point(h3_address = occdf$cell_ID))[,c("Y")]

    # Drop geometries column
    occdf <- sf::st_drop_geometry(occdf)

    # Format to dataframe
    occdf <- data.frame(occdf)

    # Should the grid be returned?
    if (return == TRUE) {
      occdf <- list(occdf, grid)
      names(occdf) <- c("occdf", "grid")
    }

    if(plot == TRUE) {
      poly <- h3jsr::h3_to_polygon(input = occdf$cell_ID, simple = TRUE)
      plot(poly, col = "blue",
           axes = TRUE,
           ylab = "Latitude",
           xlab = "Longitude")
    }

    cat("The average spacing between adjacent cells was set to",
        round(grid$avg_cendist_km, digits = 2), "km.")

    return(occdf)
  }

  #=== Distance-based groupings  ===
  if (method == "dist") {
    # Add distance to dataframe
    occdf$dist <- dist
    # Add size to dataframe
    occdf$size <- size
    # Add reps to dataframe
    occdf$reps <- reps

    # Add ID column for processing
    occdf$occ_ID <- 1:nrow(occdf)

    # Convert dist to metres for st_is_within_distance function
    dist <- dist * 1000

    # Calculate all potential samples
    dist_list <- sf::st_is_within_distance(x = occdf, y = occdf, dist = dist)

    # Randomly sample localities
    samples <- lapply(seq_len(reps), function(x) {
      # Create temp list for sampling
      tmp <- dist_list
      # Add names to list
      names(tmp) <- seq_along(tmp)
      # Create empty list
      list_samples <- list()
      # Generate spatial samples
      # Set counter
      s <- 0
      while (length(tmp) > 0) {
        s <- s + 1
        # Sample seed
        samp <- tmp[[sample(names(tmp), size = 1)]]
        # Which occurrences are not present in the sample?
        vec <- !names(tmp) %in% samp
        # Drop already sampled occurrences as seed points
        tmp <- tmp[vec]
        # Filter by sampled rows
        tmp_occdf <- occdf[samp, ]
        # Add sample ID
        tmp_occdf$bin_ID <- s
        # Drop geometries
        tmp_occdf <- sf::st_drop_geometry(tmp_occdf)
        # Add reference coordinates (first element is the locality)
        tmp_occdf$ref_lng <- tmp_occdf[, lng]
        tmp_occdf$ref_lat <- tmp_occdf[, lat]
        # Add samples to list
        list_samples[[s]] <- tmp_occdf
        # Fixed sample size desired?
        if (!is.null(size) && s == size) {
          if(any(is.na(tmp_occdf$occ_ID))) {
            stop("Number of desired spatial bins is too high for the desired
distance threshold. Reduce `dist` or `size`.")
          }
          break
        }
      }
      # If fixed sample size was desired, was the threshold met?
      if (!is.null(size) && s < size) {
        stop("Number of desired spatial bins is too high for the desired
distance threshold. Reduce `dist` or `size`.")
      }
      # Bind data
      tmp_occdf <- do.call(rbind,
                           list_samples)
      # Return data
      tmp_occdf
    })
    # Return samples
    return(samples)
    }
  }
