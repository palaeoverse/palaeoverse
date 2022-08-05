#' Assign fossil occurrences to spatial bins
#'
#' A function to assign fossil occurrences to spatial bins/samples using a
#' hexagonal equal-area grid, or a distance-based approach.
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
#' @param buffer \code{numeric}. The buffer distance for defining unique
#' localities. This argument allows the user to modify the definition of unique
#' localities to draw from a larger area than point coordinates. This might be
#' desirable if a high density of occurrences are in close proximity, but differ
#' slightly in their coordinates (e.g., < 1 kilometre).
#' Note: `buffer` should be provided in kilometres. The default value is `NULL`,
#' which skips the buffer implementation.
#' @param return \code{logical}. Should the equal-area grid be returned?
#' Only useful if the method argument is set to "grid". The default is `FALSE.`
#'
#' @return If the `method` argument is specified as "grid" and `return` as
#' `FALSE`, a dataframe is returned of the original input `occdf` with
#' cell information: ID number (`cell_ID`) and cell centroid coordinates
#' (`cell_centroid_lng` and `cell_centroid_lat`). If `return` is
#' set to `TRUE`, a list is returned with both the input `occdf` and a dggs
#' object (equal-area hexagonal grid) from the
#' \code{\link[dggridR:dgconstruct]{dggridR::dgconstruct()}} function.
#' If the `method` argument is specified as "dist", a list is returned of
#' spatial samples drawn around unique localities based on the
#' user-defined `dist`. Each element of the list contains occurrences within
#' the geographic distance of the reference locality defined by the user.
#'
#' @details Two approaches (methods) exist in the `bin_spatial()` function for
#' assigning occurrences to bins/samples:
#' - Equal-area grid: The "grid" method bins fossil occurrence data into
#' equal-area grid cells using a hexagonal grid generated via the
#' \code{\link[dggridR:dgconstruct]{dggridR::dgconstruct()}} function.
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
#' not appropriate.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' To be reviewed
#' @importFrom sf st_as_sf st_is_within_distance
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
                        dist = 250,
                        buffer = NULL,
                        return = FALSE) {

  #=== dggridR installed? ===
  if (!requireNamespace("dggridR", quietly = TRUE)) {
    install.packages("dggridR")
  }

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

  if (!is.null(buffer) && !is.numeric(buffer)) {
    stop("`buffer` should be NULL or of class numeric")
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
    dggs <- dggridR::dgconstruct(spacing = dist,
                                 metric = TRUE,
                                 resround = "nearest")

    # Extract cell ID
    occdf$cell_ID <- dggridR::dgGEO_to_SEQNUM(
      dggs = dggs,
      in_lon_deg = occdf[, lng][[1]],
      in_lat_deg = occdf[, lat][[1]])$seqnum

    # Extract cell centroids
    occdf$cell_centroid_lng <- dgSEQNUM_to_GEO(
      dggs = dggs, occdf$cell_ID)$lon_deg
    occdf$cell_centroid_lat <- dgSEQNUM_to_GEO(
      dggs = dggs, occdf$cell_ID)$lat_deg

    # Format to dataframe
    occdf <- data.frame(occdf)
    # Drop geometries column
    occdf <- occdf[, -which(colnames(occdf) == "geometry")]

    # Should the grid be returned?
    if (return == TRUE) {
      occdf <- list(occdf, dggs)
      names(occdf) <- c("occdf", "grid")
    }

    return(occdf)
  }

  #=== Distance-based groupings  ===
  if (method == "dist") {
    # Convert dist to metres for st_is_within_distance function
    dist <- dist * 1000

    # Get unique locations
    occdf2 <- unique(occdf[,c(lng, lat)])

    # Is a distance buffer desired for unique locations?
    if (!is.null(buffer)) {
      # Convert km to m
      buffer <- buffer * 1000
      # Get points within buffer zone
      buffer <- sf::st_is_within_distance(occdf2, dist = buffer)
      # Unlist
      non_uniq <- unlist(
        mapply(function(lng, lat) lng[lng < lat], buffer, seq_along(buffer)))
      # Filter unique locations according to buffer
      occdf2 <- occdf2[-non_uniq, ]
    }

    # Generate all potential subsamples for unique locations
    samples <- sf::st_is_within_distance(x = occdf2, y = occdf, dist = dist)
    # Format data
    samples <- lapply(seq_along(samples), function(i) {
      # Extract relevant occurrences from main dataframe
      # Convert to dataframe and drop geometries
      tmp <- data.frame(
        occdf[samples[[i]], ])[, -which(colnames(occdf) == "geometry")]
      # Add reference locality information
      tmp$ref_lng <- occdf[i, c("lng")][[1]]
      tmp$ref_lat <- occdf[i, c("lat")][[1]]
      # Return data
      tmp
    })
    return(samples)
    }
  }
