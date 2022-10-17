#' Assign fossil occurrences to spatial bins
#'
#' A function to assign fossil occurrences (or localities) to spatial
#' bins/samples using a hexagonal equal-area grid.
#'
#' @param occdf \code{dataframe}. A dataframe of the fossil occurrences (or
#' localities) you wish to bin. This dataframe should contain the decimal
#' degree coordinates of your occurrences, and they should be of
#' class `numeric`.
#' @param lng \code{character}. The name of the column you wish to be treated
#' as the input longitude (e.g., "lng" or "p_lng").
#' @param lat \code{character}. The name of the column you wish to be treated
#' as the input latitude (e.g., "lat" or "p_lat").
#' @param spacing \code{numeric}. The desired spacing between the center of
#' adjacent cells. This value should be provided in kilometres.
#' @param sub_grid \code{numeric}. For an optional sub-grid, the desired
#' spacing between the center of adjacent cells in the sub-grid.
#' This value should be provided in kilometres.
#' See details for information on sub-grid usage.
#' @param return \code{logical}. Should the equal-area grid information and
#' polygons be returned?
#' @param plot \code{logical}. Should the occupied cells of the equal-area grid
#' be plotted?
#'
#' @return If the `return` argument is set to `FALSE`, a dataframe is
#' returned of the original input `occdf` with cell information. If `return` is
#' set to `TRUE`, a list is returned with both the input `occdf` and grid
#' information and polygons.
#'
#' @details This function assigns fossil occurrence data into
#' equal-area grid cells using discrete hexagonal grids via the
#' \code{\link[h3jsr]{h3jsr}} package. This package relies on Uber's H3 library,
#' a geospatial indexing system that partitions the world into hexagonal cells:
#' \url{https://h3geo.org/docs}. In H3, 16 different resolutions are available:
#' \url{https://h3geo.org/docs/core-library/restable}. In the implementation of
#' the `bin_spatial()` function, the resolution is defined by the user-input
#' `spacing` which represents the distance between the centroid of adjacent
#' cells. Using this distance, the function identifies which resolution is most
#' similar to the input `spacing`, and uses this resolution.
#'
#' Additional functionality allows the user to simultaneously assign occurrence
#' data to equal-area grid cells of a finer-scale grid (i.e., a ‘sub-grid’)
#' within the primary grid via the `sub_grid` argument. This might be desirable
#' for users to evaluate the differences in the amount of area occupied by
#' occurrences within their primary grid cells. This functionality also allows
#' the user to easily rarefy across sub-grid cells within primary cells to
#' further standardise spatial sampling (see example for basic implementation).
#'
#' Note: prior to implementation, coordinate reference system (CRS) for input
#' data is defined as EPSG:4326 (World Geodetic System
#' 1984). The user should transform their data accordingly if this is
#' not appropriate. If you are unfamiliar with working with geographic data,
#' we highly recommend checking out Geocomputation with R
#' \url{https://geocompr.robinlovelace.net/index.html}.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Bethany Allen & Kilian Eichenseer
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom h3jsr point_to_cell cell_to_point cell_to_polygon
#' @examples
#' # Get internal data
#' data("reefs")
#'
#' # Reduce data for plotting
#' occdf <- reefs[1:500, ]
#'
#' # Bin data using a hexagonal equal-area grid
#' ex1 <- bin_spatial(occdf = occdf, spacing = 250, plot = TRUE)
#'
#' # Bin data using a hexagonal equal-area grid and sub-grid
#' ex2 <- bin_spatial(occdf = occdf, spacing = 250, sub_grid = 50, plot = TRUE)
#'
#' # EXAMPLE: rarefy
#' # Load data
#' data("tetrapods")
#'
#' # Assign to spatial bin
#' occdf <- bin_spatial(occdf = tetrapods, spacing = 1000, sub_grid = 250)
#'
#' # Get unique bins
#' bins <- unique(occdf$cell_ID)
#'
#' # n reps
#' n <- 10
#'
#' # Rarefy data across sub-grid grid cells
#' # Returns a list with each element a bin with respective mean genus richness
#' df <- lapply(bins, function(x) {
#'   # subset occdf for respective grid cell
#'   tmp <- occdf[which(occdf$cell_ID == x), ]
#'
#'   # Which sub-grid cells are there within this bin?
#'   sub_bin <- unique(tmp$cell_ID_sub)
#'
#'   # Sample 1 sub-grid cell n times
#'   s <- sample(sub_bin, size = n, replace = TRUE)
#'
#'   # Count the number of unique genera within each sub_grid cell for each rep
#'   counts <- sapply(s, function(i) {
#'     # Number of unique genera within each sample
#'     length(unique(tmp[which(tmp$cell_ID_sub == i), ]$genus))
#'   })
#'
#'   # Mean richness across subsamples
#'   mean(counts)
#' })
#' @export
bin_spatial <- function(occdf,
                        lng = "lng",
                        lat = "lat",
                        spacing = 100,
                        sub_grid = NULL,
                        return = FALSE,
                        plot = FALSE) {

  #=== Error handling ===
  if (!is.data.frame(occdf)) {
    stop("occdf should be of class dataframe")
  }

  if (lng %in% colnames(occdf) == FALSE ||
      lat %in% colnames(occdf) == FALSE) {
    stop("input column names do not exist in `occdf`")
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

  if (!is.numeric(spacing)) {
    stop("`spacing` should be of class numeric")
  }

  if (!is.null(sub_grid) && !is.numeric(sub_grid)) {
    stop("`sub_grid` should be of class numeric or NULL")
  }

  if (is.logical(return) == FALSE) {
    stop("`return` should be logical (TRUE/FALSE)")
  }

  #=== Set-up ===
  # Convert to sf object and add CRS
  occdf <- sf::st_as_sf(occdf, coords=c(lng, lat),
                        remove = FALSE,
                        crs = "EPSG:4326")

  #=== Grid binning  ===
  # Generate equal area hexagonal grid
  # Which resolution should be used based on input distance/spacing?
  # Use the h3jsr::h3_info_table to calculate resolution
  grid <- h3jsr::h3_info_table[
    which.min(abs(h3jsr::h3_info_table$avg_cendist_km - spacing)), ]
  # Add column grid specification
  grid$grid <- c("primary")

  # Extract cell ID
  occdf$cell_ID <- h3jsr::point_to_cell(occdf, res = grid$h3_resolution)

  # Extract cell centroids
  occdf$cell_centroid_lng <- sf::st_coordinates(
    h3jsr::cell_to_point(h3_address = occdf$cell_ID))[, c("X")]
  occdf$cell_centroid_lat <- sf::st_coordinates(
    h3jsr::cell_to_point(h3_address = occdf$cell_ID))[, c("Y")]

  # Sub-grid desired?
  if (!is.null(sub_grid)) {
    s_grid <- h3jsr::h3_info_table[
      which.min(abs(h3jsr::h3_info_table$avg_cendist_km - sub_grid)), ]
    # Throw error if grids are the same
    if (grid$h3_resolution == s_grid$h3_resolution) {
      stop("`spacing` and `sub_grid` values result in the same resolution.
    Update `spacing` and/or `sub_grid` accordingly.")
    }

    # Add column grid specification
    s_grid$grid <- c("sub-grid")
    # Extract cell ID
    occdf$cell_ID_sub <- h3jsr::point_to_cell(occdf, res = s_grid$h3_resolution)

    # Extract cell centroids
    occdf$cell_centroid_lng_sub <- sf::st_coordinates(
      h3jsr::cell_to_point(h3_address = occdf$cell_ID_sub))[, c("X")]
    occdf$cell_centroid_lat_sub <- sf::st_coordinates(
      h3jsr::cell_to_point(h3_address = occdf$cell_ID_sub))[, c("Y")]
  }

  # Drop geometries column
  occdf <- sf::st_drop_geometry(occdf)
  # Format to dataframe
  occdf <- data.frame(occdf)
  # Get base grid
  all_cells <- h3jsr::get_res0()
  # Get children at desired resolution
  children <- h3jsr::get_children(
    h3_address = all_cells, res = grid$h3_resolution, simple = TRUE)
  # Get base cells
  base_grid <- h3jsr::cell_to_polygon(input = children, simple = TRUE)

  # Get occupied cells
  primary <- h3jsr::cell_to_polygon(input = occdf$cell_ID, simple = TRUE)

  # Plot data?
  if (plot == TRUE) {

    plot(base_grid,
         setParUsrBB = TRUE,
         xlab = "Longitude",
         ylab = "Latitude",
         axes = TRUE)
    plot(primary, col = "#feb24c",
         axes = TRUE,
         ylab = "Latitude",
         xlab = "Longitude",
         add = TRUE)
    if (!is.null(sub_grid)) {
      secondary <- h3jsr::cell_to_polygon(input = occdf$cell_ID_sub,
                                        simple = TRUE)
      plot(secondary, col = "#1d91c0",
           add = TRUE)
    }
  }
  # Should the grid be returned?
  if (return == TRUE) {
    if (!is.null(sub_grid)) {
      grid <- rbind.data.frame(grid, s_grid)
      occdf <- list(occdf, grid, base_grid, primary, secondary)
      names(occdf) <- c("occdf", "grid_info", "grid_base", "grid", "sub_grid")
    } else {
      occdf <- list(occdf, grid, base_grid, primary)
      names(occdf) <- c("occdf", "grid_info", "grid_base", "grid")
    }
  }
  message(
    "Average spacing between adjacent cells in the primary grid was set to ",
      round(grid$avg_cendist_km[1], digits = 2), " km. ", "\nH3 resolution: ",
    grid$h3_resolution[1])
  return(occdf)
}
