#' Assign fossil occurrences to spatial bins
#'
#' A function to assign fossil occurrences (or localities) to spatial
#' bins/samples using a hexagonal equal-area grid.
#'
#' @param occdf \code{dataframe}. A dataframe of the fossil occurrences (or
#' localities) you wish to bin. This dataframe should contain the decimal
#' degree coordinates of your occurrences, and they should be of
#' class `numeric`.
#' @param bins \code{sfc_POLYGON}. Bins that you wish to allocate fossil
#' occurrences to, such as that returned by [`space_bins()`].
#' @param lng \code{character}. The name of the column you wish to be treated
#' as the input longitude (e.g. "lng" or "p_lng").
#' @param lat \code{character}. The name of the column you wish to be treated
#' as the input latitude (e.g. "lat" or "p_lat").
#'
#' @return If the `return` argument is set to `FALSE`, a dataframe is
#' returned of the original input `occdf` with cell information. If `return` is
#' set to `TRUE`, a list is returned with both the input `occdf` and grid
#' information and polygons.
#'
#' @details This function assigns fossil occurrence data into
#' equal-area grid cells using discrete hexagonal grids via the
#' \code{\link[h3jsr]{h3jsr}} package. This package relies on
#' [Uber's H3](https://h3geo.org/docs/) library, a geospatial indexing system
#' that partitions the world into hexagonal cells. In H3, 16 different
#' resolutions are available
#' ([see here](https://h3geo.org/docs/core-library/restable/)). In the
#' implementation of the `bin_space()` function, the resolution is defined by
#' the user-input `spacing` which represents the distance between the centroid
#' of adjacent cells. Using this distance, the function identifies which
#' resolution is most similar to the input `spacing`, and uses this resolution.
#'
#' Additional functionality allows the user to simultaneously assign occurrence
#' data to equal-area grid cells of a finer-scale grid (i.e. a ‘sub-grid’)
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
#' we highly recommend checking out [Geocomputation with R](
#' https://r.geocompx.org/index.html).
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
#' occdf <- reefs[1:250, ]
#'
#' # Bin data using a hexagonal equal-area grid
#' ex1 <- bin_space(occdf = occdf, bins = space_bins(spacing = 500))
#' head(ex1)
#'
#' # Bin data using a hexagonal equal-area grid and sub-grid
#' ex2 <- occdf |>
#'   bin_space(bins = space_bins(1000)) |>
#'   bin_space(bins = space_bins(250))
#'
#' head(ex2)
#'
#' # EXAMPLE: rarefy
#' # Load data
#' occdf <- tetrapods[1:250, ]
#'
#' # Assign to spatial bin
#' occdf <- occdf |>
#'   bin_space(bins = space_bins(1000)) |>
#'   bin_space(bins = space_bins(250))
#'
#' # Get unique bins
#' bins <- unique(occdf$cell_ID_1000)
#'
#' # n reps
#' n <- 10
#'
#' # Rarefy data across sub-grid grid cells
#' # Returns a list with each element a bin with respective mean genus richness
#' df <- lapply(bins, function(x) {
#'   # subset occdf for respective grid cell
#'   tmp <- occdf[which(occdf$cell_ID_1000 == x), ]
#'
#'   # Which sub-grid cells are there within this bin?
#'   sub_bin <- unique(tmp$cell_ID_250)
#'
#'   # Sample 1 sub-grid cell n times
#'   s <- sample(sub_bin, size = n, replace = TRUE)
#'
#'   # Count the number of unique genera within each sub_grid cell for each rep
#'   counts <- sapply(s, function(i) {
#'     # Number of unique genera within each sample
#'     length(unique(tmp[which(tmp$cell_ID_250 == i), ]$genus))
#'   })
#'
#'   # Mean richness across subsamples
#'   mean(counts)
#' })
#' df
#' @export
bin_space <- function(occdf, bins, lng = "lng", lat = "lat") {
  ensure_args_are_named(exceptions = "occdf")

  check_data_frame(occdf)
  if (!inherits(bins, "palaeo_space_bins") && !inherits(bins, "sfc_POLYGON")) {
    cli::cli_abort(
      c(
        "{.arg bins} must be of class {.cls palaeo_space_bins} or {.cls sfc_POLYGON}.",
        "i" = "Hint: you can create space bins with {.fn space_bins}."
      )
    )
  }

  check_column_presence(occdf, lat)
  check_column_presence(occdf, lng)
  check_range(occdf, lat, -90, 90)
  check_range(occdf, lng, -180, 180)

  #=== Set-up ===
  # Convert to sf object and add CRS
  occdf <- sf::st_as_sf(
    occdf,
    coords = c(lng, lat),
    remove = FALSE,
    crs = "EPSG:4326"
  )

  spacing <- attr(bins, "spacing", exact = TRUE)
  h3_resolution <- attr(bins, "h3_resolution", exact = TRUE)
  avg_cendist_km <- attr(bins, "avg_cendist_km", exact = TRUE)

  #=== Grid binning  ===
  # Extract cell ID
  cell_name <- paste0("cell_ID_", spacing)
  cent_lat_name <- paste0("cell_centroid_lng_", spacing)
  cent_lon_name <- paste0("cell_centroid_lat_", spacing)

  occdf[[cell_name]] <- h3jsr::point_to_cell(occdf, res = h3_resolution)

  # Extract cell centroids
  occdf[[cent_lon_name]] <- sf::st_coordinates(
    h3jsr::cell_to_point(h3_address = occdf[[cell_name]])
  )[, c("X")]
  occdf[[cent_lat_name]] <- sf::st_coordinates(
    h3jsr::cell_to_point(h3_address = occdf[[cell_name]])
  )[, c("Y")]

  occdf <- sf::st_drop_geometry(occdf)
  occdf <- data.frame(occdf)

  cli::cli_inform(
    c(
      paste0(
        "Average spacing between adjacent cells in the primary grid was set to ",
        round(avg_cendist_km[1], digits = 2),
        " km. "
      ),
      "i" = paste0("\nH3 resolution: ", h3_resolution[1])
    )
  )
  return(occdf)
}
