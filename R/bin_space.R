#' Assign fossil occurrences to spatial bins
#'
#' A function to assign fossil occurrences (or any set of coordinates) to spatial
#' bins (i.e. cells) using a hexagonal equal-area grid. This function is related to
#' [`space_bins()`], which creates the spatial bins required in the `bins` argument.
#'
#' @param occdf \code{data.frame}. A dataframe of the fossil occurrences (or
#' any set of coordinates) you wish to bin. This dataframe should contain the decimal
#' degree coordinates of your occurrences, and they should be of
#' class `numeric`.
#' @param bins \code{sfc_POLYGON}. An object of class \code{palaeoverse_space_bins}
#' created by [`space_bins()`], corresponding to the bins that you wish to allocate
#' fossil occurrences to.
#' @param lng \code{character}. The name of the column you wish to be treated
#' as the input longitude (e.g. "lng" or "p_lng").
#' @param lat \code{character}. The name of the column you wish to be treated
#' as the input latitude (e.g. "lat" or "p_lat").
#' @param plot `r lifecycle::badge("deprecated")` Use `plot()` on the output of
#' this function instead.
#' @param spacing,sub_grid,return `r lifecycle::badge("deprecated")` These arguments
#' are no longer used and don't have a replacement. Pass the output of `space_bins()`
#' to the `bins` argument instead.
#'
#' @return A `data.frame` with the original input data and three new columns:
#'
#' - `cell_id_{h3_resolution}`: the ID of the H3 cell that contains the row coordinates
#' - `cell_centroid_lat_{h3_resolution}`: the latitude of the H3 cell's centroid
#' - `cell_centroid_lng_{h3_resolution}`: the longitude of the H3 cell's centroid
#'
#' In these three column names, `{h3_resolution}` corresponds to the H3 resolution that
#' is used. For instance, using `bins = space_bins(spacing = 250)` results in an H3
#' resolution equal to 2, so the column name for the cell ID will be `"cell_id_2"`.
#'
#' Note: calling `bin_space()` several times can overwrite existing cell columns if `bins`
#' uses the same resolution as existing columns.
#'
#' @details This function assigns fossil occurrence data into
#' equal-area grid cells using discrete hexagonal grids via the
#' \code{\link[h3jsr]{h3jsr}} package. This package relies on
#' [Uber's H3](https://h3geo.org/docs/) library, a geospatial indexing system
#' that partitions the world into hexagonal cells. In H3, 16 different
#' resolutions are available (see the 'Details' section of [`space_bins()`]).
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
#' @seealso [`space_bins()`]
#' @examples
#' # Get internal data
#' data("reefs")
#'
#' # Reduce data for plotting
#' occdf <- reefs[1:250, ]
#'
#' # Bin data using a hexagonal equal-area grid
#' ex1 <- bin_space(occdf = occdf, bins = space_bins(spacing = 500))
#' plot(ex1)
#'
#' # Bin data using a hexagonal equal-area grid and sub-grid
#' ex2 <- occdf |>
#'   bin_space(bins = space_bins(spacing = 1000)) |>
#'   bin_space(bins = space_bins(spacing = 250))
#' plot(ex2)
#'
#' # EXAMPLE: rarefy
#' # Load data
#' occdf <- tetrapods[1:250, ]
#'
#' # Assign to spatial bins and sub-bins
#' occdf <- occdf |>
#'   bin_space(bins = space_bins(spacing = 1000)) |>
#'   bin_space(bins = space_bins(spacing = 250))
#'
#' # Get unique bins
#' bins <- unique(occdf$cell_ID_1)
#'
#' # n reps
#' n <- 10
#'
#' # Rarefy data across sub-grid grid cells
#' # Returns a list with each element a bin with respective mean genus richness
#' df <- lapply(bins, function(x) {
#'   # subset occdf for respective grid cell
#'   tmp <- occdf[which(occdf$cell_ID_1 == x), ]
#'
#'   # Which sub-grid cells are there within this bin?
#'   sub_bin <- unique(tmp$cell_ID_2)
#'
#'   # Sample 1 sub-grid cell n times
#'   s <- sample(sub_bin, size = n, replace = TRUE)
#'
#'   # Count the number of unique genera within each sub_grid cell for each rep
#'   counts <- sapply(s, function(i) {
#'     # Number of unique genera within each sample
#'     length(unique(tmp[which(tmp$cell_ID_2 == i), ]$genus))
#'   })
#'
#'   # Mean richness across subsamples
#'   mean(counts)
#' })
#' df
#' @export
bin_space <- function(
  occdf,
  bins,
  lng = "lng",
  lat = "lat",
  plot = deprecated(),
  spacing,
  sub_grid,
  return
) {
  ensure_args_are_named(exceptions = "occdf")

  for (arg in c("return", "spacing", "sub_grid")) {
    if (arg %in% names(match.call())) {
      cli::cli_abort(
        c(
          "The {.arg {arg}} argument of {.fn bin_space} is no longer used as of {.pkg palaeoverse} 2.0.0.",
          "i" = "Pass the output of {.fn space_bins} to the {.arg bins} argument instead."
        )
      )
    }
  }

  check_data_frame(occdf)

  if (!inherits(bins, "palaeoverse_space_bins")) {
    cli::cli_abort(
      c(
        "{.arg bins} must be of class {.cls palaeoverse_space_bins}.",
        "i" = "Hint: use {.fn space_bins} to create the spatial bins."
      )
    )
  }

  check_column_presence(occdf, lat)
  check_column_presence(occdf, lng)

  check_class(occdf, lat, "numeric")
  check_class(occdf, lng, "numeric")
  check_range(occdf, lat, -90, 90)
  check_range(occdf, lng, -180, 180)

  if (lifecycle::is_present(plot)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "bin_space(plot)",
      I("`plot()` on the output of this function"),
      always = TRUE
    )
    rlang::check_bool(plot)
  }

  #=== Set-up ===
  # Convert to sf object and add CRS
  occdf <- sf::st_as_sf(
    occdf,
    coords = c(lng, lat),
    remove = FALSE,
    crs = "EPSG:4326"
  )

  h3_resolution <- attr(bins, "h3_resolution", exact = TRUE)

  #=== Grid binning  ===
  # Extract cell ID
  cell_name <- paste0("cell_ID_", h3_resolution)
  cent_lat_name <- paste0("cell_centroid_lat_", h3_resolution)
  cent_lon_name <- paste0("cell_centroid_lng_", h3_resolution)

  overwritten <- intersect(
    c(cell_name, cent_lat_name, cent_lon_name),
    names(occdf)
  )
  if (length(overwritten) > 0) {
    cli::cli_inform("Overwriting column{?s} {.val {overwritten}}.")
  }

  # Point -> cell
  # Cell -> find centroid
  # Add centroid coords to the data
  occdf[[cell_name]] <- h3jsr::point_to_cell(occdf, res = h3_resolution)
  coords <- sf::st_coordinates(h3jsr::cell_to_point(
    h3_address = occdf[[cell_name]]
  ))
  occdf[[cent_lon_name]] <- coords[, "X"]
  occdf[[cent_lat_name]] <- coords[, "Y"]

  occdf <- sf::st_drop_geometry(occdf)
  occdf <- data.frame(occdf)
  class(occdf) <- c("palaeoverse_bin_space", class(occdf))

  if (isTRUE(plot)) {
    plot(occdf)
  }

  occdf
}

#' @param x `data.frame`. An object of class `"palaeoverse_bin_space"` created by `bin_space()`.
#' The base grid uses the least precise resolution among the `cell_ID_*` columns, and the occupied
#' cells of every resolution are drawn on top of it.
#' @param ... Extra arguments passed to [`plot()`][base::plot]. These are passed to the call to
#' first layer (which draws the grid), not to the following ones that fill the cells. The
#' following arguments are already set internally and must not be specified here: `setParUsrBB`.
#' @param col `character`. Colour(s) of the occupied cells. Must be of length 1 (in which case
#' all resolutions have the same colour) or of length equal to the number of resolutions in `x`,
#' ordered from the least to the most precise resolution. By default, colours are taken in order
#' from the colour-blind friendly Okabe-Ito palette (see [grDevices::palette.colors()]).
#' @inheritParams lat_bins_area
#' @param axes `logical`. Whether to draw the grid axes.
#'
#' @name bin_space
#' @export
plot.palaeoverse_bin_space <- function(
  x,
  ...,
  col = NULL,
  xlab = "Longitude",
  ylab = "Latitude",
  axes = TRUE
) {
  check_forbidden_plot_args(
    ...,
    forbidden = "setParUsrBB",
    class = "palaeoverse_bin_space"
  )

  cell_cols <- grep("^cell_ID_([0-9]|1[0-5])$", names(x), value = TRUE)
  if (length(cell_cols) == 0) {
    cli::cli_abort(
      "{.arg x} must contain at least one column named {.val cell_ID_<resolution>}."
    )
  }
  resolutions <- sort(as.integer(sub("cell_ID_", "", cell_cols, fixed = TRUE)))
  n_res <- length(resolutions)

  if (is.null(col)) {
    # Drop black, which is the first colour of Okabe-Ito
    col <- unname(grDevices::palette.colors(palette = "Okabe-Ito"))[-1]
  } else if (!(length(col) %in% c(1, n_res)) || !is.character(col)) {
    cli::cli_abort(
      "Argument {.arg col} must be of class {.cls character} and of length 1 or {n_res} (i.e. the number of resolutions in {.arg x})."
    )
  }

  base_grid <- suppressMessages(space_bins(resolution = resolutions[1]))
  plot(
    base_grid,
    setParUsrBB = TRUE,
    xlab = xlab,
    ylab = ylab,
    axes = axes,
    ...
  )

  # Coarsest cells first so that finer cells are drawn on top
  for (i in seq_along(resolutions)) {
    cells <- h3jsr::cell_to_polygon(
      input = unique(x[[paste0("cell_ID_", resolutions[i])]]),
      simple = TRUE
    )
    plot(cells, col = col[i], add = TRUE)
  }
}
