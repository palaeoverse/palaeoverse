#' Generate H3 spatial bins
#'
#' @description
#' A function to generate spatial hexagonal equal-area grid cells (i.e. bins) using the
#' [H3 discrete global grid system](https://h3geo.org/). These can then be used in
#' [`bin_space()`] to assign fossil occurrences (or localities) to spatial bins.
#'
#' @param resolution \code{numeric}. The desired H3 resolution. Must be a whole
#' number between 0 and 15. Cannot be used if `spacing` or `area` is used.
#' @param spacing \code{numeric}. The desired spacing between the centre of
#' adjacent cells. This value should be provided in kilometres. Cannot be used
#' if `resolution` or `area` is used.
#' @param area \code{numeric}. The desired area of each cell. This value should
#' be provided in square kilometres. Cannot be used if `resolution` or `spacing`
#' is used.
#'
#' @details
#' The [H3 discrete global grid system](https://h3geo.org/) uses fixed resolutions, which
#' can be directly set by passing an H3 resolution level (0-15) to `resolution`. Alternatively,
#' resolution can be inferred from a target `spacing` or `area`. `spacing` is defined as the
#' distance between the centroids of adjacent cells; the function compares the input spacing
#' to the average centroid distance at each H3 resolution and selects the closest match.
#' Likewise, a target `area` is compared to the average cell area at each resolution, and
#' the nearest resolution is used.
#'
#' The table below shows, for each H3 resolution, the number of cells covering
#' the globe, their average area (source: <https://h3geo.org/docs/core-library/restable>),
#' and the average spacing between the centroids of adjacent cells. This
#' information is also available via [h3jsr::h3_info_table].
#'
#' ```{r, echo = FALSE, results = "asis"}
#' # Values >= 1 are rounded to two decimals, smaller values to three significant digits
#' fmt <- function(x) {
#'   ifelse(
#'     x >= 1,
#'     formatC(x, format = "f", digits = 2, big.mark = ","),
#'     formatC(x, format = "fg", digits = 3, flag = "#")
#'   )
#' }
#' tab <- h3jsr::h3_info_table
#' cat(
#'   "| Resolution | Number of cells | Average area (sq. km) | Average spacing (km) |",
#'   "|---:|---:|---:|---:|",
#'   paste0(
#'     "| ", tab$h3_resolution,
#'     " | ", formatC(tab$total_unique_indexes, format = "f", digits = 0, big.mark = ","),
#'     " | ", fmt(tab$avg_area_sqkm),
#'     " | ", fmt(tab$avg_cendist_km), " |"
#'   ),
#'   sep = "\n"
#' )
#' ```
#'
#' @return An object of class `palaeoverse_space_bins`. This object can be passed
#' to the `bins` argument in [`bin_space()`] to assign occurrences (or any set of
#' coordinates)  to these bins. Under the hood, this is an `sfc_POLYGON` with CRS
#' EPSG:4326 (WGS 1984). This object has an attribute `h3_resolution` that is
#' currently used internally by `bin_space()` to name columns automatically, but
#' may change in future releases.
#'
#' @seealso [`bin_space()`]
#' @export
#' @examplesIf requireNamespace("rnaturalearthdata", quietly = TRUE)
#' bins <- space_bins(resolution = 1)
#' bins
#' plot(bins)
#'
#' space_bins(spacing = 1000)
#'
#' space_bins(area = 100000)
space_bins <- function(resolution, spacing, area) {
  ensure_args_are_named()
  rlang::check_exclusive(spacing, resolution, area)

  if (!missing(spacing)) {
    rlang::check_number_decimal(spacing)
    if (spacing <= 0) {
      cli::cli_abort("{.arg spacing} must be greater than 0.")
    }
  } else if (!missing(area)) {
    rlang::check_number_decimal(area)
    if (area <= 0) {
      cli::cli_abort("{.arg area} must be greater than 0.")
    }
  } else {
    rlang::check_number_whole(resolution)
    if (!resolution %in% 0:15) {
      cli::cli_abort(
        "{.arg resolution} must be a whole number between 0 and 15."
      )
    }
  }

  # Generate equal area hexagonal grid
  if (!missing(spacing)) {
    # Which resolution should be used based on input distance/spacing?
    grid <- h3jsr::h3_info_table[
      which.min(abs(h3jsr::h3_info_table$avg_cendist_km - spacing)),
    ]
  } else if (!missing(area)) {
    grid <- h3jsr::h3_info_table[
      which.min(abs(h3jsr::h3_info_table$avg_area_sqkm - area)),
    ]
  } else {
    grid <- h3jsr::h3_info_table[
      h3jsr::h3_info_table$h3_resolution == resolution,
    ]
  }

  all_cells <- h3jsr::get_res0()
  # Get children at desired resolution
  children <- h3jsr::get_children(
    h3_address = all_cells,
    res = grid$h3_resolution,
    simple = TRUE
  )

  out <- h3jsr::cell_to_polygon(input = children, simple = TRUE)

  cli::cli_inform(c(
    "i" = paste0("H3 resolution: ", grid$h3_resolution[1]),
    "i" = paste0(
      "Average spacing between adjacent cells: ",
      format(round(grid$avg_cendist_km[1], digits = 2), big.mark = ","),
      " km."
    ),
    "i" = paste0(
      "Average cell area: ",
      format(round(grid$avg_area_sqkm[1], digits = 2), big.mark = ","),
      " sq. km."
    )
  ))

  class(out) <- c("palaeoverse_space_bins", class(out))
  attr(out, "h3_resolution") <- grid$h3_resolution

  return(out)
}
