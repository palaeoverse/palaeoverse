#' Generate spatial bins
#'
#' @description
#' A function to generate spatial bins that can then be used in [`bin_space()`] to
#' assign fossil occurrences (or localities) to spatial bins/samples using a
#' hexagonal equal-area grid.
#'
#' This function builds a hexagonal grid using the H3 geospatial indexing.
#'
#' @param spacing \code{numeric}. The desired spacing between the center of
#' adjacent cells. This value should be provided in kilometres. Cannot be used
#' if `resolution` is used.
#' @param resolution \code{numeric}. The desired H3 resolution. Must be a whole
#' number between 0 and 15. Cannot be used if `spacing` is used.
#'
#' @details
#' `spacing` represents the distance between the centroid of adjacent cells.
#' Using this distance, the function identifies which resolution is most similar
#' to the input `spacing`, and uses this resolution. Using `resolution` to directly
#' pass an H3 resolution is a way to bypass this step.
#'
#' @return An object of class `palaeoverse_space_bins`. This object can be passed
#' to the `bins` argument in [`bin_space()`] to assign occurrences to these bins.
#' This object has an attribute `h3_resolution` that is used internally and
#' shouldn't be relied upon.
#'
#' @export
#' @examples
#' space_bins(spacing = 1000)
#'
#' space_bins(resolution = 1)
space_bins <- function(spacing, resolution) {
  ensure_args_are_named()
  rlang::check_exclusive(spacing, resolution)

  if (!missing(spacing)) {
    rlang::check_number_decimal(spacing)
    if (spacing <= 0) {
      cli::cli_abort("{.arg spacing} must be greater than 0.")
    }
  } else {
    rlang::check_number_decimal(resolution)
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
  # Get base cells
  out <- h3jsr::cell_to_polygon(input = children, simple = TRUE)

  if (!missing(spacing)) {
    cli::cli_inform(
      c(
        paste0(
          "Average spacing between adjacent cells in the primary grid was set to ",
          round(grid$avg_cendist_km[1], digits = 2),
          " km. "
        ),
        "i" = paste0("\nH3 resolution: ", grid$h3_resolution[1])
      )
    )
  }

  class(out) <- c("palaeoverse_space_bins", class(out))
  attr(out, "h3_resolution") <- grid$h3_resolution

  out
}
