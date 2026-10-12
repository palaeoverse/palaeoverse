#' Generate H3 spatial bins
#'
#' @description
#' A function to generate spatial hexagonal equal-area grid cells using the [H3 discrete
#' global grid system](https://h3geo.org/). These can then be used in [`bin_space()`] to
#' assign fossil occurrences (or localities) to spatial bins.
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
#' `spacing` represents the distance between the centroids of adjacent cells.
#' Using this distance, the function identifies which resolution is most similar
#' to the input `spacing`, and uses this resolution. Similarly, `area` is
#' compared to the average cell area of each resolution. Using `resolution` to
#' directly pass an H3 resolution is a way to bypass this step.
#'
#' The table below shows, for each H3 resolution, the number of cells covering
#' the globe, their average area (source: <https://h3geo.org/docs/core-library/restable>),
#' and the average spacing between the centroids of adjacent cells. This
#' information is also available via `h3jsr::h3_info_table`.
#'
#' | Resolution | Number of cells | Average area (km²) | Average spacing (km) |
#' |---:|---:|---:|---:|
#' | 0 | 122 | 4,357,449.42 | 1,918.61 |
#' | 1 | 842 | 609,788.44 | 725.17 |
#' | 2 | 5,882 | 86,801.78 | 274.09 |
#' | 3 | 41,162 | 12,393.43 | 103.60 |
#' | 4 | 288,122 | 1,770.35 | 39.16 |
#' | 5 | 2,016,842 | 252.90 | 14.80 |
#' | 6 | 14,117,882 | 36.13 | 5.59 |
#' | 7 | 98,825,162 | 5.16 | 2.11 |
#' | 8 | 691,776,122 | 0.737 | 0.799 |
#' | 9 | 4,842,432,842 | 0.105 | 0.302 |
#' | 10 | 33,897,029,882 | 0.0150 | 0.114 |
#' | 11 | 237,279,209,162 | 0.00215 | 0.0431 |
#' | 12 | 1,660,954,464,122 | 0.000307 | 0.0163 |
#' | 13 | 11,626,681,248,842 | 0.0000439 | 0.00617 |
#' | 14 | 81,386,768,741,882 | 0.00000627 | 0.00234 |
#' | 15 | 569,707,381,193,162 | 0.000000895 | 0.000883 |
#'
#' @return An object of class `palaeoverse_space_bins`. This object can be passed
#' to the `bins` argument in [`bin_space()`] to assign occurrences (or any set of
#' coordinates)  to these bins. This object has an attribute `h3_resolution` that
#' is used internally and shouldn't be relied upon.
#'
#' @seealso [`bin_space()`]
#' @export
#' @examples
#' space_bins(spacing = 1000)
#'
#' space_bins(area = 100000)
#'
#' space_bins(resolution = 1)
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
  } else if (!missing(area)) {
    cli::cli_inform(
      c(
        paste0(
          "Average cell area in the primary grid was set to ",
          round(grid$avg_area_sqkm[1], digits = 2),
          " km². "
        ),
        "i" = paste0("\nH3 resolution: ", grid$h3_resolution[1])
      )
    )
  }

  class(out) <- c("palaeoverse_space_bins", class(out))
  attr(out, "h3_resolution") <- grid$h3_resolution

  return(out)
}
