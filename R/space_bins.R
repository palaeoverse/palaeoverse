#' @export
space_bins <- function(spacing) {
  # This also ensures that there is no partial matching of arg names
  ensure_args_are_named(exceptions = "spacing")

  rlang::check_number_decimal(spacing, min = 0)

  # Generate equal area hexagonal grid
  # Which resolution should be used based on input distance/spacing?
  # Use the h3jsr::h3_info_table to calculate resolution
  grid <- h3jsr::h3_info_table[
    which.min(abs(h3jsr::h3_info_table$avg_cendist_km - spacing)),
  ]
  # Add column grid specification
  grid$grid <- c("primary")

  all_cells <- h3jsr::get_res0()
  # Get children at desired resolution
  children <- h3jsr::get_children(
    h3_address = all_cells,
    res = grid$h3_resolution,
    simple = TRUE
  )
  # Get base cells
  out <- h3jsr::cell_to_polygon(input = children, simple = TRUE)

  class(out) <- c("palaeo_space_bins", class(out))
  attr(out, "spacing") <- spacing

  out
}
