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
#' @examples
#' space_bins(resolution = 1)
#'
#' space_bins(spacing = 1000)
#'
#' space_bins(area = 100000)
#'
#' @examplesIf requireNamespace("rnaturalearthdata", quietly = TRUE)
#' grid <- space_bins(resolution = 1)
#' plot(grid)
#'
#' plot(grid, main = "H3 grid", subtitles = c("Africa", "Pacific", "Americas"))
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

#' @param x An object of class `"palaeoverse_space_bins"` created by `space_bins()`.
#' @param ... Extra arguments passed to [`plot()`][base::plot]. The following argument is
#' already set internally and must not be specified here: `add`.
#' @param centres `list`. Views to display (at most four). These must be either:
#' - `c(lat = , lon = )`: the whole globe centred on this point;
#' - `c(xmin = , xmax = , ymin = , ymax = )`: a bounding box (in longitude for `x` and latitude for
#'   `y`), centred on its midpoint. `xmax` must be greater than `xmin`, and `ymax` must be greater
#'   than `ymin`.
#'
#' By default, three globes are centred on longitude 0, longitude 180, and -102.
#'
#' @param subtitles `character`. Titles of each globe, in the same order as `centres`. Must be
#' `NULL` (default, no subtitles) or of length equal to the number of elements in `centres`.
#' @param first_layer `sf`, `sfc`, or `sfg`. An object from the package `sf` (e.g. lines, polygons,
#' or points) indicating the first layer to be printed on each globe. Polygons (such as continents)
#' are filled in dark grey to distinguish them from oceans. If unspecified (default), it plots the
#' modern coastlines with `sf::st_geometry(rnaturalearthdata::coastline110)`. Set
#' `first_layer = NULL` to remove this layer.
#' @name space_bins
#' @export
plot.palaeoverse_space_bins <- function(
  x,
  ...,
  centres = list(
    c(lat = 0, lon = 0),
    c(lat = 0, lon = 180),
    c(lat = 23, lon = -102)
  ),
  subtitles = NULL,
  first_layer
) {
  check_forbidden_plot_args(
    ...,
    forbidden = "add",
    class = "palaeoverse_space_bins"
  )
  rlang::check_installed(
    "rnaturalearthdata",
    reason = "to plot the output of `space_bins()`."
  )

  if (!is.list(centres)) {
    cli::cli_abort(
      "Argument {.arg centres} must be a list, not {.obj_type_friendly {centres}}."
    )
  }

  valid_centre <- function(centre) {
    if (!is.numeric(centre) || anyNA(centre) || is.null(names(centre))) {
      return(FALSE)
    }
    nms <- sort(names(centre))
    if (identical(nms, c("lat", "lon"))) {
      abs(centre[["lat"]]) <= 90 && abs(centre[["lon"]]) <= 180
    } else if (identical(nms, c("xmax", "xmin", "ymax", "ymin"))) {
      all(abs(centre[c("xmin", "xmax")]) <= 180) &&
        all(abs(centre[c("ymin", "ymax")]) <= 90)
    } else {
      FALSE
    }
  }

  invalid_centres <- !vapply(centres, valid_centre, logical(1))
  if (any(invalid_centres)) {
    len <- length(invalid_centres)
    cli::cli_abort(c(
      "{cli::qty(len)}Element{?s} {cli::cli_vec(which(invalid_centres))} of {.arg centres} {cli::qty(len)}{?is/are} invalid.",
      "i" = "{cli::qty(len)}{?It/They} must have latitudes between -90 and 90 and longitudes between -180 and 180, and be either:",
      "*" = "a named numeric vector {.code c(lat = , lon = )};",
      "*" = "a named numeric vector {.code c(xmin = , xmax = , ymin = , ymax = )}."
    ))
  }
  bad_bbox <- vapply(
    centres,
    function(centre) {
      length(centre) == 4 &&
        (centre[["xmin"]] >= centre[["xmax"]] ||
          centre[["ymin"]] >= centre[["ymax"]])
    },
    logical(1)
  )
  if (any(bad_bbox)) {
    cli::cli_abort(
      "Bounding boxes in {.arg centres} must have {.code xmin < xmax} and {.code ymin < ymax}."
    )
  }

  if (!(length(centres) %in% 1:4)) {
    cli::cli_abort(
      "Argument {.arg centres} can take 4 points at most (currently it has {length(centres)})."
    )
  }

  if (
    !is.null(subtitles) &&
      (!is.character(subtitles) || length(subtitles) != length(centres))
  ) {
    cli::cli_abort(
      "Argument {.arg subtitles} must be {.code NULL} or a {.cls character} vector of length {length(centres)} (i.e. the number of elements in {.arg centres})."
    )
  }

  # These attributes only exist when this plotting method is called from `plot.palaeoverse_bin_space()`.
  # In this case, this plotting method is also in charge of plotting the filled cells.
  resolution <- attr(x, "h3_resolution")
  filled_cells <- attr(x, "filled_cells")

  # `st_intersection()` doesn't support the `palaeoverse_space_bins` subclass
  class(x) <- setdiff(class(x), "palaeoverse_space_bins")

  if (missing(first_layer)) {
    first_layer <- sf::st_geometry(rnaturalearthdata::coastline110)
  } else if (!is.null(first_layer)) {
    if (!inherits(first_layer, c("sf", "sfc", "sfg"))) {
      cli::cli_abort(
        "{.arg first_layer} must be an object from the package {.pkg sf}, or {.code NULL}."
      )
    }
    # Some polygons (e.g. `rnaturalearthdata::countries110`) are invalid on the sphere, which
    # breaks `st_intersection()`
    first_layer <- sf::st_make_valid(
      sf::st_geometry(first_layer),
      split_crossing_edges = TRUE
    )
  }

  dots <- list(...)
  main <- dots$main
  dots$main <- NULL

  # Outer margin on top for the title of the whole figure, small inner margins to keep globes close
  old_par <- graphics::par(
    oma = c(0, 0, if (is.null(main)) 0 else 2, 0),
    mar = c(0.5, 0.5, if (is.null(subtitles)) 0.5 else 2, 0.5)
  )
  # With three globes, two on top and one centred below
  graphics::layout(switch(
    length(centres),
    `1` = matrix(1),
    `2` = matrix(1:2, nrow = 1),
    `3` = matrix(c(1, 1, 2, 2, 0, 3, 3, 0), nrow = 2, byrow = TRUE),
    `4` = matrix(1:4, nrow = 2, byrow = TRUE)
  ))
  on.exit({
    graphics::layout(1)
    graphics::par(old_par)
  })

  project <- function(geom) {
    sf::st_transform(
      sf::st_intersection(geom, hemisphere),
      crs = paste0("+proj=ortho +lon_0=", centre[1], " +lat_0=", centre[2])
    )
  }

  # Keep only the visible hemisphere before projecting so that the far side isn't drawn. The
  # radius is slightly less than a quarter of the Earth's circumference to stay on the near side.
  for (k in seq_along(centres)) {
    centre <- centres[[k]]

    # length(centre) == 2 means that we need to draw a globe. Otherwise we just need a subset
    # of the grid.
    if (length(centre) == 2) {
      grid <- x
      area <- NULL
      centre <- unname(centre[c("lon", "lat")])
    } else {
      # Densify the edges in planar lon/lat so that they follow parallels rather than great
      # circles once the CRS is set
      area <- sf::st_set_crs(
        sf::st_segmentize(
          sf::st_as_sfc(sf::st_bbox(centre[c("xmin", "ymin", "xmax", "ymax")])),
          dfMaxLength = 1
        ),
        "EPSG:4326"
      )
      # `polygon_to_cells()` only returns cells whose centroid is in `area`, so add the cells of the
      # edges and their neighbours to cover the whole area (`sf::st_cast(area, "POINT")` returns points
      # on the edge of the bbox)
      grid_cells <- c(
        unlist(h3jsr::polygon_to_cells(area, res = resolution)),
        h3jsr::point_to_cell(sf::st_cast(area, "POINT"), res = resolution)
      )
      grid <- h3jsr::cell_to_polygon(
        unique(unlist(h3jsr::get_disk(unique(grid_cells), ring_size = 1))),
        simple = TRUE
      )
      centre <- c(
        mean(centre[c("xmin", "xmax")]),
        mean(centre[c("ymin", "ymax")])
      )
    }

    # We don't want to display cells on the other side of the globe.
    hemisphere <- sf::st_buffer(
      sf::st_sfc(sf::st_point(centre), crs = "EPSG:4326"),
      dist = 0.999 * pi / 2 * 6371008,
      max_cells = 10000
    )
    if (!is.null(area)) {
      hemisphere <- sf::st_intersection(hemisphere, area)
    }

    grid_args <- utils::modifyList(
      list(project(grid), border = "grey80"),
      dots
    )
    if (!is.null(first_layer)) {
      # Draw an invisible grid first so that the grid sets the plot dimensions
      do.call(plot, utils::modifyList(grid_args, list(border = NA)))

      # Then add the first layer, filling polygons (e.g. continents) to distinguish them from oceans
      layer <- project(first_layer)
      is_polygon <- sf::st_is(layer, c("POLYGON", "MULTIPOLYGON"))
      plot(
        layer,
        col = ifelse(is_polygon, "grey65", "black"),
        border = "grey40",
        pch = 16,
        add = TRUE
      )
      grid_args$add <- TRUE
    }
    do.call(plot, grid_args)
    for (i in seq_along(filled_cells$cells)) {
      plot(
        project(filled_cells$cells[[i]]),
        col = filled_cells$col[i],
        add = TRUE
      )
    }

    if (!is.null(subtitles)) {
      graphics::title(main = subtitles[k], line = 0.5)
    }
  }

  if (!is.null(main)) {
    graphics::title(main = main, outer = TRUE)
  }
}
