#' Generate a stratigraphic section plot
#'
#' A function to plot the stratigraphic ranges of fossil taxa from occurrence
#' data.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences containing
#'   at least two columns: names of taxa, and their stratigraphic position (see
#'   `name` and `level` arguments).
#' @param name \code{character}. The name of the column you wish to be treated
#'   as the input names, e.g. "genus" (default).
#' @param level \code{character}. The name of the column you wish to be treated
#'   as the stratigraphic levels associated with each occurrence, e.g. "bed"
#'   (default) or "height". Stratigraphic levels must be \code{numeric}.
#' @param group \code{character}. The name of the column you wish to be treated
#'   as the grouping variable, e.g. "family". If not supplied, all taxa are
#'   treated as a single group.
#' @param certainty \code{character}. The name of the column you wish to be
#'   treated as the information on whether an identification is certain (1) or
#'   uncertain (0). By default (\code{certainty = NULL}), no column name is
#'   provided, and all occurrences are assumed to be certain. In the plot,
#'   certain occurrences will be plotted with a black circle and joined with
#'   solid lines, while uncertain occurrences will be plotted with a white
#'   circle and joined with dashed lines.
#' @param by \code{character}. How should the output be sorted? Either: "FAD"
#'   (first appearance; default), "LAD" (last appearance), or "name"
#'   (alphabetically by taxon names).
#' @param plot_args,x_args,y_args `r lifecycle::badge("deprecated")` Use `plot()` on the
#'   output of this function instead.
#'
#' @return Invisibly returns a \code{data.frame} of the calculated taxonomic
#'   stratigraphic ranges.
#'
#'   The function is usually used for its side effect, which is to create a plot
#'   showing the stratigraphic ranges of taxa in a section, with levels at which
#'   the taxon was sampled indicated with a point.
#'
#' @section Developer(s): Bethany Allen, William Gearty, Lewis A. Jones &
#'   Alexander Dunhill
#' @section Reviewer(s): William Gearty & Lewis A. Jones
#' @importFrom graphics axis par segments plot points box
#'
#' @examples
#' # Load tetrapod dataset
#' data(tetrapods)
#' # Sample tetrapod occurrences
#' tetrapod_names <- tetrapods$accepted_name[1:50]
#' # Simulate bed numbers
#' beds_sampled <- sample.int(n = 10, size = 50, replace = TRUE)
#' # Simulate certainty values
#' certainty_sampled <- sample(x = 0:1, size = 50, replace = TRUE)
#' # Combine into data frame
#' occdf <- data.frame(taxon = tetrapod_names,
#'                     bed = beds_sampled,
#'                     certainty = certainty_sampled)
#' # Plot stratigraphic ranges
#' # Update margins for plotting
#' par(mar = c(12, 5, 2, 2))
#' tax_range_strat(occdf, name = "taxon")
#' tax_range_strat(occdf, name = "taxon", certainty = "certainty",
#'                 plot_args = list(ylab = "Stratigraphic height (m)"))
#' # Plot stratigraphic ranges with more labelling
#' tax_range_strat(occdf, name = "taxon", certainty = "certainty", by = "name",
#'                 plot_args = list(main = "Section A",
#'                                  ylab = "Stratigraphic height (m)"))
#' eras_custom <- data.frame(name = c("Mesozoic", "Cenozoic"),
#'                           max_age = c(0.5, 3.5),
#'                           min_age = c(3.5, 10.5),
#'                           color = c("#67C5CA", "#F2F91D"))
#' axis_geo(side = 4, intervals = eras_custom, tick_labels = FALSE)
#' title(xlab = "Taxon", line = 10.5)
#' # Update margins for plotting
#' par(mar = c(12, 5, 6, 2))
#' # Pull class data
#' occdf$class <- tetrapods$class[1:50]
#' # Group stratigraphic ranges by class
#' tax_range_strat(occdf, name = "taxon", group = "class",
#'                 certainty = "certainty", by = "name",
#'                 plot_args = list(main = "Section A",
#'                                  ylab = "Stratigraphic height (m)"))
#'
#' @export
tax_range_strat <- function(
  occdf,
  name = "genus",
  level = "bed",
  group = NULL,
  certainty = NULL,
  by = "FAD",
  plot_args = deprecated(),
  x_args = deprecated(),
  y_args = deprecated()
) {
  ensure_args_are_named(exceptions = "occdf")

  check_data_frame(occdf)
  check_column_presence(occdf, name)
  check_column_presence(occdf, level)

  check_class(occdf, level, "numeric")
  check_na(occdf, name)
  check_na(occdf, level)

  if (!is.null(group)) {
    check_column_presence(occdf, group)
  }

  if (!is.null(certainty)) {
    check_column_presence(occdf, certainty)
    check_na(occdf, certainty)
  }

  rlang::check_string(by)
  by <- rlang::arg_match(by, values = c("FAD", "LAD", "name"))

  if (lifecycle::is_present(plot_args)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "tax_range_strat(plot_args)",
      I("`plot()` on the output of this function"),
      always = TRUE
    )
  }
  if (lifecycle::is_present(x_args)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "tax_range_strat(x_args)",
      I("`plot()` on the output of this function"),
      always = TRUE
    )
  }
  if (lifecycle::is_present(y_args)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "tax_range_strat(y_args)",
      I("`plot()` on the output of this function"),
      always = TRUE
    )
  }

  # Create pseudo-group if not provided (enable group_apply with no groups)
  if (is.null(group)) {
    occdf$tmp_group <- 1
    g <- "tmp_group"
  } else {
    g <- group
  }
  # Calculate ranges
  ranges <- group_apply(
    occdf,
    group = g,
    fun = function(occdf, name, level) {
      #=== Set-up ===
      unique_taxa <- unique(occdf[, name, drop = TRUE])
      # Order taxa by name
      unique_taxa <- sort(unique_taxa)

      #=== Temporal range ===
      # Generate dataframe for population
      if (is.null(certainty)) {
        ranges <- data.frame(
          taxon = unique_taxa,
          group = NA,
          min_bin = NA,
          max_bin = NA
        )
      } else {
        ranges <- data.frame(
          taxon = unique_taxa,
          group = NA,
          min_bin = NA,
          max_bin = NA,
          min_bin_certain = NA,
          max_bin_certain = NA
        )
      }
      # Run for loop across unique taxa
      for (i in seq_along(unique_taxa)) {
        occ_filter <- occdf[(occdf[, name, drop = TRUE] == unique_taxa[i]), ]
        ranges[i, 3] <- min(occ_filter[level])
        ranges[i, 4] <- max(occ_filter[level])
        if (!is.null(group)) {
          ranges[i, 2] <- occ_filter[1, group]
        }

        # If uncertainty is used, fill second set of columns for certain IDs
        if (!is.null(certainty)) {
          occ_filter <- occ_filter[
            (occ_filter[, certainty, drop = TRUE] == 1),
          ]
          if (nrow(occ_filter) == 0) {
            occ_filter[1, ] <- NA
          }
          ranges[i, 5] <- min(occ_filter[level])
          ranges[i, 6] <- max(occ_filter[level])
        }
      }
      # Should data be ordered by FAD or LAD (already sorted by name)?
      if (by == "FAD") {
        ranges <- ranges[order(ranges$max_bin), ]
        ranges <- ranges[order(ranges$min_bin), ]
      } else if (by == "LAD") {
        ranges <- ranges[order(ranges$min_bin), ]
        ranges <- ranges[order(ranges$max_bin), ]
      }
      # Return dataframe
      ranges
    },
    name = name,
    level = level
  )

  # IDs
  ID <- seq_along(seq_len(nrow(ranges)))
  ranges <- cbind.data.frame(ID, ranges)
  # Remove row names
  row.names(ranges) <- NULL
  # Get labels
  labels <- ranges[, c("taxon", "ID")]
  # Join to occdf
  occdf <- merge(occdf, labels, by.x = name, by.y = "taxon")

  # Obtain uncertain occurrences
  if (!is.null(certainty)) {
    certain <- occdf[(occdf[, certainty, drop = TRUE] != 0), ]
    uncertain <- occdf[(occdf[, certainty, drop = TRUE] == 0), ]
  } else {
    certain <- NULL
    uncertain <- NULL
  }

  # Tidy up
  if (!is.null(group)) {
    ranges$group <- NULL
  }

  class(ranges) <- c("palaeoverse_tax_range_strat", class(ranges))
  attr(ranges, "palaeoverse_tax_range_strat_group") <- group
  attr(ranges, "palaeoverse_tax_range_strat_certainty") <- certainty
  attr(ranges, "palaeoverse_tax_range_strat_certain") <- certain
  attr(ranges, "palaeoverse_tax_range_strat_uncertain") <- uncertain
  attr(ranges, "palaeoverse_tax_range_strat_level") <- level
  attr(ranges, "palaeoverse_tax_range_strat_occdf") <- occdf

  if (isTRUE(plot)) {
    extra_args <- c(plot_args, x_args, y_args)
    if (is.null(extra_args)) {
      plot(ranges)
    } else {
      do.call("plot", c(list(x = ranges), extra_args))
    }
  }

  ranges
}


#' @param x `data.frame`. An object of class `"palaeoverse_tax_range_strat"` created by `tax_range_strat()`.
#' @param ... Extra arguments passed to [`plot()`][base::plot]. The following arguments are
#' already set internally and must not be specified here: `xlim`, `ylim`, `xaxt`, `yaxt`, `yaxs`.
#' @inheritParams lat_bins_area
#' @param main `character`. The plot title.
#' @param col `character`. The colour of the range segments and points.
#' @param bg `character`. The background (fill) colour of the points, only
#'   used for `pch` values 21 to 25.
#' @param pch `numeric`. The symbol used for the first and last appearance
#'   points (see [graphics::points()]).
#' @param cex `numeric`. The size of the points.
#' @param lty `numeric`. The line type of the range segments (see
#'   [graphics::par()]).
#' @param lwd `numeric`. The line width of the range segments.
#' @param axes `logical`. Should the axes be drawn?
#' @param intervals `character`. The time interval information used to
#'   plot the x-axis: either A) a `character` string indicating a rank of
#'   intervals from the built-in [GTS2020], B) a `character`
#'   string indicating a `data.frame` hosted by
#'   [Macrostrat](https://macrostrat.org) (see [time_bins]), or C)
#'   a custom `data.frame` of time interval boundaries (see [axis_geo]
#'   Details). A list of strings or data.frames can be supplied to add
#'   multiple time scales to the same side of the plot (see [axis_geo]
#'   Details). Defaults to `"periods"`.
#'
#' @details Note that the default spacing for the x-axis title may cause it to
#'   overlap with the x-axis tick labels. To avoid this, you can call
#'   [graphics::title()] after running `tax_range_strat()` and specify both
#'   `xlab` and `line` to add the x-axis title farther from the axis (see
#'   examples).
#'
#'   The styling of the points and line segments can be adjusted by supplying
#'   named arguments to `plot_args`. `col` (segment and point color), `lwd`
#'   (segment width), `pch` (point symbol), `bg` (background point color for
#'   some values of `pch`), `lty` (segment line type), and `cex` (point size)
#'   are supported. In the case of a column being supplied to the `certainty`
#'   argument, these arguments may be vectors of length two, in which case the
#'   first value of the vector will be used for the "certain" points and
#'   segments, and the second value of the vector will be used for the
#'   "uncertain" points and segments. If only a single value is supplied, it
#'   will be used for both. The default values for these arguments are as
#'   follows:
#'   - `col` = `c("black", "black")`
#'   - `lwd` = `c(1.5, 1.5)`
#'   - `pch` = `c(19, 21)`
#'   - `bg` = `c("black", "white")`
#'   - `lty` = `c(1, 2)`
#'   - `cex` = `c(1, 1)`
#'
#' @name tax_range_strat
#' @importFrom graphics points strwidth
#' @export
plot.palaeoverse_tax_range_strat <- function(
  x,
  ...,
  intervals = "periods",
  main = "Temporal range of taxa",
  xlab = "",
  ylab = "Bed number",
  col = c("black", "black"),
  bg = c("black", "white"),
  pch = c(19, 21),
  cex = c(1, 1),
  lty = c(1, 2),
  lwd = c(1.5, 1.5),
  font = 3,
  las = 2
) {
  check_forbidden_plot_args(
    ...,
    forbidden = c("y", "xaxs", "axes", "type", "side"),
    class = "palaeoverse_tax_range_strat"
  )

  dots <- list(...)

  cols <- dots[["col"]]
  if (is.null(cols)) {
    cols <- c("black", "black")
  } else {
    cols <- rep_len(cols, 2)
  }
  lwds <- dots[["lwd"]]
  if (is.null(lwds)) {
    lwds <- c(1.5, 1.5)
  } else {
    lwds <- rep_len(lwds, 2)
  }
  pchs <- dots[["pch"]]
  if (is.null(pchs)) {
    pchs <- c(19, 21)
  } else {
    pchs <- rep_len(pchs, 2)
  }
  bgs <- dots[["bg"]]
  if (is.null(bgs)) {
    bgs <- c("black", "white")
  } else {
    bgs <- rep_len(bgs, 2)
  }
  ltys <- dots[["lty"]]
  if (is.null(ltys)) {
    ltys <- c(1, 2)
  } else {
    ltys <- rep_len(ltys, 2)
  }
  cexs <- dots[["cex"]]
  if (is.null(cexs)) {
    cexs <- c(1, 1)
  } else {
    cexs <- rep_len(cexs, 2)
  }

  group <- attr(x, "palaeoverse_tax_range_strat_group")
  certainty <- attr(x, "palaeoverse_tax_range_strat_certainty")
  certain <- attr(x, "palaeoverse_tax_range_strat_certain")
  uncertain <- attr(x, "palaeoverse_tax_range_strat_uncertain")
  level <- attr(x, "palaeoverse_tax_range_strat_level")
  occdf <- attr(x, "palaeoverse_tax_range_strat_occdf")

  do.call(
    plot,
    args = c(
      list(
        x = c(min(x$ID) - 0.5, max(x$ID + 0.5)),
        y = c(min(x$min_bin), max(x$max_bin)),
        axes = FALSE,
        type = "n",
        xaxs = "i",
        xlab = xlab,
        ylab = ylab,
        ...
      )
    )
  )

  # Groups provided?
  if (!is.null(group)) {
    # Calculate plotting values for groups
    sp <- split(x = x, f = x[, group])
    vals_rect <- lapply(sp, function(x) cbind(min(x$ID), max(x$ID)))
    # Define colours
    cols_rect <- rep(c("grey85", "grey95"), times = length(vals_rect) / 2)
    # Run across number of groups
    lapply(seq_along(vals_rect), function(idx) {
      # Add background rectangles
      rect(
        xleft = vals_rect[[idx]][1] - 0.5,
        xright = vals_rect[[idx]][2] + 0.5,
        ybottom = 0,
        ytop = max(x$max_bin) * 2,
        col = cols_rect[idx]
      )
      # Add group labels
      axis(
        3,
        at = ((min(vals_rect[[idx]]) + max(vals_rect[[idx]])) / 2),
        labels = names(vals_rect)[idx],
        tick = TRUE,
        hadj = 0.5,
        gap.axis = 50,
        line = 0,
        las = 1
      )
    })
  }
  # Add segments
  if (is.null(certainty)) {
    segments(
      y0 = x$min_bin,
      y1 = x$max_bin,
      x0 = x$ID,
      x1 = x$ID,
      col = cols[1],
      lwd = lwds[1],
      lty = ltys[1]
    )
  } else {
    segments(
      y0 = x$min_bin,
      y1 = x$max_bin,
      x0 = x$ID,
      x1 = x$ID,
      col = cols[2],
      lty = ltys[2],
      lwds[2]
    )
    segments(
      y0 = x$min_bin_certain,
      y1 = x$max_bin_certain,
      x0 = x$ID,
      x1 = x$ID,
      col = cols[1],
      lty = ltys[1],
      lwd = lwds[1]
    )
  }
  # Add points
  if (is.null(certainty)) {
    points(
      y = occdf[, level, drop = TRUE],
      x = occdf$ID,
      pch = pchs[1],
      col = cols[1],
      bg = bgs[1],
      cex = cexs[1]
    )
  } else {
    points(
      y = certain[, level, drop = TRUE],
      x = certain$ID,
      pch = pchs[1],
      col = cols[1],
      bg = bgs[1],
      cex = cexs[1]
    )
    points(
      y = uncertain[, level, drop = TRUE],
      x = uncertain$ID,
      pch = pchs[2],
      col = cols[2],
      bg = bgs[2],
      cex = cexs[2]
    )
  }
  # Use defaults if not set
  if (!("at" %in% names(dots))) {
    dots$at <- unique(occdf$bed)
  }
  do.call(axis, args = c(list(side = 2), dots))

  # Use defaults if not set
  if (!("at" %in% names(dots))) {
    dots$at <- x$ID
  }
  if (!("labels" %in% names(dots))) {
    dots$labels <- x$taxon
  }

  # Add names
  do.call(axis, args = c(list(side = 1), dots))
  # Add frame
  box()
}
