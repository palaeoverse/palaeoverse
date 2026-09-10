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
#' @param plot_args A list of optional arguments that are passed directly to
#'   [graphics::plot()]. Subsets of these arguments are also passed to
#'   [graphics::segments()] and [graphics::points()] (see Details). Useful
#'   arguments include `xlab` (the x-axis label), `ylab` (the y-axis label,
#'   default is "Bed number"), `main` (the plot title), `xlim` (the x-axis
#'   limits), and `ylim` (the y-axis limits). The `axes` and `type` arguments
#'   are not supported and will be overridden.
#' @param x_args A list of optional arguments that are passed directly to
#'   [axis()] when generating the x-axis. Useful arguments include `font` (e.g.,
#'   `3` is italic) and `las` (label orientation). The `side` argument is not
#'   supported and will be overridden. If the `at` and `labels` arguments are
#'   not specified, the x-axis tick labels will be set to the taxon names.
#' @param y_args A list of optional arguments that are passed directly to
#'   [axis()] when generating the y-axis. Useful arguments include `font` (e.g.,
#'   `3` is italic) and `las` (label orientation). The `side` argument is not
#'   supported and will be overridden. If the `at` argument is not specified, it
#'   will be set to a vector of the unique values from the `level` column.
#'
#' @return Invisibly returns a \code{data.frame} of the calculated taxonomic
#'   stratigraphic ranges.
#'
#'   The function is usually used for its side effect, which is to create a plot
#'   showing the stratigraphic ranges of taxa in a section, with levels at which
#'   the taxon was sampled indicated with a point.
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
#' # Plot a separate section per class, using group_apply()
#' # Pull class data
#' occdf$class <- tetrapods$class[1:50]
#' invisible(group_apply(occdf = occdf, group = "class",
#'                       fun = tax_range_strat, name = "taxon",
#'                       certainty = "certainty", by = "name"))
#'
#' @export
tax_range_strat <- function(
  occdf,
  name = "genus",
  level = "bed",
  certainty = NULL,
  by = "FAD",
  plot_args = NULL,
  x_args = NULL,
  y_args = NULL
) {
  ensure_args_are_named(exceptions = "occdf")

  check_data_frame(occdf)
  check_column_presence(occdf, name)
  check_column_presence(occdf, level)

  check_class(occdf, level, "numeric")
  check_na(occdf, name)
  check_na(occdf, level)

  if (!is.null(certainty)) {
    check_column_presence(occdf, certainty)
    check_na(occdf, certainty)
  }

  rlang::check_string(by)
  by <- rlang::arg_match(by, values = c("FAD", "LAD", "name"))

  #=== Set-up ===
  unique_taxa <- unique(occdf[, name, drop = TRUE])
  # Order taxa by name
  unique_taxa <- sort(unique_taxa)

  #=== Stratigraphic range ===
  # Generate dataframe for population
  if (is.null(certainty)) {
    ranges <- data.frame(
      taxon = unique_taxa,
      min_bin = NA,
      max_bin = NA
    )
  } else {
    ranges <- data.frame(
      taxon = unique_taxa,
      min_bin = NA,
      max_bin = NA,
      min_bin_certain = NA,
      max_bin_certain = NA
    )
  }
  # Run for loop across unique taxa
  for (i in seq_along(unique_taxa)) {
    occ_filter <- occdf[(occdf[, name, drop = TRUE] == unique_taxa[i]), ]
    ranges[i, "min_bin"] <- min(occ_filter[level])
    ranges[i, "max_bin"] <- max(occ_filter[level])

    # If uncertainty is used, fill second set of columns for certain IDs
    if (!is.null(certainty)) {
      occ_filter <- occ_filter[
        (occ_filter[, certainty, drop = TRUE] == 1),
      ]
      if (nrow(occ_filter) == 0) {
        occ_filter[1, ] <- NA
      }
      ranges[i, "min_bin_certain"] <- min(occ_filter[level])
      ranges[i, "max_bin_certain"] <- max(occ_filter[level])
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
  }

  #=== Plotting ===
  # Create plot
  dump <- c("x", "y", "axes", "type")
  if (any(names(plot_args) %in% dump)) {
    plot_args <- plot_args[-which(names(plot_args) %in% dump)]
  }
  # use defaults if not set
  if (!("xlab" %in% names(plot_args))) {
    plot_args$xlab <- ""
  }
  if (!("ylab" %in% names(plot_args))) {
    plot_args$ylab <- "Bed number"
  }
  cols <- plot_args$col
  if (is.null(cols)) {
    cols <- c("black", "black")
  } else {
    cols <- rep_len(cols, 2)
  }
  lwds <- plot_args$lwd
  if (is.null(lwds)) {
    lwds <- c(1.5, 1.5)
  } else {
    lwds <- rep_len(lwds, 2)
  }
  pchs <- plot_args$pch
  if (is.null(pchs)) {
    pchs <- c(19, 21)
  } else {
    pchs <- rep_len(pchs, 2)
  }
  bgs <- plot_args$bg
  if (is.null(bgs)) {
    bgs <- c("black", "white")
  } else {
    bgs <- rep_len(bgs, 2)
  }
  ltys <- plot_args$lty
  if (is.null(ltys)) {
    ltys <- c(1, 2)
  } else {
    ltys <- rep_len(ltys, 2)
  }
  cexs <- plot_args$cex
  if (is.null(cexs)) {
    cexs <- c(1, 1)
  } else {
    cexs <- rep_len(cexs, 2)
  }
  do.call(
    plot,
    args = c(
      list(
        x = c(min(ranges$ID) - 0.5, max(ranges$ID + 0.5)),
        y = c(min(ranges$min_bin), max(ranges$max_bin)),
        axes = FALSE,
        type = "n",
        xaxs = "i"
      ),
      plot_args
    )
  )
  # Add segments
  if (is.null(certainty)) {
    segments(
      y0 = ranges$min_bin,
      y1 = ranges$max_bin,
      x0 = ranges$ID,
      x1 = ranges$ID,
      col = cols[1],
      lwd = lwds[1],
      lty = ltys[1]
    )
  } else {
    segments(
      y0 = ranges$min_bin,
      y1 = ranges$max_bin,
      x0 = ranges$ID,
      x1 = ranges$ID,
      col = cols[2],
      lty = ltys[2],
      lwds[2]
    )
    segments(
      y0 = ranges$min_bin_certain,
      y1 = ranges$max_bin_certain,
      x0 = ranges$ID,
      x1 = ranges$ID,
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
  # Plot y-axis
  if ("side" %in% names(y_args)) {
    y_args <- y_args[-which(names(y_args) == "side")]
  }
  # Use defaults if not set
  if (!("at" %in% names(y_args))) {
    y_args$at <- unique(occdf$bed)
  }
  do.call(axis, args = c(list(side = 2), y_args))
  # Plot x-axis
  if ("side" %in% names(x_args)) {
    x_args <- x_args[-which(names(x_args) == "side")]
  }
  # Use defaults if not set
  if (!("font" %in% names(x_args))) {
    x_args$font <- 3
  }
  if (!("las" %in% names(x_args))) {
    x_args$las <- 2
  }
  if (!("at" %in% names(x_args))) {
    x_args$at <- ranges$ID
  }
  if (!("labels" %in% names(x_args))) {
    x_args$labels <- ranges$taxon
  }
  # Add names
  do.call(axis, args = c(list(side = 1), x_args))
  # Add frame
  box()
  # Return invisibly (still unsure about this)
  invisible(ranges)
}
