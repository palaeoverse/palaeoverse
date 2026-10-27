#' Calculate the stratigraphic ranges of fossil taxa
#'
#' A function to calculate the stratigraphic ranges of fossil taxa from occurrence
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
#' @param plot_args,x_args,y_args `r lifecycle::badge("deprecated")` These arguments have
#'   been removed. Use `plot()` on the output of this function instead.
#'
#' @return A \code{data.frame} containing the following columns: taxon ID
#'   (`ID`), unique taxa (`taxon`), lowest stratigraphic level of taxon
#'   (`min_bin`), and highest stratigraphic level of taxon (`max_bin`). If
#'   `certainty` is supplied, the lowest and highest stratigraphic levels of
#'   the certain occurrences of each taxon (`min_bin_certain` and
#'   `max_bin_certain`) are also returned.
#'
#'   This \code{data.frame} has attributes starting with
#'   `palaeoverse_tax_range_strat_` that are for internal use only.
#'
#' @details The stratigraphic range(s) of taxa are calculated by extracting
#'   all unique taxa (`name` column) from the input `occdf`, and checking their
#'   lowest and highest stratigraphic levels. If the input data columns
#'   contain NAs, these must be removed prior to function call.
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
#'
#' # Plot stratigraphic ranges
#' ex <- tax_range_strat(occdf, name = "taxon")
#' plot(ex)
#' ex <- tax_range_strat(occdf, name = "taxon", certainty = "certainty")
#' plot(ex, ylab = "Stratigraphic height (m)")
#'
#' # Plot stratigraphic ranges with more labelling
#' ex <- tax_range_strat(occdf, name = "taxon", certainty = "certainty",
#'                       by = "name")
#' eras_custom <- data.frame(name = c("Mesozoic", "Cenozoic"),
#'                           max_age = c(0.5, 3.5),
#'                           min_age = c(3.5, 10.5),
#'                           color = c("#67C5CA", "#F2F91D"))
#' plot(ex, main = "Section A", ylab = "Stratigraphic height (m)",
#'      axis = axis_geo(side = 4, intervals = eras_custom, tick_labels = FALSE))
#'
#' # Update margins for plotting
#' par(mar = c(5, 4, 6, 2))
#' # Pull class data
#' occdf$class <- tetrapods$class[1:50]
#' # Group stratigraphic ranges by class
#' ex <- tax_range_strat(occdf, name = "taxon", group = "class",
#'                       certainty = "certainty", by = "name")
#' plot(ex, main = "Section A", ylab = "Stratigraphic height (m)")
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

  if (
    lifecycle::is_present(x_args) ||
      lifecycle::is_present(y_args) ||
      lifecycle::is_present(plot_args)
  ) {
    cli::cli_abort(
      c(
        "The arguments {.arg x_args}, {.arg y_args}, and {.arg plot_args} have been removed in {.pkg palaeoverse} 2.0.0.",
        "i" = "Use {.fn plot} on the output of this function instead."
      )
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

  # Tidy up
  if (!is.null(group)) {
    ranges$group <- NULL
  }

  class(ranges) <- c("palaeoverse_tax_range_strat", class(ranges))
  attr(ranges, "palaeoverse_tax_range_strat_group") <- group
  attr(ranges, "palaeoverse_tax_range_strat_level") <- level
  attr(ranges, "palaeoverse_tax_range_strat_certainty") <- certainty
  attr(ranges, "palaeoverse_tax_range_strat_occdf") <- occdf[,
    c("ID", level, certainty)
  ]

  ranges
}


#' @param x `data.frame`. An object of class `"palaeoverse_tax_range_strat"`
#'   created by `tax_range_strat()`.
#' @param ... Extra graphical parameters (see Details). The following
#'   arguments are already set internally and must not be specified here:
#'   `y`, `axes`, `type`, `xaxs`.
#' @inheritParams lat_bins_area
#' @param main `character`. The plot title.
#' @param col.group `character`. The background colours to use when `x` has
#'   groups. Must be either a single colour used for all groups, or of the
#'   same length as the number of unique groups. If `NULL`, the group
#'   backgrounds are not coloured.
#' @param axis A function call drawing the y-axis, such as [graphics::axis()].
#'   If `NULL` (default), the y-axis has a tick at each sampled stratigraphic
#'   level.
#'
#' @details
#' The graphical parameters are reset when `plot()` exits. See 'Examples' to
#' know how to add components to the plot.
#'
#' The styling of the points and line segments can be adjusted by supplying
#' named arguments to `plot()`. `col` (segment and point color), `lwd`
#' (segment width), `pch` (point symbol), `bg` (background point color for
#' some values of `pch`), `lty` (segment line type), and `cex` (point size)
#' are supported.
#'
#' If `x` contains information on certainty, these arguments may be vectors
#' of length two, in which case the first value of the vector will be used
#' for the "certain" points and segments, and the second value of the vector
#' will be used for the "uncertain" points and segments. If only a single
#' value is supplied, it will be used for both.
#'
#' @return `plot()` invisibly returns a list with the graphical parameters
#'   `mar` and `usr` used to draw the plot (see [graphics::par()]).
#'
#' @name tax_range_strat
#' @importFrom graphics strwidth
#' @export
plot.palaeoverse_tax_range_strat <- function(
  x,
  ...,
  axis = NULL,
  main = "Stratigraphic range of taxa",
  xlab = "Taxon",
  ylab = "Bed number",
  col.group = c("grey85", "grey95")
) {
  check_forbidden_plot_args(
    ...,
    forbidden = c("y", "axes", "type", "xaxs"),
    class = "palaeoverse_tax_range_strat"
  )

  ranges <- x
  plot_args <- list(...)
  group <- attr(x, "palaeoverse_tax_range_strat_group")
  level <- attr(x, "palaeoverse_tax_range_strat_level")
  certainty <- attr(x, "palaeoverse_tax_range_strat_certainty")
  occdf <- attr(x, "palaeoverse_tax_range_strat_occdf")
  unique_levels <- unique(occdf[, level, drop = TRUE])

  axis_quo <- rlang::enquo(axis)
  if (rlang::quo_is_null(axis_quo)) {
    axis_quo <- rlang::quo(graphics::axis(side = 2, at = unique_levels))
  }
  axis_expr <- rlang::quo_get_expr(axis_quo)
  if (!rlang::is_call(axis_expr)) {
    cli::cli_abort(
      "{.arg axis} must be a function call, not {.code {rlang::expr_deparse(axis_expr)}}."
    )
  }

  # Early check so that we can throw an error early instead of having some elements
  # being plotted and then throw an error. The `axis` expression is evaluated
  # on a throwaway device so that the promise itself is only forced on the final plot.
  caller_env <- rlang::current_env()
  tryCatch(
    {
      cur_dev <- grDevices::dev.cur()
      grDevices::pdf(NULL)
      plot.new()
      axis
    },
    error = function(e) {
      cli::cli_abort(
        "Error in {.code {rlang::expr_deparse(axis_expr)}}.",
        parent = e,
        call = caller_env
      )
    },
    finally = {
      grDevices::dev.off()
      if (cur_dev > 1) grDevices::dev.set(cur_dev)
    }
  )

  if (!is.null(group)) {
    n_groups <- length(unique(ranges[[group]]))
    if (
      !missing(col.group) &&
        !is.null(col.group) &&
        !length(col.group) %in% c(1, n_groups)
    ) {
      cli::cli_abort(
        "Length of {.arg col.group} ({.val {length(col.group)}}) must be 1 or equal to the number of unique groups in the data ({.val {n_groups}})."
      )
    }
  }

  # Obtain uncertain occurrences
  if (!is.null(certainty)) {
    certain <- occdf[(occdf[, certainty, drop = TRUE] != 0), ]
    uncertain <- occdf[(occdf[, certainty, drop = TRUE] == 0), ]
  }

  # Create plot
  # use defaults if not set
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
  # Collect usr par for resetting
  usrpar <- par(no.readonly = TRUE)
  on.exit(par(usrpar))
  # Estimate max label width
  max_label_width <- max(strwidth(ranges$taxon, units = "inches", font = 3))
  # Convert inches to lines (approximate conversion factor: 0.2), labels are
  # rotated by 45 degrees
  extra_margin <- max_label_width * sin(pi / 4) / 0.2
  # Update bottom margin (add extra space, default is 5) and top margin to fit
  # the group labels below the title
  extra_top <- if (is.null(group)) 0 else 1
  par(mar = usrpar$mar + c(extra_margin, 0, extra_top, 0))
  do.call(
    plot,
    args = c(
      list(
        x = c(min(ranges$ID) - 0.5, max(ranges$ID + 0.5)),
        y = c(min(ranges$min_bin), max(ranges$max_bin)),
        axes = FALSE,
        type = "n",
        xaxs = "i",
        xlab = NA,
        ylab = ylab
      ),
      plot_args
    )
  )
  title(main = main, line = if (is.null(group)) NA else 2.5)
  # Groups provided?
  if (!is.null(group)) {
    # Calculate plotting values for groups
    s <- split(x = ranges, f = ranges[, group])
    vals_rect <- lapply(s, function(x) cbind(min(x$ID), max(x$ID)))
    # Define colours
    cols_rect <- if (is.null(col.group)) {
      NA
    } else {
      rep_len(col.group, length(vals_rect))
    }
    # Run across number of groups
    lapply(seq_along(vals_rect), function(x) {
      # Add background rectangles
      rect(
        xleft = vals_rect[[x]][1] - 0.5,
        xright = vals_rect[[x]][2] + 0.5,
        ybottom = 0,
        ytop = max(ranges$max_bin) * 2,
        col = cols_rect[x]
      )
      # Add group labels
      axis(
        3,
        at = ((min(vals_rect[[x]]) + max(vals_rect[[x]])) / 2),
        labels = names(vals_rect)[x],
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
  # Add frame
  box()
  # Drawn after the frame so that it is on top of it
  rlang::eval_tidy(axis_quo)
  # Diagonal taxon labels, ending at their tick mark
  axis(1, at = ranges$ID, labels = FALSE)
  label_offset <- diff(grconvertY(
    c(0, par("mgp")[2] * par("csi")),
    from = "inches",
    to = "user"
  ))
  text(
    x = ranges$ID,
    y = par("usr")[3] - label_offset,
    labels = ranges$taxon,
    font = 3,
    srt = 45,
    adj = c(1, 1),
    xpd = NA
  )
  title(xlab = xlab, line = 2 + extra_margin)
  # Settings needed to add elements to the plot, `par` is reset on exit
  invisible(par(c("mar", "usr")))
}
