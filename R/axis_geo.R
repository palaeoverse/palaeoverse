#' Add an axis with a geological timescale
#'
#' \code{axis_geo} behaves similarly to \code{\link[graphics]{axis}} in that it
#' adds an axis to the specified side of a base R plot. The main difference is
#' that it also adds a geological timescale between the plot and the axis. The
#' default scale includes international epochs from the the Geological Timescale
#' 2020 (\code{\link{GTS2020}}). However, international stages, periods, eras,
#' and eons are also available. Interval data hosted by
#' [Macrostrat](https://macrostrat.org) are also available (see
#' \code{\link{time_bins}}). A custom interval dataset can also be used (see
#' Details below). The appearance of the axis is highly customisable (see Usage
#' below), with the intent that plots will be publication-ready.
#'
#' If a custom \code{data.frame} is provided (with \code{intervals}), it should
#' consist of at least 3 columns of data. See \code{\link{GTS2020}} for an
#' example.
#' \itemize{
#'   \item The \code{interval_name} column (\code{name} is also allowed) lists
#'         the names of each time interval. These will be used as labels if no
#'         abbreviations are provided.
#'   \item The \code{max_ma} column (\code{max_age} is also allowed) lists the
#'         oldest boundary of each time interval. Values should always be
#'         positive.
#'   \item The \code{min_ma} column (\code{min_age} is also allowed) lists the
#'         youngest boundary of each time interval. Values should always be
#'         positive.
#'   \item The \code{abbr} column is optional and lists abbreviations that may
#'         be used as labels.
#'   \item The \code{colour} column (\code{color} is also allowed) is also
#'         optional and lists a colour for the background for each time interval
#'         (see the Color Specification section
#'         \code{\link[graphics:par]{here}}).
#'   \item The \code{font} (\code{lab_color} is also allowed) column is
#'         also optional and lists a colour for the label for each time interval
#'         (see the Color Specification section
#'         \code{\link[graphics:par]{here}}).
#' }
#'
#' \code{intervals} may also be a list if multiple time scales should be added
#' to a single side of the plot. In this case, \code{height}, \code{fill},
#' \code{lab}, \code{lab_col}, \code{lab_size}, \code{rot}, \code{abbr},
#' \code{center_end_labels}, \code{skip}, \code{bord_col}, \code{lty}, and
#' \code{lwd} can also be lists. If these lists are not as long as
#' \code{intervals}, the elements will be recycled. If individual values
#' (or vectors, e.g. for \code{skip}) are used for these parameters, they will
#' be applied to all time scales (and recycled as necessary). If multiple scales
#' are requested they will be added sequentially outwards starting from the plot
#' border. The axis will always be placed on the outside of the last scale.
#'
#' If you would like to use intervals from the Geological Time Scale 2012
#' (\code{\link{GTS2012}}), you can use \code{\link{time_bins}} and supply the
#' returned \code{data.frame} to the \code{intervals} argument.
#'
#' \code{axis_geo_phylo(...)} is shorthand for
#' \code{axis_geo(..., phylo = TRUE)}.
#'
#' @param side \code{integer}. Which side to add the axis to (\code{1}: bottom,
#'   the default; \code{2}: left; \code{3}: top; \code{4}: right).
#' @param intervals The interval information to use to plot the axis: either A)
#'   a \code{character} string indicating a rank of intervals from the built-in
#'   \code{\link{GTS2020}}, B) a \code{character} string indicating a
#'   \code{data.frame} hosted by [Macrostrat](https://macrostrat.org) (see
#'   \code{\link{time_bins}}), or C) a custom \code{data.frame} of time interval
#'   boundaries (see Details). A list of strings or data.frames can be supplied
#'   to add multiple time scales to the same side of the plot (see Details).
#' @param height \code{numeric}. The relative height (or width if \code{side} is
#'   \code{2} or \code{4}) of the scale. This is relative to the height (if
#'   \code{side} is \code{1} or \code{3}) or width (if \code{side} is \code{2}
#'   or \code{4}) of the plot.
#' @param fill \code{character}. The fill colour of the boxes. The default is to
#'   use the \code{colour} or \code{color} column included in \code{intervals}.
#'   If a custom dataset is provided with \code{intervals} without a
#'   \code{colour} or \code{color} column and without specifying \code{fill}, a
#'   greyscale will be used. Custom fill colours can be provided with this
#'   option (overriding the \code{colour} or \code{color} column) and will be
#'   recycled if/as necessary.
#' @param lab \code{logical}. Should interval labels be included?
#' @param lab_col \code{character}. The colour of the labels. The default is to
#'   use the \code{font} or \code{lab_color} column included in
#'   \code{intervals}. If a custom dataset is provided with \code{intervals}
#'   without a \code{font} or \code{lab_color} column and without
#'   specifying \code{lab_col}, all labels will be black. Custom label colours
#'   can be provided with this option (overriding the \code{font} or
#'   \code{lab_color} column) and will be recycled if/as necessary.
#' @param lab_size \code{numeric}. The size of the labels (see \code{cex} in
#'   \code{\link[graphics:par]{graphics parameters}}).
#' @param rot \code{numeric}. The amount of counter-clockwise rotation to add to
#'   the labels (in degrees). Note, labels for axes added to the left or right
#'   sides are already rotated 90 degrees.
#' @param abbr \code{logical}. Should labels be abbreviated? This only works
#'   if the data has an \code{abbr} column, otherwise the \code{name} column
#'   will be used regardless of this setting.
#' @param center_end_labels \code{logical}. Should labels be centered within the
#'   visible range of intervals at the ends of the axis?
#' @param skip A \code{character} vector of interval names indicating which
#'   intervals should not be labeled. If \code{abbr} is \code{TRUE}, this can
#'   also include interval abbreviations. Quaternary, Holocene, and Late
#'   Pleistocene are skipped by default. Set to NULL if this is not desired.
#' @param bord_col \code{character}. The border colour of the interval boxes.
#' @param lty \code{character}. Line type (see \code{lty} in
#'   \code{\link[graphics:par]{graphics parameters}}). This value (or the last
#'   value if this is a list) will also be passed to
#'   \code{\link[graphics]{axis}}.
#' @param lwd \code{numeric}. Line width (see \code{lwd} in
#'   \code{\link[graphics:par]{graphics parameters}}).
#' @param bkgd \code{character}. The colour of the background of the scale
#'   when no intervals are being shown.
#' @param neg \code{logical}. Set this to \code{TRUE} if your x-axis is using
#'   negative values. If the entire axis is already negative, this will be set
#'   to \code{TRUE} for you.
#' @param exact \code{logical}. Set this to \code{TRUE} if you want axis tick
#'   marks and numeric tick labels placed at the interval boundaries. If
#'   \code{TRUE}, this overrides \code{tick_at} and \code{tick_labels}.
#' @param round \code{integer}. Number of decimal places to which exact axis
#'   labels should be rounded (using \code{\link[base]{round}}). If no value is
#'   specified, the exact values will be used. Trailing zeros are always
#'   removed. \code{tick_at} and \code{tick_labels} can be used to include
#'   labels with trailing zeros.
#' @param tick_at A \code{numeric} vector specifying custom points at which tick
#'   marks are to be drawn on the axis. If specified, this is passed directly to
#'   \code{\link[graphics]{axis}}. If \code{phylo} is \code{TRUE}, these values
#'   are converted as necessary for the phylogenetic axis limits. If this is set
#'   to \code{NULL} (the default) tick mark locations are computed automatically
#'   (see \code{\link[graphics]{axTicks}}).
#' @param tick_labels Either a) a \code{logical} value specifying whether
#'   (numerical) annotations should be made at the tick marks specified by
#'   \code{tick_at}, or b) a custom \code{character} or \code{expression} vector of
#'   labels to be placed at the tick marks. If \code{tick_at} is specified, this
#'   argument is passed directly to \code{\link[graphics]{axis}}.
#' @param phylo \code{logical}. Is the base plot a phylogeny generated by
#'   \code{\link[ape]{plot.phylo}}, \code{\link[phytools]{plotTree}},
#'   \code{\link[phytools]{plotSimmap}}, etc?
#' @param root.time \code{numeric}. If \code{phylo} is \code{TRUE}, this is the
#'   time assigned to the root node of the tree. By default, this is taken from
#'   the \code{root.time} element of the plotted tree.
#' @param ... Further arguments that are passed directly to
#'   \code{\link[graphics]{axis}}.
#'
#' @return No return value. Function is used for its side effect, which is to
#' add an axis of the geological timescale to an already existing plot.
#'
#' @section Authors:
#'   William Gearty & Kilian Eichenseer
#' @section Reviewer:
#'   Lewis A. Jones
#' @importFrom graphics rect text clip axis par segments
#' @importFrom methods is
#' @importFrom ape .PlotPhyloEnv
#' @examples
#' # track user par
#' oldpar <- par(no.readonly = TRUE)
#' # single scale on bottom
#' par(mar = c(6.1, 4.1, 4.1, 2.1)) # modify margin
#' plot(0:100, axes = FALSE, xlim = c(100, 0), ylim = c(100, 0),
#'      xlab = NA, ylab = "Depth (m)")
#' box()
#' axis(2)
#' axis_geo(side = 1, intervals = "period")
#' # the line argument here depends on the absolute size of the plot
#' title(xlab = "Time (Ma)", line = 4)
#'
#' # stack multiple scales, abbreviate only one set of labels
#' par(mar = c(7.1, 4.1, 4.1, 2.1)) # further expand bottom margin
#' plot(0:100, axes = FALSE, xlim = c(100, 0), ylim = c(100, 0),
#'      xlab = NA, ylab = "Depth (m)")
#' box()
#' axis(2)
#' axis_geo(side = 1, intervals = list("epoch", "period"),
#'     abbr = list(TRUE, FALSE))
#' # the line argument here depends on the absolute size of the plot
#' title(xlab = "Time (Ma)", line = 6)
#'
#' # scale with MacroStrat intervals
#' par(mar = c(6.1, 4.1, 4.1, 2.1)) # modify margin
#' plot(0:30, axes = FALSE, xlim = c(30, 0), ylim = c(30, 0),
#'      xlab = NA, ylab = "Depth (m)")
#' box()
#' axis(2)
#' axis_geo(side = 1, intervals = "North American land mammal ages")
#' # the line argument here depends on the absolute size of the plot
#' title(xlab = "Time (Ma)", line = 4)
#'
#' # scale with custom intervals
#' intervals <- data.frame(min_ma = c(0, 10, 25, 32),
#'                         max_ma = c(10, 25, 32, 40),
#'                         interval_name = c("A", "B", "C", "D"))
#' par(mar = c(6.1, 4.1, 4.1, 2.1)) # modify margin
#' plot(0:40, axes = FALSE, xlim = c(40, 0), ylim = c(40, 0),
#'      xlab = NA, ylab = "Depth (m)")
#' box()
#' axis(2)
#' axis_geo(side = 1, intervals = intervals)
#' # the line argument here depends on the absolute size of the plot
#' title(xlab = "Time (Ma)", line = 4)
#'
#' @examplesIf require(phytools)
#' # scale with phylogeny
#' library(phytools)
#' data(mammal.tree)
#' plot(mammal.tree)
#' axis_geo_phylo()
#' title(xlab = "Time (Ma)", line = 4)
#'
#' @examplesIf require(paleotree)
#' # scale with fossil phylogeny
#' library(paleotree)
#' data(RaiaCopesRule)
#' plot(ceratopsianTreeRaia)
#' axis_geo_phylo()
#' title(xlab = "Time (Ma)", line = 4)
#'
#' # reset user par
#' par(oldpar)
#' @export
axis_geo <- function(
    side = 1, intervals = "epoch", height = 0.05,
    # fill arguments:
    fill = NULL,
    # label arguments:
    lab = TRUE, lab_col = NULL, lab_size = 1, rot = 0, abbr = TRUE,
    center_end_labels = TRUE,
    skip = c("Quaternary", "Holocene", "Late Pleistocene"),
    # rect border arguments:
    bord_col = "black", lty = par("lty"), lwd = par("lwd"),
    # applied to the entire axis:
    bkgd = "grey90", neg = FALSE, exact = FALSE, round = FALSE,
    # passed to axis():
    tick_at = NULL, tick_labels = TRUE, phylo = FALSE, root.time = NULL,
    ...
    ) {
  intervals <- make_list(intervals)
  n_scales <- length(intervals)

  if (phylo) {
    # adapted from ape
    lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    type <- lastPP$type
    backward <- lastPP$direction %in% c("leftwards", "downwards")
    if (type == "unrooted")
      stop("axis_geo() not available for unrooted plots;
           try ape::add.scale.bar()")
    if (type %in% c("radial", "fan"))
      stop("axis_geo() not meaningful for radial or fan plots")
    if (is.null(root.time)) root.time <- lastPP$root.time
  }

  height <- rep(make_list(height), length.out = n_scales)
  if (!all(sapply(height, function(x) is.numeric(x) && length(x) == 1))) {
    stop("Invalid value supplied for height, must be a single numeric value
         per scale", call. = FALSE)
  }
  fill <- rep(make_list(fill), length.out = n_scales)
  if (!all(sapply(fill, is_type_or_null, "character"))) {
    stop("Invalid value supplied for fill, must be character (or NULL)",
         call. = FALSE)
  }
  lab <- rep(make_list(lab), length.out = n_scales)
  if (!all(sapply(lab, function(x) is.logical(x) && length(x) == 1))) {
    stop("Invalid value supplied for lab, must be a single logical value per
         scale", call. = FALSE)
  }
  lab_col <- rep(make_list(lab_col), length.out = n_scales)
  if (!all(sapply(lab_col, is_type_or_null, "character"))) {
    stop("Invalid value supplied for lab_col, must be character (or NULL)",
         call. = FALSE)
  }
  lab_size <- rep(make_list(lab_size), length.out = n_scales)
  if (!all(sapply(lab_size, is.numeric))) {
    stop("Invalid value supplied for lab_size, must be numeric", call. = FALSE)
  }
  rot <- rep(make_list(rot), length.out = n_scales)
  if (!all(sapply(rot, function(x) is.numeric(x) && length(x) == 1))) {
    stop("Invalid value supplied for rot, must be a single numeric value per
         scale", call. = FALSE)
  }
  abbr <- rep(make_list(abbr), length.out = n_scales)
  if (!all(sapply(abbr, function(x) is.logical(x) && length(x) == 1))) {
    stop("Invalid value supplied for abbr, must be a single numeric value per
         scale", call. = FALSE)
  }
  skip <- rep(make_list(skip), length.out = n_scales)
  if (!all(sapply(skip, is_type_or_null, "character"))) {
    stop("Invalid value supplied for skip, must be character (or NULL)",
         call. = FALSE)
  }
  center_end_labels <- rep(make_list(center_end_labels), length.out = n_scales)
  if (!all(sapply(center_end_labels,
                  function(x) is.logical(x) && length(x) == 1))) {
    stop("Invalid value supplied for center_end_labels, must be a single logical
         value per scale", call. = FALSE)
  }
  bord_col <- rep(make_list(bord_col), length.out = n_scales)
  if (!all(sapply(bord_col, is_type_or_null, "character"))) {
    stop("Invalid value supplied for bord_col, must be character (or NULL)",
         call. = FALSE)
  }
  lty <- rep(make_list(lty), length.out = n_scales)
  if (!all(sapply(lty, is_type_or_null, "character"))) {
    stop("Invalid value supplied for lty, must be character (or NULL)",
         call. = FALSE)
  }
  lwd <- rep(make_list(lwd), length.out = n_scales)
  if (!all(sapply(lwd, is_type_or_null, "numeric"))) {
    stop("Invalid value supplied for lwd, must be numeric (or NULL)",
         call. = FALSE)
  }

  # get the limits of the plot
  clip_lims <- plot_lims <- par("usr") # x1, x2, y1, y2

  # height should be "negative" if the axis is reversed
  if (side %in% c(1, 3)) {
    if (phylo) {
      x_range <- range(lastPP$xx)
      phylo_lims <- c(max(lastPP$x.lim[1], x_range[1]),
                      min(lastPP$x.lim[2], x_range[2]))
      clip_lims[1:2] <- plot_lims[1:2] <- phylo_lims
    }
    height <- lapply(height, function(ht) {
      abs_ht <- ht * abs(plot_lims[3] - plot_lims[4])
      abs_ht * c(-1, 1)[(plot_lims[3] < plot_lims[4]) + 1]
    })
  } else if (side %in% c(2, 4)) {
    if (phylo) {
      y_range <- range(lastPP$yy)
      phylo_lims <- c(max(lastPP$y.lim[1], y_range[1]),
                      min(lastPP$y.lim[2], y_range[2]))
      clip_lims[3:4] <- plot_lims[3:4] <- phylo_lims
    }
    height <- lapply(height, function(ht) {
      abs_ht <- ht * abs(plot_lims[1] - plot_lims[2])
      abs_ht * c(-1, 1)[(plot_lims[1] < plot_lims[2]) + 1]
    })
  } else {
    stop("Invalid value supplied for side, must be 1, 2, 3, or 4",
         call. = FALSE)
  }
  if (phylo) {
    # adapted (and fixed) from ape
    phylo_scale <- c(0, phylo_lims[2] - phylo_lims[1])
    if (!backward) phylo_scale <- phylo_scale[2:1]
    if (!is.null(root.time)) {
      if (backward) {
        phylo_scale <- phylo_scale + root.time - phylo_scale[2]
      } else {
        phylo_scale <- phylo_scale + root.time - phylo_lims[2]
      }
    }
    phylo_beta <- diff(phylo_lims) / diff(phylo_scale)
    phylo_alpha <- phylo_lims[1] - phylo_beta * phylo_scale[1]
  }

  # expand clipping outside the desired axis
  height_sum <- do.call(sum, height)
  if (side == 1) {
    clip_lims[3] <- plot_lims[3] - height_sum
    clip_lims[4] <- plot_lims[3]
  } else if (side == 2) {
    clip_lims[1] <- plot_lims[1] - height_sum
    clip_lims[2] <- plot_lims[1]
  } else if (side == 3) {
    clip_lims[4] <- plot_lims[4] + height_sum
    clip_lims[3] <- plot_lims[4]
  } else if (side == 4) {
    clip_lims[2] <- plot_lims[2] + height_sum
    clip_lims[1] <- plot_lims[2]
  }
  do.call("clip", as.list(clip_lims))

  # set up the limits of the first scale
  if (side == 1) {
    scale_lims <- c(plot_lims[1], plot_lims[2],
                    plot_lims[3] - height[[1]], plot_lims[3])
  } else if (side == 2) {
    scale_lims <- c(plot_lims[1] - height[[1]], plot_lims[1],
                    plot_lims[3], plot_lims[4])
  } else if (side == 3) {
    scale_lims <- c(plot_lims[1], plot_lims[2],
                    plot_lims[4], plot_lims[4] + height[[1]])
  } else if (side == 4) {
    scale_lims <- c(plot_lims[2], plot_lims[2] + height[[1]],
                    plot_lims[3], plot_lims[4])
  }

  # add a neutral background color in case scales don't span entire axis
  rect(xleft = clip_lims[1], xright = clip_lims[2],
       ytop = clip_lims[3], ybottom = clip_lims[4], col = bkgd, border = NA)

  # add segments to inner side of scale
  if (side == 1) {
    segments(x0 = scale_lims[1], x1 = scale_lims[2], y0 = scale_lims[4],
             col = bord_col[[1]], lty = lty[[1]], lwd = lwd[[1]])
  } else if (side == 2) {
    segments(x0 = scale_lims[2], y0 = scale_lims[3], y1 = scale_lims[4],
             col = bord_col[[1]], lty = lty[[1]], lwd = lwd[[1]])
  } else if (side == 3) {
    segments(x0 = scale_lims[1], x1 = scale_lims[2], y0 = scale_lims[3],
             col = bord_col[[1]], lty = lty[[1]], lwd = lwd[[1]])
  } else if (side == 4) {
    segments(x0 = scale_lims[1], y0 = scale_lims[3], y1 = scale_lims[4],
             col = bord_col[[1]], lty = lty[[1]], lwd = lwd[[1]])
  }

  for (scale in 1:n_scales) {
    # get the requested data if necessary
    scale_intervals <- intervals[[scale]]
    if (!is(scale_intervals, "data.frame")) {
      # remove trailing s for backwards compatibility
      if (sub("s+$", "", scale_intervals) %in% c("period", "epoch", "era",
                                                 "stage", "eon")) {
        scale_intervals <- time_bins(interval = c("Hadean", "Phanerozoic"),
                                     rank = sub("s+$", "", scale_intervals))
      } else {
        scale_intervals <- time_bins(scale = scale_intervals)
      }
    }
    # fix column names if using deeptime data
    colnames(scale_intervals)[colnames(scale_intervals) == "name"] <-
      "interval_name"
    colnames(scale_intervals)[colnames(scale_intervals) == "max_age"] <-
      "max_ma"
    colnames(scale_intervals)[colnames(scale_intervals) == "min_age"] <-
      "min_ma"
    colnames(scale_intervals)[colnames(scale_intervals) == "color"] <-
      "colour"
    colnames(scale_intervals)[colnames(scale_intervals) == "lab_color"] <-
      "font"

    # set `neg` to TRUE if both limits are negative
    if (side %in% c(1, 3) && all(plot_lims[1:2] <= 0)) neg <- TRUE
    if (side %in% c(2, 4) && all(plot_lims[3:4] <= 0)) neg <- TRUE

    # make the min and max values negative if requested
    if (neg) {
      scale_intervals$max_ma <- -1 * (scale_intervals$max_ma)
      scale_intervals$min_ma <- -1 * (scale_intervals$min_ma)
    }
    if (phylo) {
      scale_intervals$max_ma <- phylo_beta * scale_intervals$max_ma +
                                   phylo_alpha
      scale_intervals$min_ma <- phylo_beta * scale_intervals$min_ma +
                                   phylo_alpha
    }
    scale_intervals$mid_ma <-
      (scale_intervals$max_ma + scale_intervals$min_ma) / 2

    scale_fill <- fill[[scale]]
    if (!is.null(scale_fill)) {
      scale_intervals$colour <- rep(scale_fill,
                                   length.out = nrow(scale_intervals))
    } else if (!("colour" %in% colnames(scale_intervals))) {
      scale_intervals$colour <- rep(c("grey60", "grey80"),
                                   length.out = nrow(scale_intervals))
    }
    scale_lab_color <- lab_col[[scale]]
    if (!is.null(scale_lab_color)) {
      scale_intervals$font <- rep(scale_lab_color,
                                       length.out = nrow(scale_intervals))
    } else if (!("font" %in% colnames(scale_intervals))) {
      scale_intervals$font <- "black"
    }
    if (abbr[[scale]] && "abbr" %in% colnames(scale_intervals)) {
      scale_intervals$label <- scale_intervals$abbr
      scale_intervals$label[scale_intervals$abbr %in% skip] <- ""
    } else {
      scale_intervals$label <- scale_intervals$interval_name
    }
    scale_intervals$label[scale_intervals$interval_name %in%
                            skip[[scale]]] <- ""

    # plot the desired polygons in the unclipped margin
    # also add line segments at the ends in case intervals extend beyond axis
    scale_lty <- lty[[scale]]
    scale_lwd <- lwd[[scale]]
    scale_bord_color <- bord_col[[scale]]
    if (side %in% c(1, 3)) {
      rect(xleft = scale_intervals$min_ma, xright = scale_intervals$max_ma,
           ybottom = scale_lims[3], ytop = scale_lims[4],
           col = scale_intervals$colour, border = scale_bord_color,
           lty = scale_lty, lwd = scale_lwd)
      segments(x0 = scale_lims[1:2], y0 = scale_lims[3], y1 = scale_lims[4],
               col = scale_bord_color, lty = scale_lty, lwd = scale_lwd)
    } else {
      rect(ybottom = scale_intervals$min_ma, ytop = scale_intervals$max_ma,
           xleft = scale_lims[1], xright = scale_lims[2],
           col = scale_intervals$colour, border = scale_bord_color,
           lty = scale_lty, lwd = scale_lwd)
      segments(x0 = scale_lims[1], x1 = scale_lims[2], y0 = scale_lims[3:4],
               col = scale_bord_color, lty = scale_lty, lwd = scale_lwd)
    }

    if (lab[[scale]]) {
      # add the desired text in the unclipped margin
      if (center_end_labels[[scale]]) {
        #center the labels for the time periods at the ends of the axis
        if (side %in% c(1, 3)) {
          lims <- c(scale_lims[1], scale_lims[2])
        } else {
          lims <- c(scale_lims[3], scale_lims[4])
        }
        max_end <- (scale_intervals$max_ma > max(lims) &
                      scale_intervals$min_ma < max(lims)) |
                   (scale_intervals$max_ma < max(lims) &
                      scale_intervals$min_ma > max(lims))
        min_end <- (scale_intervals$max_ma > min(lims) &
                      scale_intervals$min_ma < min(lims)) |
                   (scale_intervals$max_ma < min(lims) &
                      scale_intervals$min_ma > min(lims))
        if (any(max_end)) {
          ends <- scale_intervals[max_end, c("min_ma", "max_ma")]
          scale_intervals$mid_ma[max_end] <-
            (ends[ends < max(lims) & ends > min(lims)] + max(lims)) / 2
        }
        if (any(min_end)) {
          ends <- scale_intervals[min_end, c("min_ma", "max_ma")]
          scale_intervals$mid_ma[min_end] <-
            (ends[ends < max(lims) & ends > min(lims)] + min(lims)) / 2
        }
      }
      scale_lab_size <- lab_size[[scale]]
      scale_rot <- rot[[scale]]
      if (side %in% c(1, 3)) {
        text(x = scale_intervals$mid_ma,
             y = (scale_lims[3] + scale_lims[4]) / 2,
             adj = c(0.5, 0.5), labels = scale_intervals$label,
             col = scale_intervals$font, srt = scale_rot,
             cex = scale_lab_size)
      } else {
        text(y = scale_intervals$mid_ma,
             x = (scale_lims[1] + scale_lims[2]) / 2,
             adj = c(0.5, 0.5), labels = scale_intervals$label,
             col = scale_intervals$font, srt = 90 + scale_rot,
             cex = scale_lab_size)
      }
    }

    if (scale < n_scales) {
      # set up scale limits for the next scale
      if (side == 1) {
        scale_lims <- c(scale_lims[1], scale_lims[2],
                        scale_lims[3] - height[[scale + 1]],
                        scale_lims[4] - height[[scale]])
      } else if (side == 2) {
        scale_lims <- c(scale_lims[1] - height[[scale + 1]],
                        scale_lims[2] - height[[scale]],
                        scale_lims[3], scale_lims[4])
      } else if (side == 3) {
        scale_lims <- c(scale_lims[1], scale_lims[2],
                        scale_lims[3] + height[[scale]],
                        scale_lims[4] + height[[scale + 1]])
      } else if (side == 4) {
        scale_lims <- c(scale_lims[1] + height[[scale]],
                        scale_lims[2] + height[[scale + 1]],
                        scale_lims[3], scale_lims[4])
      }
    }
  }
  # add segments to outer side of scale
  if (side == 1) {
    segments(x0 = scale_lims[1], x1 = scale_lims[2], y0 = scale_lims[3],
             col = scale_bord_color, lty = scale_lty, lwd = scale_lwd)
  } else if (side == 2) {
    segments(x0 = scale_lims[1], y0 = scale_lims[3], y1 = scale_lims[4],
             col = scale_bord_color, lty = scale_lty, lwd = scale_lwd)
  } else if (side == 3) {
    segments(x0 = scale_lims[1], x1 = scale_lims[2], y0 = scale_lims[4],
             col = scale_bord_color, lty = scale_lty, lwd = scale_lwd)
  } else if (side == 4) {
    segments(x0 = scale_lims[2], y0 = scale_lims[3], y1 = scale_lims[4],
             col = scale_bord_color, lty = scale_lty, lwd = scale_lwd)
  }

  # after placing the scales, add an axis with ticks and labels
  # use interval boundaries if `exact` is TRUE
  if (exact == TRUE) {
    # Use the interval breaks from the outer-most scale
    tick_at <- unique(c(scale_intervals$min_ma, scale_intervals$max_ma))
    if (phylo) {
      tick_at <- (tick_at - phylo_alpha) / phylo_beta
    }
    if (is.numeric(round)) {
      tick_labels <- round(tick_at, round)
    } else {
      tick_labels <- as.character(tick_at) # removes trailing zeros
    }
  }
  if (phylo) {
    if (is.null(tick_at)) {
      tick_at <- pretty(phylo_scale)
    }
    if (is.logical(tick_labels) && tick_labels) {
      tick_labels <- as.character(tick_at)
    }
    # Convert to ape scale
    tick_at <- phylo_beta * tick_at + phylo_alpha

    # Filter to only ticks within limits
    if (side %in% c(1, 3)) {
      tick_inc <- which(tick_at <= scale_lims[2] & tick_at >= scale_lims[1])
    } else {
      tick_inc <- which(tick_at <= scale_lims[4] & tick_at >= scale_lims[3])
    }
    tick_at <- tick_at[tick_inc]
    tick_labels <- tick_labels[tick_inc]
  }

  # Add axis with ticks
  if (side == 1) {
    axis(side = side, pos = clip_lims[3], at = tick_at, labels = tick_labels,
         lty = lty[[scale]], ...)
  } else if (side == 2) {
    axis(side = side, pos = clip_lims[1], at = tick_at, labels = tick_labels,
         lty = lty[[scale]], ...)
  } else if (side == 3) {
    axis(side = side, pos = clip_lims[4], at = tick_at, labels = tick_labels,
         lty = lty[[scale]], ...)
  } else if (side == 4) {
    axis(side = side, pos = clip_lims[2], at = tick_at, labels = tick_labels,
         lty = lty[[scale]], ...)
  }
  # place an axis label as well?
  # both mtext() and title() use lines instead of coordinates, which makes
  # this tricky/messy...
}

#' @export
#' @rdname axis_geo
axis_geo_phylo <- function(...) {
  axis_geo(..., phylo = TRUE)
}

make_list <- function(x) {
  if (is.list(x) && !is(x, "data.frame")) x else list(x)
}

is_type_or_null <- function(x, type) {
  is(x, type) || is.null(x)
}
