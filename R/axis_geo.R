#' Add an axis with a geological timescale
#'
#' \code{axis_geo} behaves similarly to \code{\link[graphics]{axis}} in that it
#' adds an axis to the specified side of a base R plot. The main difference
#' is that it also adds a geological timescale between the plot and the axis.
#' The default scale includes international \code{\link[deeptime]{periods}} from
#' ICS. However, international \code{\link[deeptime]{epochs}},
#' \code{\link[deeptime]{stages}}, \code{\link[deeptime]{eons}}, and
#' \code{\link[deeptime]{eras}} and any interval data hosted by Macrostrat are
#' also available from the \code{deeptime} package (see
#' \code{\link[deeptime]{getScaleData}}). A custom interval dataset can also
#' be provided (see Details below). The appearance of the axis is highly
#' customizable (see Usage below), with the intent that plots will be
#' publication-ready.
#'
#' If a custom \code{data.frame} is provided (with \code{intervals}), it should
#' consist of at least 3 columns of data. See \code{deeptime::periods} for an
#' example.
#' \itemize{
#'   \item The \code{name} column (\code{interval_name} is also allowed) lists the names of each time interval. These
#'         will be used as labels if no abbreviations are provided.
#'   \item The \code{max_age} column (\code{max_ma} is also allowed) lists the oldest boundary of each time
#'         interval. Values should always be positive.
#'   \item The \code{min_age} column (\code{min_ma} is also allowed) lists the youngest boundary of each time
#'         interval. Values should always be positive.
#'   \item The \code{abbr} column is optional and lists abbreviations that may
#'         be used as labels.
#'   \item The \code{color} column (\code{colour} is also allowed) is also optional and lists a color for the
#'         background for each time interval (see the Color Specification
#'         section \code{\link[graphics:par]{here}}).
#'   \item The \code{lab_color} (\code{lab_colour} is also allowed) column is also optional and lists a color for
#'         the label for each time interval (see the Color Specification section
#'         \code{\link[graphics:par]{here}}).
#' }
#'
#' \code{intervals} may also be a list if multiple time scales should be added
#' to a single side of the plot. In this case, \code{height}, \code{fill},
#' \code{lab}, \code{lab_col}, \code{lab_size}, \code{rot}, \code{abbr},
#' \code{center_end_labels}, \code{skip}, \code{bord_col}, \code{lty}, and
#' \code{lwd} can also be lists. If these lists are not as long as
#' \code{intervals}, the elements will be recycled. If individual values
#' (or vectors, e.g., for \code{skip}) are used for these parameters, they will
#' be applied to all time scales (and recycled as necessary). If multiple scales
#' are requested they will be added sequentially outwards starting from the plot
#' border. The axis will always be placed on the outside of the last scale.
#'
#' @param side \code{integer}. Which side to add the axis to (\code{1}: bottom,
#'   the default; \code{2}: left; \code{3}: top; \code{4}: right).
#' @param intervals The interval information to use to plot the axis: either A)
#'   a \code{character} string indicating a built-in or remotely hosted
#'   \code{data.frame} (see \code{\link[deeptime]{getScaleData}}), or B) a
#'   custom \code{data.frame} of time interval boundaries (see Details).
#' @param height \code{numeric}. The relative height (or width if \code{side} is
#'   \code{2} or \code{4}) of the scale. This is relative to the height (if
#'   \code{side} is \code{1} or \code{3}) or width (if \code{side} is \code{2}
#'   or \code{4}) of the plot.
#' @param fill \code{character}. The fill color of the boxes. The default is to
#'   use the \code{color} column included in \code{intervals}. If a custom
#'   dataset is provided with \code{intervals} without a \code{color} column and
#'   without specifying \code{fill}, a greyscale will be used. Custom fill
#'   colors can be provided with this option (overriding the \code{color}
#'   column) and will be recycled if/as necessary.
#' @param lab \code{logical}. Should interval labels be included?
#' @param lab_col \code{character}. The color of the labels. The default is to
#'   use the \code{lab_color} or \code{lab_colour} column included in
#'   \code{intervals}. If a custom dataset is provided with \code{intervals}
#'   without a \code{lab_color} or \code{lab_colour} column and without
#'   specifying \code{lab_col}, all labels will be black. Custom label colors
#'   can be provided with this option (overriding the \code{lab_color} or
#'   \code{lab_colour} column) and will be recycled if/as necessary.
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
#' @param bord_col \code{character}. The border color of the interval boxes.
#' @param lty \code{character}. Line type (see \code{lty} in
#'   \code{\link[graphics:par]{graphics parameters}}).
#' @param lwd \code{numeric}. Line width (see \code{lwd} in
#'   \code{\link[graphics:par]{graphics parameters}}).
#' @param bkgd \code{character}. The color of the background color of the scale
#'   when no intervals are being shown.
#' @param neg \code{logical}. Set this to \code{TRUE} if your x-axis is using
#'   negative values. If the entire axis is already negative, this will be set
#'   to \code{TRUE} for you.
#' @param exact \code{logical}. Set this to \code{TRUE} if you want axis tick
#'   marks and numeric tick labels placed at the interval boundaries.
#' @param round \code{integer}. Number of decimal places to which exact axis
#'   labels should be rounded (using \code{\link[base]{round}}). If no value is
#'   specified, the exact values will be used. Trailing zeros are always
#'   removed. \code{tick_at} and \code{tick_labels} can be used to include
#'   labels with trailing zeros.
#' @param tick_at A \code{numeric} vector specifying custom points at which tick
#'   marks are to be drawn on the axis. If specified, this is passed directly to
#'   \code{\link[graphics]{axis}}. The default is to compute tick mark locations
#'   automatically (see \code{\link[graphics]{axTicks}}).
#' @param tick_labels Either a) a \code{logical} value specifying whether
#'   (numerical) annotations should be made at the tick marks specified by
#'   \code{at}, or b) a custom \code{character} or \code{expression} vector of
#'   labels to be placed at the tick marks. If \code{at} is specified, this
#'   argument is passed directly to \code{\link[graphics]{axis}}.
#' @param ... Further arguments that are passed directly to
#'   \code{\link[graphics]{axis}}.
#' @section Authors:
#'   William Gearty & Kilian Eichenseer
#' @section Reviewer:
#'   Lewis A. Jones
#' @importFrom graphics rect text clip axis par segments
#' @importFrom deeptime getScaleData
#' @importFrom methods is
#' @export
#' @examples
#' # single scale on bottom
#' par(mar = c(6.1, 4.1, 4.1, 2.1)) # modify margin
#' plot(0:100, axes = FALSE, xlim = c(100, 0), ylim = c(100, 0),
#'      xlab = NA, ylab = "Depth (m)")
#' box()
#' axis(2)
#' axis_geo(side = 1, intervals = "periods")
#' # the line argument here depends on the absolute size of the plot
#' title(xlab = "Time (Ma)", line = 4)
#'
#' # stack multiple scales
#' par(mar = c(7.1, 4.1, 4.1, 2.1)) # further expand bottom margin
#' plot(0:100, axes = FALSE, xlim = c(100, 0), ylim = c(100, 0),
#'      xlab = NA, ylab = "Depth (m)")
#' box()
#' axis(2)
#' axis_geo(side = 1, intervals = list("epochs", "periods"))
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
#' # scale with old GTS intervals
#' par(mar = c(6.1, 4.1, 4.1, 2.1)) # modify margin
#' plot(0:100, axes = FALSE, xlim = c(100, 0), ylim = c(100, 0),
#'      xlab = NA, ylab = "Depth (m)")
#' box()
#' axis(2)
#' axis_geo(side = 1, intervals = time_bins(rank = "period"))
#' # the line argument here depends on the absolute size of the plot
#' title(xlab = "Time (Ma)", line = 4)
#'
#' # scale with custom intervals
#' intervals <- data.frame(min_age = c(0, 10, 25, 32),
#'                         max_age = c(10, 25, 32, 40),
#'                         name = c("A", "B", "C", "D"))
#' par(mar = c(6.1, 4.1, 4.1, 2.1)) # modify margin
#' plot(0:40, axes = FALSE, xlim = c(40, 0), ylim = c(40, 0),
#'      xlab = NA, ylab = "Depth (m)")
#' box()
#' axis(2)
#' axis_geo(side = 1, intervals = intervals)
#' # the line argument here depends on the absolute size of the plot
#' title(xlab = "Time (Ma)", line = 4)
axis_geo <- function(
    side = 1, intervals = "epochs", height = 0.05,
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
    tick_at = NULL, tick_labels = TRUE, ...
    ) {
  intervals <- make_list(intervals)
  n_scales <- length(intervals)

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
    height <- lapply(height, function(ht) {
      abs_ht <- ht * abs(plot_lims[3] - plot_lims[4])
      abs_ht * c(-1, 1)[(plot_lims[3] < plot_lims[4]) + 1]
    })
  } else if (side %in% c(2, 4)) {
    height <- lapply(height, function(ht) {
      abs_ht <- ht * abs(plot_lims[1] - plot_lims[2])
      abs_ht * c(-1, 1)[(plot_lims[1] < plot_lims[2]) + 1]
    })
  } else {
    stop("Invalid value supplied for side, must be 1, 2, 3, or 4",
         call. = FALSE)
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
      scale_intervals <- getScaleData(scale_intervals)
    }
    # fix column names if using palaeoverse data
    colnames(scale_intervals)[colnames(scale_intervals) == "interval_name"] <-
      "name"
    colnames(scale_intervals)[colnames(scale_intervals) == "max_ma"] <-
      "max_age"
    colnames(scale_intervals)[colnames(scale_intervals) == "min_ma"] <-
      "min_age"
    colnames(scale_intervals)[colnames(scale_intervals) == "colour"] <-
      "color"
    colnames(scale_intervals)[colnames(scale_intervals) == "lab_colour"] <-
      "lab_color"

    # set `neg` to TRUE if both limits are negative
    if (side %in% c(1, 3) && all(plot_lims[1:2] <= 0)) neg <- TRUE
    if (side %in% c(2, 4) && all(plot_lims[3:4] <= 0)) neg <- TRUE

    # make the min and max values negative if requested
    if (neg) {
      scale_intervals$max_age <- -1 * (scale_intervals$max_age)
      scale_intervals$min_age <- -1 * (scale_intervals$min_age)
    }
    scale_intervals$mid_age <-
      (scale_intervals$max_age + scale_intervals$min_age) / 2

    scale_fill <- fill[[scale]]
    if (!is.null(scale_fill)) {
      scale_intervals$color <- rep(scale_fill,
                                   length.out = nrow(scale_intervals))
    } else if (!("color" %in% colnames(scale_intervals))) {
      scale_intervals$color <- rep(c("grey60", "grey80"),
                                   length.out = nrow(scale_intervals))
    }
    scale_lab_color <- lab_col[[scale]]
    if (!is.null(scale_lab_color)) {
      scale_intervals$lab_color <- rep(scale_lab_color,
                                       length.out = nrow(scale_intervals))
    } else if (!("lab_color" %in% colnames(scale_intervals))) {
      scale_intervals$lab_color <- "black"
    }
    if (abbr[[scale]] && "abbr" %in% colnames(scale_intervals)) {
      scale_intervals$label <- scale_intervals$abbr
      scale_intervals$label[scale_intervals$abbr %in% skip] <- ""
    } else {
      scale_intervals$label <- scale_intervals$name
    }
    scale_intervals$label[scale_intervals$name %in% skip[[scale]]] <- ""

    # plot the desired polygons in the unclipped margin
    # also add line segments at the ends in case intervals extend beyond axis
    scale_lty <- lty[[scale]]
    scale_lwd <- lwd[[scale]]
    scale_bord_color <- bord_col[[scale]]
    if (side %in% c(1, 3)) {
      rect(xleft = scale_intervals$min_age, xright = scale_intervals$max_age,
           ybottom = scale_lims[3], ytop = scale_lims[4],
           col = scale_intervals$color, border = scale_bord_color,
           lty = scale_lty, lwd = scale_lwd)
      segments(x0 = scale_lims[1:2], y0 = scale_lims[3], y1 = scale_lims[4],
               col = scale_bord_color, lty = scale_lty, lwd = scale_lwd)
    } else {
      rect(ybottom = scale_intervals$min_age, ytop = scale_intervals$max_age,
           xleft = scale_lims[1], xright = scale_lims[2],
           col = scale_intervals$color, border = scale_bord_color,
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
        max_end <- (scale_intervals$max_age > max(lims) &
                      scale_intervals$min_age < max(lims)) |
                   (scale_intervals$max_age < max(lims) &
                      scale_intervals$min_age > max(lims))
        min_end <- (scale_intervals$max_age > min(lims) &
                      scale_intervals$min_age < min(lims)) |
                   (scale_intervals$max_age < min(lims) &
                      scale_intervals$min_age > min(lims))
        if (any(max_end)) {
          ends <- scale_intervals[max_end, c("min_age", "max_age")]
          scale_intervals$mid_age[max_end] <-
            (ends[ends < max(lims) & ends > min(lims)] + max(lims)) / 2
        }
        if (any(min_end)) {
          ends <- scale_intervals[min_end, c("min_age", "max_age")]
          scale_intervals$mid_age[min_end] <-
            (ends[ends < max(lims) & ends > min(lims)] + min(lims)) / 2
        }
      }
      scale_lab_size <- lab_size[[scale]]
      scale_rot <- rot[[scale]]
      if (side %in% c(1, 3)) {
        text(x = scale_intervals$mid_age,
             y = (scale_lims[3] + scale_lims[4]) / 2,
             adj = c(0.5, 0.5), labels = scale_intervals$label,
             col = scale_intervals$lab_color, srt = scale_rot,
             cex = scale_lab_size)
      } else {
        text(y = scale_intervals$mid_age,
             x = (scale_lims[1] + scale_lims[2]) / 2,
             adj = c(0.5, 0.5), labels = scale_intervals$label,
             col = scale_intervals$lab_color, srt = 90 + scale_rot,
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
  if (exact == TRUE && is.null(tick_at)) {
    # Use the interval breaks from the outer-most scale
    tick_at <- unique(c(scale_intervals$min_age, scale_intervals$max_age))
    if (is.numeric(round)) {
      tick_labels <- round(tick_at, round)
    } else {
      tick_labels <- as.character(tick_at) # removes trailing zeros
    }
  }
  if (side == 1) {
    axis(side = side, pos = clip_lims[3], at = tick_at, labels = tick_labels,
         ...)
  } else if (side == 2) {
    axis(side = side, pos = clip_lims[1], at = tick_at, labels = tick_labels,
         ...)
  } else if (side == 3) {
    axis(side = side, pos = clip_lims[4], at = tick_at, labels = tick_labels,
         ...)
  } else if (side == 4) {
    axis(side = side, pos = clip_lims[2], at = tick_at, labels = tick_labels,
         ...)
  }
  # place an axis label as well?
  # both mtext() and title() use lines instead of coordinates, which makes
  # this tricky/messy...
}

make_list <- function(x) {
  if (is.list(x) && !is(x, "data.frame")) x else list(x)
}

is_type_or_null <- function(x, type) {
  is(x, type) || is.null(x)
}
