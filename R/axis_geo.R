#' Add an axis with a geological timescale
#'
#' \code{axis_geo} behaves similarly to \code{\link[graphics]{axis}} in that it
#' adds an axis to the specified side of of base R plot. The main difference
#' is that it also adds a geological timescale between the plot and the axis.
#'
#' If a custom data.frame is provided (with \code{dat}), it should consist of at least 3 columns of data. See \code{deeptime::periods} for an example.
#' \itemize{
#'   \item The \code{name} column lists the names of each time interval. These will be used as labels if no abbreviations are provided.
#'   \item The \code{max_age} column lists the oldest boundary of each time interval.
#'   \item The \code{min_age} column lists the youngest boundary of each time interval.
#'   \item The \code{abbr} column is optional and lists abbreviations that may be used as labels.
#'   \item The \code{color} column is also optional and lists a  for the background for each time interval (see the Color Specification section \code{\link[graphics:par]{here}}).
#'   \item The \code{lab_color} column is also optional and lists a color for the label for each time interval (see the Color Specification section \code{\link[graphics:par]{here}}).
#' }
#'
#' \code{dat} may also be a list if multiple time scales should be added to the plot.
#' In this case, \code{height}, \code{fill}, \code{lab}, \code{lab_color}, \code{size},
#' \code{rot}, \code{abbrv}, \code{center_end_labels}, \code{skip}, \code{bord_color},
#' \code{lty}, and \code{lwd} can also be lists.
#' If these lists are not as long as \code{dat}, the elements will be recycled.
#' If individual values (or vectors, e.g., for \code{skip}) are used for these parameters, they will be applied to all time scales (and recycled as necessary).
#' @param side Which side to add the axis to (\code{1}: bottom, \code{2}: left, \code{3}: top, \code{4}: right).
#' @param dat Either A) a string indicating a built-in dataframe with interval data from the ICS ("periods", "epochs", "stages", "eons", or "eras"),
#'   B) a string indicating a timescale from macrostrat (see list here: \url{https://macrostrat.org/api/defs/timescales?all}),
#'   or C) a custom data.frame of time interval boundaries (see Details).
#' @param height The relative height (or width if \code{side} is \code{2} or \code{4}) of the scale.
#'   This is relative to the height (if \code{side} is \code{1} or \code{3}) or
#'   width (if \code{side} is \code{2} or \code{4}) of the plot.
#' @param fill The fill color of the boxes. The default is to use the \code{color} column included in \code{dat}.
#'   If a custom dataset is provided with \code{dat} without a \code{color} column and without fill, a greyscale will be used.
#'   Custom fill colors can be provided with this option (overriding the \code{color} column) and will be recycled if/as necessary.
#' @param lab Whether to include interval labels.
#' @param lab_color The color of the labels. The default is to use the \code{lab_color} column included in \code{dat}.
#'   If a custom dataset is provided with \code{dat} without a \code{lab_color} column and without fill, all labels will be black.
#'   Custom label colors can be provided with this option (overriding the \code{lab_color} column) and will be recycled if/as necessary.
#' @param size Label size (see \code{cex} in \code{\link[graphics:par]{graphics parameters}}).
#' @param rot The amount of counter-clockwise rotation to add to the labels (in degrees).
#'   Note, labels for axes added to the left or right sides are already rotated 90 degrees.
#' @param abbrv If including labels, whether to use abbreviations instead of full interval names.
#'   This only works if the data has an \code{abbr} column, otherwise the
#'   \code{name} column will be used regardless of this setting.
#' @param center_end_labels Should labels be centered within the visible range of intervals at the ends of the axis?
#' @param skip A vector of interval names indicating which intervals should not be labeled.
#'   If \code{abbrv} is \code{TRUE}, this can also include interval abbreviations.
#' @param bord_color The border color of the interval boxes.
#' @param lty Line type (see \code{lty} in \code{\link[graphics:par]{graphics parameters}}).
#' @param lwd Line width (see \code{lwd} in \code{\link[graphics:par]{graphics parameters}}).
#' @param neg Set this to true if your x-axis is using negative values.
#' @param ... Further arguments to pass to \code{\link[graphics]{axis}}.
#' @importFrom graphics rect text clip axis
#' @importFrom deeptime getScaleData
#' @export
#' @examples
#' #single scale on bottom
#' par(mar = c(5.1, 4.1, 4.1, 2.1)) # default margins
#' plot(0:100, axes = FALSE, xlim = c(100, 0), ylim = c(100, 0), xlab = NA, ylab = "Depth (m)")
#' box()
#' axis(2)
#' axis_geo(side = 1, dat = "periods")
#'
#' #stack multiple scales
#' par(mar = c(6.6, 4.1, 4.1, 2.1)) # expanded bottom margin
#' plot(0:100, axes = FALSE, xlim = c(100, 0), ylim = c(100, 0), xlab = NA, ylab = "Depth (m)")
#' box()
#' axis(2)
#' axis_geo(side = 1, dat = list("epochs", "periods"))
axis_geo <- function(side = 1, dat = "epochs", height = 0.05,
                     fill = NULL, # fill arguments
                     lab = TRUE, lab_color = NULL, size = 1, rot = 0, abbrv = TRUE, # label arguments
                     center_end_labels = FALSE, skip = c("Quaternary", "Holocene", "Late Pleistocene"),
                     bord_color = "black", lty = par("lty"), lwd = par("lwd"), # rect border arguments
                     neg = FALSE,
                     at = NULL, ...) { # passed on to axis()
  dat <- make_list(dat)
  n_scales <- length(dat)

  height <- rep(make_list(height), length.out = n_scales)
  fill <- rep(make_list(fill), length.out = n_scales)
  lab <- rep(make_list(lab), length.out = n_scales)
  lab_color <- rep(make_list(lab_color), length.out = n_scales)
  size <- rep(make_list(size), length.out = n_scales)
  rot <- rep(make_list(rot), length.out = n_scales)
  abbrv <- rep(make_list(abbrv), length.out = n_scales)
  skip <- rep(make_list(skip), length.out = n_scales)
  center_end_labels <- rep(make_list(center_end_labels), length.out = n_scales)
  bord_color <- rep(make_list(bord_color), length.out = n_scales)
  lty <- rep(make_list(lty), length.out = n_scales)
  lwd <- rep(make_list(lwd), length.out = n_scales)

  # get the limits of the plot
  clip_lims <- plot_lims <- par("usr") # x1, x2, y1, y2

  # height should be "negative" if the axis is reversed
  if (side %in% c(1,3)) {
    height <- lapply(height, function(ht) {
      abs_ht <- ht * abs(plot_lims[3] - plot_lims[4])
      abs_ht * c(-1, 1)[(plot_lims[3] < plot_lims[4]) + 1]
    })
  } else if (side %in% c(2,4)) {
    height <- lapply(height, function(ht) {
      abs_ht <- ht * abs(plot_lims[1] - plot_lims[2])
      abs_ht * c(-1, 1)[(plot_lims[1] < plot_lims[2]) + 1]
    })
  } else {
    stop("Invalid value supplied for side, must be 1, 2, 3, or 4")
  }

  # expand clipping outside the desired axis
  height_sum <- do.call(sum, height)
  if (side == 1) {
    clip_lims[3] <- plot_lims[3] - height_sum
  } else if (side == 2) {
    clip_lims[1] <- plot_lims[1] - height_sum
  } else if (side == 3) {
    clip_lims[4] <- plot_lims[4] + height_sum
  } else if (side == 4) {
    clip_lims[2] <- plot_lims[2] + height_sum
  }
  do.call("clip", as.list(clip_lims))

  # set up the limits of the first scale
  if (side == 1) {
    scale_lims <- c(plot_lims[1], plot_lims[2],
                    plot_lims[3], plot_lims[3] - height[[1]])
  } else if (side == 2) {
    scale_lims <- c(plot_lims[1], plot_lims[1] - height[[1]],
                    plot_lims[3], plot_lims[4])
  } else if (side == 3) {
    scale_lims <- c(plot_lims[1], plot_lims[2],
                    plot_lims[4], plot_lims[4] + height[[1]])
  } else if (side == 4) {
    scale_lims <- c(plot_lims[2], plot_lims[2] + height[[1]],
                    plot_lims[3], plot_lims[4])
  }

  for (scale in 1:n_scales) {
    # get the requested data if necessary
    scale_dat <- dat[[scale]]
    if (!is(scale_dat, "data.frame")) {
      scale_dat <- getScaleData(scale_dat)
    }

    # subset the data to only those that will fit on the axis?
    dat_sub <- dat

    # make the min and max values negative if requested
    if (neg) {
      scale_dat$max_age <- -1 * (scale_dat$max_age)
      scale_dat$min_age <- -1 * (scale_dat$min_age)
    }
    scale_dat$mid_age <- (scale_dat$max_age + scale_dat$min_age)/2

    scale_fill <- fill[[scale]]
    if (!is.null(scale_fill)) {
      scale_dat$color <- rep(scale_fill, length.out = nrow(scale_dat))
    } else if (!("color" %in% colnames(scale_dat))) {
      scale_dat$color <- rep(c("grey60","grey80"), length.out = nrow(scale_dat))
    }
    scale_lab_color <- lab_color[[scale]]
    if (!is.null(scale_lab_color)) {
      scale_dat$lab_color <- rep(scale_lab_color, length.out = nrow(scale_dat))
    } else if (!("lab_color" %in% colnames(scale_dat))) {
      scale_dat$lab_color <- "black"
    }
    if (abbrv[[scale]] & "abbr" %in% colnames(scale_dat)) {
      scale_dat$label <- scale_dat$abbr
      scale_dat$label[scale_dat$abbr %in% skip] <- ""
    } else {
      scale_dat$label <- scale_dat$name
    }
    scale_dat$label[scale_dat$name %in% skip[[scale]]] <- ""

    # plot the desired polygons in the unclipped margin
    scale_lty <- lty[[scale]]
    scale_lwd <- lwd[[scale]]
    scale_bord_color <- bord_color[[scale]]
    if (side %in% c(1,3)) {
      rect(xleft = scale_dat$min_age, xright = scale_dat$max_age,
           ybottom = scale_lims[3], ytop = scale_lims[4],
           col = scale_dat$color, border = scale_bord_color,
           lty = scale_lty, lwd = scale_lwd)
    } else {
      rect(ybottom = scale_dat$min_age, ytop = scale_dat$max_age,
           xleft = scale_lims[1], xright = scale_lims[2],
           col = scale_dat$color, border = scale_bord_color,
           lty = scale_lty, lwd = scale_lwd)
    }

    if (lab[[scale]]) {
      # add the desired text in the unclipped margin
      if (center_end_labels[[scale]]) {
        #center the labels for the time periods at the ends of the axis
        if (side %in% c(1,3)) {
          lims <- c(scale_lims[1], scale_lims[2])
        } else {
          lims <- c(scale_lims[3], scale_lims[4])
        }
        max_end <- (scale_dat$max_age > max(lims) & scale_dat$min_age < max(lims)) |
          (scale_dat$max_age < max(lims) & scale_dat$min_age > max(lims))
        min_end <- (scale_dat$max_age > min(lims) & scale_dat$min_age < min(lims)) |
          (scale_dat$max_age < min(lims) & scale_dat$min_age > min(lims))
        if (any(max_end)) {
          ends <- scale_dat[max_end,c("min_age","max_age")]
          scale_dat$mid_age[max_end] <- (ends[ends < max(lims) & ends > min(lims)] + max(lims))/2
        }
        if (any(min_end)) {
          ends <- scale_dat[min_end,c("min_age","max_age")]
          scale_dat$mid_age[min_end] <- (ends[ends < max(lims) & ends > min(lims)] + min(lims))/2
        }
      }
      scale_size <- size[[scale]]
      scale_rot <- rot[[scale]]
      if (side %in% c(1,3)) {
        text(x = scale_dat$mid_age,
             y = (scale_lims[3] + scale_lims[4])/2,
             adj = c(0.5, 0.5), labels = scale_dat$label,
             col = scale_dat$lab_color, srt = scale_rot, cex = scale_size)
      } else {
        text(y = scale_dat$mid_age,
             x = (scale_lims[1] + scale_lims[2])/2,
             adj = c(0.5, 0.5), labels = scale_dat$label,
             col = scale_dat$lab_color, srt = 90 + scale_rot, cex = scale_size)
      }
    }

    if (scale < n_scales) {
      # set up scale limits for the next scale
      if (side == 1) {
        scale_lims <- c(scale_lims[1], scale_lims[2],
                        scale_lims[3] - height[[scale]],
                        scale_lims[4] - height[[scale + 1]])
      } else if (side == 2) {
        scale_lims <- c(scale_lims[1] - height[[scale]],
                        scale_lims[2] - height[[scale + 1]],
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
  # after placing the scales, add an axis with ticks and labels
  if (is.null(at)) {
    # Use the interval breaks from the outer-most scale
    at <- unique(c(scale_dat$min_age, scale_dat$max_age))
  }
  if (side == 1) {
    axis(side = side, pos = clip_lims[3], at = at, ...)
  } else if (side == 2) {
    axis(side = side, pos = clip_lims[1], at = at, ...)
  } else if (side == 3) {
    axis(side = side, pos = clip_lims[4], at = at, ...)
  } else if (side == 4) {
    axis(side = side, pos = clip_lims[2], at = at, ...)
  }
  # place an axis label as well?

}

make_list <- function(x) {
  if (is.list(x) & !is(x, 'data.frame')) x else list(x)
}
