#' Calculate the temporal range of fossil taxa
#'
#' A function to calculate the temporal range of fossil taxa from occurrence
#' data.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences containing
#' at least three columns: names of taxa, minimum age and maximum age
#' (see `name`, `min_ma`, and `max_ma` arguments).
#' These ages should constrain the age range of the fossil occurrence
#' and are assumed to be in millions of years before present.
#' @param name \code{character}. The name of the column you wish to be treated
#' as the input names, e.g. "genus" (default).
#' @param min_ma \code{character}. The name of the column you wish to be treated
#' as the minimum limit of the age range, e.g. "min_ma" (default).
#' @param max_ma \code{character}. The name of the column you wish to be treated
#' as the maximum limit of the age range, e.g. "max_ma" (default).
#' @param group \code{character}. The name of the column you wish to be treated
#' as the grouping variable, e.g. "family". If not supplied, all taxa are
#'   treated as a single group.
#' @param by \code{character}. How should the output be sorted?
#' Either: "FAD" (first-appearance date; default), "LAD" (last-appearance data),
#' or "name" (alphabetically by taxon names).
#'
#' @return A \code{data.frame} containing the following columns:
#' unique taxa (`taxon`), taxon ID (`taxon_id`), first appearance of taxon
#' (`max_ma`), last appearance of taxon (`min_ma`), duration of temporal
#' range (`range_myr`), and number of occurrences per taxon (`n_occ`) is
#' returned.
#'
#' @details The temporal range(s) of taxa are calculated by extracting all
#'   unique taxa (`name` column) from the input `occdf`, and checking their
#'   first and last appearance. The temporal duration of each taxon is also
#'   calculated. If the input data columns contain NAs, these must be
#'   removed prior to function call.
#'
#' Note: this function provides output based solely on the user input data.
#' The true duration of a taxon is likely confounded by uncertainty in
#' dating occurrences, and incomplete sampling and preservation.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Bethany Allen, Christopher D. Dean & Kilian Eichenseer
#' @importFrom graphics points strwidth
#' @examples
#' # Grab internal data
#' occdf <- tetrapods
#' # Remove NAs
#' occdf <- subset(occdf, !is.na(order) & order != "NO_ORDER_SPECIFIED")
#' # Temporal range
#' ex <- tax_range_time(occdf = occdf, name = "order")
#' # Temporal range ordered by class
#' # Update margins for plotting
#' par(mar = c(8, 5, 6, 6))
#' ex <- tax_range_time(occdf = occdf, name = "order", group = "class")
#' # Customise appearance
#' ex <- tax_range_time(occdf = occdf, name = "order", group = "class")
#'
#' plot(ex, ylab = "Orders", pch = 21, col = "black", bg = "blue", lty = 2,
#'      intervals = list("periods", "eras"))
#'
#' # Control plotting order of groups
#' occdf$class <- factor(x = occdf$class,
#'                       levels = c("Reptilia", "Osteichthyes"))
#' ex <- tax_range_time(occdf = occdf, name = "order",
#'                      group = "class")
#' @export
tax_range_time <- function(
  occdf,
  name = "genus",
  min_ma = "min_ma",
  max_ma = "max_ma",
  group = NULL,
  by = "FAD"
) {
  #=== Handling errors ===
  if (!is.data.frame(occdf)) {
    stop("`occdf` should be a dataframe")
  }

  if (
    !is.numeric(occdf[, max_ma, drop = TRUE]) ||
      !is.numeric(occdf[, min_ma, drop = TRUE])
  ) {
    stop("`max_ma` and `min_ma` must be of class numeric.")
  }

  if (!all(c(name, min_ma, max_ma) %in% colnames(occdf))) {
    stop(
      "Either `name`, `min_ma`, or `max_ma`, is not a named column in
         `occdf`"
    )
  }

  if (anyNA(occdf[, name, drop = TRUE])) {
    stop("The `name` column contains NA values")
  }

  if (
    anyNA(occdf[, min_ma, drop = TRUE]) ||
      anyNA(occdf[, max_ma, drop = TRUE])
  ) {
    stop("`min_ma` and/or `max_ma` columns contain NA values")
  }

  if (length(group) > 1) {
    stop("`group` length is >1, only a single grouping variable is accepted.")
  }

  if (!is.null(group) && (!group %in% colnames(occdf))) {
    stop("`group` is not a named column in `occdf`")
  }

  if (!by %in% c("name", "FAD", "LAD")) {
    stop('`by` must be either "FAD", "LAD", or "name"')
  }

  # Create pseudo-group if not provided (enable group_apply with no groups)
  if (is.null(group)) {
    occdf$tmp_group <- 1
    g <- "tmp_group"
  } else {
    g <- group
  }
  # Calculate ranges
  temp_df <- group_apply(
    occdf,
    g,
    function(occdf, name, min_ma, max_ma) {
      #=== Set-up ===
      unique_taxa <- unique(occdf[, name, drop = TRUE])
      # Order taxa by name
      unique_taxa <- sort(unique_taxa)

      #=== Temporal range ===
      # Generate dataframe for population
      temp_df <- data.frame(
        taxon = unique_taxa,
        taxon_id = seq(1, length(unique_taxa), 1),
        max_ma = rep(NA, length(unique_taxa)),
        min_ma = rep(NA, length(unique_taxa)),
        range_myr = rep(NA, length(unique_taxa)),
        n_occ = rep(NA, length(unique_taxa))
      )
      # Run for loop across unique taxa
      for (i in seq_along(unique_taxa)) {
        vec <- which(occdf[, name, drop = TRUE] == unique_taxa[i])
        temp_df$max_ma[i] <- max(occdf[vec, max_ma])
        temp_df$min_ma[i] <- min(occdf[vec, min_ma])
        temp_df$range_myr[i] <- temp_df$max_ma[i] - temp_df$min_ma[i]
        temp_df$n_occ[i] <- length(vec)
      }
      # Should data be ordered by FAD, LAD, or name?
      if (by == "FAD") {
        temp_df <- temp_df[order(temp_df$max_ma), ]
      } else if (by == "LAD") {
        temp_df <- temp_df[order(temp_df$min_ma), ]
      }
      # Return dataframe
      temp_df
    },
    name = name,
    min_ma = min_ma,
    max_ma = max_ma
  )
  # Assign taxon_ids
  temp_df$taxon_id <- seq_len(nrow(temp_df))
  # Round off values
  temp_df[, c("max_ma", "min_ma", "range_myr")] <- round(
    x = temp_df[, c("max_ma", "min_ma", "range_myr")],
    digits = 3
  )
  # Remove row names
  row.names(temp_df) <- NULL

  # Tidy up
  if (is.null(group)) {
    temp_df <- temp_df[, -which(colnames(temp_df) == "tmp_group")]
  }

  class(temp_df) <- c("palaeo_tax_range_time", class(temp_df))
  attr(temp_df, "palaeo_group") <- group

  return(temp_df)
}

#' Plotting methods for palaeoverse functions
#'
#' @name plot_palaeo
#' @param intervals \code{character}. The time interval information used to
#'   plot the x-axis: either A) a \code{character} string indicating a rank of
#'   intervals from the built-in \code{\link{GTS2020}}, B) a \code{character}
#'   string indicating a \code{data.frame} hosted by
#'   [Macrostrat](https://macrostrat.org) (see \code{\link{time_bins}}), or C)
#'   a custom \code{data.frame} of time interval boundaries (see [axis_geo]
#'   Details). A list of strings or data.frames can be supplied to add
#'   multiple time scales to the same side of the plot (see [axis_geo]
#'   Details). Defaults to "periods".
#' @export
plot.palaeo_tax_range_time <- function(x, y, ..., intervals = "period") {
  # We want to pass `plot(bins)` but `bins` should be used on the y-axis only.
  if (missing(y)) {
    invisible()
  }

  group <- attr(x, "palaeo_group")

  dots <- list(...)

  # Default plot args
  main <- if (!"main" %in% names(dots)) {
    "Temporal range of taxa"
  } else {
    dots[["main"]]
  }
  xlab <- if (!"xlab" %in% names(dots)) "Time (Ma)" else dots[["xlab"]]
  ylab <- if (!"ylab" %in% names(dots)) "Taxon" else dots[["ylab"]]
  col <- if (!"col" %in% names(dots)) "black" else dots[["col"]]
  bg <- if (!"bg" %in% names(dots)) "black" else dots[["bg"]]
  pch <- if (!"pch" %in% names(dots)) 20 else dots[["pch"]]
  cex <- if (!"cex" %in% names(dots)) 1 else dots[["cex"]]
  lty <- if (!"lty" %in% names(dots)) 1 else dots[["lty"]]
  lwd <- if (!"lwd" %in% names(dots)) 1 else dots[["lwd"]]

  # Collect usr par for resetting
  usrpar <- par(no.readonly = TRUE)
  # Estimate max label width
  max_label_width <- max(strwidth(x$taxon, units = "inches"))
  # Convert inches to lines (approximate conversion factor: 0.2)
  extra_margin <- max_label_width / 0.2
  # Update left margin (add extra space, default is 4)
  par(mar = usrpar$mar + c(0, extra_margin, 0, 0))
  # Define plot lims
  xlim <- c(max(x$max_ma), min(x$min_ma))
  ylim <- c(0.5, nrow(x) + 0.5)
  # Base plot
  plot(
    x = NA,
    y = NA,
    xlim = xlim,
    ylim = ylim,
    xlab = NA,
    ylab = NA,
    main = main,
    xaxt = "n",
    yaxt = "n",
    yaxs = "i",
    axes = TRUE
  )
  # Add ylabels
  axis(2, at = seq_len(nrow(x)), labels = x$taxon, las = 2)
  # Add yaxis title
  title(ylab = ylab, line = 2 + extra_margin)
  # Groups provided?
  if (!is.null(group)) {
    # Calculate plotting values for groups
    s <- split(x = x, f = x[, group])
    vals_rect <- lapply(s, function(x) {
      cbind(min(x$taxon_id), max(x$taxon_id))
    })
    # Define colours
    cols_rect <- rep(c("grey85", "grey95"), times = length(vals_rect) / 2)
    # Run across number of groups
    lapply(seq_along(vals_rect), function(x) {
      # Add background rectangles
      rect(
        xleft = xlim[1] * 2,
        xright = 0,
        ybottom = vals_rect[[x]][1] - 0.5,
        ytop = vals_rect[[x]][2] + 0.5,
        col = cols_rect[x]
      )
      # Add group labels
      axis(
        4,
        at = ((min(vals_rect[[x]]) + max(vals_rect[[x]])) / 2),
        labels = names(vals_rect)[x],
        tick = TRUE,
        hadj = 0.5,
        gap.axis = 10,
        line = 0,
        las = 3
      )
    })
  }
  # Add ranges
  segments(
    x0 = x$max_ma,
    x1 = x$min_ma,
    y0 = x$taxon_id,
    col = col,
    lty = lty,
    lwd = lwd
  )
  points(
    x = x$max_ma,
    y = x$taxon_id,
    pch = pch,
    col = col,
    bg = bg,
    cex = cex
  )
  points(
    x = x$min_ma,
    y = x$taxon_id,
    pch = pch,
    col = col,
    bg = bg,
    cex = cex
  )
  axis_geo(side = 1, intervals = intervals, title = xlab)
  # Reset par
  par(usrpar)
}
