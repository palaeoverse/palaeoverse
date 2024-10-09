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
#' @param plot \code{logical}. Should a plot of the ranges be generated?
#' @param plot_args \code{list}. A list of optional arguments relevant to
#'   plotting. See Details for options.
#' @param intervals \code{character}. The time interval information used to
#'   plot the x-axis: either A) a \code{character} string indicating a rank of
#'   intervals from the built-in \code{\link{GTS2020}}, B) a \code{character}
#'   string indicating a \code{data.frame} hosted by
#'   [Macrostrat](https://macrostrat.org) (see \code{\link{time_bins}}), or C)
#'   a custom \code{data.frame} of time interval boundaries (see [axis_geo]
#'   Details). A list of strings or data.frames can be supplied to add
#'   multiple time scales to the same side of the plot (see [axis_geo]
#'   Details). Defaults to "periods".
#'
#' @return A \code{dataframe} containing the following columns:
#' unique taxa (`taxon`), taxon ID (`taxon_id`), first appearance of taxon
#' (`max_ma`), last appearance of taxon (`min_ma`), duration of temporal
#' range (`range_myr`), and number of occurrences per taxon (`n_occ`) is
#' returned.
#'
#' @details The temporal range(s) of taxa are calculated by extracting all
#'   unique taxa (`name` column) from the input `occdf`, and checking their
#'   first and last appearance. The temporal duration of each taxon is also
#'   calculated. If the input data columns contain NAs, these must be
#'   removed prior to function call. A plot of the temporal range of each
#'   taxon is also returned if `plot = TRUE`. Customisable argument options
#'   (i.e. [graphics::par()]) to pass to `plot_args` as a list (and their
#'   defaults) for plotting include:
#'   - xlab = "Time (Ma)"
#'   - ylab = "Taxon ID"
#'   - col = "black"
#'   - bg = "black"
#'   - pch = 20
#'   - cex = 1
#'   - lty = 1
#'   - lwd = 1
#'
#' Note: this function provides output based solely on the user input data.
#' The true duration of a taxon is likely confounded by uncertainty in
#' dating occurrences, and incomplete sampling and preservation.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Bethany Allen, Christopher D. Dean & Kilian Eichenseer
#' @importFrom graphics points
#' @examples
#' # Grab internal data
#' occdf <- tetrapods
#' # Remove NAs
#' occdf <- subset(occdf, !is.na(order) & order != "NO_ORDER_SPECIFIED")
#' # Temporal range
#' ex <- tax_range_time(occdf = occdf, name = "order", plot = TRUE)
#' # Temporal range ordered by class
#' ex <- tax_range_time(occdf = occdf, name = "order", group = "class",
#'                      plot = TRUE)
#' # Customise appearance
#' ex <- tax_range_time(occdf = occdf, name = "order", group = "class",
#'                      plot = TRUE,
#'                      plot_args = list(ylab = "Orders",
#'                                       pch = 21, col = "black", bg = "blue",
#'                                       lty = 2),
#'                      intervals = list("periods", "eras"))
#' @export
tax_range_time <- function(occdf,
                           name = "genus",
                           min_ma = "min_ma",
                           max_ma = "max_ma",
                           group = NULL,
                           by = "FAD",
                           plot = FALSE,
                           plot_args = NULL,
                           intervals = "periods") {

  #=== Handling errors ===
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe")
  }

  if (is.logical(plot) == FALSE) {
    stop("`plot` should be logical (TRUE/FALSE)")
  }

  if (!is.numeric(occdf[, max_ma, drop = TRUE]) ||
      !is.numeric(occdf[, min_ma, drop = TRUE])) {
    stop("`max_ma` and `min_ma` must be of class numeric.")
  }

  if (any(c(name, min_ma, max_ma) %in% colnames(occdf) == FALSE)) {
    stop("Either `name`, `min_ma`, or `max_ma`, is not a named column in
`occdf`")
  }

  if (any(is.na(occdf[, name, drop = TRUE]))) {
    stop("The `name` column contains NA values")
  }

  if (any(is.na(occdf[, min_ma, drop = TRUE])) ||
      any(is.na(occdf[, max_ma, drop = TRUE]))) {
    stop("`min_ma` and/or `max_ma` columns contain NA values")
  }

  if (!is.null(group) && (group %in% colnames(occdf) == FALSE)) {
    stop('`group` is not a named column in `occdf`')
  }

  if (!by %in% c("name", "FAD", "LAD")) {
    stop('`by` must be either "FAD", "LAD", or "name"')
  }

  if (!is.null(plot_args) && !is.list(plot_args)) {
    stop("`plot_args` must be either NULL, or a list")
  }

  #=== Set-up ===
  unique_taxa <- unique(occdf[, name, drop = TRUE])
  # Order taxa
  unique_taxa <- unique_taxa[order(unique_taxa)]

  #=== Temporal range ===
  # Generate dataframe for population
  temp_df <- data.frame(taxon = unique_taxa,
                        taxon_id = seq(1, length(unique_taxa), 1),
                        max_ma = rep(NA, length(unique_taxa)),
                        min_ma = rep(NA, length(unique_taxa)),
                        group = rep(NA, length(unique_taxa)),
                        range_myr = rep(NA, length(unique_taxa)),
                        n_occ = rep(NA, length(unique_taxa)))
    # Run for loop across unique taxa
    for (i in seq_along(unique_taxa)) {
      vec <- which(occdf[, name, drop = TRUE] == unique_taxa[i])
      temp_df$max_ma[i] <- max(occdf[vec, max_ma])
      temp_df$min_ma[i] <- min(occdf[vec, min_ma])
      temp_df$range_myr[i] <- temp_df$max_ma[i] - temp_df$min_ma[i]
      temp_df$n_occ[i] <- length(vec)
      if (!is.null(group)) {
        temp_df$group[i] <- occdf[vec[1], group]
      }
    }
    # Remove row names
    row.names(temp_df) <- NULL
    # Round off values
    temp_df[, c("max_ma", "min_ma", "range_myr")] <- round(
      x = temp_df[, c("max_ma", "min_ma", "range_myr")], digits = 3)

    # Should data be ordered by FAD or LAD?
    if (by == "FAD") {
      temp_df <- temp_df[order(temp_df$max_ma), ]
      temp_df$taxon_id <- seq_len(nrow(temp_df))
    }

    if (by == "LAD") {
      temp_df <- temp_df[order(temp_df$min_ma), ]
      temp_df$taxon_id <- seq_len(nrow(temp_df))
    }

    if (is.null(group)) {
      temp_df <- temp_df[order(temp_df$group), ]
      temp_df$taxon_id <- seq_len(nrow(temp_df))
    }

    # Plot data?
    if (plot == TRUE) {
      # Default plot args
      args <- list(main = "Temporal range of taxa",
                   xlab = "Time (Ma)",
                   ylab = "Taxon ID",
                   col = "black",
                   bg = "black",
                   pch = 20,
                   cex = 1,
                   lty = 1,
                   lwd = 1)
      # Update any provided
      rpl <- match(names(plot_args), names(args))
      if (length(rpl) != 0) {
        args[rpl] <- plot_args
      }

      xlim <- c(max(temp_df$max_ma), min(temp_df$min_ma))
      ylim <- c(0, nrow(temp_df))
      plot(x = NA, y = NA, xlim = xlim, ylim = ylim, axes = TRUE,
           xaxt = "n", xlab = NA, ylab = args$ylab, main = args$main)
      segments(x0 = temp_df$max_ma, x1 = temp_df$min_ma,
               y0 = temp_df$taxon_id,
               col = args$col, lty = args$lty, lwd = args$lwd)
      points(x = temp_df$max_ma, y = temp_df$taxon_id,
             pch = args$pch, col = args$col, bg = args$bg,
             cex = args$cex)
      points(x = temp_df$min_ma, y = temp_df$taxon_id,
             pch = args$pch, col = args$col, bg = args$bg,
             cex = args$cex)
      axis_geo(side = 1, intervals = intervals)
      title(xlab = args$xlab, line = 4)
    }

    # Return dataframe
    return(temp_df)
  }
