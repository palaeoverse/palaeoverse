#' Calculate the temporal range of fossil taxa
#'
#' A function to calculate the temporal range of fossil taxa from occurrence
#' data.
#'
#' @param data \code{dataframe}. A dataframe of fossil occurrences containing
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
#' @param group `r lifecycle::badge("deprecated")` Use [group_apply()] with
#'   this function instead (see examples).
#' @param sort \code{character}. How should the output be sorted?
#' Either: "FAD" (first-appearance date; default), "LAD" (last-appearance data),
#' or "name" (alphabetically by taxon names).
#' @param by `r lifecycle::badge("deprecated")` Use `sort` instead.
#' @param plot,plot_args `r lifecycle::badge("deprecated")` Use `plot()` on the
#'   output of this function instead.
#' @param intervals `r lifecycle::badge("deprecated")` Use the argument `axis`
#' in `plot()` on the output of this function instead.
#'
#' @return A \code{data.frame} containing the following columns:
#' unique taxa (`taxon`), taxon ID (`taxon_id`), first appearance of taxon
#' (`max_ma`), last appearance of taxon (`min_ma`), duration of temporal
#' range (`range_myr`), and number of occurrences per taxon (`n_occ`) is
#' returned.
#'
#' This \code{data.frame} has an attribute `palaeoverse_tax_range_time_group`
#' that is for internal use only.
#'
#' @details The temporal range(s) of taxa are calculated by extracting all
#'   unique taxa (`name` column) from the input `data`, and checking their
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
#' @examples
#' # Grab internal data
#' data <- tetrapods
#' # Remove NAs
#' data <- subset(data, !is.na(order) & order != "NO_ORDER_SPECIFIED")
#' # Temporal range
#' ex <- tax_range_time(data = data, name = "order")
#' plot(ex)
#'
#' # Customise appearance
#' plot(ex, ylab = "Orders", pch = 21, col = "black", bg = "blue", lty = 2,
#'      axis = axis_geo(intervals = "eras", abbr = FALSE))
#'
#' # Add elements to the plot
#' op <- par(plot(ex))
#' abline(v = 290, lty = 2, col = "red")
#' par(op)
#'
#' # Temporal range per class
#' ex <- group_apply(data = data, group = "class", fun = tax_range_time,
#'                   name = "order")
#' @export
tax_range_time <- function(
  data,
  name = "genus",
  min_ma = "min_ma",
  max_ma = "max_ma",
  group = deprecated(),
  sort = "FAD",
  by = deprecated(),
  plot = deprecated(),
  plot_args = deprecated(),
  intervals = deprecated()
) {
  ensure_args_are_named(exceptions = "data")

  check_data_frame(data)

  check_column_presence(data, name)
  check_column_presence(data, min_ma)
  check_column_presence(data, max_ma)

  check_na(data, name)
  check_na(data, min_ma)
  check_na(data, max_ma)

  check_class(data, min_ma, "numeric")
  check_class(data, max_ma, "numeric")
  check_min_lower_than_max(data, min_ma, max_ma)

  if (lifecycle::is_present(group)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "tax_range_time(group)",
      "group_apply()",
      always = TRUE
    )
  } else {
    group <- NULL
  }
  if (!is.null(group)) {
    check_column_presence(data, group)
  }

  if (lifecycle::is_present(by)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "tax_range_time(by)",
      "tax_range_time(sort)",
      always = TRUE
    )
    rlang::check_exclusive(by, sort)

    rlang::check_string(by)
    by <- rlang::arg_match(by, values = c("FAD", "LAD", "name"))
    sort <- by
  }

  rlang::check_string(sort)
  sort <- rlang::arg_match(sort, values = c("FAD", "LAD", "name"))

  if (lifecycle::is_present(intervals)) {
    cli::cli_abort(
      c(
        "The argument {.arg intervals} has been removed in {.pkg palaeoverse} 2.0.0.",
        "i" = "Use {.code plot(axis =)} on the output of this function instead."
      )
    )
  }
  if (lifecycle::is_present(plot)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "tax_range_time(plot)",
      I("`plot()` on the output of this function"),
      always = TRUE
    )
    rlang::check_bool(plot)
  }
  if (lifecycle::is_present(plot_args)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "tax_range_time(plot_args)",
      I("`plot()` on the output of this function"),
      always = TRUE
    )
    if (!is.null(plot_args) && !is.list(plot_args)) {
      cli::cli_abort(
        "{.arg plot_args} must be of class {.cls list} or {.code NULL}, not {obj_type_friendly(plot_args)}."
      )
    }
  } else {
    plot_args <- NULL
  }

  # Create pseudo-group if not provided (enable group_apply with no groups)
  if (is.null(group)) {
    data$tmp_group <- 1
    g <- "tmp_group"
  } else {
    g <- group
  }
  # Calculate ranges
  temp_df <- group_apply(
    data,
    group = g,
    fun = function(data, name, min_ma, max_ma) {
      #=== Set-up ===
      unique_taxa <- unique(data[, name, drop = TRUE])
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
        vec <- which(data[, name, drop = TRUE] == unique_taxa[i])
        temp_df$max_ma[i] <- max(data[vec, max_ma])
        temp_df$min_ma[i] <- min(data[vec, min_ma])
        temp_df$range_myr[i] <- temp_df$max_ma[i] - temp_df$min_ma[i]
        temp_df$n_occ[i] <- length(vec)
      }
      # Should data be ordered by FAD, LAD, or name?
      if (sort == "FAD") {
        temp_df <- temp_df[order(temp_df$max_ma), ]
      } else if (sort == "LAD") {
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

  class(temp_df) <- c("palaeoverse_tax_range_time", class(temp_df))
  attr(temp_df, "palaeoverse_tax_range_time_group") <- group
  if (isTRUE(plot)) {
    if (is.null(plot_args)) {
      plot(temp_df)
    } else {
      do.call("plot", c(list(x = temp_df), plot_args))
    }
  }

  return(temp_df)
}


#' @param x `data.frame`. An object of class `"palaeoverse_tax_range_time"` created by `tax_range_time()`.
#' @param ... Extra graphical parameters (see Details). The following arguments are already set internally
#' and must not be specified here: `xlim`, `ylim`, `xaxt`, `yaxt`.
#' @inheritParams lat_bins_area
#' @param main `character`. The plot title.
#' @param col.group `character`. The background colours to use when `x` has groups. Must be of the same length
#' as the number of unique groups.
#' @param axis A function call drawing an axis, such as [`axis_geo()`]
#' (default) to add the geological timescale at the bottom of the plot, or
#' [graphics::axis()].
#'
#' @details
#' The following graphical parameters have default values that differ from base R:
#'
#' - `bg = "black"`: the background (fill) colour of the points, only used for `pch` values 21 to 25.
#' - `pch = 20`: the symbol used for the first and last appearance points (see [graphics::points()]).
#' - `yaxs = "i"`: the style of y-axis interval calculation (see [graphics::par()]).
#'
#' The graphical parameters are reset when `plot()` exits. See 'Examples' to know how to add
#' components to the plot.
#'
#' @return `plot()` invisibly returns a list with the graphical parameters `mar`
#' and `usr` used to draw the plot (see [graphics::par()]).
#'
#' @name tax_range_time
#' @importFrom graphics points strwidth plot.new title
#' @export
plot.palaeoverse_tax_range_time <- function(
  x,
  ...,
  axis = axis_geo(side = 1, intervals = "periods", title = xlab),
  main = "Temporal range of taxa",
  xlab = "Time (Ma)",
  ylab = "Taxon",
  col.group = c("grey85", "grey95")
) {
  axis_expr <- substitute(axis)
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

  check_forbidden_plot_args(
    ...,
    forbidden = c("xlim", "ylim", "xaxt", "yaxt"),
    class = "palaeoverse_tax_range_time"
  )

  args <- list(...)
  if (!"bg" %in% names(args)) {
    args[["bg"]] <- "black"
  }
  if (!"pch" %in% names(args)) {
    args[["pch"]] <- 20
  }
  if (!"yaxs" %in% names(args)) {
    args[["yaxs"]] <- "i"
  }
  group <- attr(x, "palaeoverse_tax_range_time_group")

  if (!is.null(group)) {
    n_groups <- length(unique(x[[group]]))
    if (length(col.group) != n_groups) {
      cli::cli_abort(
        "Length of {.arg col.group} ({.val {length(col.group)}}) must be equal to the number of unique groups in the data ({.val {n_groups}})."
      )
    }
  }

  # Collect usr par for resetting
  usrpar <- par(no.readonly = TRUE)
  on.exit(par(usrpar))
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
  do.call(
    plot,
    c(
      list(
        x = NA,
        y = NA,
        xlim = xlim,
        ylim = ylim,
        xlab = NA,
        ylab = NA,
        main = main,
        xaxt = "n",
        yaxt = "n"
      ),
      # These arguments are used later, not in the first plot()
      args[!rlang::names2(args) %in% c("col", "bg", "pch", "cex", "lty", "lwd")]
    )
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
    cols_rect <- rep(col.group, times = length(vals_rect) / 2)
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
  segments_args <- args[rlang::names2(args) %in% c("col", "lty", "lwd")]
  points_args <- args[rlang::names2(args) %in% c("col", "bg", "pch", "cex")]
  do.call(
    segments,
    c(list(x0 = x$max_ma, x1 = x$min_ma, y0 = x$taxon_id), segments_args)
  )
  do.call(points, c(list(x = x$max_ma, y = x$taxon_id), points_args))
  do.call(points, c(list(x = x$min_ma, y = x$taxon_id), points_args))
  eval(axis_expr)
  # Settings needed to add elements to the plot, `par` is reset on exit
  invisible(par(c("mar", "usr")))
}
