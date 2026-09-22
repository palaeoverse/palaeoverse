#' Generate equal-width latitudinal bins
#'
#' A function to generate latitudinal bins of a given size for a user-defined
#' latitudinal range. If the desired size of the bins is not compatible with
#' the defined latitudinal range, bin size can be updated to the nearest integer
#' which is divisible into this range.
#'
#' @param size \code{numeric}. A single numeric value defining the width of the
#' latitudinal bins. This value must be more than 0, and less than or equal to
#' 90 (defaults to 10).
#' @param min \code{numeric}. A single numeric value defining the lower limit
#' of the latitudinal range (defaults to -90).
#' @param max \code{numeric}. A single numeric value defining the upper limit
#' of the latitudinal range (defaults to 90).
#' @param fit \code{logical}. Should bin size be checked to ensure that the
#' entire latitudinal range is covered? If \code{fit = TRUE}, bin size is
#' set to the nearest integer which is divisible by the user-input range.
#' If \code{fit = FALSE}, and bin size is not divisible into the range, the
#' upper part of the latitudinal range will be missing.
#' @return A \code{dataframe} of latitudinal bins of user-defined size. The
#'   \code{data.frame} contains the following columns: bin (bin number), min
#'   (minimum latitude of the bin), mid (midpoint latitude of
#'   the bin), max (maximum latitude of the bin).
#' @inheritParams lat_bins_area
#' @seealso
#' For equal-area latitudinal bins, see \link{lat_bins_area}.
#' @importFrom graphics polygon abline title
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Bethany Allen
#' @export
#' @examples
#' # Generate 20 degrees latitudinal bins
#' bins <- lat_bins_degrees(size = 20)
#'
#' # Generate latitudinal bins with closest fit to 13 degrees
#' bins <- lat_bins_degrees(size = 13, fit = TRUE)
#'
#' # Generate latitudinal bins for defined latitudinal range
#' bins <- lat_bins_degrees(size = 10, min = -50, max = 50)
lat_bins_degrees <- function(
  size = 10,
  min = -90,
  max = 90,
  fit = FALSE,
  plot = deprecated()
) {
  ensure_args_are_named()

  rlang::check_number_decimal(size, min = 0, max = 90)
  rlang::check_number_decimal(min, min = -90, max = 90)
  rlang::check_number_decimal(max, min = -90, max = 90)
  rlang::check_bool(fit)

  if (lifecycle::is_present(plot)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "lat_bins_degrees(plot)",
      I("`plot()` on the output of this function"),
      always = TRUE
    )
    rlang::check_bool(plot)
  }

  if (min >= max) {
    cli::cli_abort("{.arg min} must be less than {.arg max}.")
  }

  # Latitudinal range
  lat_range <- abs(max - min)
  # Divide latitudinal range by size of bins
  bins <- lat_range / size
  # If fit is set true, generate equal size bins to fit range
  if (fit) {
    if (!is.integer(bins)) {
      int <- lat_range / seq(from = 1, to = 90, by = 1)
      int <- which(int %% 1 == 0)
      size <- int[which.min(abs(int - size))]
      bins <- lat_range / size
    }
  }
  # Generate latitudinal bins for specified range
  df <- seq(from = min, to = max, by = size)
  min <- df[1:bins]
  max <- df[1:bins] + size
  mid <- (max + min) / 2
  bin <- 1:bins
  df <- cbind(min, mid, max)
  df <- df[order(-max), ]
  df <- cbind.data.frame(bin, df)

  class(df) <- c("palaeoverse_lat_bins_degrees", class(df))
  attr(df, "palaeoverse_lat_bins_degrees_fit") <- fit
  attr(df, "palaeoverse_lat_bins_degrees_size") <- size

  if (isTRUE(plot)) {
    plot(df)
  }

  if (fit) {
    cli::cli_inform(
      "Bin size set to {size} degree{?s} to fit latitudinal range."
    )
  }

  return(df)
}

#' Generate equal-width latitudinal bins
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `lat_bins()` was renamed to `lat_bins_degrees()` to be consistent
#' with `lat_bins_area().`
#' @inheritParams lat_bins_degrees
#' @export
lat_bins <- function(
  size = 10,
  min = -90,
  max = 90,
  fit = FALSE,
  plot = FALSE
) {
  lifecycle::deprecate_warn("1.4.0", "lat_bins()", "lat_bins_degrees()")
  argg <- as.list(environment())
  do.call(lat_bins_degrees, argg)
}


#' @param x `data.frame`. An object of class `"palaeoverse_lat_bins_degrees"` created by `lat_bins_degrees()`.
#' @inheritParams lat_bins_area
#'
#' @name lat_bins_degrees
#' @export
plot.palaeoverse_lat_bins_degrees <- function(
  x,
  ...,
  col = c("#01665e", "#80cdc1"),
  xlab = "Longitude (\u00B0)",
  ylab = "Latitude (\u00B0)"
) {
  if (length(col) != 2 || !is.character(col)) {
    cli::cli_abort(
      "Argument {.arg col} must be an object of class {.cls character} of length 2, not {obj_type_friendly(col)}."
    )
  }

  dots <- list(...)
  if (length(dots) > 0) {
    nms <- names(dots)
    forbidden <- nms[nms %in% c("type", "xlim", "ylim")]
    if (length(forbidden) > 0) {
      cli::cli_abort(
        c(
          "{cli::qty(forbidden)} Cannot pass argument{?s} {.arg {forbidden}} when calling {.fn plot} on an object of class {.cls palaeoverse_lat_bins_degrees}.",
          "i" = "{cli::qty(forbidden)}{?This/These} argument{?s} {?is/are} already set by `plot()` internally."
        )
      )
    }
  }

  plot(
    1,
    type = "n",
    xlim = c(-180, 180),
    ylim = c(min(x$min), max(x$max)),
    xlab = xlab,
    ylab = ylab
  )
  cols <- rep(col, nrow(x))
  for (i in seq_len(nrow(x))) {
    polygon(
      x = c(-180, -180, 180, 180),
      y = c(x$min[i], x$max[i], x$max[i], x$min[i]),
      col = cols[i],
      border = "black"
    )
  }
  if (isTRUE(attr(x, "palaeoverse_lat_bins_degrees_fit"))) {
    title(paste0(
      "Bin size set to ",
      attr(x, "palaeoverse_lat_bins_degrees_size")
    ))
  }
}
