#' Create a `palaeo` object to store information on your data
#'
#' @description
#' This function allows you to define information on your data only once so that
#' it is automatically used in subsequent `palaeoverse` functions.
#'
#' @param x Dataframe
#' @param lat Name of the column that contains the latitude.
#' @param lon Name of the column that contains the longitude.
#'
#' @details
#' `as_palaeo()` stores information in the data attributes, available via `attributes()`.
#' It uses explicitly passed values (e.g. `lat = "my_lat"`) and default values if they
#' exist in the data (e.g. it will store `lat = "lat"` if `"lat"` exists in the dataframe,
#' even if `lat = "lat"`) isn't explicitly passed.
#'
#' This information is used in other `palaeoverse` functions so that users don't need to
#' pass it every time. All `palaeoverse` functions use this preference order:
#'
#' 1. if a value is explicitly passed in a call, e.g. `bin_lat(lat = "lat", ...)`, then it
#'    is preferred;
#' 2. if the value is not explicitly passed, e.g. `bin_lat(...)` without specifying `lat` then
#'    the function will first look for the corresponding attribute (in this case, `"lat"`) which
#'    would have been stored by `as_palaeo()`:
#'
#'      a. if this attribute is found, it is used;
#'
#'      b. otherwise, the default argument value is used.
#'
#' @export
#'
#' @examples
#' # We have a dataframe where the latitude values are stored in "my_lat" column
#' dat <- tetrapods[1:5, ]
#' names(dat)[names(dat) == "lat"] <- "my_lat"
#'
#' # We can store this information in as_palaeo()
#' dat <- as_palaeo(dat, lat = "my_lat")
#'
#' # Other palaeoverse functions that have a "lat" argument will automatically use
#' # this information (unless another value is explicitly passed in the call)
#' bins <- lat_bins_degrees(size = 10)
#' bin_lat(occdf = dat, bins = bins)
as_palaeo <- function(x, lat = "lat", lon = "lon") {
  ensure_args_are_named(exceptions = "x")
  rlang::check_data_frame(x)

  if (missing(lon)) {
    if ("lon" %in% names(x)) {
      attr(x, "palaeo_lon") <- "lon"
    }
  } else {
    check_column_presence(x, lon)
    attr(x, "palaeo_lon") <- lon
  }
  if (missing(lat)) {
    if ("lat" %in% names(x)) {
      attr(x, "palaeo_lat") <- "lat"
    }
  } else {
    check_column_presence(x, lat)
    attr(x, "palaeo_lat") <- lat
  }

  class(x) <- c("palaeo", class(x))
  x
}

#' @export
print.palaeo <- function(x, ...) {
  att <- names(attributes(x))
  att <- att[startsWith(att, "palaeo")]

  att_list <- vapply(att, function(nm) attr(x, nm), FUN.VALUE = character(1))
  att[att == "palaeo_lat"] <- "Latitude"
  att[att == "palaeo_lon"] <- "Longitude"
  cli::cli_inform(
    "A dataframe with {nrow(x)} row{?s} and {ncol(x)} column{?s}."
  )
  if (length(att) > 0) {
    cli::cli_inform(
      c(
        "i" = "Attributes:",
        "*" = paste0(att, ": ", att_list)
      )
    )
  }
  cat("\n")
  print.data.frame(x)
}

#' Preference order:
#' 1. `column` explicitly passed by user
#' 2. value stored in the data attribute
#' 3. `column` default value in the function definition
#'
#' @noRd
resolve_info <- function(data, column) {
  column_present_in_call <- column %in%
    names(rlang::call_args(rlang::caller_call()))
  if (isTRUE(column_present_in_call)) {
    return(column)
  } else {
    value_from_attr <- attr(data, paste0("palaeo_", column))
    if (!is.null(value_from_attr)) {
      return(value_from_attr)
    } else {
      return(column)
    }
  }
}
