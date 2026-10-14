#' Create a `palaeoverse_data` object to store information on your data
#'
#' @description
#' This function allows you to define information on your data only once so that
#' it is automatically used in subsequent `palaeoverse` functions.
#'
#' @param data Dataframe
#' @param lat Name of the column that contains the latitude.
#' @param lon Name of the column that contains the longitude.
#'
#' @details
#' `as_palaeoverse()` stores information in the data attributes, available via `attributes()`.
#' All arguments must be explicitly passed. You can find pre-filled templates for common
#' data sources in the "Templates" section below.
#'
#' This information is used in other `palaeoverse` functions so that users don't need to
#' pass it every time. All `palaeoverse` functions use this preference order:
#'
#' 1. if a value is explicitly passed in a call, e.g. `bin_lat(lat = "lat", ...)`, then it
#'    is preferred;
#' 2. if the value is not explicitly passed, e.g. `bin_lat(...)` without specifying `lat` then
#'    the function will first look for the corresponding attribute (in this case, `"lat"`) which
#'    would have been stored by `as_palaeoverse()`:
#'
#'      a. if this attribute is found, it is used;
#'
#'      b. otherwise, the default argument value is used.
#'
#' @section Templates:
#' We provide pre-filled templates for common data sources. You can use these templates in your
#' code but you should ensure that they match your data.
#'
#' **Paleobiology Database (PBDB)**
#'
#' ```r
#' as_palaeoverse(data, lat = "lat", lon = "lng")
#' ```
#'
#' **Darwin Core (e.g. GBIF)**
#'
#' ```r
#' as_palaeoverse(data, lat = "decimalLatitude", lon = "decimalLongitude")
#' ```
#'
#' @export
#'
#' @examples
#' # We have a dataframe where the latitude values are stored in "my_lat" column
#' dat <- tetrapods[1:5, ]
#' names(dat)[names(dat) == "lat"] <- "my_lat"
#'
#' # We can store this information in as_palaeoverse()
#' dat <- as_palaeoverse(dat, lat = "my_lat", lon = "lng")
#'
#' # Other palaeoverse functions that have a "lat" argument will automatically use
#' # this information (unless another value is explicitly passed in the call)
#' bins <- lat_bins_degrees(size = 10)
#' bin_lat(occdf = dat, bins = bins)
as_palaeoverse <- function(data, lat, lon) {
  ensure_args_are_named(exceptions = "data")

  # All arguments must be provided
  missing_args <- setdiff(names(formals()), names(match.call())[-1])
  if (length(missing_args) > 0) {
    n_more <- length(missing_args) - 5
    more <- if (n_more > 0) paste0(" (and ", n_more, " more)") else ""
    to_report <- cli::cli_vec(
      head(missing_args, n = 5),
      list(`vec-last` = ", ", `vec-sep2` = ", ")
    )
    cli::cli_abort(
      c(
        "All arguments must be provided.",
        "x" = "{cli::qty(to_report)}Missing argument{?s}: {.arg {to_report}}{more}.",
        "i" = "You can find pre-filled templates at {.help as_palaeoverse}."
      )
    )
  }

  check_data_frame(data)
  check_column_presence(data, lon)
  check_column_presence(data, lat)

  attr(data, "palaeoverse_lon") <- lon
  attr(data, "palaeoverse_lat") <- lat

  class(data) <- c("palaeoverse_data", class(data))
  data
}

#' @export
print.palaeoverse_data <- function(x, ...) {
  # Large dataframes can have many rows and/or columns and entirely fill the console.
  # We want attributes to be visible to the users so we print them at the end
  NextMethod("print", "x")
  cat("\n")

  att <- names(attributes(x))
  att <- att[startsWith(att, "palaeo")]

  att_list <- vapply(att, function(nm) attr(x, nm), FUN.VALUE = character(1))
  att <- gsub("^palaeoverse\\_", "", att)
  cli::cli_inform(
    "A dataframe with {nrow(x)} row{?s} and {ncol(x)} column{?s}."
  )
  if (length(att) > 0) {
    cli::cli_inform(c("i" = "Attributes:"))
    # Indent the bullet points
    d <- cli::cli_div(
      theme = list(ul = list("margin-left" = 2, "padding-left" = 0))
    )
    cli::cli_ul(paste0(att, ": \"", att_list, "\""))
    cli::cli_end(d)
  }
}

#' Preference order:
#' 1. `arg` explicitly passed by user
#' 2. value stored in the data attribute
#' 3. `arg` default value in the function definition
#'
#' @noRd
resolve_info <- function(data, arg) {
  arg_name <- rlang::caller_arg(arg)
  arg_val <- arg

  arg_present_in_call <- arg_name %in%
    names(rlang::call_args(rlang::caller_call()))
  value_from_attr <- attr(data, paste0("palaeoverse_", arg_name))

  if (isTRUE(arg_present_in_call)) {
    if (!is.null(value_from_attr)) {
      cli::cli_inform(
        c(
          "i" = "Using function input {.arg {arg_name}} ({.val {arg_val}}) instead of {.arg {arg_name}} value set in {.fn as_palaeoverse} ({.val {value_from_attr}})."
        )
      )
    }
    return(arg_val)
  } else {
    if (!is.null(value_from_attr)) {
      return(value_from_attr)
    } else {
      return(arg_val)
    }
  }
}
