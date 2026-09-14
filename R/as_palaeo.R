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
#' @export
as_palaeo <- function(x, lat = "lat", lon = "lon") {
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

  att_list <- lapply(att, function(nm) attr(x, nm))
  att[att == "palaeo_lat"] <- "Latitude"
  att[att == "palaeo_lon"] <- "Longitude"
  names(att_list) <- paste0("- ", att, ":")

  cat(
    "A dataframe with",
    nrow(x),
    "rows and",
    ncol(x),
    "columns\n\nAttributes:"
  )
  vals <- unlist(att_list)
  df <- data.frame(vals, ...)
  names(df) <- ""
  print(df)
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
