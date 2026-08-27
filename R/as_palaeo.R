#' @export
as_palaeo <- function(x, lat = "lat", lon = "lon") {
  rlang::check_data_frame(x)

  if (missing(lon)) {
    if ("lon" %in% names(x)) {
      attr(x, "palaeo_lon") <- "lon"
    }
  } else {
    if (!lon %in% names(x)) {
      cli::cli_abort(
        "Column {.val {lon}} doesn't exist in {.arg {rlang::caller_arg(x)}}."
      )
    }
    attr(x, "palaeo_lon") <- lon
  }
  if (missing(lat)) {
    if ("lat" %in% names(x)) {
      attr(x, "palaeo_lat") <- "lat"
    }
  } else {
    if (!lat %in% names(x)) {
      cli::cli_abort(
        "Column {.val {lat}} doesn't exist in {.arg {rlang::caller_arg(x)}}."
      )
    }
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
  att <- paste0("- ", att, ":")
  names(att_list) <- att

  print_key_values <- function(title, vals, ...) {
    df <- data.frame(vals, ...)
    names(df) <- ""
    cat(
      "A dataframe with",
      nrow(x),
      "rows and",
      ncol(x),
      "columns\n\nAttributes:"
    )
    print(df)
  }

  print_key_values("Attributes", unlist(att_list))
}

#' Preference order:
#' 1. `column` explicitly passed by user
#' 2. value stored in the data attribute
#' 3. `column` default value in the function definition
#'
#' @noRd
resolve_info <- function(data, column, column_present_in_call) {
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
