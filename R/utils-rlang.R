#' Check whether a column exists in the data
#'
#' This errors if `column` doesn't exist in `data`, and it returns no value otherwise.
#'
#' @param data dataframe to check
#' @param column A single column name to check.
#'
#' @noRd
check_column_presence <- function(data, column) {
  rlang::check_string(
    column,
    call = rlang::caller_env(),
    arg = rlang::caller_arg(column)
  )
  if (!column %in% names(data)) {
    cli::cli_abort(
      "Column {.val {column}} not found in {.arg {rlang::caller_arg(data)}}.",
      call = rlang::caller_env()
    )
  }
}

#' Check whether a column contains missing values
#'
#' This errors if there are missing values.
#'
#' @param data dataframe to check
#' @param column A single column name to check.
#'
#' @noRd
check_na <- function(data, column) {
  values <- data[[column]]
  if (anyNA(values)) {
    cli::cli_abort(
      "Column {.val {column}}  in {.arg {rlang::caller_arg(data)}} must not have missing values.",
      call = rlang::caller_env()
    )
  }
}

#' Check whether all values of a numeric column fall in a custom range
#'
#' This errors if any of the following cases:
#'
#' - `column` doesn't exist in `data`
#' - `column` isn't numeric
#' - at least one value of `column` is outside the `[min:max]` range
#'
#' `NA` are considered to be outside the range.
#'
#' @param data dataframe to check
#' @param column A single column name to check.
#' @param min,max Range limits.
#'
#' @noRd
check_range <- function(data, column, min, max) {
  vals <- data[[column]]
  if (!is.numeric(vals)) {
    cli::cli_abort(
      "Column {.val {rlang::caller_arg(column)}} in {.arg {rlang::caller_arg(data)}} must be {.cls numeric}, not {.cls {class(vals)}}.",
      call = rlang::caller_env()
    )
  }
  rng <- vals[vals < min | vals > max]
  if (length(rng) > 0) {
    to_report <- unique(rng)
    truncated <- if (length(to_report) > 5) " (first 5)" else ""
    to_report <- cli::cli_vec(head(to_report, n = 5), list(`vec-last` = ", "))
    cli::cli_abort(
      c(
        "All values of column {.val {rlang::caller_arg(column)}} in {.arg {rlang::caller_arg(data)}} must be between {min} and {max}.",
        "i" = "Value(s) outside the range{truncated}: {.val {to_report}}."
      ),
      call = rlang::caller_env()
    )
  }
}

#' Check whether an object is a numeric vector
#'
#' @param x Values to check
#' @param ... Unused
#' @param allow_na Whether missing values are allowed
#' @param allow_null Whether `x` can be NULL.
#' @param required_length Specific length that `x` must match
#' @param arg Name of the object to report in the error message
#' @param call Call to report in the error message
#'
#' @noRd
check_numeric <- function(
  x,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  required_length = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!missing(x)) {
    if (allow_null && is.null(x)) {
      return(invisible(NULL))
    }
    if (!is.null(required_length) && length(x) != required_length) {
      cli::cli_abort(
        "{.code {arg}} must be of length {required_length}, not {length(x)}.",
        arg = arg,
        call = call
      )
    }
    if (is.numeric(x)) {
      if (!allow_na && anyNA(x)) {
        cli::cli_abort(
          "{.code {arg}} can't contain NA values.",
          arg = arg,
          call = call
        )
      }
      return(invisible(NULL))
    }
  }

  rlang::stop_input_type(
    x,
    "a numeric value",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' Check whether an object is a dataframe
#'
#' This errors if `data` isn't a dataframe, and it returns no value otherwise.
#'
#' This duplicates the behaviour of `rlang::check_data_frame()` because we wanted
#' the error message to contain "be of class dataframe".
#'
#' @param data dataframe to check
#'
#' @noRd
check_data_frame <- function(data) {
  if (rlang::is_missing(data) || !is.data.frame(data)) {
    cli::cli_abort(
      "{.arg {rlang::caller_arg(data)}} must be of class {.cls data.frame}, not {obj_type_friendly(data)}.",
      call = rlang::caller_env()
    )
  }
}
