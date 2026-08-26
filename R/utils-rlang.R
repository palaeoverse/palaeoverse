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

# Same as check_column_presence() but can take more than one column name
check_columns_presence <- function(data, columns) {
  if (length(columns) == 0) {
    cli::cli_abort(
      "{.arg {rlang::caller_arg(columns)}} must specify at least one column.",
      call = rlang::caller_env()
    )
  }
  check_character(
    columns,
    call = rlang::caller_env(),
    arg = rlang::caller_arg(columns)
  )
  non_existent_cols <- columns[!columns %in% names(data)]
  if (length(non_existent_cols)) {
    to_report <- cli::cli_vec(non_existent_cols, list(`vec-last` = ", and "))
    cli::cli_abort(
      "{cli::qty(to_report)} Column{?s} {.val {to_report}} not found in {.arg {rlang::caller_arg(data)}}.",
      call = rlang::caller_env()
    )
  }
}


check_range <- function(data, x, min, max) {
  vals <- data[[x]]
  if (!is.numeric(vals)) {
    cli::cli_abort(
      "Column {.val {rlang::caller_arg(x)}} in {.arg {rlang::caller_arg(data)}} must be numeric, not {.cls {class(vals)}}.",
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
        "All values of column {.val {rlang::caller_arg(x)}} in {.arg {rlang::caller_arg(data)}} must be between {min} and {max}.",
        "i" = "Value(s) outside the range{truncated}: {.val {to_report}}."
      ),
      call = rlang::caller_env()
    )
  }
}
