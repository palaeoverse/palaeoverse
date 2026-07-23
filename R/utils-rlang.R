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
