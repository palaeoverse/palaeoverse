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
