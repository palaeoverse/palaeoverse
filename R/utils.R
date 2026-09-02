#' Check whether there are unnamed arguments that should be named.
#'
#' This errors if some arguments are unnamed or if their names are partially
#' matched.
#'
#' @noRd
ensure_args_are_named <- function() {
  args_in_call_from_user <- rlang::call_args_names(rlang::caller_call())
  unnamed_args <- args_in_call_from_user[which(args_in_call_from_user == "")]
  named_args <- args_in_call_from_user[which(
    !is.null(args_in_call_from_user) & args_in_call_from_user != ""
  )]

  args_in_function_def <- names(formals(rlang::caller_fn()))
  partially_matched_names <- setdiff(named_args, args_in_function_def)
  if (length(partially_matched_names) > 0) {
    cli::cli_abort(
      c(
        "Argument names must be fully written.",
        "i" = "Partially matched argument name{?s}: {.val {cli::cli_vec(partially_matched_names)}}"
      ),
      call = rlang::caller_env()
    )
  }

  if (length(unnamed_args) > 0) {
    cli::cli_abort(
      c(
        "All arguments must be named.",
        "i" = "Currently, there {?is/are} {length(unnamed_args)} argument{?s} that should be named."
      ),
      call = rlang::caller_env()
    )
  }
}
