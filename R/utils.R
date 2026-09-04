#' Check whether there are unnamed arguments that should be named.
#'
#' This errors if some arguments (apart from `exceptions`) are unnamed or
#' if their names are partially matched.
#'
#' @param exceptions Arguments that can be unnamed.
#'
#' @noRd
ensure_args_are_named <- function(exceptions = NULL) {
  args_in_call_from_user <- rlang::call_args_names(rlang::caller_call())
  unnamed_exceptions <- setdiff(exceptions, args_in_call_from_user)
  unnamed_args <- args_in_call_from_user[which(args_in_call_from_user == "")]
  named_args <- args_in_call_from_user[which(
    !is.null(args_in_call_from_user) & args_in_call_from_user != ""
  )]

  args_in_function_def <- names(formals(rlang::caller_fn()))
  partially_matched_names <- setdiff(named_args, args_in_function_def)

  # If the original function has `...` then the user can pass extra arguments whose names
  # shouldn't be compared to the function arguments.
  if (
    !("..." %in% args_in_function_def) && length(partially_matched_names) > 0
  ) {
    cli::cli_abort(
      c(
        "Argument names must be fully written.",
        "i" = "Partially matched argument name{?s}: {.val {cli::cli_vec(partially_matched_names)}}"
      ),
      call = rlang::caller_env()
    )
  }

  if (length(unnamed_args) > length(unnamed_exceptions)) {
    extra <- if (length(exceptions) > 0) {
      " (except for {.val {cli::cli_vec(exceptions)}})"
    } else {
      ""
    }
    msg <- paste0("All arguments must be named", extra, ".")
    n <- length(unnamed_args) - length(unnamed_exceptions)
    cli::cli_abort(
      c(
        msg,
        "i" = "Currently, there {?is/are} {n} argument{?s} that should be named."
      ),
      call = rlang::caller_env()
    )
  }
}
