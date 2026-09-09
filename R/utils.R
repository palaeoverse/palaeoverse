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

  # When the function that is being checked is used internally, the call may contain
  # `...` (e.g. when `tax_unique()` is called from `group_apply()`). We remove `...`
  # from the count of unnamed arguments.
  args_values_in_call_from_user <- rlang::call_args(rlang::caller_call())
  dots_explicitly_passed <- vapply(
    args_values_in_call_from_user,
    function(x) {
      identical(rlang::expr_text(x), "...")
    },
    FUN.VALUE = logical(1)
  )
  args_in_call_from_user <- args_in_call_from_user[!dots_explicitly_passed]

  unnamed_exceptions <- setdiff(exceptions, args_in_call_from_user)
  unnamed_args <- args_in_call_from_user[which(args_in_call_from_user == "")]
  named_args <- args_in_call_from_user[which(
    !is.null(args_in_call_from_user) & args_in_call_from_user != ""
  )]

  args_in_function_def <- names(formals(rlang::caller_fn()))

  # Dots could be passed to other functions and we can't check whether they are partially
  # named.
  if ("..." %in% args_in_function_def) {
    partially_matched_names <- NULL
  } else {
    partially_matched_names <- setdiff(named_args, args_in_function_def)
  }
  if (length(partially_matched_names) > 0) {
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
