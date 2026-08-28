#' Check whether there are unnamed arguments that should be named.
#'
#' @param exceptions Arguments that can be unnamed
#'
#' @noRd
ensure_args_are_named <- function(exceptions = NULL) {
  args_in_call_from_user <- rlang::call_args_names(rlang::caller_call())
  unnamed_exceptions <- setdiff(exceptions, args_in_call_from_user)
  unnamed_args <- args_in_call_from_user[which(args_in_call_from_user == "")]

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
