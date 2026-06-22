### Shared helper for the check-rds-*.R scripts.
###
### palaeorotate() relies on external web services (GPlates, Zenodo). When those
### are unreachable, the call errors out. We do NOT want to report this as a
### failure in scheduled CI (it would be a false positive). fetch_or_skip()
### runs an API call and, if it fails for connectivity/server reasons, stops the
### script early *without error* (exit status 0) after printing an explanatory
### message. Any other error, as well as a genuine difference in the results,
### still fails as usual.

fetch_or_skip <- function(expr) {
  tryCatch(
    force(expr),
    error = function(e) {
      msg <- conditionMessage(e)
      connectivity <- grepl(
        paste(
          "not available",
          "not connected",
          "Could not resolve",
          "Timeout was reached",
          "download rotation file",
          "Connection (timed out|refused)",
          "HTTP",
          sep = "|"
        ),
        msg,
        ignore.case = TRUE
      )
      if (connectivity) {
        message(
          "Skipping RDS check: the external web service appears ",
          "to be unreachable (connectivity issue), this is not a ",
          "real change.\nOriginal error: ",
          msg
        )
        quit(save = "no", status = 0)
      }
      stop(e)
    }
  )
}
