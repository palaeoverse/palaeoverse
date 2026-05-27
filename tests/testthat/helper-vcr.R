library("vcr")

vcr_dir <- vcr::vcr_test_path("fixtures")

if (!dir.exists(vcr_dir)) {
  stop("No {vcr} cassettes, tests cannot be run.", call. = FALSE)
}

invisible(vcr::vcr_configure(dir = vcr_dir))
