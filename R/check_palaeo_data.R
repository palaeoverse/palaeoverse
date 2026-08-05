check_palaeo_data <- function(data) {
  # check names

  # check duplicated rows

  # check NA

  out <- data
  class(out) <- c("palaeoverse", class(out))
  out
}

dplyr_reconstruct.palaeoverse <- function(data, template) {
  sfc_name <- attr(template, "sf_column")
  if (inherits(template, "tbl_df")) {
    data <- dplyr::as_tibble(data)
  }
  # Return a bare data frame if the geometry column is no longer there
  if (sfc_name %in% names(data)) {
    # reconstruct sf:
    st_as_sf(
      data,
      sf_column_name = sfc_name,
      crs = st_crs(template),
      precision = st_precision(template)
    )
  } else {
    data
  }
}
