#' Palaeorotate fossil occurrences
#'
#' A function to generate palaeocoordinates for fossil occurrence data
#' (i.e. reconstruct the geographic distribution of organisms'
#' remains at time of deposition). Each occurrence is assigned
#' palaeocoordinates based on its current geographic position and age
#' estimate.
#'
#' @param occdf \code{dataframe}. Fossil occurrences to be palaeogeographically
#' reconstructed. \code{occdf} should contain columns with longitudinal and
#' latitudinal values, as well as age estimates. The age of rotation should be
#' supplied in millions of years before present.
#' @param lng \code{character}. The name of the column you wish to be treated as
#' longitude (defaults to "lng").
#' @param lat \code{character}. The name of the column you wish to be treated as
#' latitude (defaults to "lat").
#' @param age \code{character}. The name of the column you wish to be treated as
#' the age for rotation (defaults to "age").
#' @param model \code{character}. The name(s) of the plate rotation model(s)
#' to be used to reconstruct palaeocoordinates. See details for available
#' models.
#' @param method \code{character}. Method used to calculate palaeocoordinates
#' for fossil occurrences. Either "grid" to use reconstruction files,
#' or "point" (default) to use the GPlates API service. See details section
#' for specific details.
#' @param uncertainty \code{logical}. Should the uncertainty in
#' palaeogeographic reconstructions be returned? If set to TRUE, the
#' palaeolatitudinal range and maximum geographic distance (in km) between
#' output palaeocoordinates are calculated. This argument is only relevant if
#' more than one plate rotation model is specified in `model`.
#' @param round \code{numeric}. Numeric value indicating the number of decimal
#' places `lng`, `lat` and `age` should be rounded to. This functionality is
#' only relevant for the "point" `method`. Rounding can speed up palaeorotation
#' by reducing the number of unique coordinate pairs. Defaults to a value of
#' 3. If no rounding is desired, set this value to `NULL`.
#'
#' @return A \code{dataframe} containing the original input occurrence
#'   dataframe and the reconstructed coordinates (i.e. "p_lng", "p_lat"). The
#'   "grid" `method` also returns the age of rotation ("rot_age") and the
#'   reference coordinates rotated ("rot_lng" and "rot_lat"). If only one
#'   model is requested, a column containing the rotation model used
#'   ("rot_model") is also appended. Otherwise, the name of each model is
#'   appended to the name of each column containing palaeocoordinates (e.g.
#'   "p_lng_GOLONKA"). If `uncertainty` is set to `TRUE`, the
#'   palaeolatitudinal range ("range_p_lat") and the maximum geographic
#'   distance ("max_dist") in km between palaeocoordinates will also be
#'   returned (the latter calculated via \code{\link[geosphere]{distGeo}}).
#'
#' @details This function can generate palaeocoordinates using two different
#'   approaches (`method`):
#'
#' - Reconstruction files: The "grid" `method` uses reconstruction files to
#'   spatiotemporally link present-day geographic coordinates and age
#'   estimates with an equal-area hexagonal grid (spacing = 100 km) rotated
#'   to the midpoint of Phanerozoic (0--540 Ma) stratigraphic stages
#'   (Geological Time Scale, 2020). The grid was generated using the
#'   \code{\link[h3jsr]{h3jsr}} R package and 'h3_resolution' 3 (see
#'   \code{\link[h3jsr]{h3_info_table}}). If specific ages of rotation are
#'   required, or fine-scale spatial analyses are being conducted, use of the
#'   "point" `method` is recommended (particularly if occurrences are close to
#'   plate boundaries). As implemented, points within the same grid cell will
#'   be assigned equivalent palaeocoordinates due to spatial aggregation.
#'   However, the reconstruction files provide pre-generated palaeocoordinates
#'   enabling efficient estimation of the past distribution of fossil
#'   occurrences. The reconstruction files along with additional documentation
#'   are deposited on [Zenodo](https://zenodo.org/record/7615203). Note: each
#'   reconstruction file is 5--10 MB in size.
#'
#' - GPlates API: The "point" `method` uses the [GPlates Web Service](
#'   https://gwsdoc.gplates.org) to reconstruct palaeorotations for point
#'   data. The use of this `method` is slower than the "grid" `method` if many
#'   unique time intervals exist in your dataset. However, it provides
#'   palaeocoordinates with higher precision.
#'
#' Available models and timespan for each `method`:
#' - "MERDITH2021" (Merdith et al., 2021)
#'   - 0--540 Ma (grid)
#'   - 0--1000 Ma (point)
#' - "MULLER2016" (Müller et al., 2016)
#'   - 0--230 Ma (grid/point)
#' - "MATTHEWS2016_pmag_ref"  (Matthews et al., 2016)
#'   - 0--410 Ma (grid/point)
#' - "SETON2012" (Seton et al., 2012)
#'   - 0--200 Ma (grid/point)
#' - "GOLONKA" (Wright et al., 2013)
#'   - 0--540 Ma (grid/point)
#' - "PALEOMAP" (Scotese & Wright, 2018)
#'   - 0--540 Ma (grid)
#'   - 0--750 Ma (point)
#'
#' Access is also provided for the following mantle reference frame models.
#' However, they are generally not recommended for reconstructing
#' palaeocoordinates.
#'
#' - "MULLER2022" (Müller et al., 2022)
#'   - 0--540 Ma (grid)
#'   - 0--1000 Ma (point)
#' - "MULLER2019" (Müller et al., 2019)
#'   - 0--250 Ma (grid/point)
#' - "MATTHEWS2016_mantle_ref" (Matthews et al., 2016)
#'   - 0--410 Ma (grid/point)
#'
#' @section References:
#'
#' - Matthews, K.J., Maloney, K.T., Zahirovic, S., Williams, S.E., Seton, M.,
#' and Müller, R.D. (2016). Global plate boundary evolution and kinematics
#' since the late Paleozoic. Global and Planetary Change, 146, 226-250.
#' \doi{10.1016/j.gloplacha.2016.10.002}.
#'
#' - Merdith, A., Williams, S.E., Collins, A.S., Tetley, M.G., Mulder, J.A.,
#' Blades, M.L., Young, A., Armistead, S.E., Cannon, J., Zahirovic, S.,
#' Müller. R.D. (2021).
#' Extending full-plate tectonic models into deep time: Linking the
#' Neoproterozoic and the Phanerozoic.
#' Earth-Science Reviews, 214(103477). \doi{10.1016/j.earscirev.2020.103477}.
#'
#' - Müller, R. D., Flament, N., Cannon, J., Tetley, M. G., Williams,
#' S. E., Cao, X., Bodur, Ö. F., Zahirovic, S., and Merdith, A. (2022).
#' A tectonic-rules-based mantle reference frame since 1 billion years ago –
#' implications for supercontinent cycles and plate–mantle system evolution,
#' Solid Earth, 13, 1127–1159. \doi{10.5194/se-13-1127-2022}.
#'
#' - Müller, R. D., Zahirovic, S., Williams, S. E., Cannon, J., Seton, M.,
#' Bower, D. J., Tetley, M. G., Heine, C., Le Breton, E., Liu, S.,
#' Russell, S. H. J., Yang, T., Leonard, J., and Gurnis, M. (2019).
#' A global plate model including lithospheric deformation along major rifts
#' and orogens since the Triassic.
#' Tectonics, 38(6) 1884-1907. \doi{10.1029/2018TC005462}.
#'
#' - Müller R.D., Seton, M., Zahirovic, S., Williams, S.E., Matthews, K.J.,
#' Wright, N.M., Shephard, G.E., Maloney, K.T., Barnett-Moore, N.,
#' Hosseinpour, M., Bower, D.J., Cannon, J. (2016).
#' Ocean basin evolution and global-scale plate reorganization events since
#' Pangea breakup. Annual Review of Earth and Planetary Sciences 44(1),
#' 107-138. \doi{10.1146/annurev-earth-060115-012211}.
#'
#' - Scotese, C., & Wright, N. M. (2018). PALEOMAP Paleodigital Elevation Models
#' (PaleoDEMs) for the
#' Phanerozoic. [PALEOMAP Project](
#' https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/).
#'
#' - Seton, M., Müller, R.D., Zahirovic, S., Gaina, C., Torsvik, T.H.,
#' Shephard, G., Talsma, A., Gurnis, M., Turner, M., Maus, S., Chandler, M.
#' (2012). Global continental and ocean basin reconstructions since 200 Ma.
#' Earth-Science Reviews, 113(3-4), 212-270.
#' \doi{10.1016/j.earscirev.2012.03.002}.
#'
#' - Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013). Towards
#' community-driven paleogeographic
#' reconstructions: integrating open-access paleogeographic and paleobiology
#' data with plate tectonics.
#' Biogeosciences, 10(3), 1529-1541. \doi{10.5194/bg-10-1529-2013}.
#'
#' See [GPlates documentation](https://gwsdoc.gplates.org/reconstruction)
#' for additional information and details.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Kilian Eichenseer, Lucas Buffan & Will Gearty
#' @importFrom geosphere distm distHaversine
#' @importFrom h3jsr point_to_cell
#' @importFrom sf st_as_sf
#' @importFrom utils download.file
#' @importFrom pbapply pblapply
#' @importFrom httr RETRY GET content
#' @importFrom curl nslookup
#' @importFrom stats na.omit
#' @examples
#' \dontrun{
#' #Generic example with a few occurrences
#' occdf <- data.frame(lng = c(2, -103, -66),
#'                 lat = c(46, 35, -7),
#'                 age = c(88, 125, 200))
#'
#' #Calculate palaeocoordinates using reconstruction files
#' ex1 <- palaeorotate(occdf = occdf, method = "grid")
#'
#' #Calculate palaeocoordinates using the GPlates API
#' ex2 <- palaeorotate(occdf = occdf, method = "point")
#'
#' #Calculate uncertainity in palaeocoordinates from models
#' ex3 <- palaeorotate(occdf = occdf,
#'                     method = "grid",
#'                     model = c("MERDITH2021",
#'                               "GOLONKA",
#'                               "PALEOMAP"),
#'                     uncertainty = TRUE)
#'
#' #Now with some real fossil occurrence data!
#'
#' #Grab some data from the Paleobiology Database
#' data(tetrapods)
#'
#' #Assign midpoint age of fossil occurrence data for reconstruction
#' tetrapods$age <- (tetrapods$max_ma + tetrapods$min_ma)/2
#'
#' #Rotate the data
#' ex3 <- palaeorotate(occdf = tetrapods)
#'
#' #Calculate uncertainity in palaeocoordinates from models
#' ex4 <- palaeorotate(occdf = tetrapods,
#'                     model = c("MERDITH2021",
#'                               "GOLONKA",
#'                               "PALEOMAP",
#'                               "SETON2012"),
#'                     uncertainty = TRUE)
#' }
#' @export
palaeorotate <- function(occdf, lng = "lng", lat = "lat", age = "age",
                         model = "MERDITH2021", method = "point",
                         uncertainty = FALSE, round = 3) {
  # Error-handling ----------------------------------------------------------
  if (!exists("occdf") || !is.data.frame(occdf)) {
    stop("Please supply occdf as a dataframe")
  }

  if (any(c(lng, lat, age) %in% colnames(occdf) == FALSE)) {
    stop("defined `lng`, `lat`, or `age` not found in `occdf`")
  }

  if (any(!is.numeric(occdf[, lat]), is.na(occdf[, lat]),
          !is.numeric(occdf[, lng]), is.na(occdf[, lng]),
          !is.numeric(occdf[, age]), is.na(occdf[, age]))) {
    stop("`lng`, `lat` and `age` should be of class numeric")
  }

  if (any(occdf[, age] < 0)) {
    stop("`age` contains negative values. Input ages should be positive.")
  }

  if (sum(abs(occdf[, lat]) > 90) != 0) {
    stop("`lat` values should be >= -90\u00B0 and <= 90\u00B0")
  }

  if (sum(abs(occdf[, lng]) > 180) != 0) {
    stop("`lng` values should be >= -180\u00B0 and <= 180\u00B0")
  }

  if (method %in% c("grid", "point") == FALSE) {
    stop("`method` should be grid or point")
  }

  if (!is.null(round) && !is.numeric(round)) {
    stop("`round` should be NULL or of class numeric")
  }

  if (!is.logical(uncertainty)) {
    stop("`uncertainty` should be of class logical (TRUE/FALSE)")
  }

  # Add warnings for use of mantle reference frame models
  if (any(model %in% c("MULLER2022", "MULLER2019",
                   "MATTHEWS2016_mantle_ref"))) {
    warning(paste0("Selected model(s) use a mantle reference frame and are ",
    "not recommended for reconstructing palaeocoordinates. See details."))
  }

  if (length(model) < 2 && uncertainty == TRUE) {
    stop("At least two models are required to calculate `uncertainty`.")
  }

  # Model available?
  available <- c("MULLER2022",
                 "MERDITH2021",
                 "MULLER2019",
                 "MULLER2016",
                 "MATTHEWS2016_mantle_ref",
                 "MATTHEWS2016_pmag_ref",
                 "SETON2012",
                 "GOLONKA",
                 "PALEOMAP")
  # Match input
  model <- available[charmatch(x = model, table = available)]
  # Invalid model input?
  if (any(is.na(model))) {
    stop("Unavailable model(s). Choose one from the following: \n",
         toString(available))
  }

  # Set-up ------------------------------------------------------------------
  # If only one model selected, add column of model name
  if (length(model) == 1) {
    occdf$rot_model <- model
  }

  # Create columns for coordinates
  mdls <- data.frame(matrix(nrow = nrow(occdf), ncol = length(model) * 2))
  cnames <- paste(paste0("p_lng_", model), paste0("p_lat_", model))
  cnames <- strsplit(x = cnames, split = " ")
  colnames(mdls) <- unlist(cnames)
  occdf <- cbind.data.frame(occdf, mdls)
  # Should coordinates be rounded off?
  if (!is.null(round)) {
    occdf[, c(lng, lat, age)] <- round(occdf[, c(lng, lat, age)],
                                       digits = round)
  }
  # Unique localities for rotating
  coords <- unique(occdf[, c(lng, lat, age)])
  # Add columns for populating
  uni_ages <- unique(coords[, c(age)])

  # Grid rotations ----------------------------------------------------------
  if (method == "grid") {
    # Check Zenodo (or user) is online
    tryCatch(
      {
        nslookup("zenodo.org")
      },
      error = function(e) {
        stop(paste0("Zenodo is not available.",
        " Either the website is down or you are not connected ",
        "to the internet."),
             call. = FALSE)
      })
    # Get temp directory and download files
    files <- tempdir()
    # OS-specific mode for downloading
    if (.Platform$OS.type == "windows") {
      dl_mode <- "wb"
    } else {
      dl_mode <- "w"
    }
    # Reconstruction files
    rot_files <- list(
      BASE = "https://zenodo.org/record/7615203/files/",
      MULLER2022 = "MULLER2022.RDS",
      MERDITH2021 = "MERDITH2021.RDS",
      PALEOMAP = "PALEOMAP.RDS",
      GOLONKA = "GOLONKA.RDS",
      MULLER2019 = "MULLER2019.RDS",
      MULLER2016 = "MULLER2016.RDS",
      SETON2012 = "SETON2012.RDS",
      MATTHEWS2016_pmag_ref = "MATTHEWS2016_pmag_ref.RDS",
      MATTHEWS2016_mantle_ref = "MATTHEWS2016_mantle_ref.RDS"
    )
    # Rotations files to download
    nme <- names(rot_files[model])
    # Already downloaded check
    nme <- nme[which(file.exists(paste0(files, "/", nme, ".RDS")) == FALSE)]
    if (length(nme) != 0) {
      for (f in nme) {
        # Generate download link
        dl <- paste0(rot_files$BASE, rot_files[f], sep = "")
        # Download file
        download.file(url = dl,
                      destfile = paste0(files, "/", f, ".RDS"),
                      mode = dl_mode)
        }
    }
    # Generate reference object for linking
    assign("ref_model", readRDS(paste0(files, "/", model[1], ".RDS")))

    # Get available rotation ages
    rot_age <- colnames(ref_model)[3:ncol(ref_model)]
    rot_age <- unique(as.numeric(sub(".*_", "", rot_age)))

    # Calculate rotation ages for data
    occdf$rot_age <- rot_age[sapply(seq_len(nrow(occdf)), function(i) {
        which.min(abs(occdf[i, age] - rot_age))
      })]

    # Set-up
    # Convert to sf object and add CRS
    occdf_sf <- st_as_sf(x = occdf,
                         coords = c(lng, lat),
                         remove = FALSE,
                         crs = "EPSG:4326")
    ref_model_sf <- st_as_sf(x = ref_model,
                              coords = c("lng", "lat"),
                              remove = FALSE,
                              crs = "EPSG:4326")

    # Match points with cells
    occ_cell <- point_to_cell(input = occdf_sf,
                              res = 3, # Grid resolution
                              simple = TRUE)
    model_cell <- point_to_cell(input = ref_model_sf,
                                res = 3, # Grid resolution
                                simple = TRUE)

    # Generate row index
    pc_ind <- match(x = occ_cell, table = model_cell)

    # Assign rotation coordinates
    occdf$rot_lng <- ref_model[pc_ind, c("lng")]
    occdf$rot_lat <- ref_model[pc_ind, c("lat")]

    for (m in model) {
      # Load reconstruction files
      assign("prm", readRDS(paste0(files, "/", m, ".RDS")))
      # Extract coordinates
      tmp <- data.frame(
        t(
          sapply(seq_len(nrow(occdf)), function(i) {
            as.numeric(prm[pc_ind[i],
                                 c(paste0("lng_", occdf$rot_age[i]),
                                   paste0("lat_", occdf$rot_age[i]))])
          })))
      colnames(tmp) <- c("p_lng", "p_lat")
      # Assign to columns
      occdf[, paste0("p_lng_", m)] <- tmp$p_lng
      occdf[, paste0("p_lat_", m)] <- tmp$p_lat
    }
    # If only one model, update column name
    if (length(model) == 1) {
      colnames(occdf)[which(
        colnames(occdf) == paste0("p_lng_", m))] <- c("p_lng")
      colnames(occdf)[which(
        colnames(occdf) == paste0("p_lat_", m))] <- c("p_lat")
    }
  }

  # Point rotations ---------------------------------------------------------
  # Point method to be used?
  if (method == "point") {
    # Check GPlates Web Service (or user) is online
    tryCatch(
      {
        nslookup("gws.gplates.org")
      },
      error = function(e) {
        stop(paste("GPlates Web Service is not available.",
        "Either the website is down or you are not connected to the internet."),
             call. = FALSE)
      })
  # Define maximum chunk size for API calls
  chunks <- 300
  # Run across models
  for (m in model) {
    # Inform user which model is running
    message(m)
    # Run across unique ages
    rotations <- pbapply::pblapply(X = uni_ages, function(i) {
      # Subset to age of interest
      tmp <- coords[which(coords[, age] == i), ]
      # How many rows?
      nr <- nrow(tmp)
      # Generate chunk bins
      chk <- seq(from = 0, to = nr + chunks, by = chunks)
      # Update final bin to equal nrow
      chk[length(chk)] <- nr
      # Run across chunks
      for (x in 2:length(chk)) {
      # Lower index
      ind_l <- chk[x - 1] + 1
      # Upper index
      ind_u <- chk[x]
      # Generate API
      tmp_chunk <- tmp[ind_l:ind_u, c(lng, lat)]
      tmp_chunk <- toString(as.vector(t(tmp_chunk)))
      api <- sprintf("?points=%s&time=%f&model=%s",
                     gsub(" ", "", tmp_chunk), i, m)
      api <- paste0("https://gws.gplates.org/reconstruct/reconstruct_points/",
                    api, "&return_null_points")
      # Call API
      rots <- httr::RETRY(verb = "GET",
                          url = api,
                          times = 5,
                          pause_min = 1,
                          pause_base = 1,
                          pause_cap = 10)
      # Extract coordinates
      rots <- httr::content(x = rots, as = "parsed")$coordinates
      # Replace NULL values with NA
      rpl <- which(rots == "NULL")
      if (length(rpl) != 0) {
        for (r in rpl) {
          rots[[r]] <- list(NA, NA)
        }
      }
      # Bind rows
      rots <- do.call(rbind.data.frame, rots)
      # col names
      colnames(rots) <- c("p_lng", "p_lat")
      # Replace modern returned coordinates with NA as some models do not
      # return NULL when outside of time range (e.g. SETON2012)
      rots[which(tmp[, lng] == rots[, c("p_lng")] &
            tmp[, lat] == rots[, c("p_lat")]), c("p_lng", "p_lat")] <- NA
      # Update col names if more than one model requested
      if (length(model) > 1) {
        colnames(rots) <- c(paste0("p_lng_", m), paste0("p_lat_", m))
      }
    }
      rots
    })
    # Bind data
    rotations <- do.call(rbind, rotations)
    coords <- cbind.data.frame(coords, rotations)
  }

  # Set-up matching
  coords$match <- paste0(coords[, lng],
                         coords[, lat],
                         coords[, age])
  occdf$match <- paste0(occdf[, lng],
                        occdf[, lat],
                        occdf[, age])

  # Match data
  for (i in seq_len(nrow(coords))) {
    match <- which(coords$match[i] == occdf$match)
    occdf[match, colnames(coords)] <- coords[i, ]
  }

  # Drop match column
  occdf <- occdf[, -which(colnames(occdf) == "match")]
  }

  # Uncertainty calculation -------------------------------------------------
  if (uncertainty) {
    # Calculate uncertainty (range)
    lng_nme <- paste0("p_lng_", model)
    lat_nme <- paste0("p_lat_", model)
    uncertain_lng <- occdf[, lng_nme]
    uncertain_lat <- occdf[, lat_nme]

    # Calculate palaeolatitudinal range
    range_p_lat <- vector("numeric")
    for (i in seq_len(nrow(uncertain_lat))) {
      tmp_lat <- na.omit(as.numeric(uncertain_lat[i, ]))
      if (length(tmp_lat) <= 1) {
        range_p_lat[i] <- NA
        next
      } else {
        range_p_lat[i] <- max(as.numeric(tmp_lat)) - min(as.numeric(tmp_lat))
      }
    }

    # Calculate max distance between points
    max_dist <- vector("numeric")
    for (i in seq_len(nrow(uncertain_lat))) {
      # Get combination of coordinates for all models
      tmpdf <- cbind(p_lng = as.numeric(uncertain_lng[i, ]),
                     p_lat = as.numeric(uncertain_lat[i, ]))
      # Exclude NAs
      tmpdf <- na.omit(tmpdf)
      # Allocate NA if only one or less models are available
      if (nrow(tmpdf) <= 1) {
          max_dist[i] <- NA
          next
      } else {
        # Calculate GCD matrix
        dist <- geosphere::distm(x = tmpdf,
                                 fun = geosphere::distGeo)
        # Convert to km
        dist <- dist / 10^3
        # Get maximum GCD in km
        max_dist[i] <- round(as.numeric(max(dist)), digits = round)
        }
      }
      # Bind data
      occdf <- cbind.data.frame(occdf, range_p_lat, max_dist)
  }

  # Wrap up -----------------------------------------------------------------
  # Add warning
  if (length(model) == 1) {
    cnames <- c("p_lng", "p_lat")
  }
  if (any(is.na(occdf[, unlist(cnames)]))) {
    warning(
      paste0(
      "Palaeocoordinates could not be reconstructed for all points.",
        "\n",
        "Either assigned plate does not exist at time of ",
        "reconstruction or the plate rotation model(s) does not cover ",
        "the age of reconstruction."
      )
    )
  }
  # Return data
  return(occdf)
}
