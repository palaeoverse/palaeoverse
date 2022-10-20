#' Palaeorotate fossil occurrences
#'
#' A function to generate palaeocoordinates for fossil occurrence data
#' (i.e., reconstruct the geographic distribution of organisms'
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
#' @param model \code{character}. The name of the plate rotation model to be
#' used to reconstruct palaeocoordinates. See details for available models.
#' @param method \code{character}. Method used to calculate palaeocoordinates
#' for fossil occurrences. Either "grid" (default) to use reconstruction files,
#' or "point" to use the GPlates API service. See details section for specific
#' details.
#' @param uncertainty \code{logical}. Should the uncertainty in
#' palaeogeographic reconstructions be returned? If set to TRUE, the
#' palaeocoordinates from all reconstruction files (models) are returned, along
#' with their respective palaeolatitudinal range and the maximum Great
#' Circle Distance between palaeocoordinates (in km). This argument is only
#' relevant if `method` is set to "grid".
#' @param round \code{numeric}. Numeric value indicating the number of decimal
#' places `lng`, `lat` and `age` should be rounded to. This functionality is
#' only relevant for the "point" `method`. Rounding can speed up palaeorotation
#' by reducing the number of unique coordinate pairs. Defaults to `NULL`
#' (no rounding of values).
#'
#' @return A \code{dataframe} containing the original input occurrence
#' dataframe, the rotation model ("rot_model"),
#'  age of rotation ("rot_age"), the reference coordinates rotated
#' ("rot_lng" and "rot_lat"), and the reconstructed coordinates
#' (i.e., "p_lng" and "p_lat"). The "point" `method` uses the input coordinates
#' and age as the reference; reference coordinates are therefore not returned.
#' If uncertainty is set to `TRUE`, palaeocoordinates for all available models
#' will be returned, along with the palaeolatitudinal range (`range_p_lat`) and
#' the maximum Great Circle Distance (`max_dist`) in km (calculated via
#' \code{\link[geosphere]{distHaversine}}).
#'
#' @details This function can generate palaeocoordinates using two different
#' approaches (`method`):
#'
#' - Reconstruction files: The "grid" `method` uses reconstruction files to
#' spatiotemporally
#' link present-day geographic coordinates and age estimates with a spatial
#' grid (1&deg; x 1&deg;) rotated to the midpoint of Phanerozoic (0--540 Ma)
#' stratigraphic stages (Geological Timescale, 2020). If specific ages of
#' rotation are required, or fine-scale spatial analyses are being conducted,
#' use of the "point" `method` might be preferable for the user (particularly
#' if occurrences are close to plate boundaries). As implemented, points within
#' the same grid cell will be assigned equivalent palaeocoordinates due to
#' spatial aggregation. The reconstruction files provide pre-generated
#' palaeocoordinates for a grid of 1&deg; x 1&deg;, allowing the past
#' distribution of fossil occurrences to be estimated efficiently. Access to
#' the reconstruction files and documentation is available via the
#' [palaeorotate](https://github.com/LewisAJones/palaeorotate) package.
#' Note: each reconstruction file is 5--10 MB in size.
#'
#' - GPlates API: The "point" `method` uses the [GPlates Web Service](
#' https://gwsdoc.gplates.org) to reconstruct palaeorotations for point
#' data. The use of this `method` is slower than the "grid" `method` if many
#' unique time intervals exist in your dataset. However, it provides
#' palaeocoordinates with higher precision.
#'
#' Available models and timespan for each `method`:
#' - "MERDITH2021" (Merdith et al., 2021)
#'   - 0--540 Ma (grid)
#'   - 0--1000 Ma (point)
#' - "MULLER2019" (Müller et al., 2019)
#'   - 0--540 Ma (grid)
#'   - 0--1100 Ma (point)
#' - "MULLER2016" (Müller et al., 2016)
#'   - 0--230 Ma (grid/point)
#' - "MATTHEWS2016_mantle_ref" (Matthews et al., 2016)
#'   - 0--410 Ma (grid/point)
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
#' @section References:
#'
#' - Matthews, K.J., Maloney, K.T., Zahirovic, S., Williams, S.E., Seton, M.,
#' and Müller, R.D. (2016). Global plate boundary evolution and kinematics
#' since the late Paleozoic, Global and Planetary Change, 146, 226-250.
#' \doi{10.1016/j.gloplacha.2016.10.002}.
#'
#' - Merdith, A., Williams, S.E., Collins, A.S., Tetley, M.G., Mulder, J.A.,
#' Blades, M.L., Young, A., Armistead, S.E., Cannon, J., Zahirovic, S.,
#' Müller. R.D. (2021).
#' Extending full-plate tectonic models into deep time: Linking the
#' Neoproterozoic and the Phanerozoic.
#' Earth-Science Reviews 214 (103477). \doi{10.1016/j.earscirev.2020.103477}.
#'
#' - Müller, R. D., Zahirovic, S., Williams, S. E., Cannon, J., Seton, M.,
#' Bower, D. J., Tetley, M. G., Heine, C., Le Breton, E., Liu, S.,
#' Russell, S. H. J., Yang, T., Leonard, J., and Gurnis, M. (2019).
#' A global plate model including lithospheric deformation along major rifts
#' and orogens since the Triassic.
#' Tectonics, vol. 38. \doi{10.1029/2018TC005462}.
#'
#' - Müller R.D., Seton, M., Zahirovic, S., Williams, S.E., Matthews, K.J.,
#' Wright, N.M., Shephard, G.E., Maloney, K.T., Barnett-Moore, N.,
#' Hosseinpour, M., Bower, D.J., Cannon, J., 2016.
#' Ocean basin evolution and global-scale plate reorganization events since
#' Pangea breakup, Annual Review of Earth and Planetary Sciences, Vol 44,
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
#' Earth-Science Reviews, Volume 113, Issues 3-4, July 2012, Pages 212--270.
#' \doi{10.1016/j.earscirev.2012.03.002}.
#'
#' - Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013). Towards
#' community-driven paleogeographic
#' reconstructions: integrating open-access paleogeographic and paleobiology
#' data with plate tectonics.
#' Biogeosciences, 10(3), 1529–1541. \doi{10.5194/bg-10-1529-2013}.
#'
#' See [GPlates documentation](https://gwsdoc.gplates.org/reconstruction)
#' for additional information and details.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Kilian Eichenseer & Lucas Buffan
#' @importFrom geosphere distm distHaversine
#' @importFrom utils download.file
#' @importFrom pbapply pblapply
#' @importFrom httr RETRY GET content
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
#' ex4 <- palaeorotate(occdf = tetrapods, uncertainty = TRUE)
#' }
#' @export
palaeorotate <- function(occdf, lng = "lng", lat = "lat", age = "age",
                         model = "MERDITH2021", method = "grid",
                         uncertainty = FALSE, round = NULL) {
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
    stop("`uncertainty` should be a logical value: TRUE/FALSE")
  }

  # Model available?
  available <- c("MERDITH2021",
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
  if (is.na(model)) {
    stop("Unavailable model. Choose one from the following: \n",
         toString(available))
  }

  # Set-up ------------------------------------------------------------------
  # Set up dataframe for populating
  occdf$rot_model <- model
  if (uncertainty == TRUE) {
    occdf$rot_model <- "All available"
    }

  # Should coordinates be rounded off?
  if (!is.null(round)) {
    occdf[, c(lng, lat, age)] <- round(occdf[, c(lng, lat, age)],
                                       digits = round)
  }

  # Unique localities for rotating
  coords <- unique(occdf[, c(lng, lat, age)])
  # Add columns for populating
  coords$p_lng <- NA
  coords$p_lat <- NA
  uni_ages <- unique(coords[, c(age)])

  # Grid rotations ----------------------------------------------------------
  if (method == "grid") {
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
      BASE = "https://github.com/LewisAJones/palaeorotate/raw/master/data-raw/",
      MERDITH2021 = "MERDITH2021.RDS",
      PALEOMAP = "PALEOMAP.RDS",
      GOLONKA = "GOLONKA.RDS",
      MULLER2019 = "MULLER2019.RDS",
      MULLER2016 = "MULLER2016.RDS",
      SETON2012 = "SETON2012.RDS",
      MATTHEWS2016_pmag_ref = "MATTHEWS2016_pmag_ref.RDS",
      MATTHEWS2016_mantle_ref = "MATTHEWS2016_mantle_ref.RDS"
    )

    if (uncertainty == TRUE) {
      # Download all rotations
      nme <- names(rot_files[-1])
      # Already downloaded check
      if (any(file.exists(paste0(files, "/", nme, ".RDS")) == FALSE)) {
        for (f in nme) {
          # Generate download link
          dl <- paste0(rot_files$BASE, rot_files[f], sep = "")
          # Download file
          download.file(url = dl,
                        destfile = paste0(files, "/", f, ".RDS"),
                        mode = dl_mode)
        }
      }
      # Load reconstruction files
      for (f in nme) {
        assign(f, readRDS(paste0(files, "/", f, ".RDS")))
      }
    } else {
      # Download specific reconstruction file
      if (!file.exists(paste0(files, "/", model, ".RDS"))) {
        dl <- paste0(rot_files$BASE, rot_files[model])
        download.file(url = dl,
                      destfile = paste0(files, "/", model, ".RDS"),
                      mode = dl_mode)
      }
    }

    # Generate reference object for linking
    assign("base_model", readRDS(paste0(files, "/", model, ".RDS")))

    # Get available rotation ages
    rot_age <- colnames(base_model)[3:ncol(base_model)]
    rot_age <- unique(as.numeric(sub(".*_", "", rot_age)))

    # Calculate rotation ages for data
    occdf$rot_age <- rot_age[sapply(seq_len(nrow(occdf)), function(i) {
        which.min(abs(occdf[i, age] - rot_age))
      })]

    # Search for matching longitude and  latitude
    occdf$rot_lng <- sapply(seq_len(nrow(occdf)), function(i) {
      # Extract closest longitude
      base_model[which.min(abs(base_model[, c("lng")]  - occdf[i, lng])), 1]
    }, simplify = TRUE)

    occdf$rot_lat <- sapply(seq_len(nrow(occdf)), function(i) {
      # Extract closest latitude
      base_model[which.min(abs(base_model[, c("lat")]  - occdf[i, lat])), 2]
    }, simplify = TRUE)

    # Generate row index
    pc_ind <- sapply(seq_len(nrow(occdf)), function(i) {
      which(base_model[, c("lng")] == occdf[i, "rot_lng"] &
              base_model[, c("lat")] == occdf[i, "rot_lat"])
    })

    # Assign coordinates and calculate uncertainty
    if (uncertainty == TRUE) {
      # Create empty list
      rot_files <- list()
      # Load rotations
      for (f in seq_along(nme)) {
        rot_files[[f]] <- readRDS(paste0(files, "/", nme[f], ".RDS"))
      }
      names(rot_files) <- nme
      # Assign coordinates
      for (f in nme) {
        # Get reconstruction file
        tmp <- rot_files[[f]]
        # Find matching palaeocoordinates
        tmp <- data.frame(
          t(
            sapply(seq_len(nrow(occdf)), function(i) {
              as.numeric(tmp[pc_ind[i],
                c(paste0("lng_", occdf$rot_age[i]),
                  paste0("lat_", occdf$rot_age[i]))])
        })))
        colnames(tmp) <- c(paste0("p_lng_", f), paste0("p_lat_", f))
        occdf <- cbind.data.frame(occdf, tmp)
      }

      # Calculate uncertainty (range)
      lng_nme <- paste0("p_lng_", nme)
      lat_nme <- paste0("p_lat_", nme)
      uncertain_lng <- occdf[, lng_nme]
      uncertain_lat <- occdf[, lat_nme]

      # Calculate palaeolatitudinal range
      range_p_lat <- vector("numeric")
      for (i in seq_len(nrow(uncertain_lat))) {
        tmp_lat <- na.omit(as.numeric(uncertain_lat[i, ]))
        if (length(tmp_lat) <= 1) {
          range_p_lat[i] <- NA
          next
        }
        range_p_lat[i] <- max(as.numeric(tmp_lat)) -
          min(as.numeric(tmp_lat))
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
        }
        # Calculate GCD matrix
        dist <- geosphere::distm(x = tmpdf,
                                 fun = geosphere::distHaversine)
        # Convert to km
        dist <- dist / 10^3
        # Get maximum GCD in km
        max_dist[i] <- round(as.numeric(max(dist)), digits = 2)
      }

      # Bind data
      occdf <- cbind.data.frame(occdf, range_p_lat, max_dist)

    } else {
      tmp <- data.frame(
        t(
          sapply(seq_len(nrow(occdf)), function(i) {
            as.numeric(base_model[pc_ind[i],
                           c(paste0("lng_", occdf$rot_age[i]),
                             paste0("lat_", occdf$rot_age[i]))])
          })))
      colnames(tmp) <- c("p_lng", "p_lat")
      occdf <- cbind.data.frame(occdf, tmp)
    }
  }

  # Point rotations ---------------------------------------------------------
  # Point method to be used?
  if (method == "point") {
  # Define maximum chunk size for API calls
  chunks <- 300
  # Run across unique ages
  rotations <- pbapply::pblapply(X = uni_ages, function(i) {
    # Subset to age of interest
    tmp <- coords[which(coords[, age] == i), ]
    # How many rows?
    nr <- nrow(tmp)
    # Size of chunks to be rotated
    chk <- nr / chunks
    # Generate chunk bins
    chk <- chunks * 1:chk
    # Add starting value
    chk <- append(0, chk)
    # Chunk size exceeds number of rows?
    if (chk[2] > nr) {
      chk <- append(0, nr)
    }
    for (x in 2:length(chk)) {
    # Lower index
    ind_l <- chk[x - 1] + 1
    # Upper index
    ind_u <- chk[x]
    # Generate API
    tmp_chunk <- tmp[ind_l:ind_u, c(lng, lat)]
    tmp_chunk <- toString(as.vector(t(tmp_chunk)))
    api <- sprintf("?points=%s&time=%f&model=%s",
                   gsub(" ", "", tmp_chunk), i, model)
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
    # Update col names
    colnames(rots) <- c("p_lng", "p_lat")
    # Return data
    tmp[ind_l:ind_u, c("p_lng", "p_lat")] <- rots
  }
    tmp
  })
    # Bind data
    rotations <- do.call(rbind, rotations)
    # Set-up matching
    rotations$match <- paste0(rotations[, lng],
                              rotations[, lat],
                              rotations[, age])
    occdf$match <- paste0(occdf[, lng],
                          occdf[, lat],
                          occdf[, age])

    # Set up empty columns for populating
    occdf$p_lng <- NA
    occdf$p_lat <- NA
    # Match data
    for (i in seq_len(nrow(rotations))) {
      match <- which(rotations$match[i] == occdf$match)
      occdf[match, c("p_lng", "p_lat")] <- rotations[i, c("p_lng", "p_lat")]
    }

    # Drop match column
    occdf <- occdf[, -which(colnames(occdf) == "match")]

    # Add warning
    if (any(!is.na(occdf$p_lng)) || any(!is.na(occdf$p_lat))) {
      if ((sum(occdf$p_lng == occdf[, lng], na.rm = TRUE) +
          sum(occdf$p_lng == occdf[, lng], na.rm = TRUE)) > 0) {
        message(
          paste0("Palaeocoordinates equal to input coordinates detected.",
                 "\n",
                 "Check desired model covers the temporal range of your data."
          )
        )
      }
    }
  }
  # Add warning
  if (any(is.na(occdf$p_lng) | is.na(occdf$p_lat))) {
    message(
      paste0("Palaeocoordinates could not be reconstructed for all points.",
             "\n",
             "Georeferenced plate does not exist at time of reconstruction."
      )
    )
  }
  # Return data
  return(occdf)
}
