#' Palaeorotate fossil occurrences
#'
#' A function to estimate palaeocoordinates for fossil occurrence data
#' (i.e. reconstruct the geographic distribution of organisms'
#' remains at time of deposition). Each occurrence is assigned
#' palaeocoordinates based on its current geographic position and age
#' estimate.
#'
#' @param occdf \code{data.frame}. Fossil occurrences to be
#'   palaeogeographically reconstructed. \code{occdf} should contain columns
#'   with longitudinal and latitudinal coordinates, as well as age estimates.
#'   The age of rotation should be supplied in millions of years before
#'   present.
#' @param lng \code{character}. The name of the column you wish to be treated
#'   as longitude (defaults to "lng").
#' @param lat \code{character}. The name of the column you wish to be treated
#'   as latitude (defaults to "lat").
#' @param age \code{character}. The name of the column you wish to be treated
#'   as the age for rotation (defaults to "age").
#' @param model \code{character}. The name(s) of the Global Plate Model(s)
#'   to be used to reconstruct palaeocoordinates. See details for available
#'   models.
#' @param method \code{character}. Method used to calculate palaeocoordinates
#'   for fossil occurrences. Either "grid" to use reconstruction files, or
#'   "point" (default) to use the GPlates API service. See details section for
#'   specific details.
#' @param uncertainty \code{logical}. Should the uncertainty in
#'   palaeogeographic reconstructions be returned? If set to TRUE (default),
#'   the palaeolatitudinal range and maximum geographic distance (in km)
#'   between output palaeocoordinates are calculated. This argument is only
#'   relevant if more than one Global Plate Model is specified in `model`.
#' @param round \code{numeric}. Numeric value indicating the number of decimal
#'   places `lng`, `lat` and `age` should be rounded to. This functionality is
#'   only relevant for the "point" `method`. Rounding can speed up
#'   palaeorotation by reducing the number of unique coordinate pairs.
#'   Defaults to a value of 3. If no rounding is desired, set this value to
#'   `NULL`.
#'
#' @return A \code{data.frame} containing the original input occurrence
#'   `data.frame` and the reconstructed coordinates (i.e. "p_lng", "p_lat"). The
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
#' @details This function can estimate palaeocoordinates using two different
#'   approaches (`method`):
#'
#' - Reconstruction files: The "grid" `method` uses reconstruction files from
#' Jones & Domeier (2024) to spatiotemporally link present-day geographic
#' coordinates and age estimates with a discrete global grid rotated at one
#' million-year time steps throughout the Phanerozoic (540--0 Ma). Here,
#' resolution 3 (~119 km spacing) of the reconstruction files is used. All
#' files, and the process used to generate them, are available and documented
#' in Jones & Domeier (2024). If fine-scale spatial analyses are being
#' conducted, use of the "point" `method` (see GPlates API below) may be
#' preferred (particularly if occurrences are close to plate boundaries). When
#' using the "grid" `method`, coordinates within the same grid cell will be
#' assigned equivalent palaeocoordinates due to spatial aggregation. However,
#' this approach enables efficient estimation of the past distribution of
#' fossil occurrences. Note: each reconstruction file is ~45 MB in size.
#'
#' - GPlates API: The "point" `method` uses the [GPlates Web Service](
#' https://gwsdoc.gplates.org) to reconstruct palaeocoordinates for point
#' data. The use of this `method` is slower than the "grid" `method` if many
#' unique time intervals exist in your dataset. However, it provides
#' palaeocoordinates with higher precision.
#'
#' Available models and timespan for each `method`:
#' - "MERDITH2021" (Merdith et al., 2021)
#'   - 0--1000 Ma (point)
#'   - 0--540 Ma (grid)
#' - "TorsvikCocks2017" (Torsvik and Cocks, 2016)
#'   - 0--540 Ma (point/grid)
#' - "PALEOMAP" (Scotese, 2016)
#'   - 0--1100 Ma (point)
#'   - 0--540 Ma (grid)
#' - "MATTHEWS2016_pmag_ref"  (Matthews et al., 2016)
#'   - 0--410 Ma (grid/point)
#' - "GOLONKA" (Wright et al., 2013)
#'   - 0--540 Ma (grid/point)
#'
#' @section References:
#'
#'- Jones, L.A., Domeier, M. A Phanerozoic gridded dataset for palaeogeographic
#'  reconstructions. Sci Data 11, 710 (2024). \doi{10.1038/s41597-024-03468-w}.
#'
#' - Matthews, K.J., Maloney, K.T., Zahirovic, S., Williams, S.E., Seton, M.,
#'   and Müller, R.D. (2016). Global plate boundary evolution and kinematics
#'   since the late Paleozoic. Global and Planetary Change, 146, 226-250.
#'   \doi{10.1016/j.gloplacha.2016.10.002}.
#'
#' - Merdith, A., Williams, S.E., Collins, A.S., Tetley, M.G., Mulder, J.A.,
#'   Blades, M.L., Young, A., Armistead, S.E., Cannon, J., Zahirovic, S.,
#'   Müller. R.D. (2021). Extending full-plate tectonic models into deep time:
#'   Linking the Neoproterozoic and the Phanerozoic. Earth-Science Reviews,
#'   214(103477). \doi{10.1016/j.earscirev.2020.103477}.
#'
#' - Scotese, C., & Wright, N. M. (2018). PALEOMAP Paleodigital Elevation Models
#'   (PaleoDEMs) for the Phanerozoic. [PALEOMAP Project](
#'   https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/).
#'
#' - Torsvik, T. H. & Cocks, L. R. M. Earth History and Palaeogeography.
#'   Cambridge University Press, 2016.
#'
#' - Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013). Towards
#'   community-driven paleogeographic reconstructions: integrating open-access
#'   paleogeographic and paleobiology data with plate tectonics. Biogeosciences,
#'   10(3), 1529-1541. \doi{10.5194/bg-10-1529-2013}.
#'
#' See [GPlates documentation](https://gwsdoc.gplates.org/reconstruction)
#' for additional information and details.
#'
#' @section Developer(s):
#'   Lewis A. Jones
#' @section Reviewer(s):
#'   Kilian Eichenseer, Lucas Buffan & Will Gearty
#' @importFrom geosphere distm distGeo
#' @importFrom h3jsr point_to_cell cell_to_point
#' @importFrom sf st_as_sf st_coordinates
#' @importFrom utils download.file
#' @importFrom pbapply pblapply
#' @importFrom httr GET content
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
#'                               "PALEOMAP"),
#'                     uncertainty = TRUE)
#' }
#' @export
palaeorotate <- function(occdf, lng = "lng", lat = "lat", age = "age",
                         model = "MERDITH2021", method = "point",
                         uncertainty = TRUE, round = 3) {
  # Error-handling ----------------------------------------------------------
  if (!exists("occdf") || !is.data.frame(occdf)) {
    stop("Please supply `occdf` as a data.frame.")
  }

  if (any(c(lng, lat, age) %in% colnames(occdf) == FALSE)) {
    stop("Defined `lng`, `lat`, or `age` not found in `occdf`.")
  }

  if (any(!is.numeric(occdf[, lat, drop = TRUE]),
          is.na(occdf[, lat, drop = TRUE]),
          !is.numeric(occdf[, lng, drop = TRUE]),
          is.na(occdf[, lng, drop = TRUE]),
          !is.numeric(occdf[, age, drop = TRUE]),
          is.na(occdf[, age, drop = TRUE]))) {
    stop("`lng`, `lat` and `age` should be of class numeric.")
  }

  if (any(occdf[, age, drop = TRUE] < 0)) {
    stop("`age` contains negative values. Input ages should be positive.")
  }

  if (sum(abs(occdf[, lat, drop = TRUE]) > 90) != 0) {
    stop("`lat` values should be >= -90\u00B0 and <= 90\u00B0.")
  }

  if (sum(abs(occdf[, lng, drop = TRUE]) > 180) != 0) {
    stop("`lng` values should be >= -180\u00B0 and <= 180\u00B0.")
  }

  if (method %in% c("grid", "point") == FALSE) {
    stop("`method` should be either 'grid' or 'point'.")
  }

  if (!is.null(round) && !is.numeric(round)) {
    stop("`round` should be NULL or of class numeric.")
  }

  if (!is.logical(uncertainty)) {
    stop("`uncertainty` should be of class logical (TRUE/FALSE).")
  }

  # Add stop for removed models
  removed <- c("MULLER2022", "MULLER2019", "MULLER2016",
               "MATTHEWS2016_mantle_ref", "SETON2012")
  m <- removed %in% model
  if (any(m)) {
    stop(paste0("Selected model(s) (", toString(removed[m]), ") have recently",
                " been removed as they are not in a palaeomagnetic reference",
                " frame. See details for available models."))
  }

  # Model available?
  available <- c("MERDITH2021", "MATTHEWS2016_pmag_ref", "TorsvikCocks2017",
                 "GOLONKA", "PALEOMAP")
  # Match input
  model <- available[charmatch(x = model, table = available)]
  # Invalid model input?
  if (any(is.na(model))) {
    stop("Unavailable model(s). Choose one from the following: \n",
         toString(available))
  }

  # Set-up ------------------------------------------------------------------
  # If only one model selected, add column of model name and set uncertainty
  # to FALSE
  if (length(model) == 1) {
    occdf$rot_model <- model
    uncertainty <- FALSE
  }

  # Should coordinates be rounded off?
  if (!is.null(round)) {
    occdf[, c(lng, lat, age)] <- round(occdf[, c(lng, lat, age)],
                                       digits = round)
  }
  # Unique localities for rotating
  coords <- unique(occdf[, c(lng, lat, age)])
  # Add columns for populating
  uni_ages <- unique(coords[, age, drop = TRUE])

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
      BASE = paste0(GET("https://zenodo.org/record/7390065")$url,
                    "/files/"),
      MERDITH2021 = "MERDITH2021.RDS",
      PALEOMAP = "PALEOMAP.RDS",
      TorsvikCocks2017 = "TorsvikCocks2017.RDS",
      GOLONKA = "GOLONKA.RDS",
      MATTHEWS2016_pmag_ref = "MATTHEWS2016_pmag_ref.RDS"
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
    # Calculate rotation ages for data
    occdf$rot_age <- round(occdf[, age, drop = TRUE], digits = 0)

    # Set-up
    # Convert to sf object and add CRS
    occdf_sf <- st_as_sf(x = occdf,
                         coords = c(lng, lat),
                         remove = FALSE,
                         crs = "EPSG:4326")

    # Match points with cells
    h3 <- point_to_cell(input = occdf_sf,
                        res = 3, # Grid resolution
                        simple = TRUE)
    # Get coordinates for h3 cell
    xy <- st_coordinates(cell_to_point(h3_address = h3, simple = TRUE))
    # Create df
    xy <- data.frame(h3 = h3, rot_lng = xy[, 1], rot_lat = xy[, 2])
    # Bind rotation coordinates
    occdf <- cbind.data.frame(occdf, xy)
    # Assign model coordinates
    for (m in model) {
      # Load reconstruction files
      assign("prm", readRDS(paste0(files, "/", m, ".RDS")))
      # Update lng/lat columns names for indexing
      colnames(prm)[which(colnames(prm) %in% c("lng", "lat"))] <-
        c("lng_0", "lat_0")
      # Extract coordinates
      row_match <- match(x = occdf$h3, table = prm$h3)

      p_lng <- sapply(seq_len(nrow(occdf)), function(i) {
        prm[row_match[i], c(paste0("lng_", occdf$rot_age[i]))]
      })

      p_lat <- sapply(seq_len(nrow(occdf)), function(i) {
        prm[row_match[i], c(paste0("lat_", occdf$rot_age[i]))]
      })

      # Replace NULL values
      rpl <- which(unlist(lapply(p_lng, is.null)) == TRUE)
      if (length(rpl) != 0) {
        p_lng[rpl] <- NA
        p_lat[rpl] <- NA
      }
      # Assign to columns
      if (length(model) > 1) {
        occdf[, paste0("p_lng_", m)] <- unlist(p_lng)
        occdf[, paste0("p_lat_", m)] <- unlist(p_lat)
      } else {
        occdf$p_lng <- unlist(p_lng)
        occdf$p_lat <- unlist(p_lat)
      }
    }
    # Drop h3 column
    occdf <- occdf[, -which(colnames(occdf) == "h3")]
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
                   "Either the website is down or you are not",
                   "connected to the internet."),
             call. = FALSE)
      })
    # Define root
    pbase <- "https://gws.gplates.org/reconstruct/reconstruct_points/"
    # Setup query structure
    query <- list(time = NULL, lons = NULL, lats = NULL,
                  model = NULL, anchor_plate_id = 0,
                  return_null_points = TRUE)
    query <- list(MERDITH2021 = query,
                  PALEOMAP = query,
                  TorsvikCocks2017 = query,
                  MATTHEWS2016_pmag_ref = query,
                  GOLONKA = query)
    # Define GPlates model names
    query[["MERDITH2021"]]$model <- "MERDITH2021"
    query[["PALEOMAP"]]$model <- "PALEOMAP"
    query[["TorsvikCocks2017"]]$model <- "TorsvikCocks2017"
    # anchor plate id 1 required for palaeomagnetic reference frame
    query$TorsvikCocks2017$anchor_plate_id <- 1
    query[["MATTHEWS2016_pmag_ref"]]$model <- "MATTHEWS2016_pmag_ref"
    query[["GOLONKA"]]$model <- "GOLONKA"
    # Define maximum chunk size for API calls
    chunks <- 300
    # Set-up matching for later merge
    coords$match <- paste0(coords[, lng, drop = TRUE], "_",
                           coords[, lat, drop = TRUE], "_",
                           coords[, age, drop = TRUE])
    occdf$match <- paste0(occdf[, lng, drop = TRUE], "_",
                          occdf[, lat, drop = TRUE], "_",
                          occdf[, age, drop = TRUE])
    # Prepare points query
    # Split dataframe by age
    coord_list <- split(x = coords, f = coords[, age, drop = TRUE])
    # Split by chunk size
    list_size <- lapply(coord_list, nrow)
    subsplit <- names(which(list_size > chunks))
    for (i in subsplit) {
      tmp <- coord_list[[i]]
      coord_list[[i]] <- split(x = tmp,
                               f = ceiling(seq_len(nrow(tmp)) / chunks))
    }
    # Run across models
    multi_model <- lapply(model, function(m) {
      # Inform user which model is running
      message(m)
      # Run across unique ages
      rotations <- pblapply(X = uni_ages, function(i) {
        # Subset to age of interest
        if (is.data.frame(coord_list[[as.character(i)]])) {
          tmp <- coord_list[as.character(i)]
        } else {
          tmp <- coord_list[[as.character(i)]]
        }
        # Build requests
        request <- lapply(tmp, function(x) {
          req <- query[[m]]
          req$time <- i
          req$model <- m
          req$lats <- paste(x[, lat], sep = ",", collapse = ",")
          req$lons <- paste(x[, lng], sep = ",", collapse = ",")
          return(req)
        })
        # Call API and extract coordinates
        rots <- lapply(request, function(x) {
          # Avoid overwhelming server (a recent issue?)
          Sys.sleep(1)
          coords <- GET(url = pbase, query = x)
          # If occurrences exceeds age of model
          if (coords$status_code == 400) {
            coords <- vector("list", nrow(tmp[[1]]))
          } else {
            coords <- content(x = coords, as = "parsed")$coordinates
          }
          # Replace NULL values
          rpl <- sapply(coords, is.null)
          if (any(rpl)) {
            rpl <- which(rpl == TRUE)
            for (r in rpl) coords[[r]] <- list(NA, NA)
          }
          xy <- do.call(rbind.data.frame, coords)
          colnames(xy) <- c("x", "y")
          xy
        })
        # Bind data
        rots <- do.call(rbind.data.frame, rots)
        # Set col names if more than one model requested
        if (length(model) > 1) {
          colnames(rots) <- c(paste0("p_lng_", m), paste0("p_lat_", m))
        } else {
          colnames(rots) <- c("p_lng", "p_lat")
        }
        # Bind output
        tmp <- do.call(rbind, tmp)
        rots <- cbind.data.frame(tmp, rots)
        return(rots)
      })
      # Bind data
      do.call(rbind, rotations)
    })

    # Match data
    for (i in seq_along(multi_model)) {
      x <- multi_model[[i]]
      mch <- match(x = occdf$match, table = x$match)
      occdf[, colnames(x)] <- x[mch, ]
    }

    # Drop match column
    occdf <- occdf[, -which(colnames(occdf) == "match"), drop = TRUE]
  }

  # Uncertainty calculation -------------------------------------------------
  if (uncertainty) {
    # Calculate uncertainty (range)
    lng_nme <- paste0("p_lng_", model)
    lat_nme <- paste0("p_lat_", model)
    uncertain_lng <- occdf[, lng_nme, drop = TRUE]
    uncertain_lat <- occdf[, lat_nme, drop = TRUE]

    # Calculate palaeolatitudinal range
    range_p_lat <- vector("numeric")
    for (i in seq_len(nrow(uncertain_lat))) {
      tmp_lat <- na.omit(as.numeric(uncertain_lat[i, ]))
      if (length(tmp_lat) <= 1) {
        range_p_lat[i] <- NA
        next
      } else {
        range_p_lat[i] <- max(tmp_lat) - min(tmp_lat)
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
        dist <- distm(x = tmpdf, fun = distGeo)
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
  if (length(model) > 1) {
    cnames <- c(paste0("p_lng_", model), paste0("p_lat_", model))
  } else {
    cnames <- c("p_lng", "p_lat")
  }
  if (any(is.na(occdf[, cnames]))) {
    warning(
      paste0(
        "Palaeocoordinates could not be reconstructed for all points.",
        "\n",
        "Either assigned plate does not exist at time of ",
        "reconstruction or the Global Plate Model(s) does not cover ",
        "the age of reconstruction."
      )
    )
  }
  # Return data
  return(occdf)
}
