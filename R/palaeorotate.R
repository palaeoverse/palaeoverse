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
#' longitude (e.g. "lng").
#' @param lat \code{character}. The name of the column you wish to be treated as
#' latitude (e.g. "lat").
#' @param age \code{numeric}. The name of the column you wish to be treated as
#' the age for rotation (e.g. "age").
#' @param model \code{character}. The name of the plate rotation model to be
#' used to reconstruct palaeocoordinates. See details for available models
#' (`method` specific).
#' @param method \code{character}. Method used to calculate palaeocoordinates
#' for fossil occurrences. Either "grid" to use rotation files, or "point"
#' to use the GPlates API service.
#' @param uncertainty \code{logical}. Should the uncertainty in
#' palaeogeographic reconstructions be returned? If set to TRUE, the
#' palaeocoordinates from the three available rotation files are returned
#' ("MERDITH2021", "PALEOMAP", and "WRIGHT2013"), along with their
#' respective longitudinal and latitudinal range. This argument is only
#' relevant if `method` is set to "grid".
#'
#' @return A \code{dataframe} containing the original input occurrence
#' dataframe, age of rotation ("rot_age"), the reference coordinates rotated
#' ("rot_lng" and "rot_lat"), and the reconstructed coordinates
#' (i.e., "p_lng" and "p_lat"). The "point" `method` uses the input coordinates and
#' age as the reference and are therefore not returned.
#'
#' @details This function can generate palaeocoordinates using two different
#' approaches (`method`):
#'
#' - Rotation files: The "grid" `method` uses rotation files to spatiotemporally
#' link present-day geographic coordinates and age estimates with a spatial
#' grid (1&deg; x 1&deg;) rotated to the midpoint of stratigraphic stages
#' (Geological Timescale, 2020). If specific ages of rotation are required,
#' or fine-scale spatial analyses are being conducted, use of the "point"
#' `method` might be preferable for the user (particularly if occurrences are
#' close to plate boundaries). As implemented, points within the same grid cell
#' will be assigned equivalent palaeocoordinates due to spatial aggregation. The
#' current palaeorotations (0--540 Ma) provided were generated using a
#' 1&deg; x 1&deg; spatial grid with the GPlates software
#' \url{https://www.gplates.org} and three plate rotation models
#' (Wright et al. 2013; Scotese & Wright, 2018; Merdith et al., 2021).
#'
#' - GPlates API: The "point" `method` uses the GPlates API service
#' \url{https://gwsdoc.gplates.org} to reconstruct palaeorotations for point
#' data. The use of this `method` is slower than the "grid" `method` if many
#' unique time intervals exist in your dataset. However, it provides
#' palaeocoordinates with higher precision.
#'
#' Available models for each method:
#' - "MERDITH2021" (grid & point)
#' - "PALEOMAP" (grid & point)
#' - "MULLER2019" (point)
#' - "MATTHEWS2016" (point)
#' - "WRIGHT2013" (grid)
#' - "RODINIA2013" (point)
#' - "SETON2012" (point)
#' - "GOLONKA" (point)
#'
#' @section References:
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
#' - Scotese, C., & Wright, N. M. (2018). PALEOMAP Paleodigital Elevation Models
#' (PaleoDEMs) for the
#' Phanerozoic. PALEOMAP Project.
#' \url{https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/}.
#'
#' - Matthews, K.J., Maloney, K.T., Zahirovic, S., Williams, S.E., Seton, M.,
#' and Müller, R.D. (2016). Global plate boundary evolution and kinematics
#' since the late Paleozoic, Global and Planetary Change, 146, 226-250.
#' \doi{10.1016/j.gloplacha.2016.10.002}.
#'
#' - Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013). Towards
#' community-driven paleogeographic
#' reconstructions: integrating open-access paleogeographic and paleobiology
#' data with plate tectonics.
#' Biogeosciences, 10(3), 1529–1541. \doi{10.5194/bg-10-1529-2013}.
#'
#' - Seton, M., Müller, R.D., Zahirovic, S., Gaina, C., Torsvik, T.H.,
#' Shephard, G., Talsma, A., Gurnis, M., Turner, M., Maus, S., Chandler, M.
#' (2012). Global continental and ocean basin reconstructions since 200 Ma.
#' Earth-Science Reviews, Volume 113, Issues 3-4, July 2012, Pages 212--270.
#' \doi{10.1016/j.earscirev.2012.03.002}.
#'
#' See GPlates documentation for additional information:
#' \url{https://gwsdoc.gplates.org/reconstruction}.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Kilian Eichenseer
#' @importFrom utils download.file
#' @importFrom pbapply pblapply
#' @examples
#' #Generic example with a few occurrences
#' occdf <- data.frame(lng = c(2, -103, -66),
#'                 lat = c(46, 35, -7),
#'                 age = c(88, 125, 200))
#'
#' #Calculate palaeocoordinates using rotation files
#' palaeorotate(occdf = occdf, method = "grid")
#'
#' #Calculate palaeocoordinates using the GPlates API
#' palaeorotate(occdf = occdf, method = "point")
#'
#' #Now with some real fossil occurrence data!
#'
#' #Grab some data from the Paleobiology Database
#' data(tetrapods)
#'
#' #Assign midpoint age of fossil occurrence data for rotation
#' tetrapods$age <- (tetrapods$max_ma + tetrapods$min_ma)/2
#'
#' #Rotate the data
#' palaeorotate(occdf = tetrapods)
#'
#' #Calculate uncertainity in palaeocoordinates from models
#' palaeorotate(occdf = tetrapods, uncertainty = TRUE)
#' @export
palaeorotate <- function(occdf, lng = "lng", lat = "lat", age = "age",
                         model = "MERDITH2021", method = "grid",
                         uncertainty = FALSE) {
    # Error handling
    if (!exists("occdf") || !is.data.frame(occdf)) {
      stop("Please supply occdf as a dataframe")
    }

    if (sum((c(lng, lat, age) %in% colnames(occdf))) != 3) {
      stop("defined `lng`, `lat`, and `age` not found in `occdf`")
    }

    if (any(!is.numeric(occdf[, lat]) | is.na(occdf[, lat])) ||
        any(!is.numeric(occdf[, lng]) | is.na(occdf[, lng])) ||
        any(!is.numeric(occdf[, age]) | is.na(occdf[, age]))) {
      stop("`lng`, `lat` and `age` should be of class numeric")
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

    if (!is.logical(uncertainty)) {
      stop("`uncertainty` should be a logical value: TRUE/FALSE")
    }

    # Reconstruct coordinates
    # Should the grid approach be used?
    if(method == "grid"){

      # Model matching for grid call
      available <- c("MERDITH2021", "PALEOMAP",
                     "WRIGHT2013")
      model <- available[charmatch(x = model, table = available)]

      if (model %in% c("MERDITH2021", "PALEOMAP", "WRIGHT2013") == FALSE) {
        stop(paste("`model` should be one of the following:",
      "\nMERDITH2021, PALEOMAP, WRIGHT2013"))
      }

    #get temp directory and download files
    files <- tempdir()

    if (.Platform$OS.type == "windows") {
      mode <- "wb"
    }else {
      mode <- "w"
    }

    if (uncertainty == TRUE) {
      #download all rotations
      if (!file.exists(paste0(files, "/MERDITH2021.RDS"))) {
        download.file(url =
    "https://dl.dropboxusercontent.com/s/fmt7mb0799952qy/Merdith2021.RDS?dl=0",
                    destfile = paste0(files, "/Merdith2021.RDS"), mode = mode)
      }
      if (!file.exists(paste0(files, "/Scotese2018.RDS"))) {
        download.file(url =
    "https://dl.dropboxusercontent.com/s/zqi2jmjhjecka0s/Scotese2018.RDS?dl=0",
                    destfile = paste0(files, "/Scotese2018.RDS"), mode = mode)
      }
      if (!file.exists(paste0(files, "/Wright2013.RDS"))) {
        download.file(url =
    "https://dl.dropboxusercontent.com/s/gf7t2wo6iwo8ut2/Wright2013.RDS?dl=0",
                    destfile = paste0(files, "/Wright2013.RDS"), mode = mode)
      }

      #load rotation files
      MERDITH2021 <- readRDS(paste0(files, "/Merdith2021.RDS"))
      PALEOMAP <- readRDS(paste0(files, "/Scotese2018.RDS"))
      WRIGHT2013 <- readRDS(paste0(files, "/Wright2013.RDS"))

      rot_age <- colnames(MERDITH2021)[3:ncol(MERDITH2021)]
      rot_age <- unique(as.numeric(sub(".*_", "", rot_age)))
      #calculate rotation ages for data
      occdf$rot_age <-
        rot_age[sapply(seq_len(nrow(occdf)), function(i) {
          which.min(abs(occdf[i, age] - rot_age))
        })]
      #search for matching longitude, latitude and ages
      occdf$rot_lng <- sapply(seq_len(nrow(occdf)), function(i) {
        #extract closest longitude
        MERDITH2021[which.min(abs(MERDITH2021[, c("lng")]  - occdf[i, lng])), 1]
      }, simplify = TRUE)

      occdf$rot_lat <- sapply(seq_len(nrow(occdf)), function(i) {
        #extract closest latitude
        MERDITH2021[which.min(abs(MERDITH2021[, c("lat")]  - occdf[i, lat])), 2]
      }, simplify = TRUE)

      #get coordinates for each model
      merdith <- data.frame(t(sapply(seq_len(nrow(occdf)), function(i) {
        as.numeric(MERDITH2021[which(
          MERDITH2021[, c("lng")] == occdf[i, "rot_lng"] &
            MERDITH2021[, c("lat")] == occdf[i, "rot_lat"]),
                               c(paste0("lng_", occdf$rot_age[i]),
                                 paste0("lat_", occdf$rot_age[i]))])
      })))

      scotese <- data.frame(t(sapply(seq_len(nrow(occdf)), function(i) {
        as.numeric(PALEOMAP[which(PALEOMAP[, c("lng")] ==
                                       occdf[i, "rot_lng"] &
                                       PALEOMAP[, c("lat")] ==
                                       occdf[i, "rot_lat"]),
                               c(paste0("lng_", occdf$rot_age[i]),
                                 paste0("lat_", occdf$rot_age[i]))])
      })))

      wright <- data.frame(t(sapply(seq_len(nrow(occdf)), function(i) {
        as.numeric(WRIGHT2013[which(
          WRIGHT2013[, c("lng")] == occdf[i, "rot_lng"] &
            WRIGHT2013[, c("lat")] == occdf[i, "rot_lat"]),
                              c(paste0("lng_", occdf$rot_age[i]),
                                paste0("lat_", occdf$rot_age[i]))])
      })))

      #bind data
      colnames(merdith) <-
        c("p_lng_MERDITH2021", "p_lat_MERDITH2021")
      colnames(scotese) <-
        c("p_lng_PALEOMAP", "p_lat_PALEOMAP")
      colnames(wright) <-
        c("p_lng_WRIGHT2013", "p_lat_WRIGHT2013")
      occdf <- cbind.data.frame(occdf, merdith, scotese, wright)
      #calculate uncertainty
      uncertain_lng <- cbind(
        merdith$p_lng_MERDITH2021,
        scotese$p_lng_PALEOMAP,
        wright$p_lng_WRIGHT2013
      )
      uncertain_lat <- cbind(
        merdith$p_lat_MERDITH2021,
        scotese$p_lat_PALEOMAP,
        wright$p_lat_WRIGHT2013
      )

      uncertainty_p_lng <- vector("numeric")
      for (i in seq_len(nrow(uncertain_lng))) {
        mx <- max(as.numeric(uncertain_lng[i, ]))
        mn <- min(as.numeric(uncertain_lng[i, ]))

        range <- abs((mx %% 360) - (mn %% 360))

        if (is.na(range)) {
          uncertainty_p_lng[i] <- range
          next
        }

        if (range >= 180) {
          range <- abs(range - 360)
        }else {
          range <- abs(range)
        }
        uncertainty_p_lng[i] <- range
      }

      uncertainty_p_lat <- vector("numeric")
      for (i in seq_len(nrow(uncertain_lat))) {
        uncertainty_p_lat[i] <- max(as.numeric(uncertain_lat[i, ])) -
          min(as.numeric(uncertain_lat[i, ]))
      }

      occdf <- cbind.data.frame(occdf, uncertainty_p_lng, uncertainty_p_lat)

    }

    if (uncertainty == FALSE) {
      #which model should be used?
      if (model == "MERDITH2021") {
        if (!file.exists(paste0(files, "/Merdith2021.RDS"))) {
        download.file(url =
    "https://dl.dropboxusercontent.com/s/fmt7mb0799952qy/Merdith2021.RDS?dl=0",
                      destfile = paste0(files, "/Merdith2021.RDS"),
    mode = mode)
        }
        palaeo_rots <- readRDS(paste0(files, "/Merdith2021.RDS"))

      } else if (model == "PALEOMAP") {
        if (!file.exists(paste0(files, "/Scotese2018.RDS"))) {
        download.file(url =
    "https://dl.dropboxusercontent.com/s/zqi2jmjhjecka0s/Scotese2018.RDS?dl=0",
                      destfile = paste0(files, "/Scotese2018.RDS"),
    mode = mode)
        }
        palaeo_rots <- readRDS(paste0(files, "/Scotese2018.RDS"))
      } else if (model == "WRIGHT2013") {
        if (!file.exists(paste0(files, "/Wright2013.RDS"))) {
        download.file(url =
    "https://dl.dropboxusercontent.com/s/gf7t2wo6iwo8ut2/Wright2013.RDS?dl=0",
                      destfile = paste0(files, "/Wright2013.RDS"),
    mode = mode)
        }
        palaeo_rots <- readRDS(paste0(files, "/Wright2013.RDS"))
      }

      rot_age <- colnames(palaeo_rots)[3:ncol(palaeo_rots)]
      rot_age <- unique(as.numeric(sub(".*_", "", rot_age)))
      #calculate rotation ages for data
      occdf$rot_age <-
        rot_age[sapply(seq_len(nrow(occdf)), function(i) {
          which.min(abs(occdf[i, age] - rot_age))
        })]

      #search for matching longitude, latitude and ages
      occdf$rot_lng <- sapply(seq_len(nrow(occdf)), function(i) {
        #extract closest longitude
        palaeo_rots[which.min(abs(palaeo_rots[, c("lng")]  - occdf[i, lng])), 1]
      }, simplify = TRUE)

      occdf$rot_lat <- sapply(seq_len(nrow(occdf)), function(i) {
        #extract closest latitude
        palaeo_rots[which.min(abs(palaeo_rots[, c("lat")]  - occdf[i, lat])), 2]
      }, simplify = TRUE)

      pcoords <- sapply(seq_len(nrow(occdf)), function(i) {
        palaeo_rots[which(palaeo_rots[, c("lng")] == occdf[i, "rot_lng"] &
                            palaeo_rots[, c("lat")] == occdf[i, "rot_lat"]),
                    c(paste0("lng_", occdf$rot_age[i]),
                      paste0("lat_", occdf$rot_age[i]))]
      })

      occdf$p_lng <- as.numeric(pcoords[1, ])
      occdf$p_lat <- as.numeric(pcoords[2, ])
      occdf$model <- model
      }
    }
    # Should the point approach be used?
    if(method == "point"){

      # Model matching for API call
      available <- c("MERDITH2021", "PALEOMAP",
                     "MULLER2019",
                     "MATTHEWS2016",
                     "RODINIA2013", "SETON2012",
                     "GOLONKA")
      model <- available[charmatch(x = model, table = available)]

      if(is.na(model)) {
        stop(paste("Unavailable model name. Choose from:",
             "\nMERDITH2021, PALEOMAP, MULLER2019, MATTHEWS2016,",
             "RODINIA2013, SETON2012, GOLONKA"))
      }

      if(model == "MATTHEWS2016") {model <- c("MATTHEWS2016_pmag_ref")}

      # Set up dataframe for populating
      occdf$p_lng <- NA
      occdf$p_lat <- NA
      occdf$model <- model

      # Unique localities for rotating
      coords <- unique(occdf[, c(lng, lat, age)])
      uni_ages <- unique(coords[, c(age)])

      # For subsequent coordinate matching
      occdf$match <- NA
      occdf$match <- paste(occdf[1:nrow(occdf), lng],
                       occdf[1:nrow(occdf), lat],
                       occdf[1:nrow(occdf), age])
      coords$match <- paste(occdf[1:nrow(coords), lng],
                            occdf[1:nrow(coords), lat],
                            occdf[1:nrow(coords), age])

      # Generate list of API requests
      API_request <- lapply(uni_ages, function(i) {
        tmp <- coords[which(coords[, age] == i), c(lng, lat)]
        tmp <- toString(as.vector(t(tmp)))
        API <- sprintf('?points=%s&time=%f&model=%s',
                       gsub(" ", "", tmp), i, model)
        API <- paste0("https://gws.gplates.org/reconstruct/reconstruct_points/",
                      API,
                      "&return_null_points")
        API
      })

      # Add unique ages to API request
      names(API_request) <- uni_ages

      # Run API call with progress bar
      API_return <- pbapply::pblapply(X = 1:length(API_request), function(i) {
        # Age of rotation
        age <- names(API_request)[i]
        # Grab data from API
        API_return <- readLines(API_request[[i]], warn = FALSE)
        # Replace NULL values if they exist
        API_return <- gsub("null", "[[-9999, -9999]]", API_return)
        # Extract and format data from call
        API_return <- matrix(
          unlist(
            regmatches(x = API_return,
                       m = gregexpr("-?[[:digit:]]+\\.*[[:digit:]]+",
                                    API_return)
            )
          ), ncol = 2, byrow = TRUE)
        # Update any -9999 values to NA
        API_return[API_return == -9999] <- NA
        # Add age data
        API_return <- cbind(API_return, age)
        # Return data
        API_return
      })
      # Bind data
      API_return <- do.call(rbind, API_return)
      # Convert to dataframe
      API_return <- data.frame(API_return)
      # Update column names
      colnames(API_return) <- c("p_lng", "p_lat", "rot_age")
      # Bind data
      coords <- cbind.data.frame(coords, API_return)
      # Add rotated coordinates to input dataframe based on matching
      for(i in 1:nrow(coords)) {
        matched <- which(occdf$match == coords[i, c("match")])
        occdf$p_lng[matched] <- as.numeric(coords$p_lng[i])
        occdf$p_lat[matched] <- as.numeric(coords$p_lat[i])
      }

      # Drop match column
      occdf <- occdf[, -ncol(occdf)]
    }
  if (any(is.na(occdf$p_lng) | is.na(occdf$p_lat))) {
    message(
      paste0("Palaeocoordinates could not be reconstructed for all points.",
             "\n",
             "Georeferenced plate does not exist at time of reconstruction."
      )
    )
  }
    return(occdf)
  }
