#' Palaeorotate fossil occurrences
#'
#' A function to generate stage-level palaeocoordinates (0--540 Ma) for fossil
#' occurrence data (i.e., reconstruct the geographic distribution of organisms'
#' remains at time of deposition). Each occurrence is assigned
#' palaeocoordinates based on its current geographic distribution and age
#' estimate.
#'
#' @param x \code{dataframe}. Fossil occurrences to be palaeogeographically
#' reconstructed. \code{x} should be a dataframe containing the following named
#' columns: "lng", "lat", "age". Age should be supplied in millions of years
#' before present (Ma). This format is intentionally strict to ensure that data
#' is entered correctly to prevent errors such as longitude and latitude being
#' confused.
#' @param model \code{character}. The name of the plate rotation model to be
#' used to reconstruct palaeocoordinates. Choose from: "Merdith2021",
#' "Scotese2018", and "Wright2013". The default is "Merdith2021". See details
#' below for further information on each model.
#' @param uncertainty \code{logical}. Should the uncertainty in
#' palaeogeographic reconstructions be returned? If set to TRUE, the
#' palaeocoordinates from the three plate rotation models are returned
#' ("Merdith2021", "Scotese2018", and "Wright2013"), along with their
#' respective longitudinal and latitudinal range.
#'
#' @return A \code{dataframe} containing the original input occurrence
#' dataframe, age of rotation (Ma), the reference coordinates rotated, and the
#' reconstructed coordinates (i.e., palaeocoordinates). The "rot_age" column
#' refers to the age of rotation, and is deduced from the reference
#' age provided and the closest midpoint age of a stratigraphic stage. The
#' "rot_lng" and "rot_lat" columns refer to the reference coordinates rotated.
#' The "p_lng" and "p_lat" are the reconstructed coordinates for respective
#' plate rotation models.
#'
#' @details This function generates palaeocoordinates by spatiotemporally
#' linking present-day geographic coordinates and age estimates with a spatial
#' grid (1&deg; x 1&deg;) rotated to the midpoint of stratigraphic stages
#' (Geological Timescale, 2020;
#' \url{https://stratigraphy.org/ICSchart/ChronostratChart2020-03.pdf}). As
#' such, palaeocoordinates can be efficiently generated for large datasets with
#' relatively little computational power. In addition, no additional software
#' or knowledge is required from the user (i.e. GPlates). However, it should be
#' noted that if specific ages of rotation are required, or fine-scale spatial
#' analyses are being conducted, use of \url{https://www.gplates.org} might be
#' preferable for the user (particularly if your occurrences are close to plate
#' boundaries).
#'
#' The current palaeorotations (0--540 Ma) provided were generated using a
#' 1&deg; x 1&deg; spatial grid with the GPlates software
#' \url{https://www.gplates.org} and three different plate rotation models
#' "Merdith2021" (Merdith et al., 2021), "Scotese2018"
#' (Scotese & Wright, 2018), and "Wright2013" (Wright et al. 2013).
#'
#' @section References:
#'
#' Merdith, A., Williams, S.E., Collins, A.S., Tetley, M.G., Mulder, J.A.,
#' Blades, M.L., Young, A., Armistead, S.E., Cannon, J., Zahirovic, S.,
#' Müller. R.D. (2021).
#' Extending full-plate tectonic models into deep time: Linking the
#' Neoproterozoic and the Phanerozoic.
#' Earth-Science Reviews 214 (103477). \doi{10.1016/j.earscirev.2020.103477}.
#'
#' Scotese, C., & Wright, N. M. (2018). PALEOMAP Paleodigital Elevation Models
#' (PaleoDEMs) for the
#' Phanerozoic. PALEOMAP Project.
#' \url{https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/}.
#'
#' Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013). Towards
#' community-driven paleogeographic
#' reconstructions: integrating open-access paleogeographic and paleobiology
#' data with plate tectonics.
#' Biogeosciences, 10(3), 1529–1541. \doi{10.5194/bg-10-1529-2013}.
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Missing
#' @importFrom utils download.file
#' @examples
#' #Generic example with a few occurrences
#' x <- data.frame(lng = c(2, -103, -66),
#'                 lat = c(46, 35, -7),
#'                 age = c(88, 125, 200))
#' palaeorotate(x = x)
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
#' palaeorotate(x = tetrapods)
#'
#' #Calculate uncertainity in palaeocoordinates from models
#' palaeorotate(x = tetrapods, uncertainty = TRUE)
#' @export
palaeorotate <-
  function(x,
           model = "Merdith2021",
           uncertainty = FALSE) {
    #error handling
    if (!exists("x") || !is.data.frame(x)) {
      stop("Please supply x as a dataframe")
    }
    if (sum((c("lng", "lat", "age") %in% colnames(x))) != 3) {
      stop("`x` must contain the following columns: lng, lat, and age")
    }

    if (sum(x$lat > 90) != 0 || sum(x$lat < -90) != 0) {
      stop("Latitude should be more than -90 and less than 90")
    }

    if (model %in% c("Merdith2021", "Scotese2018", "Wright2013") == FALSE) {
      stop("`model` should be one of the following:
      Merdith2021, Scotese2018, Wright2013")
    }

    if (!is.logical(uncertainty)) {
      stop("`uncertainty` should be a logical value: TRUE/FALSE")
    }

    #reconstruct coordinates

    #get temp directory and download files
    files <- tempdir()

    if (.Platform$OS.type == "windows") {
      mode <- "wb"
    }else {
      mode <- "w"
    }

    if (uncertainty == TRUE) {
      #download all rotations
      download.file(url =
    "https://dl.dropboxusercontent.com/s/fmt7mb0799952qy/Merdith2021.RDS?dl=0",
                    destfile = paste0(files, "/Merdith2021.RDS"), mode = mode)
      download.file(url =
    "https://dl.dropboxusercontent.com/s/zqi2jmjhjecka0s/Scotese2018.RDS?dl=0",
                    destfile = paste0(files, "/Scotese2018.RDS"), mode = mode)
      download.file(url =
    "https://dl.dropboxusercontent.com/s/gf7t2wo6iwo8ut2/Wright2013.RDS?dl=0",
                    destfile = paste0(files, "/Wright2013.RDS"), mode = mode)

      #load rotation files
      merdith2021 <- readRDS(paste0(files, "/Merdith2021.RDS"))
      scotese2018 <- readRDS(paste0(files, "/Scotese2018.RDS"))
      wright2013 <- readRDS(paste0(files, "/Wright2013.RDS"))

      rot_age <- colnames(merdith2021)[3:ncol(merdith2021)]
      rot_age <- unique(as.numeric(sub(".*_", "", rot_age)))
      #calculate rotation ages for data
      x$rot_age <-
        rot_age[sapply(seq_len(nrow(x)), function(i) {
          which.min(abs(x[i, c("age")] - rot_age))
        })]
      #search for matching longitude, latitude and ages
      x$rot_lng <- sapply(seq_len(nrow(x)), function(i) {
        #extract closest longitude
        merdith2021[which.min(abs(merdith2021[, c("lng")]  - x$lng[i])), 1]
      }, simplify = TRUE)

      x$rot_lat <- sapply(seq_len(nrow(x)), function(i) {
        #extract closest latitude
        merdith2021[which.min(abs(merdith2021[, c("lat")]  - x$lat[i])), 2]
      }, simplify = TRUE)

      #get coordinates for each model
      merdith <- data.frame(t(sapply(seq_len(nrow(x)), function(i) {
        as.numeric(merdith2021[which(
          merdith2021[, c("lng")] == x[i, "rot_lng"] &
            merdith2021[, c("lat")] == x[i, "rot_lat"]),
                               c(paste0("lng_", x$rot_age[i]),
                                 paste0("lat_", x$rot_age[i]))])
      })))

      scotese <- data.frame(t(sapply(seq_len(nrow(x)), function(i) {
        as.numeric(scotese2018[which(scotese2018[, c("lng")] ==
                                       x[i, "rot_lng"] &
                                       scotese2018[, c("lat")] ==
                                       x[i, "rot_lat"]),
                               c(paste0("lng_", x$rot_age[i]),
                                 paste0("lat_", x$rot_age[i]))])
      })))

      wright <- data.frame(t(sapply(seq_len(nrow(x)), function(i) {
        as.numeric(wright2013[which(
          wright2013[, c("lng")] == x[i, "rot_lng"] &
            wright2013[, c("lat")] == x[i, "rot_lat"]),
                              c(paste0("lng_", x$rot_age[i]),
                                paste0("lat_", x$rot_age[i]))])
      })))

      #bind data
      colnames(merdith) <-
        c("p_lng_Merdith2021", "p_lat_Merdith2021")
      colnames(scotese) <-
        c("p_lng_Scotese2018", "p_lat_Scotese2018")
      colnames(wright) <- c("p_lng_Wright2013", "p_lat_Wright2013")
      x <- cbind.data.frame(x, merdith, scotese, wright)
      #calculate uncertainty
      uncertain_lng <- cbind(
        merdith$p_lng_Merdith2021,
        scotese$p_lng_Scotese2018,
        wright$p_lng_Wright2013
      )
      uncertain_lat <- cbind(
        merdith$p_lat_Merdith2021,
        scotese$p_lat_Scotese2018,
        wright$p_lat_Wright2013
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

      x <- cbind.data.frame(x, uncertainty_p_lng, uncertainty_p_lat)

    }

    if (uncertainty == FALSE) {
      #which model should be used?
      if (model == "Merdith2021") {
        download.file(url =
    "https://dl.dropboxusercontent.com/s/fmt7mb0799952qy/Merdith2021.RDS?dl=0",
                      destfile = paste0(files, "/Merdith2021.RDS"),
    mode = mode)
        palaeo_rots <- readRDS(paste0(files, "/Merdith2021.RDS"))

      } else if (model == "Scotese2018") {
        download.file(url =
    "https://dl.dropboxusercontent.com/s/zqi2jmjhjecka0s/Scotese2018.RDS?dl=0",
                      destfile = paste0(files, "/Scotese2018.RDS"),
    mode = mode)
        palaeo_rots <- readRDS(paste0(files, "/Scotese2018.RDS"))
      } else if (model == "Wright2013") {
        download.file(url =
    "https://dl.dropboxusercontent.com/s/gf7t2wo6iwo8ut2/Wright2013.RDS?dl=0",
                      destfile = paste0(files, "/Wright2013.RDS"),
    mode = mode)
        palaeo_rots <- readRDS(paste0(files, "/Wright2013.RDS"))
      }

      rot_age <- colnames(palaeo_rots)[3:ncol(palaeo_rots)]
      rot_age <- unique(as.numeric(sub(".*_", "", rot_age)))
      #calculate rotation ages for data
      x$rot_age <-
        rot_age[sapply(seq_len(nrow(x)), function(i) {
          which.min(abs(x[i, c("age")] - rot_age))
        })]

      #search for matching longitude, latitude and ages
      x$rot_lng <- sapply(seq_len(nrow(x)), function(i) {
        #extract closest longitude
        palaeo_rots[which.min(abs(palaeo_rots[, c("lng")]  - x$lng[i])), 1]
      }, simplify = TRUE)

      x$rot_lat <- sapply(seq_len(nrow(x)), function(i) {
        #extract closest latitude
        palaeo_rots[which.min(abs(palaeo_rots[, c("lat")]  - x$lat[i])), 2]
      }, simplify = TRUE)

      pcoords <- sapply(seq_len(nrow(x)), function(i) {
        palaeo_rots[which(palaeo_rots[, c("lng")] == x[i, "rot_lng"] &
                            palaeo_rots[, c("lat")] == x[i, "rot_lat"]),
                    c(paste0("lng_", x$rot_age[i]),
                      paste0("lat_", x$rot_age[i]))]
      })

      x$p_lng <- as.numeric(pcoords[1, ])
      x$p_lat <- as.numeric(pcoords[2, ])
      x$model <- model
      if (any(is.na(x$p_lng) | is.na(x$p_lat))) {
        message(
          "Palaeocoordinates could not be reconstructed for all points.
Georeferenced plate does not exist at time of reconstruction."
        )
      }
    }

    #remove downloaded files
    unlink(x = paste0(files, "/", list.files(files)))

    return(x)
  }
