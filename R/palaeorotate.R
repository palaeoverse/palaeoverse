#' Palaeorotate fossil occurrences
#'
#' A function to generate stage-level palaeocoordinates (0--540 Ma) for fossil occurrence data (i.e. reconstruct the geographic distribution of organism's remains
#' at time of deposition). Each occurrence is assigned palaeocoordinates based on its current geographic distribution and age estimate.
#'
#' @param x \code{dataframe}. Fossil occurrences to be palaeogeographically reconstructed. \code{x} should be a dataframe containing the
#' following named columns: "lng", "lat", "age". Age should be supplied in millions of years before present (Ma). This format is intentionally strict to ensure
#' that data is entered correctly to prevent errors such as longitude and latitude being confused.
#' @param model \code{character}. The name of the plate rotation model to be used to reconstruct palaeocoordinates. Choose from: "Merdith2021", "Scotese2018", and "Wright2013".
#' The default is "Merdith2021". See details below for further information on each model.
#' @param uncertainty \code{logical}. Should uncertainty in palaeogeographic reconstructions be returned? If set to TRUE, the palaeocoordinates from the three plate rotation models
#' are returned ("Merdith2021", "Scotese2018", and "Wright2013"), along with their respective longitudinal and latitudinal range.
#' @importFrom utils download.file
#'
#' @return A \code{dataframe} containing the original input occurrence dataframe, age of rotation (Ma), and
#' the reference coordinates rotated. "rot_age" refers to the age of rotation and is deduced from the reference age
#' provided and the closest midpoint age of a stratigraphic stage. "rot_lng" and "rot_lat" refer to the reference coordinates rotated.
#' "p_lng" and "p_lat" are the reconstructed coordinates for respective plate rotation models.
#'
#' @details This function generates palaeocoordinates by spatiotemporally linking present-day geographic coordinates and age estimates with a spatial
#' grid (1º x 1º) rotated to the midpoint of stratigraphic stages (Geological Timescale, 2020; \url{https://stratigraphy.org/timescale/}). As such, palaeocoordinates can be efficiently generated for large datasets with
#' relatively little computational power. In addition, no additional software or knowledge is required from the user (i.e. GPlates). However, it should be noted that if specific ages of rotation are required,
#' or fine-scale spatial analyses is being conducted, use of \url{https://www.gplates.org} might be preferable for the user (particularly if your occurrences are close to plate boundaries).
#'
#' The current palaeorotations (0--540 Ma) provided were generated using a 1º x 1º spatial grid with the GPlates software \url{https://www.gplates.org} and three different plate rotation models
#' "Merdith2021" (Merdith et al., 2021), "Scotese2018" (Scotese & Wright, 2018), and "Wright2013" (Wright et al. 2013). A finer-scale spatial grid will be implemented in the future, along with the inclusion of more plate rotation models.
#'
#' @section Reference:
#'
#'
#' Merdith, A., Williams, S.E., Collins, A.S., Tetley, M.G., Mulder, J.A., Blades, M.L.,
#' Young, A., Armistead, S.E., Cannon, J., Zahirovic, S.,  Müller. R.D. (2021).
#' Extending full-plate tectonic models into deep time: Linking the Neoproterozoic and the Phanerozoic.
#' Earth-Science Reviews 214 (103477). \url{https://doi.org/10.1016/j.earscirev.2020.103477}.
#'
#' Scotese, C., & Wright, N. M. (2018). PALEOMAP Paleodigital Elevation Models (PaleoDEMs) for the Phanerozoic. PALEOMAP Project.
#' \url{https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/}.
#'
#' Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013). Towards community-driven paleogeographic reconstructions:
#' integrating open-access paleogeographic and paleobiology data with plate tectonics. Biogeosciences, 10(3), 1529–1541.
#' \url{https://doi.org/10.5194/bg-10-1529-2013}.
#' \cr
#' @section Developer:
#' Lewis A. Jones
#' @section Auditor:
#' Missing
#' @examples
#' \dontrun{
#' #Generic example with a few occurrences
#' x <- data.frame(lng = c(2, 95, 12), lat = c(46, 12, -65), age = c(88, 203, 467))
#' palaeorotate(x = x)
#'
#' #Now with some real fossil occurrence data! (to be updated with internal data from Bethany/Emma)
#'
#' #Grab some data from the Paleobiology Database
#' x <- read.csv(paste0("https://paleobiodb.org/data1.2/colls/list.csv",
#'                      "?base_name=Scleractinia&interval=Anisian,Piacenzian"))
#'
#' #Assign midpoint age of fossil occurrence data for rotation
#' x$age <- (x$max_ma + x$min_ma)/2
#'
#' #Rotate the data
#' palaeorotate(x = x)
#'
#' #Calculate uncertainity in palaeocoordinates from models
#' palaeorotate(x = x, uncertainty = TRUE)
#' }
#' @export
palaeorotate <- function(x, model = "Merdith2021", uncertainty = FALSE) {
    #error handling
    if (!exists("x") | !is.data.frame(x)) {
      stop("Please supply x as a dataframe")
    }
    if (sum((c("lng", "lat", "age") %in% colnames(x))) != 3) {
      stop("Column names should be: lng, lat, and age")
    }

    if (sum(x$lat > 90) != 0 | sum(x$lat < -90) != 0) {
      stop("Latitude should be more than -90 and less than 90")
    }

    #reconstruct coordinates

    #generate temp directory and download files
    files <- tempdir()

    if (uncertainty == TRUE) {
      #download all rotations
      download.file(
        url = "https://dl.dropboxusercontent.com/s/g5ii9ymu6ew0r7i/Merdith2021.RDS?dl=0",
        destfile = paste0(files, "/Merdith2021.RDS")
      )
      download.file(
        url = "https://dl.dropboxusercontent.com/s/tomlmx6qc56xsun/Scotese2018.RDS?dl=0",
        destfile = paste0(files, "/Scotese2018.RDS")
      )
      download.file(
        url = "https://dl.dropboxusercontent.com/s/d7emo8gdlhitrds/Wright2013.RDS?dl=0",
        destfile = paste0(files, "/Wright2013.RDS")
      )

      #load rotation files
      Merdith2021 <- readRDS(paste0(files, "/Merdith2021.RDS"))
      Scotese2018 <- readRDS(paste0(files, "/Scotese2018.RDS"))
      Wright2013 <- readRDS(paste0(files, "/Wright2013.RDS"))

      rot_age <- colnames(Merdith2021)[3:ncol(Merdith2021)]
      rot_age <- unique(as.numeric(sub(".*_", "", rot_age)))
      #calculate rotation ages for data
      x$rot_age <-
        rot_age[sapply(1:nrow(x), function(i) {
          which.min(abs(x[i, c("age")] - rot_age))
        })]
      #search for matching longitude, latitude and ages
      x$rot_lng <- sapply(1:nrow(x), function(i) {
        Merdith2021[which.min(abs(Merdith2021[, c("lng")]  - x$lng[i])), 1] #extract closest longitude
      }, simplify = TRUE)

      x$rot_lat <- sapply(1:nrow(x), function(i) {
        Merdith2021[which.min(abs(Merdith2021[, c("lat")]  - x$lat[i])), 2] #extract closest longitude
      }, simplify = TRUE)

      #get coordinates for each model
      Merdith <- data.frame(t(sapply(1:nrow(x), function(i) {
        as.numeric(Merdith2021[which(Merdith2021[, c("lng")] == x[i, "rot_lng"] &
                                                         Merdith2021[, c("lat")] == x[i, "rot_lat"]),
                                  c(paste0("lng_", x$rot_age[i]), paste0("lat_", x$rot_age[i]))])
      })))

      Scotese <- data.frame(t(sapply(1:nrow(x), function(i) {
        as.numeric(Scotese2018[which(Scotese2018[, c("lng")] == x[i, "rot_lng"] &
                                                     Scotese2018[, c("lat")] == x[i, "rot_lat"]),
                                  c(paste0("lng_", x$rot_age[i]), paste0("lat_", x$rot_age[i]))])
      })))

      Wright <- data.frame(t(sapply(1:nrow(x), function(i) {
        as.numeric(Wright2013[which(Wright2013[, c("lng")] == x[i, "rot_lng"] &
                                                    Wright2013[, c("lat")] == x[i, "rot_lat"]),
                                 c(paste0("lng_", x$rot_age[i]), paste0("lat_", x$rot_age[i]))])
      })))

      #bind data
      colnames(Merdith) <- c("p_lng_Merdith2021", "p_lat_Merdith2021")
      colnames(Scotese) <- c("p_lng_Scotese2018", "p_lat_Scotese2018")
      colnames(Wright) <- c("p_lng_Wright2013", "p_lat_Wright2013")
      x <- cbind.data.frame(x, Merdith, Scotese, Wright)
      #calculate uncertainty
      uncertain_lng <- cbind(Merdith$p_lng_Merdith2021,
                                  Scotese$p_lng_Scotese2018,
                                  Wright$p_lng_Wright2013)
      uncertain_lat <- cbind(Merdith$p_lat_Merdith2021,
                                  Scotese$p_lat_Scotese2018,
                                  Wright$p_lat_Wright2013)

      uncertainty_p_lng <- vector("numeric")
      for(i in 1:nrow(uncertain_lng)){

        mx <- max(as.numeric(uncertain_lng[i,]))
        mn <- min(as.numeric(uncertain_lng[i,]))

        range <- abs((mx %% 360) - (mn %% 360))

        if(is.na(range)){
          uncertainty_p_lng[i] <- range
          next
          }

        if(range >= 180){
          range <- abs(range - 360)
          }
        else{
          range <- abs(range)
          }
        uncertainty_p_lng[i] <- range
      }

      uncertainty_p_lat <- vector("numeric")
      for(i in 1:nrow(uncertain_lat)){
        uncertainty_p_lat[i] <- max(as.numeric(uncertain_lat[i,])) - min(as.numeric(uncertain_lat[i,]))
      }

      x <- cbind.data.frame(x, uncertainty_p_lng, uncertainty_p_lat)

    }

    if (uncertainty == FALSE) {
      #which model should be used?
      if (model == "Merdith2021") {
        download.file(
          url = "https://dl.dropboxusercontent.com/s/g5ii9ymu6ew0r7i/Merdith2021.RDS?dl=0",
          destfile = paste0(files, "/Merdith2021.RDS")
        )
        palaeo_rots <- readRDS(paste0(files, "/Merdith2021.RDS"))

      } else if (model == "Scotese2018") {
        download.file(
          url = "https://dl.dropboxusercontent.com/s/tomlmx6qc56xsun/Scotese2018.RDS?dl=0",
          destfile = paste0(files, "/Scotese2018.RDS")
        )
        palaeo_rots <- readRDS(paste0(files, "/Scotese2018.RDS"))
      } else if (model == "Wright2013") {
        download.file(
          url = "https://dl.dropboxusercontent.com/s/d7emo8gdlhitrds/Wright2013.RDS?dl=0",
          destfile = paste0(files, "/Wright2013.RDS")
        )
        palaeo_rots <- readRDS(paste0(files, "/Wright2013.RDS"))
      }

      rot_age <- colnames(palaeo_rots)[3:ncol(palaeo_rots)]
      rot_age <- unique(as.numeric(sub(".*_", "", rot_age)))
      #calculate rotation ages for data
      x$rot_age <-
        rot_age[sapply(1:nrow(x), function(i) {
          which.min(abs(x[i, c("age")] - rot_age))
        })]

      #search for matching longitude, latitude and ages
      x$rot_lng <- sapply(1:nrow(x), function(i) {
        palaeo_rots[which.min(abs(palaeo_rots[, c("lng")]  - x$lng[i])), 1] #extract closest longitude
      }, simplify = TRUE)

      x$rot_lat <- sapply(1:nrow(x), function(i) {
        palaeo_rots[which.min(abs(palaeo_rots[, c("lat")]  - x$lat[i])), 2] #extract closest longitude
      }, simplify = TRUE)

      pcoords <- sapply(1:nrow(x), function(i) {
        palaeo_rots[which(palaeo_rots[, c("lng")] == x[i, "rot_lng"] &
                            palaeo_rots[, c("lat")] == x[i, "rot_lat"]),
                    c(paste0("lng_", x$rot_age[i]), paste0("lat_", x$rot_age[i]))]
      })

      x$p_lng <- pcoords[1, ]
      x$p_lat <- pcoords[2, ]
      x$model <- model
      if (any(is.na(x$p_lng) | is.na(x$p_lat))) {
        message(
          "Palaeocoordinates could not be reconstructed for all points. Georeferenced plate does not exist at time of reconstruction."
        )
      }
    }

    #remove downloaded files
    unlink(x = paste0(files, "/", list.files(files)))

    return(x)
  }
