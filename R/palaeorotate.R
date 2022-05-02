#' Palaeorotate fossil occurrences
#'
#' A function to generate stage-level palaeocoordinates (0--540 Ma) for fossil occurrence data (i.e. reconstruct the geographic distribution of organism's remains
#' at time of deposition). Each occurrence will be assigned palaeocoordinates based on its current geographic distribution and age estimate.
#'
#' @param x \code{data.frame}. Fossil occurrences to be palaeogeographically reconstructed. \code{x} should be a dataframe containing the
#' following named columns: "lng", "lat", "age". Age should be supplied in millions of years before present (Ma). This format is intentionally strict to ensure
#' that data is entered correctly to prevent errors such as longitude and latitude being confused.
#'
#' @return A \code{data.frame} containing the original input occurrence dataframe, age of rotation (Ma), and
#' reconstructed coordinates. Age of rotation is deduced from the reference ages supplied and closest midpoint of a stratigraphic stage.
#'
#' @details This function generates palaeocoordinates by spatiotemporally linking present-day geographic coordinates and age estimates with a spatial
#' grid (1º x 1º) rotated to the midpoint of stratigraphic stages (GTS, 2020; \url{https://stratigraphy.org/timescale/}). As such, palaeocoordinates can be efficiently generated for large datasets with
#' relatively little computational power. A further benefit of this approach is that no internet connection is required as in previously implemented API approaches.
#' In addition, no additional software or knowledge is required from the user (i.e. GPlates). However, it should be noted that if specific ages of rotation are required,
#' or fine-scale spatial analyses is being conducted, use of \url{https://www.gplates.org} might be preferable for the user.
#'
#' The current palaeorotations (0--540 Ma) provided were generated using a 1º x 1º spatial grid with the GPlates software \url{https://www.gplates.org} and the plate rotation model
#' "PALEOMAP" (Scotese and Wright, 2018). A finer-scale spatial grid may be implemented in the future, along with the inclusion of more plate rotation models.
#'
#' @section References:
#' Scotese, C., & Wright, N. M. (2018). PALEOMAP Paleodigital Elevation Models (PaleoDEMS) for the Phanerozoic. PALEOMAP Project.
#' \url{https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/}
#' \cr
#'
#' @examples
#' Generic example with a few occurrences
#' x <- data.frame(lng = c(54, 95, 12), lat = c(86, 12, -65), age = c(45, 203, 467))
#' palaeorotate(x = x)
#'
#' Now with some real fossil occurrence data
#'
#' Grab some data from the Paleobiology Database
#' x <- read.csv("https://paleobiodb.org/data1.2/colls/list.csv?base_name=Scleractinia&interval=Anisian,Piacenzian")
#'
#' Assign midpoint age of fossil occurrence data for rotation
#' x$age <- (x$max_ma + x$min_ma)/2
#'
#' Rotate the data
#' x <- palaeorotate(x = x)
palaeorotate <- function(x){
  #error handling to go here
  if(sum((c("lng", "lat", "age") %in% colnames(x))) != 3){
    stop("Column names should be: lng, lat, age")
  }

  if(sum(x$lat > 90) != 0 | sum(x$lat < -90) != 0){
    stop("Latitude should be more than -90 and less than 90")
  }

  #reconstruct coordinates
  #get palaeorotation grid
  palaeo_rots <- palaeoverse::palaeo_rots
  rot_age <- colnames(palaeo_rots)[3:ncol(palaeo_rots)]
  rot_age <- unique(as.numeric(sub(".*_", "", rot_age)))
  #calculate rotation ages for data
  x$rot_age <- rot_age[sapply(1:nrow(x), function(i){which.min(abs(x[i,c("age")] - rot_age))})]

  #search for matching longitude, latitude and ages
  age_lng <- paste0("lng_",x$age)
  age_lat <- paste0("lat_",x$age)

  x$p_lng <- sapply(1:nrow(x), function(i){
    palaeo_rots[which.min(abs(palaeo_rots[,c("lng")] - x$lng[i])),
                which(colnames(palaeo_rots) == paste0("lng_", x$rot_age[i]))]
    })

  x$p_lat <- sapply(1:nrow(x), function(i){
    palaeo_rots[which.min(abs(palaeo_rots[,c("lat")] - x$lat[i])),
                which(colnames(palaeo_rots) == paste0("lat_", x$rot_age[i]))]
  })

  return(x)
}
