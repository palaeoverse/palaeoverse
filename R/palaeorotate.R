#' Palaeorotate fossil occurrences
#'
#' A function to generate stage-level palaeocoordinates (0--540 Ma) for fossil occurrence data (i.e. reconstruct the geographic distribution of organism's remains
#' at time of deposition). Each occurrence are assigned palaeocoordinates based on its current geographic distribution and age estimate.
#'
#' @param x \code{dataframe}. Fossil occurrences to be palaeogeographically reconstructed. \code{x} should be a dataframe containing the
#' following named columns: "lng", "lat", "age". Age should be supplied in millions of years before present (Ma). This format is intentionally strict to ensure
#' that data is entered correctly to prevent errors such as longitude and latitude being confused.
#'
#' @return A \code{dataframe} containing the original input occurrence dataframe, age of rotation (Ma), and
#' the reference coordinates rotated. "rot_age" refers to the age of rotation and is deduced from the reference age
#' provided and closest midpoint age of a stratigraphic stage. "rot_lng" and "rot_lat" refer to the reference coordinates rotated.
#' "p_lng" and "p_lat" are the reconstructed coordinates.
#'
#' @details This function generates palaeocoordinates by spatiotemporally linking present-day geographic coordinates and age estimates with a spatial
#' grid (1º x 1º) rotated to the midpoint of stratigraphic stages (GTS, 2020; \url{https://stratigraphy.org/timescale/}). As such, palaeocoordinates can be efficiently generated for large datasets with
#' relatively little computational power. A further benefit of this approach is that no internet connection is required as in previously implemented API approaches.
#' In addition, no additional software or knowledge is required from the user (i.e. GPlates). However, it should be noted that if specific ages of rotation are required,
#' or fine-scale spatial analyses is being conducted, use of \url{https://www.gplates.org} might be preferable for the user.
#'
#' The current palaeorotations (0--540 Ma) provided were generated using a 1º x 1º spatial grid with the GPlates software \url{https://www.gplates.org} and the full-plate rotation model
#' "Merdith2021" (Merdith et al., 2021). A finer-scale spatial grid will be implemented in the future, along with the inclusion of more plate rotation models. For now, Merdith2021 is the only model included
#' as it is the newest model, and one of the few full-plate rotation models available.
#'
#' @section Reference:
#' Andrew S. Merdith, Simon E. Williams, Alan S. Collins, Michael G. Tetley, Jacob A. Mulder, Morgan L. Blades,
#' Alexander Young, Sheree E. Armistead, John Cannon, Sabin Zahirovic, R. Dietmar Müller. (2021).
#' Extending full-plate tectonic models into deep time: Linking the Neoproterozoic and the Phanerozoic.
#' Earth-Science Reviews 214 (103477). \url{https://doi.org/10.1016/j.earscirev.2020.103477}.
#' \cr
#' @section Developer:
#' Lewis A. Jones
#' @section Auditor:
#' Missing
#' @examples
#' #Generic example with a few occurrences
#' x <- data.frame(lng = c(54, 95, 12), lat = c(86, 12, -65), age = c(45, 203, 467))
#' palaeorotate(x = x)
#'
#' #Now with some real fossil occurrence data!
#'
#' #Grab some data from the Paleobiology Database
#' x <- read.csv("https://paleobiodb.org/data1.2/colls/list.csv?base_name=Scleractinia&interval=Anisian,Piacenzian")
#'
#' #Assign midpoint age of fossil occurrence data for rotation
#' x$age <- (x$max_ma + x$min_ma)/2
#'
#' #Rotate the data
#' x <- palaeorotate(x = x)
#' @export
palaeorotate <- function(x, model = "Merdith2021"){
  #error handling
  if(!exists("x") | !is.data.frame(x)) {
    stop("Please supply x as a dataframe")
  }
  if(sum((c("lng", "lat", "age") %in% colnames(x))) != 3){
    stop("Column names should be: lng, lat, and age")
  }

  if(sum(x$lat > 90) != 0 | sum(x$lat < -90) != 0){
    stop("Latitude should be more than -90 and less than 90")
  }

  #reconstruct coordinates
  #which model should be used?

  if(model == "Merdith2021"){
    palaeo_rots <- palaeoverse:::Merdith2021
  }

  rot_age <- colnames(palaeo_rots)[3:ncol(palaeo_rots)]
  rot_age <- unique(as.numeric(sub(".*_", "", rot_age)))
  #calculate rotation ages for data
  x$rot_age <- rot_age[sapply(1:nrow(x), function(i){which.min(abs(x[i,c("age")] - rot_age))})]

  #search for matching longitude, latitude and ages
  x$rot_lng <- sapply(1:nrow(x), function(i){
    palaeo_rots[which.min(abs(palaeo_rots[,c("lng")]  - x$lng[i])),1] #extract closest longitude
  }, simplify = TRUE)

  x$rot_lat <- sapply(1:nrow(x), function(i){
    palaeo_rots[which.min(abs(palaeo_rots[,c("lat")]  - x$lat[i])),2] #extract closest longitude
  }, simplify = TRUE)

  pcoords <- sapply(1:nrow(x), function(i){
    palaeo_rots[which(palaeo_rots[,c("lng")] == x[i,"rot_lng"] & palaeo_rots[,c("lat")] == x[i,"rot_lat"]),
           c(paste0("lng_",x$rot_age[i]), paste0("lat_", x$rot_age[i]))]
  })

  x$p_lng <- pcoords[1,]
  x$p_lat <- pcoords[2,]
  x$model <- model

  if(any(is.na(x$p_lng) | is.na(x$p_lat))){
    message("Palaeocoordinates could not be reconstructed for all points. Georeferenced plate does not exist at time of reconstruction.")
  }

  return(x)
}
