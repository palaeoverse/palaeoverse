# Load libraries
library(raster)
library(sf)

#### Define initial grid ####

# Generate spatial grid (1º x 1º) for rotation
r <- raster(res = 1)
# Convert raster to spatial points
r <- data.frame(rasterToPoints(x = r, spatial = FALSE))
# Assign column names
colnames(r) <- c("lng", "lat")
r$begin <- 540
r$end <- 0
r <- SpatialPointsDataFrame(coords = r[,c("lng", "lat")], data = r)
# Save as shapefile to palaeorotate in GPlates software
shapefile(x = r, "./data-raw/spatial-grid.shp", overwrite = TRUE)

#### Rotate shapefile in GPlates software####
# Rotate data to given midpoints using Merdith et al. (2021) plate rotation model
# Load GTS2020 ages
GTS2020 <- palaeoverse:::GTS2020
# Subset to stages
rot_ages <- subset(GTS2020, rank == "Stage")
# Remove time intervals older than 1000 Myr
rot_ages <- subset(rot_ages, mid_ma < 1000)
# Get midpoint ages of stages
rot_ages <- rot_ages[,c("interval_name", "mid_ma")]
# Round to nearest million years for rotation
rot_ages$mid_ma <- round(rot_ages$mid_ma, digits = 0)
# Save data
saveRDS(object = rot_ages, file = "./data-raw/rot_ages.RDS")

#### Build rotation grid ####
#####################
# Andrew S. Merdith, Simon E. Williams, Alan S. Collins, Michael G. Tetley, Jacob A. Mulder, Morgan L. Blades,
# Alexander Young, Sheree E. Armistead, John Cannon, Sabin Zahirovic, R. Dietmar Müller. (2021).
# Extending full-plate tectonic models into deep time: Linking the Neoproterozoic and the Phanerozoic.
# Earth-Science Reviews 214 (103477). \url{https://doi.org/10.1016/j.earscirev.2020.103477}.
#####################

# Convert spatial grid to dataframe to serve as reference frame for rotations
Merdith2021 <- raster::as.data.frame(r)
Merdith2021 <- Merdith2021[,c("lng", "lat")]

#run loop to bind all ages
for(i in unique(rot_ages$mid_ma)){
  #load shape file
  shp <- shapefile(paste0("./data-raw/Merdith2021/reconstructed_", i, ".00Ma.shp"))
  #extract coordinates
  reconstruction <- raster::as.data.frame(shp[,c("lng", "lat")])
  #name coordinates
  colnames(reconstruction) <- c("lng", "lat", paste0("lng_", i), paste0("lat_", i))

  #search for matching longitude, latitude and ages
  reconstruction$index <- sapply(1:nrow(reconstruction), function(j){
    which(reconstruction$lng[j] == Merdith2021$lng & reconstruction$lat[j] == Merdith2021$lat) #extract closest longitude
  }, simplify = TRUE)

  #add empty columns to dataframe
  Merdith2021[, c(paste0("lng_", i), paste0("lat_", i))] <- NA
  #add rotation coordinates to reference dataframe
  Merdith2021[reconstruction$index,][, c(paste0("lng_", i), paste0("lat_", i))] <- round(c(reconstruction[,3], reconstruction[,4]), digits = 2)
}
# Save data
saveRDS(object = rot_ages, file = "./data-raw/Merdith2021.RDS", compress = "xz")
