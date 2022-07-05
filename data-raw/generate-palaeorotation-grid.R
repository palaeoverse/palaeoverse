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
shapefile(x = r, "./inst/extdata/spatial-grid.shp", overwrite = TRUE)

#### Rotate shapefile in GPlates software####
# Rotate data to given midpoints using Merdith et al. (2021) plate rotation model
# Load GTS2020 ages
data("GTS2020")
# Subset to stages
rot_ages <- subset(GTS2020, rank == "Stage")
# Remove time intervals older than 541 Myr
rot_ages <- subset(rot_ages, mid_ma < 541)
# Get midpoint ages of stages
rot_ages <- rot_ages[,c("interval_name", "mid_ma")]
# Round to nearest million years for rotation
rot_ages$mid_ma <- round(rot_ages$mid_ma, digits = 0)
# Save data
saveRDS(object = rot_ages, file = "./inst/extdata/rot_ages.RDS")

#### Build rotation grids ####
######WRIGHT2013#####
# Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013).
# Towards community-driven paleogeographic reconstructions: integrating open-access
# paleogeographic and paleobiology data with plate tectonics. Biogeosciences, 10(3),
# 1529-1541. doi:10.5194/bg-10-1529-2013.

Wright2013 <- raster::as.data.frame(r)
Wright2013 <- Wright2013[,c("lng", "lat")]

#run loop to bind all ages
for(i in unique(rot_ages$mid_ma)){
  #load shape file
  shp <- shapefile(paste0("./inst/extdata/Wright2013/spatial-grid/reconstructed_", i, ".00Ma.shp"))
  #extract coordinates
  reconstruction <- raster::as.data.frame(shp[,c("lng", "lat")])
  #name coordinates
  colnames(reconstruction) <- c("lng", "lat", paste0("lng_", i), paste0("lat_", i))

  #search for matching longitude, latitude and ages
  reconstruction$index <- sapply(1:nrow(reconstruction), function(j){
    which(reconstruction$lng[j] == Wright2013$lng & reconstruction$lat[j] == Wright2013$lat) #extract closest longitude
  }, simplify = TRUE)

  #add empty columns to dataframe
  Wright2013[, c(paste0("lng_", i), paste0("lat_", i))] <- NA
  #add rotation coordinates to reference dataframe
  Wright2013[reconstruction$index,][, c(paste0("lng_", i), paste0("lat_", i))] <- round(c(reconstruction[,3], reconstruction[,4]), digits = 2)
}
# Set points not rotated to NA
for(i in 1:nrow(Wright2013)){
  if(length(unique(as.numeric(Wright2013[i,]))) == 2){
    Wright2013[i,3:ncol(Wright2013)] <- NA
  }else{next}
}
# Save data
saveRDS(object = Wright2013, file = "./inst/extdata/Wright2013.RDS", compress = "xz")

######SCOTESE2018#####
# Scotese, C., & Wright, N. M. (2018). PALEOMAP Paleodigital Elevation Models
# (PaleoDEMS) for the Phanerozoic. PALEOMAP Project.
# https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/.

Scotese2018 <- raster::as.data.frame(r)
Scotese2018 <- Scotese2018[,c("lng", "lat")]

#run loop to bind all ages
for(i in unique(rot_ages$mid_ma)){
  #load shape file
  shp <- shapefile(paste0("./inst/extdata/Scotese2018/spatial-grid/reconstructed_", i, ".00Ma.shp"))
  #extract coordinates
  reconstruction <- raster::as.data.frame(shp[,c("lng", "lat")])
  #name coordinates
  colnames(reconstruction) <- c("lng", "lat", paste0("lng_", i), paste0("lat_", i))

  #search for matching longitude, latitude and ages
  reconstruction$index <- sapply(1:nrow(reconstruction), function(j){
    which(reconstruction$lng[j] == Scotese2018$lng & reconstruction$lat[j] == Scotese2018$lat) #extract closest longitude
  }, simplify = TRUE)

  #add empty columns to dataframe
  Scotese2018[, c(paste0("lng_", i), paste0("lat_", i))] <- NA
  #add rotation coordinates to reference dataframe
  Scotese2018[reconstruction$index,][, c(paste0("lng_", i), paste0("lat_", i))] <- round(c(reconstruction[,3], reconstruction[,4]), digits = 2)
}
# Set points not rotated to NA
for(i in 1:nrow(Scotese2018)){
  if(length(unique(as.numeric(Scotese2018[i,]))) == 2){
    Scotese2018[i,3:ncol(Scotese2018)] <- NA
  }else{next}
}
# Save data
saveRDS(object = Scotese2018, file = "./inst/extdata/Scotese2018.RDS", compress = "xz")


#####MERDITH2021#########
# Merdith, A.S., Williams, S.E., Collins, A.S., Tetley, M. G.,  Mulder, J. A., Blades, M. L.,
# Young, A., Armistead, S.E., Cannon, J., Zahirovic, S., Müller, R. D. (2021).
# Extending full-plate tectonic models into deep time: Linking the Neoproterozoic and the Phanerozoic.
# Earth-Science Reviews 214 (103477). https://doi.org/10.1016/j.earscirev.2020.103477.

# Convert spatial grid to dataframe to serve as reference frame for rotations
Merdith2021 <- raster::as.data.frame(r)
Merdith2021 <- Merdith2021[,c("lng", "lat")]

#run loop to bind all ages
for(i in unique(rot_ages$mid_ma)){
  #load shape file
  shp <- shapefile(paste0("./inst/extdata/Merdith2021/spatial-grid/reconstructed_", i, ".00Ma.shp"))
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
# Set points not rotated to NA
for(i in 1:nrow(Merdith2021)){
  if(length(unique(as.numeric(Merdith2021[i,]))) == 2){
    Merdith2021[i,3:ncol(Merdith2021)] <- NA
  }else{next}
}
# Save data
saveRDS(object = Merdith2021, file = "./inst/extdata/Merdith2021.RDS", compress = "xz")
