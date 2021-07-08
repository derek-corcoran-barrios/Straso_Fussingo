library(sf)
library(tidyverse)
library(raster)
library(rgdal)

Straso <- read_sf("Stråsø - basismodel tilpasset.TAB")
Straso <- Straso[1,] %>% st_transform(crs= "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")

## generate a list of the shapefiles for cropping the rasters


Shapes <- list(Straso) %>%  
  purrr::map(st_union) %>% 
  purrr::map(st_as_sf)

# crop the lidar rasters

## TWI

Wetness_lidar_files <- list.files(path = "O:/Nat_Ecoinformatics-tmp/au634851/dk_lidar_backup_2021-03-09/outputs/twi/", full.names = T, pattern = ".vrt")
Wetness_lidar_files <- raster(Wetness_lidar_files)


Twis <- Shapes %>% 
  purrr::map(~crop(Wetness_lidar_files, .x)) %>% 
  purrr::map2(Shapes,~mask(.x, .y))

## Canopy height

Canopy_Cover <- list.files(path = "O:/Nat_Ecoinformatics-tmp/au634851/dk_lidar_backup_2021-03-09/outputs/canopy_height/", full.names = T, pattern = ".vrt")
Canopy_Cover <- raster(Canopy_Cover)


Canopy_Heights <- Shapes %>% 
  purrr::map(~crop(Canopy_Cover, .x)) %>% 
  purrr::map2(Shapes,~mask(.x, .y))

## Vegetation density

Vegetation_dens <- list.files(path = "O:/Nat_Ecoinformatics-tmp/au634851/dk_lidar_backup_2021-03-09/outputs/proportions/vegetation_density/", full.names = T, pattern = ".vrt")
Vegetation_dens <- raster(Vegetation_dens)

VegDens <- Shapes %>% 
  purrr::map(~crop(Vegetation_dens, .x)) %>% 
  purrr::map2(Shapes,~mask(.x, .y))


## Distance to the coast
#shapefile of denmark

Denmark <- getData(name = "GADM", country = "DNK", level = 0) %>% 
  st_as_sf()

Denmark <- st_transform(Denmark, crs = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")

#only extract the points in the limits of Iceland

grid <- Shapes %>% purrr::map(~st_make_grid(.x, cellsize = 10, what = "centers")) %>% 
  purrr::map2(Shapes, ~ st_intersection(.x, .y))


#transform Iceland from polygon shape to line
Denmark <- st_cast(Denmark, "MULTILINESTRING")

#calculation of the distance between the coast and our points
dist <-  grid %>% purrr::map(~st_distance(Denmark, .x)) %>% 
  purrr::map(as.numeric) %>% 
  purrr::map(round) %>% 
  purrr::map(as.integer)

#distance with unit in meters
head(dist[[1]][1,])

df <- dist %>% purrr::map2(grid,~data.frame(dist = .x,
                                            st_coordinates(.y)))

ext <-  grid %>% purrr::map(~extent(as(.x, "Spatial")))

#raster destination
r <- ext %>% 
  purrr::map(~raster(resolution = 10, ext = .x, crs = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")) 

#convert the points to a spatial object class sf
dist_sf <- df %>% purrr::map(~st_as_sf(.x, coords = c("X", "Y"), crs = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")) 

#create the distance raster
dist_raster <- dist_sf %>% purrr::map2(r, ~rasterize(.x, .y, "dist", fun = mean))

## Slope

slope <- list.files(path = "O:/Nat_Ecoinformatics-tmp/au634851/dk_lidar_backup_2021-03-09/outputs/slope/", full.names = T, pattern = ".vrt")
slope <- raster(slope)

Slope <- Shapes %>% 
  purrr::map(~crop(slope, .x)) %>% 
  purrr::map2(Shapes,~mask(.x, .y))

## Solar Radiation


Solar <- list.files(path = "O:/Nat_Ecoinformatics-tmp/au634851/dk_lidar_backup_2021-03-09/outputs/solar_radiation/", full.names = T, pattern = ".vrt")
Solar <- raster(Solar)

Solar_Rad <- Shapes %>% 
  purrr::map(~crop(Solar, .x)) %>% 
  purrr::map2(Shapes,~mask(.x, .y))


##Stacks

Straso <- stack(Twis[[1]], Canopy_Heights[[1]], VegDens[[1]], raster::resample(dist_raster[[1]], Twis[[1]]), Slope[[1]], Solar_Rad[[1]]) %>% readAll()

names(Straso)[4] <- "Dist_to_coast"

saveRDS(Straso, "StrasoRaster.rds")


## Add the Soil rasters

SoilRasters <- list.files(path = "O:/AUIT_Geodata/Denmark/Natural_ressources/Soil_geology/Texture3D_2014/geotiffs/",pattern = ".tif", recursive = T, full.names = T)
SoilRasters <- SoilRasters[str_detect(SoilRasters,pattern = ".aux", negate = T)]
SoilRasters <- SoilRasters[str_detect(SoilRasters,pattern = ".xml", negate = T)]
SoilRasters <- SoilRasters[str_detect(SoilRasters,pattern = ".ovr", negate = T)]
SoilRasters <- SoilRasters[str_detect(SoilRasters,pattern = "/a", negate = F)]


Soil3D <- SoilRasters[c(5:7,10)] %>% purrr::map(raster) %>% purrr::reduce(stack)

Soil <- Shapes %>% 
  purrr::map(~crop(Soil3D, .x)) %>% 
  purrr::map2(Shapes,~mask(.x, .y)) %>%
  purrr::map2(Twis,~ resample(.x,.y))


Straso <- stack(Straso, Soil[[1]]) %>% readAll()

saveRDS(Straso, "StrasoRaster.rds")

Trojborg <- stack(Trojborg, Soil[[2]]) %>% readAll()

saveRDS(Trojborg, "TrojborgRaster.rds")
