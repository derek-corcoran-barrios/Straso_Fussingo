library(sf)
library(tidyverse)
library(raster)
library(rgdal)

Straso <- read_sf("Stråsø - basismodel tilpasset.TAB")
Straso <- Straso[1,]

#Fussingo <- read_sf("Hegn_linje_polyline.shp", SHAPE_RESTORE_SHX = YES)
