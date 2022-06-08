
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 01 abr 2021

#Modified by Tiago Teixeira 
#Date: april 2022

# Cropping variables from Worldclim ---------------------------------------


# Required packages 

library(raster)
library(dplyr)
library(rgeos)
library(reshape)
library(tidyr)


## Current variables ------------------------------------------------------


# Reading rasters
# use pattern = '.tif$' or something else if you have multiple files in this folder
raster_files <- list.files("./data/raw_data/env/present", full.names = T, 'tif$|bil$')
head(raster_files)

envi <- stack(raster_files)

# Cropping rasters

# Choose your extention
# All America
envi.cut<-crop(envi, c(-160, -28, -60, 90))
plot(envi.cut[[1]])

# Projections

# geographical, datum WGS84
crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
# projected, South America Albers Equal Area Conic
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 
                  +y_0=0 +ellps=aust_SA +units=m +no_defs") 


# Creating mask area for variables' correlation
species_df <- list.files("./data/processed_data/pollinators/03_thinned_records/5km",full.names = T, 'csv$') 
species_df <- lapply(species_df, read.csv) %>% bind_rows()
write.csv(species_df,"./data/processed_data/pollinators/03_thinned_records/03_thin_rec.csv")
coords <- species_df[ ,2:3]
coordinates(coords) <- c("lon", "lat")
proj4string(coords) <- crs.wgs84  # define original projection - wgs84
coords <- spTransform(coords, crs.albers)  # project to Albers Equal Area
mcp <- gConvexHull(coords) # create minimum convex polygon
# If you want to add a buffer with a distance or an area around the mpc
# Attention: gBuffer and gArea are in meters, you have to convert in km if you want to
mcp_buffer <- gBuffer(mcp, width = gArea(mcp)*2e-07) # 20% bigger than mcp
# mcp_buffer <- SpatialPolygonsDataFrame(mcp_buffer, data = data.frame("val" = 1, row.names = "buffer"))
mcp_buffer <- spTransform(mcp_buffer, crs.wgs84)

envi.mask <- crop(envi.cut,mcp_buffer)
envi.mask2 <- mask(envi.mask,mcp_buffer)

# Saving rasters
dir.create(paste0("./data/processed_data/env_cropped/present/pollinators/", "."))
writeRaster(envi.mask2, filename='./data/processed_data/env_cropped/present/pollinators/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


