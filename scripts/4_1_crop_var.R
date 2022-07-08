
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
#library(sf)


## Current variables ------------------------------------------------------


# Reading rasters
# use pattern = '.tif$' or something else if you have multiple files in this folder
raster_files <- list.files("./data/raw_data/", full.names = T, 'tif$|bil$')
head(raster_files)

envi <- stack(raster_files)

# Cropping rasters

# Choose your extention
# All America
envi.cut<-crop(envi, c(-160, -28, -60, 90))
plot(envi.cut[[1]])
st_crs(envi.cut) <- 32610
# Projections

# geographical, datum WGS84
crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
# projected, South America Albers Equal Area Conic
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 
                  +y_0=0 +ellps=aust_SA +units=m +no_defs") 

# Creating mask area for variables' correlation
species_df <- list.files(path = 'outputs/sp/', 
                                    pattern = '.+_5.csv',
                         full.names = T) %>% 
  lapply(., read.csv) %>% 
  bind_rows() %>%
  mutate(species = as.character(species))
write.csv(species_df,"./data/processed_data/03_thin_rec.csv")

interaction_df <- read.csv('data/raw_data/interaction_list.csv')

taxa_groups <- c('both_groups', 'plants', 'pollinators')

for(taxa in taxa_groups){
  
  
  
  if(taxa == 'plants'){
    species_df_to_use <- species_df %>%
      filter(species %in% interaction_df$Plant)
  }else if(taxa == 'pollinators'){
    species_df_to_use <- species_df %>%
      filter(species %in% interaction_df$Pollinator)
  }else if(taxa == 'both_groups'){
    species_df_to_use <- species_df
  }
  
  coords <- species_df %>%
    # filter so that we only have rows where the absolute value of
    # latitude is less than or equal to 90
    filter(abs(lat) <= 90) %>%
    # filter so that we only have rows where the absolute value of
    # longitude is less than or equal to 90
    filter(abs(lon) <= 180) %>%
    select(lon, lat)
  
  coordinates(coords) <- c("lon", "lat")
  proj4string(coords) <- crs.wgs84  # define original projection - wgs84
  
  # library(ggplot2)
  # coords_wgs <- slot(coords, 'coords') %>%
  #   as_tibble()
  # ggplot(coords_wgs, aes(x = lon, y = lat)) + geom_point()+ 
  #   ggtitle('after using proj4string(coords) <- crs.wgs84')
  # ggsave('figures/wgs_plot.jpeg')
  # coords <- spTransform(coords, crs.albers)  # project to Albers Equal Area
  # 
  # coords_albers <- slot(coords, 'coords') %>%
  #   as_tibble()
  # ggplot(coords_albers, aes(x = lon, y = lat)) + geom_point()+ 
  #   ggtitle('after using coords <- spTransform(coords, crs.albers)')
  # ggsave('figures/albers_plot.jpeg')
  mcp <- gConvexHull(coords) # create minimum convex polygon
  # If you want to add a buffer with a distance or an area around the mpc
  # Attention: gBuffer and gArea are in meters, you have to convert in km if you want to
  mcp_buffer <- gBuffer(mcp, width = gArea(mcp)*2e-09) # 2% bigger than mcp
  mcp_buffer <- SpatialPolygonsDataFrame(mcp_buffer, data = data.frame("val" = 1, row.names = "buffer"))
  
  mcp_buffer <- spTransform(mcp_buffer, crs.wgs84)
  
  envi.mask <- crop(envi.cut,mcp_buffer)
  envi.mask2 <- mask(envi.mask,mcp_buffer)
  
  initial_dir <- './data/processed_data/env_cropped/present'
  
  # Saving rasters
  if(!dir.exists(initial_dir)){
    dir.create(initial_dir)
  }
  
  dirstring <- paste0(initial_dir, '/', taxa, '/')
  
  if(!dir.exists(dirstring)){
    dir.create(dirstring)
  }
  
  writeRaster(envi.mask2, filename=dirstring , format="GTiff", 
              bylayer=TRUE, suffix="names", overwrite=TRUE)
  
  
  
}
