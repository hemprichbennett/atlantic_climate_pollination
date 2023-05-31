
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 08 may 2021


# Cropping variables from Worldclim ---------------------------------------


# Required packages 

library(raster)
library(dplyr)
library(rgeos)
library(reshape)
library(rgdal)


# Present variables ------------------------------------------------------

initial_dir <- "./data/processed_data/env_cropped/present/"
cropped_dirs <- list.dirs(initial_dir, recursive = F,
                          full.names = F)

cropped_dirs_full <- paste0(initial_dir, cropped_dirs)

results_root <- './results/var_selection/'

for(species in cropped_dirs){
  
  
  # the variables we decided in 4_2 to keep
  desired_variables <- paste0(results_root, species, '_retained_pearson.csv') %>%
    read_csv() %>%
    pull(x) #%>%
  #gsub('X', '', .)
  
  
  # Present variables ------------------------------------------------------
  
  
  # make a nice string that we can use to identify these in the file-loading step
  desired_str <- paste0(species, '/_', desired_variables, '.tif')
  
  present_vars_dir <- paste0('./data/processed_data/env_cropped/present/', species)
  
  raster_files <- list.files(present_vars_dir, full.names = T, 'tif$|bil$',
                             recursive = T)
  
  raster_paths_to_use <- raster_files[
    grep(paste(desired_str, collapse = '|'), raster_files)
    ]
  
  crop_by <- stack(raster_paths_to_use)
  
  
  
  # If future biovariables come separately, build a list like you did before
  
  
  # BCC ---------------------------------------------------------------------
  
  # spp126 ------------------------------------------------------------------
  
  
  # Choosing another future scenario
  
  future_files <- list.files('./data/raw_data/env/future',
                             full.names = T)
  if(!dir.exists('./data/processed_data/env_sel/')){
    dir.create("./data/processed_data/env_sel/")
  }
  
  base_dir <- 'data/processed_data/env_sel/future/'
  if(!dir.exists(base_dir)){
    dir.create(base_dir)
  }
  
  sp_dir <- paste0(base_dir, species)
  if(!dir.exists(sp_dir)){
    dir.create(sp_dir)
  }
  
  for(f in future_files){
    print(f)
    # get the climate scenario of the file, format it nicely
    scenario <- gsub('.+ssp|_2061.+', '', f)
    scenario <- sub('.', 'RCP', scenario)
    
    # make the output directory, if it doesn't already exist
    out_dir <- paste0(sp_dir, '/', scenario)
    if(!dir.exists(out_dir)){dir.create(out_dir)}
    
    # load in the data
    future_var <- stack(f)
    # select only our desired variables
    no_hyphens <- gsub('\\-', '.', f)
    to_use <- paste0(
      gsub('.+\\/|.tif', '', no_hyphens),
      '.',
      gsub('X', '', desired_variables)
    )
    future_var_stk <- future_var[[to_use]]
    # crop them by our selected present variables
    envi_fut.cut<- crop(future_var_stk, crop_by)
    
    # rename the variables for simplicity
    names(envi_fut.cut) <- paste0('bio_', gsub('.+\\.', '', names(envi_fut.cut)))
    
    # save the raster
    writeRaster(envi_fut.cut, filename=paste0(out_dir, '/'),
                format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)
  }
  
}
