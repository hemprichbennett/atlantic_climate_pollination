
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


# Opening GCMs ------------------------------------------------------------

# Now, for other GCMs
# And selected variables
# By Spearman test (0.6): 
# "X_wc2.1_2.5m_bio_15" "X_wc2.1_2.5m_bio_18" 
# "X_wc2.1_2.5m_bio_3"   "X_wc2.1_2.5m_bio_8"

## Future variables ------------------------------------------------------

to_crop <- raster('./data/processed_data/env_cropped/present/pollinators/_wc2.1_5m_bio_2.tif')
#to_crop <- envi.mask2[[1]]

# If future biovariables come separately, build a list like you did before


# BCC ---------------------------------------------------------------------

# spp126 ------------------------------------------------------------------

# Ps. 2021-2040 was done in the step "5_crop_environmental_variables.R"

# Choosing another future scenario

# 2041-2060

future_var <- stack("./data/raw_data/env/future/wc2.1_5m_bioc_BCC-CSM2-MR_ssp585_2061-2080.tif")
#future_var <- stack("./data/raw_data/env/future/wc2.1_5m_bioc_BCC-CSM2-MR_ssp245_2061-2080.tif")

# Chose the variables that you selected in "5_crop_environmental_variables.R"
future_var_stk <- future_var[[c(2,5,7,8,14,15,18)]]
envi_fut.cut<- crop(future_var_stk, to_crop)
envi_fut.mask<- mask(envi_fut.cut, to_crop)

# Save only that selected variables 5_crop_environmental_variables.R step
names(envi_fut.mask) <- c("bio_2","bio_5","bio_7","bio_8", "bio_14", "bio_15","bio_18")
#dir.create(paste0("./data/processed_data/env_sel/future/RCP45/"))
writeRaster(envi_fut.mask, filename='./data/processed_data/env_sel/future/pollinators/RCP85/', 
            format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)

# writeRaster(envi_fut.mask, filename='./data/processed_data/env_sel/future/pollinators/RCP45/', 
#            format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)


# Repeat this routine for other scenarios, GCMs, and years that interest you
