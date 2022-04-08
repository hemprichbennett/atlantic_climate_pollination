
# SpThin ------------------------------------------------------------------

# Script was done by
# Bruno M. Carvalho (https://github.com/brunomc-eco)

# Script was edited by
# Luara Tourinho

# Date: 01 abr 2021

library(spThin) # Aiello-Lammens et al. 2015
# https://cran.r-project.org/web/packages/spThin/spThin.pdf
# https://doi.org/10.1111/ecog.01132

library(tidyverse)
library(data.table)

# loading clean occs
clean_df <- read_csv("outputs/02_clean_occ.csv")

# parameters for use on the cluster
task_id <- commandArgs(trailingOnly = TRUE)
task_id <- as.numeric(task_id[1])

# getting clean species list
spp <- sort(unique(clean_df$species))

thin_function <- function(chosen_sp, thin_dist){
  # specify the species to analyse, and the distance in km to thin
  df <- clean_df %>%
    filter(species == chosen_sp)
  thinned <- thin(df,
                  lat.col = "lat",
                  long.col = "lon",
                  spec.col = "species",
                  thin.par = thin_dist, # distance in km
                  reps = 1,
                  locs.thinned.list.return = TRUE,
                  write.files = FALSE,
                  write.log.file = FALSE)
  out_df <- data.frame(species = rep(chosen_sp, 
                                          nrow(thinned[[1]])),
                            lon = thinned[[1]]$Longitude,
                            lat = thinned[[1]]$Latitude)
  return(out_df)
}

# thinning records by 5 km
this_sp <- spp[task_id]
sp_df_5 <- thin_function(chosen_sp = this_sp, thin_dist = 5)


write_csv(sp_df_5, 
          path = paste0('outputs/03_', gsub(' ', '_', this_sp),
                        '_5.csv'))

# and now 10 km
this_sp <- spp[task_id]
sp_df_10 <- thin_function(chosen_sp = this_sp, thin_dist = 10)


write_csv(sp_df_10, 
          path = paste0('outputs/03_', gsub(' ', '_', this_sp),
                        '_10.csv'))
