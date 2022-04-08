
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

five_list <- lapply(spp, function(x) thin_function(x, thin_dist = 5))

clean_df_thin_5 <- bind_rows(five_list)

write_csv(clean_df_thin_5, path = "outputs/03_clean_df_thin_5.csv")

# and now 10 km
ten_list <- lapply(spp, function(x) thin_function(x, thin_dist = 10))

clean_df_thin_10 <- bind_rows(ten_list)

write_csv(clean_df_thin_10, path = "outputs/03_clean_df_thin_10.csv")