
library(devtools)
library(tidyverse)
library(rgbif)
library(taxize) # for get_gbifid_
library(data.table)
library(dplyr)
library(purrr)
library(readr)
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(Rocc) # fpr spescieslink
library(bit64)

 # read in file that gbif created
# DOI10.15468/dl.j5t9hg
gbif_df <- fread("./data/raw_data/gbif_output.csv", na.strings = c("", NA)) %>%
  # add file from second gbif query (some synonyms for species were used in 2022 query,
  # whoops)
bind_rows(., fread("data/raw_data/0036248-230530130749713.csv"))

just_new <- fread("data/raw_data/0036248-230530130749713.csv") %>%
  pull(species) %>%
  unique(.)
gbif_df2 <- gbif_df %>%
  select(family, species, decimalLongitude, decimalLatitude, year, countryCode) %>%
  rename(lat = decimalLatitude,
         lon = decimalLongitude,
         countrycode = countryCode)
    
#colnames(gbif_df2) <- c("family", "species", "lon", "lat", "year", "country")
# Before
# [1] "family"           "species"          "decimalLongitude" "decimalLatitude"  "year"            
# [6] "countryCode" 


splist <- unique(gbif_df2$species)

tibble(splist = splist) %>%
  write_csv(file = 'data/processed_data/splist.csv')

# speciesLink -------------------------------------------------------------


# https://rdrr.io/github/saramortara/rspeciesLink/man/rspeciesLink.html


# it looks like the below crashes if you request too many species at once

# split the call to rspecieslink in two, so that it isn't rejected

n_iterations <- 60
per_iteration <- seq(1,length(splist))

batches <- split(per_iteration, sort(per_iteration%%n_iterations))
splink_list <- list()
for(i in 1:length(batches)){
  sp <- splist[batches[[i]]]
  if(i == 7){
    sp <- sp[-1]
  }
  splink_list[[i]] <- rspeciesLink (dir = "data/processed_data/" ,
                filename = paste0("splist_",i) ,
                save = TRUE,
                basisOfRecord = NULL,
                # improve the following line:
                species = sp,
                collectionCode = NULL,
                country = NULL,
                stateProvince = NULL,
                county = NULL,
                Coordinates = "Yes", #		Yes | No | Original | Automatic | Blocked
                CoordinatesQuality = "Good",	#Good | Bad
                Typus = FALSE,
                Images = NULL,
                RedList = FALSE,
                MaxRecords = NULL)
  Sys.sleep(60)
}

  splink_list <- bind_rows(splink_list)

write_csv(x = splink_list, file = 'data/processed_data/splink_list.csv')  
splink_list <-  read_csv(file = 'data/processed_data/splink_list.csv')



splist_specieslink <- splink_list %>%
  select(family, genus, specificEpithet, decimalLongitude, decimalLatitude, year, country) %>%
  unite(species, genus, specificEpithet, sep = ' ')
# [1] "family"           "genus"            "species"          "scientificName"   "decimalLongitude"
# [6] "decimalLatitude"  "year"             "countryCode" 



# splist_specieslink <- splist_specieslink[, c(1,4,5,6,7,8)]
# colnames(splist_specieslink) <- c("family", "species", "lon", "lat", "year", "country")


# Table with search results -----------------------------------------------

splist_specieslink_table <-
  tibble(species = splist,
         date_of_search = rep(Sys.Date(), length(splist))) %>%
  left_join(splist_specieslink, by = c("species" = "species"))

write_csv(splist_specieslink_table, 'data/raw_data/splink_data.csv') 
