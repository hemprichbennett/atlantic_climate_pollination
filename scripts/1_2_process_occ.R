
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
gbif_df <- fread("./data/raw_data/gbif_output.csv", na.strings = c("", NA))


gbif_df2 <- gbif_df[,c(8,10,23,22,33,16)]
colnames(gbif_df2) <- c("family", "species", "lon", "lat", "year", "country")
# Before
# [1] "family"           "species"          "decimalLongitude" "decimalLatitude"  "year"            
# [6] "countryCode" 


interaction_file <- read_csv("./data/raw_data/interaction_list.csv")
splist <- unique(c(interaction_file$Pollinator, interaction_file$Plant))

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
  Sys.sleep(30)
}

specieslink <- bind_rows(splink_list)

splist_specieslink <- splist_specieslink[,c(11,12,13,14,25,26,18,22)]
colnames(splist_specieslink)
# [1] "family"           "genus"            "species"          "scientificName"   "decimalLongitude"
# [6] "decimalLatitude"  "year"             "countryCode" 

splist_specieslink$species <- with(splist_specieslink, 
                                    paste(splist_specieslink$genus, splist_specieslink$specificEpithet))

splist_specieslink <- splist_specieslink[, c(1,4,5,6,7,8)]
colnames(splist_specieslink) <- c("family", "species", "lon", "lat", "year", "country")



# Table with search results -----------------------------------------------

splist_specieslink_table <-
  tibble(species = splist,
         date_of_search = rep(Sys.Date(), length(splist))) %>%
  left_join(splist_specieslink, by = c("species" = "species"))

gbif_table <-
  tibble(species = splist,
         date_of_search = rep(Sys.Date(), length(splist))) %>%  
  left_join(gbif_df2, by = "species")

searches <- rbind(splist_specieslink_table, gbif_table)

only_keys <- tibble(taxonKey = only_keys)



# Saving outputs ----------------------------------------------------------

write_csv(searches, "./data/raw_data/pollinators/01_search_refined_results.csv")
write_csv(splist_specieslink, "./data/raw_data/pollinators/01_specieslink_refined.csv")
write_csv(gbif_df2, "./data/raw_data/pollinators/01_gbif_refined.csv")
write_csv(splist_specieslink, "./data/raw_data/pollinators/01_unclean_records_specieslink.csv")
write_csv(gbif_df, "./data/raw_data/pollinators/01_unclean_records_gbif.csv")
write_csv(only_keys, "./data/raw_data/pollinators/01_gbif_taxonkeys.csv")
