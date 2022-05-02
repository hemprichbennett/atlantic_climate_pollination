  # Script was done by
  # Bruno M. Carvalho (https://github.com/brunomc-eco)
  
  # Script was edited by
  # Luara Tourinho
  
  # Date: 01 abr 2021

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

# Creating/reading our species list
splist <- read.csv("./data/raw_data/interaction_list.csv")
splist <- unique(splist$Pollinator)


# GBIF --------------------------------------------------------------------


# getting records from gbif
# got this code from https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/
# Manual of rgbif: https://cran.r-project.org/web/packages/rgbif/rgbif.pdf

gbif_taxon_keys <- splist %>%
  get_gbifid_(method="backbone") %>% # get taxonkeys for each species name
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back to data.frame
  bind_rows() # combine all results in a single data.frame

only_keys <- gbif_taxon_keys %>%
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted names
  pull(usagekey) #retain only the taxonkeys

# download data directly at GBIF
# (file needs to be mannualy fetched at the user's downloads page at gbif.org)

# Do you need a login on GBIF
# enter GBIF credentials
user <- "barbadensa" # your gbif.org username
pwd <- "Pap!to66" # your gbif.org password
email <- "vovaldo2000@gmail.com" # your email

occ_download(
  pred_in("taxonKey", only_keys),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = user, pwd = pwd, email = email
)

# DOI10.15468/dl.j5t9hg
gbif_df <- fread("./data/0154668-210914110416597.csv", na.strings = c("", NA))
gbif_df2 <- gbif_df[,c(8,10,23,22,33,16)]
colnames(gbif_df2) <- c("family", "species", "lon", "lat", "year", "country")
# Before
# [1] "family"           "species"          "decimalLongitude" "decimalLatitude"  "year"            
# [6] "countryCode" 



# speciesLink -------------------------------------------------------------


# https://rdrr.io/github/saramortara/rspeciesLink/man/rspeciesLink.html

splist_specieslink <- rspeciesLink (dir = "C:/Users/barba/Documents/UERJ/An?lises/Pollinators/data" ,
                                   filename = "splist" ,
                                   save = TRUE,
                                   basisOfRecord = NULL,
                                   species = splist,
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
