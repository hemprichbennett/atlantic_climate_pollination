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
splist <- unique(splist$Plant)
splist1 <- splist[1:25]
splist2 <- splist[26:60]
splist3 <- splist[61:90]
splist4 <- splist[91:110]
splist5 <- splist[111:130]
splist6 <- splist[131:160]
splist7 <- splist[161:180]
splist8 <- splist[181:200]
splist9 <- splist[201:220]
splist10 <- splist[221:240]
splist11 <- splist[241:260]
splist12 <- splist[261:280]
splist13 <- splist[281:300]
splist14 <- splist[301:320]
splist15 <- splist[321:340]
splist16 <- splist[341:356]
splist17 <- splist[357:377]
splist18 <- splist[378:400]
splist19 <- splist[401:420]
splist20 <- splist[421:440]
splist21 <- splist[441:460]
splist22 <- splist[461:480]
splist23 <- splist[481:500]
splist24 <- splist[501:520]
splist25 <- splist[521:540]
splist26 <- splist[541:560]
splist27 <- splist[561:580]
splist28 <- splist[581:600]
splist29 <- splist[601:620]
splist30 <- splist[621:640]
splist31 <- splist[641:660]
splist32 <- splist[661:680]
splist33 <- splist[681:700]
splist34 <- splist[701:720]
splist35 <- splist[721:740]
splist36 <- splist[740:770]

splist.f <- c(splist1,splist2,splist3,splist4,splist5,splist6,splist7,splist8,splist9,
              splist10,splist11,splist12,splist13,splist14,splist15,splist16,splist17,
              splist18,splist19,splist20,splist21,splist22,splist23,splist24,splist25,
              splist26,splist27,splist28,splist29,splist30,splist31,splist32,splist33,
              splist34,splist35,splist36)


# GBIF =--------------------------------------------------------------------


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
credential_df <- read_csv('my_credentials.csv')

temp <- occ_download(
  pred_in("taxonKey", only_keys),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = credential_df$username, 
  pwd = credential_df$password, 
  email = credential_df$email
)

# DOI10.15468/dl.j5t9hg
gbif_df <- fread("./data/0210749-210914110416597.csv", na.strings = c("", NA))
gbif_df2 <- gbif_df[,c(8,10,23,22,33,16)]
colnames(gbif_df2) <- c("family", "species", "lon", "lat", "year", "country")
# Before
# [1] "family"           "species"          "decimalLongitude" "decimalLatitude"  "year"            
# [6] "countryCode" 



# speciesLink -------------------------------------------------------------


# https://rdrr.io/github/saramortara/rspeciesLink/man/rspeciesLink.html

splist_specieslink1 <- rspeciesLink (dir = "C:/Users/barba/Documents/UERJ/An?lises/Plants/data/" ,
                                   filename = "splist1" ,
                                   save = TRUE,
                                   basisOfRecord = NULL,
                                   species = splist1,
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

splist_specieslink1b <- splist_specieslink1[,c("family","genus","specificEpithet",
                                                 "scientificName","decimalLongitude",
                                               "decimalLatitude","year","country")]
colnames(splist_specieslink1b)
# [1] "family"           "genus"            "species"          "scientificName"   "decimalLongitude"
# [6] "decimalLatitude"  "year"             "countryCode" 

splist_specieslink1b$species <- with(splist_specieslink1b, 
                                    paste(splist_specieslink1b$genus, 
                                          splist_specieslink1b$specificEpithet))

splist_specieslink1 <- splist_specieslink1b[, c(1,4,5,6,7,8)]
colnames(splist_specieslink1) <- c("family", "species", "lon", "lat", 
                                    "year", "country")

splist_specieslink_final <- rbind(splist_specieslink1,splist_specieslink2,splist_specieslink3,
                                  splist_specieslink4,splist_specieslink5,splist_specieslink6,
                                  splist_specieslink7,splist_specieslink8,splist_specieslink9,
                                  splist_specieslink10,splist_specieslink11,splist_specieslink12,
                                  splist_specieslink13,splist_specieslink14,splist_specieslink15,
                                  splist_specieslink16,splist_specieslink17,splist_specieslink18,
                                  splist_specieslink19,splist_specieslink20,splist_specieslink21,
                                  splist_specieslink22,splist_specieslink23,splist_specieslink24,
                                  splist_specieslink25,splist_specieslink26,splist_specieslink27,
                                  splist_specieslink28,splist_specieslink29,splist_specieslink30,
                                  splist_specieslink31,splist_specieslink32,splist_specieslink33,
                                  splist_specieslink34,splist_specieslink35,splist_specieslink36)
splist_specieslink_final <- as.data.frame(splist_specieslink_final)

# Table with search results -----------------------------------------------

splist_specieslink_table <-
  tibble(species = splist,
         date_of_search = rep(Sys.Date(), length(splist))) %>%
  left_join(splist_specieslink1, by = c("species" = "species"))


gbif_table <-
  tibble(species = splist,
         date_of_search = rep(Sys.Date(), length(splist))) %>%  
  left_join(gbif_df2, by = "species")

searches <- rbind(splist_specieslink_table, gbif_table)

only_keys <- tibble(taxonKey = only_keys)



# Saving outputs ----------------------------------------------------------

write_csv(searches, "./data/raw_data/plants/01_search_refined_results.csv")
write_csv(splist_specieslink_table, "./data/raw_data/plants/01_specieslink_refined.csv")
write_csv(gbif_df2, "./data/raw_data/plants/01_gbif_refined.csv")
write_csv(splist_specieslink_final, "./data/raw_data/plants/01_unclean_records.csv")
write_csv(gbif_df, "./data/raw_data/plants/01_unclean_records_gbif.csv")
write_csv(only_keys, "./data/raw_data/plants/01_gbif_taxonkeys.csv")
