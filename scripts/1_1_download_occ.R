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
# reading the data in will give a warning, because some rows have more columns
# for the source reference than others
interaction_file <- read_csv("./data/raw_data/interaction_list.csv")

splist <- unique(c(interaction_file$Pollinator, interaction_file$Plant))

# some species had synonyms, so were missing from gbif etc
missing_sp <- read_csv('data/raw_data/missing_sp.csv',col_names = F) %>%
  pull(X1)
splist <- c(splist, missing_sp)

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

gbif_taxon_keys %>%
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted names
  select(usagekey) %>% #retain only the taxonkeys
  #rename(taxonKey = only_keys) %>%
  write_csv(., 'data/raw_data/01_gbif_taxonkeys.csv')

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


# The above command requests that GBIF creates a file for you,
# which needs to be downloaded in a browser session.
# The request may take a few hours
