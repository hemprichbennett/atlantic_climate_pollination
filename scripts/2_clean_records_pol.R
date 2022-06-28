
# Cleaning records --------------------------------------------------------

# Script was done by
# Bruno M. Carvalho (https://github.com/brunomc-eco)

# Script was edited by
# Luara Tourinho

# Date: 01 abr 2021

library(tidyverse)
library(CoordinateCleaner)
library(countrycode)

# reading data

searches_df  <- read_csv("./data/raw_data/pollinators/01_search_refined_results.csv")

# removing records with NA coordinates, keeping only species from our list
searches_occs <- searches_df %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  filter(species %in% splist)


# Viewing unclean records
ggplot()+ coord_fixed()+
  borders("world", colour="gray50", fill="gray50") +
  geom_point(data = searches_occs, aes(x = lon, y = lat),
             colour = "yellow", size = 0.5)+
  theme_bw()

# standardizing country names
searches_occs$countrycode <- countrycode(searches_occs$country, origin = 'iso2c', destination = 'iso3c')
searches_occs$countrycode <- ifelse(is.na(searches_occs$countrycode), 
                                    "BRA", searches_occs$countrycode)

# NA <- 0
searches_occs$lon <- ifelse(is.na(searches_occs$lon), 
                            0, searches_occs$lon)

flags_occs <- clean_coordinates(
  x = searches_occs,
  lon = "lon",
  lat = "lat",
  countries = "countrycode",
  centroids_rad = 2000,
  # had to increase this limit because was not flagging the centroid of Brazil
  species = "species",
  tests = c(
    "capitals",
    # flags records at adm-0 capitals
    "centroids",
    # flags records at country centroids
    "equal",
    # flags records with equal lon and lat
    "gbif",
    # flags records at gbif headquarters
    "institutions",
    # flags records at biodiversity institutions
    "seas",
    # flags records at sea
    "zeros"
  )
) # flags records with zero lon or lat

# Viewing flagged records
plot(flags_occs, lon = "lon", lat = "lat")

# Removing flagged records and duplicates
searches_occs_clean1 <- searches_occs[flags_occs$.summary, ] %>%
  distinct()

# Cleaning old date -------------------------------------------------------

search_occ_by_date <- search_occ_extracted %>%
  filter(year >= 1950)

# Number of records -------------------------------------------------------

n_records <- count(search_occ_by_date, species)

# Writing outputs ---------------------------------------------------------

write_csv(n_records, path = "./data/processed_data/pollinators/02_n_records.csv")
write_csv(search_occ_by_date, path = "./data/processed_data/pollinators/02_clean_occ.csv")

