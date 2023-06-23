
# Cleaning records --------------------------------------------------------

# Script was done by
# Bruno M. Carvalho (https://github.com/brunomc-eco)

# Script was edited by
# Luara Tourinho

# Date: 01 abr 2021

library(tidyverse)
library(CoordinateCleaner)
library(countrycode)


# reading data using vroom as it doesn't move the file into ram
gbif_df <-read_tsv("./data/raw_data/gbif_output.csv") %>%
  bind_rows(read_tsv('data/raw_data/0036248-230530130749713.csv'))
coords <- read_csv('parameters/coordinate_boundaries.csv')
# make the coordinates into a named vector
coords_vec <- deframe(coords)


gbif_df2 <- gbif_df %>%
  select(family, species, countryCode, decimalLongitude, decimalLatitude, year) %>%
  rename('country' = 'countryCode', 'lon' = 'decimalLongitude', 'lat' = 'decimalLatitude') %>%
  filter(year >= 1950) %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  filter(lon <= coords_vec['max_lon'] & lon >= coords_vec['min_lon']) %>%
  filter(lat <= coords_vec['max_lat'] & lat >= coords_vec['min_lat']) %>%
  mutate(date_of_search = Sys.Date())

write_csv(x = gbif_df2, path = 'data/raw_data/01_gbif_more_refined.csv')


splist <- read_csv('data/processed_data/splist.csv') %>%
  pull(splist)

# 
# # Before
# # [1] "family"           "species"          "decimalLongitude" "decimalLatitude"  "year"
# # [6] "countryCode"
# 
# 

splist_specieslink_table <- read_csv('data/raw_data/splink_data.csv') %>%
  rename(lat = decimalLatitude, 
         lon = decimalLongitude)
gbif_table <-
  tibble(species = splist,
         date_of_search = rep(Sys.Date(), length(splist))) %>%
  left_join(gbif_df2, by = "species")

searches <- bind_rows(splist_specieslink_table, gbif_df2)



# Saving outputs ----------------------------------------------------------

write_csv(searches, "./data/raw_data/01_search_refined_results.csv")
#write_csv(splist_specieslink, "./data/raw_data/01_specieslink_refined.csv")
write_csv(gbif_df2, "./data/raw_data/01_gbif_refined.csv")
# why is the same object being saved twice?
#write_csv(splist_specieslink, "./data/raw_data/01_unclean_records_specieslink.csv")
write_csv(gbif_df, "./data/raw_data/01_unclean_records_gbif.csv")
# 


searches_df  <- searches



# removing records with NA coordinates, keeping only species from our list
searches_occs <- searches_df %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  filter(species %in% splist) %>%
  filter(lon <= coords_vec['max_lon'] & lon >= coords_vec['min_lon']) %>%
  filter(lat <= coords_vec['max_lat'] & lat >= coords_vec['min_lat'])


# Viewing unclean records
ggplot()+ coord_fixed()+
  borders("world", colour="gray50", fill="gray50") +
  geom_point(data = searches_occs, aes(x = lon, y = lat),
             colour = "yellow", size = 0.5)+
  theme_bw()

# standardizing country names

searches_occs <- searches_occs %>%
  mutate(country = gsub('Brasil', 'Brazil', country)) %>%
  mutate(countrycode = ifelse(nchar(country)> 2,
                              countrycode(country, origin = 'country.name', destination = 'iso3c'),
                              countrycode(country, origin = 'iso2c', destination = 'iso3c')
                              ))
# searches_occs$countrycode <- ifelse(is.na(searches_occs$countrycode), 
#                                     "BRA", searches_occs$countrycode)

# # NA <- 0
# searches_occs$lon <- ifelse(is.na(searches_occs$lon), 
#                             0, searches_occs$lon)

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



# Cleaning by worldclim files ------------------------------------------------

library(raster)

searches_occs_clean2 = searches_occs_clean1
# file from https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_2.5m_bio.zip
variable_world <- raster("data/raw_data/wc2.1_2.5m_bio_1.tif")
# All Americas
variable <- crop(variable_world, c(-160, -28, -60, 90))
coordinates(searches_occs_clean2) <- ~lon+lat
proj4string(searches_occs_clean2)=CRS("+proj=longlat +datum=WGS84")
searches_prj<-spTransform(searches_occs_clean2,
                          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(variable, axes=T)
plot(searches_prj, add=T, col= "red")

varcrop = crop(variable, searches_prj)

searches_extract <- raster::extract(varcrop, searches_prj, method = "bilinear")
searches_prjext<- data.frame(searches_prj,searches_extract)

which(searches_prjext$searches_extract ==0)
which(is.na(searches_prjext$searches_extract))
search_occ_extracted <- searches_prjext[!is.na(searches_prjext$searches_extract),]
head(search_occ_extracted)

search_occ_extracted <- search_occ_extracted[,c(1,7,8,4,5,6,2)]
head(search_occ_extracted)
dim(search_occ_extracted)



# Cleaning non-endemic species (example) ----------------------------------


# which species have records in other continent outside Americas? (lon < -20)

clean_df = search_occ_extracted

western_species <- clean_df %>%
  mutate(western = lon <= -20) %>%
  filter(western == TRUE) %>%
  #group_by(species, western) %>%
  #summarize(n_records = n()) %>%
  pull(species) %>%
  unique() %>%
  tibble() %>%
  rename("westernmost_species" = ".") %>%
  arrange(westernmost_species)


# removing these species from clean_df
`%notin%` <- Negate(`%in%`)

clean_df <- clean_df %>%
  filter(species %notin% as.character(western_species$westernmost_species))


# plotting clean records # All Americas
ggplot() +
  borders("world", colour="gray50", fill="gray50") +
  geom_point(data = clean_df, aes(x = lon, y = lat),
             colour = "blue", size = 0.5) +
  coord_sf(xlim = c(-160, -28), ylim = c(-60,90)) +
  theme_bw()



# Cleaning old date -------------------------------------------------------

search_occ_by_date <- search_occ_extracted %>%
  filter(year >= 1950)

# Number of records -------------------------------------------------------

n_records <- count(search_occ_by_date, species)

# Writing outputs ---------------------------------------------------------

write_csv(n_records, path = "./data/processed_data/02_n_records.csv")
write_csv(search_occ_by_date, path = "./data/processed_data/02_clean_occ.csv")

