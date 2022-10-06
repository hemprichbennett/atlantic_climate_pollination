# Credits ---------------------------

# Original routine by
# Vitor Cavalcante


# Edited by
# Julia Niemeyer & Luara Tourinho

# Further edited by Dave Hemprich-Bennett

# Date: 04 Aug 2021

# This script is an example for generating pseudoabsence using biomod2 ----



# Load packages -----------------------------------------------------------



library(biomod2)
library(raster)
library(dplyr)


# Set parameters ----------------------------------------------------------

task_id <- commandArgs(trailingOnly = TRUE)
task_id <- as.numeric(task_id[1])

if(is.na(task_id)){
  interactive <- T
  print('is interactive')
}else{
  interactive <- F
  print('is non-interactive')
}

if(grepl('Dropbox', getwd())){
  # job is local, run in a loop
  local <- T
  print('is local')
}else{
  local <- F
  print('is remote')
}




if(local == T){
  processed_dir = './data/processed_data/'
}else{
  processed_dir = '/data/zool-mosquito_ecology/zool2291/atlantic_climate_pollination/data/processed_data/'
}
## Entrar com a planilha limpa pós spthin
thin_df = paste0(processed_dir, "03_thin_rec.csv") ##enter the name of your table

##minimum occurrence records to run analysis
n_min <- 15

min_distances <- c(50000, 10000) #it will run for 50km first. For species with restricted distribution, 50km will
#return an error and the function will run for 10km



# Functions ---------------------------------------------------------------

intersect_mask <- function(x){
  values_x <- getValues(x)
  inter_x <- values_x %*% rep(1,nlayers(x))
  mask <- setValues(subset(x,1),values = (inter_x>0))
  return(mask)
}


## function to get PA dataset
get_PAtab <- function(bfd){
  dplyr::bind_cols(
    x = bfd@coord[, 1],
    y = bfd@coord[, 2],
    status = bfd@data.species,
    bfd@PA
  )
}

## function to get background mask
get_mask <- function(bfd){
  bfd@data.mask
}



########## END OF ACTION  NEEDED ############

# Reading files -----------------------------------------------------------
sp <- read.table(thin_df, header=TRUE, sep=",")#%>%
sp_names <- unique(sp$species)

if(interactive == F){
  sp_names <- sp_names[task_id]
}

#length(sp_names) <- 300

for (a in 1:length(sp_names)){
  sp <- read.table(thin_df, header=TRUE, sep=",") %>%
    filter(species == paste0(sp_names[a])) %>%
    select(species, lon, lat)

  message("starting the analysis for ", paste0(sp_names[a]))

  
  sp_dir <- paste0(processed_dir, 'sp_polygons/', sp_names[a])
  if (nrow(sp) < n_min){ ##Will not analyze species with less than 15 occurences
    print('species has less than 15 records and will not be analyzed')
    if()
    next
  }

  

  #Get only lat and long
  My_target_species <- sp[,2:3]



  # Reading environmental variables -----------------------------------------

  # Read your environmental raster selected by correlation
  raster_files <- list.files(paste0(sp_dir, "/Pres_env_crop"), full.names = T, 'tif$|bil$')
  
  head(raster_files)

  environment <- stack(raster_files)

  ## keep only all cells that are defined for all layers
  environment <- stack(mask(environment, intersect_mask(environment)))

  

  occurrence.resp <- rep(1, length(My_target_species$lon))

  skip_to_next <<- FALSE


# Model 1 -----------------------------------------------------------------

    

  # Loop tries 50km, breaks if it succeeds, tries 10km if it fails
  for (i in 1:length(min_distances)) {

    tryCatch(Mymodel <- BIOMOD_FormatingData(
      resp.var = occurrence.resp,
      expl.var = environment,
      resp.xy = My_target_species,
      resp.name = "Occurrence",
      PA.nb.rep = 1,
      PA.nb.absences = length(sp$species),
      PA.strategy = "disk",
      PA.dist.min = min_distances[i],
      PA.dist.max = 20000000,
      na.rm = TRUE) , error = function(e) {skip_to_next <<- TRUE})

    if(skip_to_next == TRUE) {
      print(paste0('Species has restricted distribution, trying 10km'))
      next
    } else {

      print(paste0('using minimum distance of ', min_distances[i]))
      break
    }}
  #Mymodel

  gc()
  
  (pres.xy <- get_PAtab(Mymodel) %>%
      filter(status == 1) %>%
      select(x, y))

  ## get the coordinates of pseudoabsences
  ## all repetition of pseudoabsences sampling merged
  (pa.all.xy <- get_PAtab(Mymodel) %>%
      filter(is.na(status)) %>%
      select(x, y)) %>%
    distinct()
  
  write.csv(pa.all.xy, paste0(sp_dir, "/pseudoabs1.csv"), row.names = F) ##no loop colocar dentro da pasta de cada espécie
  
  
  ##esse aqui não farei
  pseudoabs <- pa.all.xy
  
  
  pres = sp
  # Replace using your species name in "Genus_epithet" ##aqui ajeitar para loop
  pres$`species` <- sub(pattern = paste0(sp_names[a]), replacement = "1", x = pres$`species`)
  
  pseudo_0 <- rep(0,nrow(pseudoabs))
  
  pseudoabs$species <- pseudo_0
  pseudoabs <- pseudoabs[,c(3,1,2)]
  names(pseudoabs) <-c("species","lon","lat")
  pres_pseudo_table <- rbind(pres,pseudoabs)
  
  names(pres_pseudo_table) <-c("pa","lon","lat")
  
  write.csv(pres_pseudo_table,paste0(sp_dir,"/pres_pseudoabs.csv"), row.names = F)
  
# Model 2 -----------------------------------------------------------------

  
  ##Another model
  # Loop tries 50km, breaks if it succeeds, tries 10km if it fails
  for (i in 1:length(min_distances)) {

    tryCatch(Mymodel2 <- BIOMOD_FormatingData(
      resp.var = occurrence.resp,
      expl.var = environment,
      resp.xy = My_target_species,
      resp.name = "Occurrence",
      PA.nb.rep = 1,
      PA.nb.absences = length(sp$species)*10, ##Isso tem que entrar no loop
      PA.strategy = "disk",
      PA.dist.min = min_distances[i],
      PA.dist.max = 20000000,
      na.rm = TRUE) , error = function(e) {skip_to_next <<- TRUE})

    if(skip_to_next == TRUE) {
      print(paste0('Species has restricted distribution, trying 10km'))
      next
    } else {

      print(paste0('using minimum distance of ', min_distances[i]))
      break
    }}
  #Mymodel2

  (pres.xy2 <- get_PAtab(Mymodel2) %>%
      filter(status == 1) %>%
      select(x, y))
  
  
  ## get the coordinates of pseudoabsences
  ## all repetition of pseudoabsences sampling merged
  (pa.all.xy2 <- get_PAtab(Mymodel2) %>%
      filter(is.na(status)) %>%
      select(x, y)) %>%
    distinct()

  write.csv(pa.all.xy2, paste0(sp_dir, "/pseudoabs2.csv"), row.names = F) ##no loop colocar dentro da pasta de cada espécie
  
  
  pseudoabs2 <- pa.all.xy2
  
  
  pres = sp
  # Replace using your species name in "Genus_epithet" ##aqui ajeitar para loop
  pres$`species` <- sub(pattern = paste0(sp_names[a]), replacement = "1", x = pres$`species`)
  
  pseudo_0 <- rep(0,nrow(pseudoabs2))
  pseudoabs2$species <- pseudo_0
  pseudoabs2 <- pseudoabs2[,c(3,1,2)]
  names(pseudoabs2) <-c("species","lon","lat")
  pres_pseudo_table2 <- rbind(pres,pseudoabs2)
  names(pres_pseudo_table2) <-c("pa","lon","lat")
  
  ##aqui mudar no loop para entrar dentro da pasta da espécie
  
  write.csv(pres_pseudo_table2,paste0(sp_dir,"/pres_pseudoabs2.csv"), row.names = F)
  
# both --------------------------------------------------------------------







# both --------------------------------------------------------------------





# both --------------------------------------------------------------------



  
  


# 1 -----------------------------------------------------------------------

  

 


# 2 -----------------------------------------------------------------------


  


# Both --------------------------------------------------------------------

  

  ##aqui mudar no loop para entrar dentro da pasta da espécie
  
  write.csv(pres_pseudo_table2,paste0(sp_dir,"/pres_pseudoabs2.csv"), row.names = F)
  
  
gc()
}


