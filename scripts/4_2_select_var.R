
# Credits ---------------------------

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)

# Using a set of script from github, e.g.: 
# Diogo S. B. Rocha (https://github.com/diogosbr)
# Daniele Moreira (https://github.com/daniomoreira)

# Date: 01 abr 2021



# Select variable with less collinearity ---------------------------------------


library(raster)
library(dplyr)
library(corrplot)

initial_dir <- "./data/processed_data/env_cropped/present/"
cropped_dirs <- list.dirs(initial_dir, recursive = F,
                          full.names = F)
cropped_dirs_full <- paste0(initial_dir, cropped_dirs)

for(i in 1:length(cropped_dirs)){
  treatment <- cropped_dirs[i]
  in_dir <- cropped_dirs_full[i]
  
  
  # list of files of present variables
  present_list <- list.files(in_dir, pattern = "tif$", full.names = T)
  
  # object with present variables
  present_ras <- stack(present_list)
  
  # id from cells without NA
  mi <- Which(present_ras[[1]], cells = TRUE)
  
  max_sample <- 5000
  if(mi >= max_sample){
    # sampling cells to extract values
    sampled <- sample(mi, max_sample)
  }else{
    sampled <- mi
  }
  
  
  # values of selected cells from rasters of present variables
  vals <- present_ras[sampled]
  
  
  # An alternative using spearman ------------------------------------------------
  
  # selecting variables to exclude with correlation 0.6 or more
  # First try low values, as 0.6, if it return few variables, try higher as 0.7
  exclude_vars <- caret::findCorrelation(cor(vals, method = 'spearman'), cutoff = 0.7, names = TRUE)
  all_table <- as.data.frame(cor(vals, method = 'spearman'))
  
  # selecting variables with lower correlation (<0.6)
  pres_vars_sel <- present_ras[[which(!names(present_ras) %in% exclude_vars)]]
  pres_vars_sel
  
  # selecting variables with lower correlation (<0.6)
  pres_vars_sel_names <- names(present_ras)[!names(present_ras) %in% exclude_vars]
  pres_vars_sel_names
  # 2, 7, 8, 15, 18
  
  
  # for 0.6 [1] "X_wc2.1_2.5m_bio_15" "X_wc2.1_2.5m_bio_2"  "X_wc2.1_2.5m_bio_8"
  # for 0.7 [1] "X_wc2.1_2.5m_bio_15" "X_wc2.1_2.5m_bio_18" "X_wc2.1_2.5m_bio_2"  "X_wc2.1_2.5m_bio_8" 
  
  
  # An alternative using pearson -------------------------------------------------
  
  exclude_vars2 <- caret::findCorrelation(cor(vals, method = 'pearson'), cutoff = 0.6, names = TRUE)
  all_table2 <- as.data.frame(cor(vals, method = 'pearson'))
  
  pres_vars_sel2 <- present_ras[[which(!names(present_ras) %in% exclude_vars2)]]
  pres_vars_sel2
  
  pres_vars_sel_names2 <- names(present_ras)[!names(present_ras) %in% exclude_vars2]
  pres_vars_sel_names2
  #2,7,8,14,15
  
  
  # for 0.6 [1] "X_wc2.1_2.5m_bio_14" "X_wc2.1_2.5m_bio_15" "X_wc2.1_2.5m_bio_2"  "X_wc2.1_2.5m_bio_5" 
  # for 0.7 [1] "X_wc2.1_2.5m_bio_14" "X_wc2.1_2.5m_bio_15" "X_wc2.1_2.5m_bio_18" 
  #"X_wc2.1_2.5m_bio_19" "X_wc2.1_2.5m_bio_2"  "X_wc2.1_2.5m_bio_5"
  
  results_root <- './results/var_selection/'
  if(!dir.exists(results_root)){
    dir.create(results_root)
    }
  
  treatment_results <- paste0(results_root, treatment, '_')
  
  
  write.csv(all_table, paste0(treatment_results, "variables_correlation_spearman.csv"))
  write.csv(pres_vars_sel_names, paste0(treatment_results, "retained_spearman.csv"))
  write.csv(all_table2, paste0(treatment_results, "variables_correlation_pearson.csv"))
  write.csv(pres_vars_sel_names2, paste0(treatment_results, "retained_pearson.csv"))
  
  
  #BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
  
  #BIO3 = Isothermality (BIO2/BIO7) (?100)
  
  #BIO10 = Mean Temperature of Warmest Quarter
  
  #BIO15 = Precipitation Seasonality (Coefficient of Variation)
  
  #BIO18 = Precipitation of Warmest Quarter
  
  # Check Worldclim to chose which set of variable make more sense, if by spearman
  # or pearson
  # https://www.worldclim.org/data/bioclim.html
  
  
  # Copy selected variables in a new directory
  # creating directory
  if(!dir.exists('./data/processed_data/env_sel/')){
    dir.create("./data/processed_data/env_sel/")
  }
  
  # Add the chosen variables there
  
  
  
  # Building graphics -------------------------------------------------------
  
  # corrplot ----------------------------------------------------------------
  
  
  corrplot_graph <- cor(as.matrix(all_table))
  # As smaller the square, as less significant the correlation
  # We want to the less significant relationships
  corrplot(corrplot_graph, method="square")
  
  #Exploring...
  
  #Combining correlogram with the significance test
  res1 <- cor.mtest(corrplot_graph, conf.level = .95)
  ## specialized the insignificant value according to the significant level
  corrplot(corrplot_graph, p.mat = res1$p, sig.level = .05) 
  ## add p-values on no significant coefficient
  corrplot(corrplot_graph, p.mat = res1$p, insig = "p-value") 
  corrplot(corrplot_graph, method = "circle")
  # Display the correlation coefficient
  corrplot(corrplot_graph, method = "number") 
  #corrplot.mixed() is a wrapped function for mixed visualization style.
  corrplot.mixed(corrplot_graph) 
  #"FPC" for the first principal component order
  corrplot(corrplot_graph, type = "upper", order = "FPC")
  corrplot(corrplot_graph, method="square", type = "upper", order = "FPC")
  
  
  
}
