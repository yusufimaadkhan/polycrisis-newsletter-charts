# Load packages

pacman::p_load(tidyverse, readr, readxl, janitor, here)

# set paths to make working with "here" easier (note - these are all relative to project)

input_path <- "newsletter-charts/1-hockey-sticks-and-crosses-globalisation/inputs"
output_path <- "newsletter-charts/1-hockey-sticks-and-crosses-globalisation/outputs"

# This script imports data for the IGBP/Steffen et al. (2015) great acceleration charts
# It goes through each of the tabs and stitches the data between 1750 and 2010 (where available) into one tidy dataframe

# First write a function to get all data from the excel sheets

get_data_from_all_sheets <- function(file_path) {
  sheets <- readxl::excel_sheets(file_path)
  data <- list()
  
  for (sheet_name in sheets) {
    sheet_data <- readxl::read_excel(file_path, sheet = sheet_name)
    data[[sheet_name]] <- sheet_data
  }
  
  return(data)
}

sheet_data <- get_data_from_all_sheets(here(input_path,"igbp_great_acceleration_data_collection.xlsx"))

# Remove read me (but obviously have a read of it)

sheet_data <- sheet_data[!names(sheet_data) %in% c("Read me")]

# Fix the var name row - should have one column in it that contains "Year"

fixed_list <- lapply(sheet_data, function(tibble) {
  year_row <- which(apply(tibble, 1, function(row) any(grepl("Year", row, ignore.case = FALSE))))
  
  if (length(year_row) > 0) {
    col_names <- as.character(tibble[year_row[1], ])
    tibble <- tibble[-year_row, ]
    names(tibble) <- col_names
  }
  
  return(tibble)
})

# Get rid of empty vars

fixed_list <- lapply(fixed_list, function(tibble) {
  tibble <- tibble[, colSums(is.na(tibble)) < nrow(tibble)]
  tibble <- tibble[, colnames(tibble) != "" & !is.na(colnames(tibble))]
  return(tibble)
})


# Rename any slightly different year columns so we can join it all up

fixed_list <- map(fixed_list, ~ {
  .x %>%
    rename_with(~ if_else(. %in% c("Year AD"), "Year", .), .cols = everything())
})

# Remove redundant vars

fixed_list <- map(fixed_list, ~ select(.x, -one_of(c("OECD", "BRICS", "Rest", "OEDC", "OECD accumulative", "BRICS accumulative", "Rest  accumulative")))) ## rest accumalative - 2 spaces

# Get rid of NAs in Year variable *only* 
# fixed this on 23/08/2023 - was previously an na.omit that caused trouble 
# with fertilizer data. It cut off a small part of the time series.
# Lesson learned - be specific and careful with NAs!

fixed_list <- map(fixed_list, ~ filter(.x, !is.na(Year)))

# Add list element name to each var except year so we can discern what each var is referring to

fixed_list <- imap(fixed_list, ~ {
  var_names <- names(.x)
  year_var <- var_names[var_names == "Year"]
  other_vars <- var_names[var_names != "Year"]
  other_vars_new <- paste0(.y, "_", other_vars)
  names(.x) <- c(year_var, other_vars_new)
  .x
})

# Join everything in the nested list together, put it in long format, and then classify trend type for facet

igbp_combined_df <- reduce(fixed_list, full_join, by = "Year") %>% 
  clean_names() %>% 
  pivot_longer(2:25, names_to = "indicator", values_to = "values") %>%
  
  mutate(trend_type = if_else(indicator%in%c(
    
    "x1_population_world",                                                 
    "x2_real_gdp_world",                                                   
    "x3_fdi_world",                                                        
    "x4_urban_population_world",                                           
    "x5_primary_energy_use_exajoule_ej",                                   
    "x6_fertilizer_consumption_world_incl_historic",                       
    "x7_large_dams_world_accumulative",                                    
    "x8_water_use_world",                                                  
    "x9_paper_production_world",                                           
    "x10_transportation_world",                                            
    "x11_telecommunications_world",                                        
    "x12_international_tourism_world"),
    
  "Socio-economic Trends", "Earth System Trends")) %>% 
  
  # Filter for range (some variables include projections)
  
  filter(!year>2010,
         !year<1750) %>% 
  
  # convert to numeric
  
  mutate(values = as.numeric(values)) %>% 
  
  # Filter empty observations
  
  filter(!is.na(values)) %>% 
  
  # Order by year
  
  arrange(year) %>% 
  
  # sort out the levels
  
  arrange(factor(indicator, levels=c(
    
    "x1_population_world",                                                 
    "x2_real_gdp_world",                                                   
    "x3_fdi_world" ,                                                       
    "x4_urban_population_world"  ,                                         
    "x5_primary_energy_use_exajoule_ej"  ,                                 
    "x6_fertilizer_consumption_world_incl_historic" ,                      
    "x7_large_dams_world_accumulative",                    
    "x8_water_use_world",                   
    "x9_paper_production_world",                  
    "x10_transportation_world",                 
    "x11_telecommunications_world",                
    "x12_international_tourism_world",
    
    "x1_carbon_dioxide_carbon_dioxide_ppm",              
    "x2_nitrous_oxide_nitrous_oxide_ppb",             
    "x3_methane_methane_ppb",            
    "x4_ozone_ozone_percent_loss",           
    "x5_temperature_temperature_anomaly_deg_c",          
    "x6_ocean_acidification_mean_hydrogen_ion_concentraion_h_nmol_kg",     
    "x7_marine_fish_marine_fish_capture_million_tonnes",    
    "x8_shrimp_aqu_shrimp_aquaculture_million_tonnes",   
    "x9_nitrogen_nitrogen_flux_mtons_yr_1",  
    "x10_tropical_forest_tropical_forset_loss_percent", 
    "x11_dom_land_domesticated_land_percent",
    "x12_terrestrial_biosph_degradati_percent_decr_mean_species_abundance"
    
  ))) %>% 
  
  filter(
    
    #!values==0, # unsure why I did this in original version. Fix it and note error. Removes one observation in 1943. Thankfully Observable Plot interpolated and it does not affect the visual
    
    ## filter x4 ozone for 1962 - this year keeps extending the y axis very far because there is a single negative value for ozone loss. I need to be careful here. I HAVE MADE A JUDGMENT HERE TO OMIT THIS VALUE FROM THE DATASET - IT IS AN ANALYTICAL CHOICE BASED ON ME THINKING THAT THE READER SHOULD BE ABLE TO VIEW THE GENERAL TRENDS WITH THE MAXIMUM CLARITY POSSIBLE. LET THIS BE KNOWN. 
  
    !(year == "1962" & indicator == "x4_ozone_ozone_percent_loss" & values<0)) %>% 
  
# Keep as numeric for now - convert in OJS
        mutate(year = as.numeric(year),
         
# Some values for year presented as e.g. 2010.5 - round this for now
        year = ceiling(year),
         
# Rename indicators to readable versions
         
        labels = case_match(indicator,
                             "x1_population_world"~"World Population",                                                 
                             "x2_real_gdp_world"~"Real GDP",                                                   
                             "x3_fdi_world"~"Foreign Direct Investment",                                                       
                             "x4_urban_population_world"~"Urban Population",                                         
                             "x5_primary_energy_use_exajoule_ej"~"Primary Energy Use",                                 
                             "x6_fertilizer_consumption_world_incl_historic"~"Fertilizer Consumption",                      
                             "x7_large_dams_world_accumulative"~"Large Dams",                    
                             "x8_water_use_world"~"Water Use",                   
                             "x9_paper_production_world"~"Paper Production",                  
                             "x10_transportation_world"~"Transportation",                 
                             "x11_telecommunications_world"~"Tele- communications",                
                             "x12_international_tourism_world"~"International Tourism",               
                             "x1_carbon_dioxide_carbon_dioxide_ppm"~"Carbon Dioxide",              
                             "x2_nitrous_oxide_nitrous_oxide_ppb"~"Nitrous Oxide",             
                             "x3_methane_methane_ppb"~"Methane",            
                             "x4_ozone_ozone_percent_loss"~"Stratospheric Ozone",           
                             "x5_temperature_temperature_anomaly_deg_c"~"Surface Temperature",          
                             "x6_ocean_acidification_mean_hydrogen_ion_concentraion_h_nmol_kg"~"Ocean Acidification",     
                             "x7_marine_fish_marine_fish_capture_million_tonnes"~"Marine Fish Capture",    
                             "x8_shrimp_aqu_shrimp_aquaculture_million_tonnes"~"Shrimp Aquaculture",   
                             "x9_nitrogen_nitrogen_flux_mtons_yr_1"~"Coastal Nitrogen",  
                             "x10_tropical_forest_tropical_forset_loss_percent"~"Tropical Forest Loss", 
                             "x11_dom_land_domesticated_land_percent"~"Domesticated Land",
                             "x12_terrestrial_biosph_degradati_percent_decr_mean_species_abundance"~"Terrestrial Biosphere Degradation",
                             .default = indicator),
         
# write up the labels - FT style for millions/billions - https://aboutus.ft.com/press_release/ft-makes-change-to-style-guide
         
         yAxisLabels = case_match(indicator,
                                  "x1_population_world"~"bn",                                                 
                                  "x2_real_gdp_world"~"tn US$",                                                   
                                  "x3_fdi_world"~"tn US$",                                                       
                                  "x4_urban_population_world"~"bn",                                         
                                  "x5_primary_energy_use_exajoule_ej"~"Exajoule - EJ",                                 
                                  "x6_fertilizer_consumption_world_incl_historic"~"mn tonnes",                      
                                  "x7_large_dams_world_accumulative"~"k dams",                    
                                  "x8_water_use_world"~"k km³",                   
                                  "x9_paper_production_world"~"mn tonnes",                  
                                  "x10_transportation_world"~"mn motor vehicles",                 
                                  "x11_telecommunications_world"~"bn phone subscriptions",                
                                  "x12_international_tourism_world"~"mn arrivals",               
                                  "x1_carbon_dioxide_carbon_dioxide_ppm"~"atmos. conc. ppm",              
                                  "x2_nitrous_oxide_nitrous_oxide_ppb"~"atmos. conc. ppb",             
                                  "x3_methane_methane_ppb"~"atmos. conc. ppb",            
                                  "x4_ozone_ozone_percent_loss"~"% loss",           
                                  "x5_temperature_temperature_anomaly_deg_c"~"temp. anomaly °C",          
                                  "x6_ocean_acidification_mean_hydrogen_ion_concentraion_h_nmol_kg"~"hydrogen ion, nmol kg⁻¹",     
                                  "x7_marine_fish_marine_fish_capture_million_tonnes"~"mn tonnes",    
                                  "x8_shrimp_aqu_shrimp_aquaculture_million_tonnes"~"mn tonnes",   
                                  "x9_nitrogen_nitrogen_flux_mtons_yr_1"~"Human N flux mtons yr⁻¹",  
                                  "x10_tropical_forest_tropical_forset_loss_percent"~"% loss area", 
                                  "x11_dom_land_domesticated_land_percent"~"% of total land area",
                                  "x12_terrestrial_biosph_degradati_percent_decr_mean_species_abundance"~"% dec. mean species abundance",
                                  .default = indicator))  
  

# Some smaller edits - fixing a few variables, and making sure that the surface temp var will shade underneath the line

igbp_combined_df <- igbp_combined_df %>% 

# Convert to readable % values i.e. 40 rather than 0.4. This is because the y axis label will tell you if its % or not. 
#  No need for extra code to adjust each axis value for each of 24 vars
  
  mutate(values = if_else (indicator == "x11_dom_land_domesticated_land_percent", values*100, values)) %>%  
  
  # this is to consistently shade values under the line for surface temp facet + 
  # leave a little room underneath so the line isn't flush with bottom of multiple. 
  # Observable plot area mark defaults to shading within constraint of y = 0, but also offers y1 + y2 values
  
  group_by(indicator) %>% 
  mutate(min = if_else (indicator == "x5_temperature_temperature_anomaly_deg_c", min(values) - 0.1, NA)) %>% 
  ungroup() %>% 
  
  # for the conditional bit of code that will fix differences between surface temp and other small multiples
  
  mutate(values_temp = values,
        values_all_else = if_else (indicator == "x5_temperature_temperature_anomaly_deg_c", NA, values)) 


# Export

write.csv(igbp_combined_df, 
          here(output_path, "igbp_combined_df.csv"), 
          row.names=FALSE)