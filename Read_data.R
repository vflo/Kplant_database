#' =============================================================================
#' Title: Kplant database
#' Description: Script for extracting, cleaning, and combining plant hydraulic
#'              conductance (K) data from multiple Excel files, including 
#'              taxonomic corrections and standardization for the Kplant database.
#' 
#' Author(s): Victor Flo
#' Affiliation: CREAF
#' Contact: v.flo@creaf.uab.cat
#' 
#' Date created: 15/12/2025
#' Last modified: 22/01/2026
#' 
#' Version: 0.0.8
#' 
#' License: MIT
#' 
#' Dependencies:
#'   - R packages: readxl, dplyr, purrr, ggplot2, tidyr, readr
#'   - External script: species_names_check.R (must be run first to generate
#'     species_names_clean.csv)
#'   - Input: Excel files with "data" sheets located in data/ folder
#' 
#' Output: Kplant_0.0.8.csv
#' 
#' Usage:
#'   1. Ensure data/ folder contains source Excel files
#'   2. Run species_names_check.R to generate species_names_clean.csv
#'   3. Run this script
#' =============================================================================



#### Load required libraries ####
library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)

#### Function to extract "data" sheet from Excel files ####
extract_data_sheets <- function(data_folder = "data") {
  
  # Check if the DATA folder exists
  if (!dir.exists(data_folder)) {
    stop(paste("Folder", data_folder, "does not exist!"))
  }
  
  # Find all Excel files (.xlsx and .xls) recursively in DATA folder and subfolders
  excel_files <- list.files(
    path = data_folder,
    pattern = "\\.(xlsx|xls)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(excel_files) == 0) {
    message("No Excel files found in the specified folder and subfolders.")
    return(NULL)
  }
  
  message(paste("Found", length(excel_files), "Excel file(s)"))
  
  # Function to safely read the "data" sheet from a single file
  read_data_sheet <- function(file_path) {
    tryCatch({
      # Get sheet names to check if "data" sheet exists
      sheet_names <- excel_sheets(file_path)
      
      # Check if "data" sheet exists
      if ("data" %in% sheet_names) {
        # Read the data with better column name handling
        data <- read_excel(file_path, sheet = "data", .name_repair = "unique_quiet")
        
        # Check for duplicate column names and report them
        original_names <- names(read_excel(file_path, sheet = "data", .name_repair = "minimal"))
        duplicate_cols <- original_names[duplicated(original_names) | duplicated(original_names, fromLast = TRUE)]
        
        if (length(duplicate_cols) > 0) {
          message(paste("Note:", basename(file_path), "has duplicate column names:", paste(unique(duplicate_cols), collapse = ", ")))
        }
        
        message(paste("Successfully read 'data' sheet from", basename(file_path), "- Rows:", nrow(data), "Columns:", ncol(data)))
        
        # Add source information
        data$source_file <- basename(file_path)
        data$file_path <- file_path
        
        return(data)
      } else {
        message(paste("'data' sheet not found in", basename(file_path), ". Available sheets:", paste(sheet_names, collapse = ", "), ". Skipping."))
        return(NULL)
      }
      
    }, error = function(e) {
      message(paste("Error reading", basename(file_path), ":", e$message))
      return(NULL)
    })
  }
  
  # Apply the function to all Excel files
  message("Processing files...")
  all_data <- map(excel_files, read_data_sheet)
  
  # Remove NULL entries (failed reads)
  all_data <- all_data[!map_lgl(all_data, is.null)]
  
  if (length(all_data) == 0) {
    message("No data could be extracted from any files.")
    return(NULL)
  }
  
  message(paste("Successfully processed", length(all_data), "file(s)"))
  
  # Return list of data frames or combine them
  return(all_data)
}

#### Function to standardize column types across data frames ####
standardize_column_types <- function(data_list) {
  if (length(data_list) == 0) return(data_list)
  
  # Get all unique column names
  all_cols <- unique(unlist(lapply(data_list, names)))
  
  # For each column, determine the most appropriate type
  for (col_name in all_cols) {
    # Skip source columns we added
    if (col_name %in% c("source_file", "file_path")) next
    
    # Get all values for this column across all data frames
    all_values <- unlist(lapply(data_list, function(df) {
      if (col_name %in% names(df)) df[[col_name]] else NA
    }))
    
    # Remove NA values for type detection
    non_na_values <- all_values[!is.na(all_values)]
    
    if (length(non_na_values) == 0) next
    
    # Convert all instances of this column to character first (safest conversion)
    data_list <- lapply(data_list, function(df) {
      if (col_name %in% names(df)) {
        df[[col_name]] <- as.character(df[[col_name]])
      }
      return(df)
    })
  }
  
  return(data_list)
}

# Function to combine all data sheets into one data frame
combine_data_sheets <- function(data_folder = "DATA") {
  data_list <- extract_data_sheets(data_folder)
  
  if (is.null(data_list) || length(data_list) == 0) {
    return(NULL)
  }
  
  tryCatch({
    # Standardize column types before combining
    message("Standardizing column types across files...")
    data_list <- standardize_column_types(data_list)
    
    # Combine all data frames
    combined_data <- bind_rows(data_list)
    message(paste("Combined data has", nrow(combined_data), "rows and", ncol(combined_data), "columns"))
    return(combined_data)
  }, error = function(e) {
    message(paste("Error combining data frames:", e$message))
    message("Attempting alternative combination method...")
    
    # Alternative: convert all columns to character before binding
    tryCatch({
      data_list_char <- lapply(data_list, function(df) {
        # Convert all non-source columns to character
        for (col in names(df)) {
          if (!col %in% c("source_file", "file_path")) {
            df[[col]] <- as.character(df[[col]])
          }
        }
        return(df)
      })
      
      combined_data <- bind_rows(data_list_char)
      message(paste("Successfully combined data (all columns as text) - Rows:", nrow(combined_data), "Columns:", ncol(combined_data)))
      message("Note: All data columns converted to text format to resolve type conflicts.")
      return(combined_data)
      
    }, error = function(e2) {
      message(paste("Alternative method also failed:", e2$message))
      message("Returning list of individual data frames instead.")
      return(data_list)
    })
  })
}


#### Extract and combine into one data frame ####
combined_data <- combine_data_sheets("data")




#### Filtering of include data sets and taxonomic corrections ####
data <- combined_data |> 
  dplyr::filter(Included == "yes",
                !is.na(Kwp)) |> 
  rename(pl_species_original = pl_species) |> 
  mutate(pl_species_original = case_when(pl_species_original == 
                                           "Citrus sinensus x C. limmetoides + C. aurantium"~"Citrus sinensis",
                                         pl_species_original == 
                                           "Citrus sinensus x C. aurantium"~"Citrus sinensis",
                                         TRUE~pl_species_original))

#First run species_names_check.R script
sp_names <- readr::read_csv("species_names_clean.csv")

data <- data |> 
  left_join(sp_names |> select(-1), by = join_by(pl_species_original)) |>
  relocate(pl_family, gbif_id, pl_species_corrected, .after = pl_species_original)

data <- data |> 
  mutate(
    pl_species_corrected = case_when(
      pl_species_original == "Acacia robusta" ~ "Vachellia robusta",
      pl_species_original == "Citrus sinensis" ~ "Citrus aurantium",
      pl_species_original == "Sclerolobium paniculatum var.
Subvelutinum" ~ "Tachigali vulgaris",
      pl_species_original == "Phyllostachys
pubescens" ~ "Phyllostachys edulis",
      pl_species_original == "Quercus pubescens"  ~ "Quercus pubescens",
      pl_species_original == "Zea mays L" ~ "Zea mays",
      pl_species_original == "Tanacetum cinerariifolium (Trevir.) Sch. Bip" ~ "Tanacetum cinerariifolium",
      pl_species_original == "Callitris rhomboidea R. Br. ex A. Rich. & Rich" ~ "Callitris rhomboidea",
      pl_species_original == "Populus deltoides x nigra" & is.na(pl_species_corrected) ~ "Populus deltoides x nigra",
      pl_species_original == "Ribes uva-crispa" & is.na(pl_species_corrected) ~ "Ribes uva-crispa",
      TRUE ~ pl_species_corrected
    ),
    gbif_id = case_when(
      pl_species_original == "Acacia robusta" ~ 8166188,
      pl_species_original == "Quercus pubescens" ~ 2881283,
      pl_species_original == "Tanacetum cinerariifolium (Trevir.) Sch. Bip" ~ 3118284,
      pl_species_original == "Callitris rhomboidea R. Br. ex A. Rich. & Rich" ~ 2684320,
      pl_species_original == "Citrus sinensis" ~ 8077391,
      pl_species_original == "Ribes uva-crispa" ~ 2986185,
      pl_species_original == "Sclerolobium paniculatum var.
Subvelutinum" ~ 3932173,
      pl_species_original == "Phyllostachys
pubescens" ~ 5290168,
      TRUE ~ gbif_id
    ),
    pl_family = case_when(
      pl_species_original == "Acacia robusta" ~ "Fabaceae",
      pl_species_original == "Quercus pubescens" ~ "Fagaceae",
      pl_species_original == "Populus deltoides x nigra" ~ "Salicaceae",
      pl_species_original == "Tanacetum cinerariifolium (Trevir.) Sch. Bip" ~ "Asteraceae",
      pl_species_original == "Callitris rhomboidea R. Br. ex A. Rich. & Rich" ~ "Cupresaceae",
      pl_species_original == "Citrus sinensis" ~ "Rutaceae",
      pl_species_original == "Ribes uva-crispa" ~ "Grossulariaceae",
      pl_species_original == "Sclerolobium paniculatum var.
Subvelutinum" ~ "Fabaceae",
      pl_species_original == "Phyllostachys
pubescens" ~ "Poaceae",
      pl_species_original == "Zea mays L" ~ "Poaceae",
      TRUE ~ pl_family
    )
  )


#### Relabel columns ####
data <- data |> 
  rename(pl_age_mean = pl_age,
         pl_height_mean = pl_height,
         st_LAI = pl_LAI,
         K_original = Kwp,
         K_original_standard_error = Standard_error_Kwp,
         K_original_units = Kwp_original_units,
         Kleaf = Kwp_cor_Leaf,
         Ksapwood = Kwp_cor_sapwood,
         Kplant = Kwp_cor_plant,
         Kwood = Kwp_cor_wood,
         Kground = Kwp_cor_ground,
         Kmethod = Kwp_method,
         WaterFlux_standard_error = Standard_error_WaterFlux,
         wp_leaf_midday_standard_error = Standard_error_wp_leaf_midday,
         wp_leaf_predawn_standard_error = Standard_error_wp_leaf_predawn,
         wp_soil_standard_error = Standard_error_wp_soil)

#### Normalize data types ####
data <- data |> 
  mutate(
    # Integer columns
    across(c(N, gbif_id), as.integer),
    
    # Numeric columns - coordinates and site
    across(c(si_lat, si_long, si_altitude), as.numeric),
    
    # Numeric columns - plant traits
    across(c(pl_age_mean, pl_height_mean, pl_basal_area, pl_DBH,  pl_LA, pl_SA, 
             pl_huber_value), as.numeric),
    
    # Numeric columns - stand and soil
    across(c(st_LAI, st_basal_area, st_density, soil_sand_perc, soil_silt_perc, 
             soil_clay_perc, soil_om_perc, soil_bulk_density, 
             volumetric_water_content), as.numeric),
    
    # Numeric columns - water potentials
    across(c(wp_leaf_midday, wp_leaf_predawn, wp_soil, wp_soil_depth,
             wp_leaf_midday_standard_error, wp_leaf_predawn_standard_error, 
             wp_soil_standard_error, deltaWP), as.numeric),
    
    # Numeric columns - fluxes and conductance
    across(c(WaterFlux, WaterFlux_standard_error, K_original, 
             K_original_standard_error, Kleaf, Ksapwood, Kplant, Kwood, 
             Kground), as.numeric),
    
    # Numeric columns - gas exchange and hydraulic traits
    across(c(gs, E, VPD, CO2, P50, Pmin, TLP, Gsmax, Ks), as.numeric)
  )

#### Include pl_basal_area if not calculated from DBH ####
data <- data |> 
  mutate(pl_basal_area = case_when(is.na(pl_basal_area)&!is.na(pl_DBH)~pi*(pl_DBH/200)^2,
                                   TRUE~pl_basal_area))


#### Write database ####
readr::write_csv(data, file = "Kplant_0.0.8.csv")


# data <- readr::read_csv(file = "Kplant_0.0.7.csv")
# 
# names(data)
# data |> 
#   select(pl_SA, pl_basal_area,file_path, IDref,pl_species_corrected,Contributor,PaperDOI) |>
#   mutate(diff = pl_basal_area - pl_SA) |> 
#   arrange(diff) |> print(n=60)