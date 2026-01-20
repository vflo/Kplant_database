#### Code to correct species names ####
library(dplyr)
library(taxize)
library(purrr)
library(stringr)

data_clean <- data |>
  mutate(
    pl_species_clean = str_trim(pl_species_original) |> str_squish()
  )

unique_species <- data_clean |>
  distinct(pl_species_clean) |>
  filter(!is.na(pl_species_clean), pl_species_clean != "") |> 
  mutate(pl_species_clean = case_when(pl_species_clean == "Quercus pubescens" ~ "Quercus humilis",
                                      TRUE~pl_species_clean)) |> 
  pull(pl_species_clean)

get_taxonomy <- function(name) {
  name <- str_replace(name, " x ", " ")  # Normalizar símbolo
  clean_name <- str_extract(name, "^[A-Z][a-z]+ [a-z]+")
  if (is.na(clean_name)) return(NULL)
  
  class_result <- classification(clean_name, db = "gbif")[[1]]
  
  if (is.null(class_result) || !is.data.frame(class_result)) return(NULL)
  
  family <- class_result$name[class_result$rank == "family"]
  family <- if(length(family) > 0) family[1] else NA_character_
  
  species_row <- class_result[class_result$rank == "species", ]
  gbif_id <- if(nrow(species_row) > 0) species_row$id[1] else NA_character_
  
  tibble(
    pl_species_original = name,
    pl_family = family,
    gbif_id = gbif_id,
    pl_species_corrected = species_row$name
  )
}

taxonomy_list <- map(unique_species[order(unique_species)], ~{
  Sys.sleep(0.3)
  get_taxonomy(.x)
})

species_taxonomy <- map(taxonomy_list, ~{
  if (is.null(.x)) return(NULL)
  .x |> mutate(gbif_id = as.character(gbif_id))
}) |> bind_rows()

not_resolved <- setdiff(unique_species, species_taxonomy$pl_species_original)


write.csv(species_taxonomy,"species_names_clean.csv")
write.csv(not_resolved,"not_resolved_species_names.csv")
