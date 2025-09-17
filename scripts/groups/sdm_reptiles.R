

### Background #################################################

background_atlas <- atlas |> 
  filter(group_en %in% c("Reptiles", "Amphibians")) |>
  collect()

background_gbif <- gbif |> 
  filter(class %in% c("Squamata", "Amphibia", "Testudines")) |>
  collect()


### Observations ###############################################

obs_atlas <- atlas |> 
  mutate(genus = case_match(genus, "Emys" ~ "Emydoidea", .default = genus)) |>
  filter(genus == !!genus) |> 
  collect() |>
  mutate(species = sapply(strsplit(valid_scientific_name, " "), function(i){paste(i[1:2], collapse = " ")})) |>
  filter(species == !!sp)

obs_gbif <- gbif |>
  mutate(species = case_match(species, "Emys blandingii" ~ "Emydoidea blandingii", .default = species)) |>
  filter(species == !!sp) |>
  collect()
