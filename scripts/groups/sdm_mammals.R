
### Background #################################################

background_atlas <- atlas |> 
  filter(group_en %in% c("Mammals")) |>
  filter(!dataset_name %in% c("Données de localisation des grands mammifères")) |>
  collect()

background_gbif <- gbif |> 
  filter(class %in% c("Mammalia")) |>
  collect()

### Observations ###############################################

obs_atlas <- atlas |> 
  filter(genus == !!genus) |> 
  collect() |>
  mutate(species = sapply(strsplit(valid_scientific_name, " "), function(i){paste(i[1:2], collapse = " ")})) |>
  filter(species == !!sp)

obs_gbif <- gbif |>
  filter(species == !!sp) |>
  collect()