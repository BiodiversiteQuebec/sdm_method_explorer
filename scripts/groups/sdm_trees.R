
### Background #################################################

background_placettes <- atlas |> 
  filter(kingdom %in% c("Plantae")) |>
  rename(species = valid_scientific_name) |>
  collect() |>
  (\(x) x[grepl("Placettes-échantillons|Points d'observation écologique|Relevés écologiques terrestres nordiques", x$dataset_name), ])() |>
  mutate(coordinate_uncertainty = if_else(dataset_name %in% c("Points d'observation écologique"), "10", coordinate_uncertainty)) |> # assign high 
  #count(longitude, latitude) |> arrange(-n) |> nrow()
  #count(dataset_name)# |> _$n |> sum()#|>
  distinct(longitude, latitude, .keep_all = TRUE)

background_nonplacettes <- atlas |> 
  filter(kingdom %in% c("Plantae")) |>
  filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
  rename(species = valid_scientific_name) |>
  collect() |>
  (\(x) x[!grepl("Placettes-échantillons|Points d'observation écologique|Relevés écologiques terrestres nordiques", x$dataset_name), ])()

background_atlas <- rbind(background_placettes, background_nonplacettes) 

#atlas |> 
#  filter(kingdom %in% c("Plantae")) |>
#  count(dataset_name) |>
#  arrange(-n) |>
#  collect() |>
#  as.data.frame() |>
#  head(20)

#background_atlas <- atlas |> 
#  filter(kingdom %in% c("Plantae")) |>
#  filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
#  #filter(!dataset_name %in% c("Données de localisation des grands mammifères")) |>
#  collect()

background_gbif <- gbif |> 
  filter(kingdom %in% c("Plantae")) |>
  collect()

### Observations ###############################################

obs_atlas <- atlas |> 
  filter(genus == !!genus) |> 
  filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
  mutate(coordinate_uncertainty = if_else(dataset_name %in% c("Points d'observation écologique"), "10", coordinate_uncertainty)) |> # assign high 
  collect() |>
  mutate(species = sapply(strsplit(valid_scientific_name, " "), function(i){paste(i[1:2], collapse = " ")})) |>
  filter(species == !!sp)

obs_gbif <- gbif |>
  filter(species == !!sp) |>
  collect()