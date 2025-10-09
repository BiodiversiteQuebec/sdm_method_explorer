
# the main difference with trees is that we remove the observations coming from the inventaire forestier

### Background #################################################

#x <- atlas |> 
#  filter(kingdom %in% c("Plantae")) |>
#  filter(dataset_name %in% c("Points d'observation écologique")) |>
#  rename(species = valid_scientific_name) |>
#  count(species) |>
#  arrange(-n) |>
#  collect() |>
#  (\(x) x[!grepl("Placettes-échantillons", x$dataset_name), ])()# |>

#x <- atlas |> 
#  filter(kingdom %in% c("Plantae")) |>
#  filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
#  count(dataset_name) |>
#  arrange(-n) |>
#  collect() |>
#  (\(x) x[!grepl("Placettes-échantillons", x$dataset_name), ])()# |>

#plants_poe <- atlas |> 
#  filter(kingdom %in% c("Plantae")) |>
#  filter(dataset_name %in% c("Points d'observation écologique")) |>
#  rename(species = valid_scientific_name) |>
#  count(species) |>
#  arrange(species, -n) |>
#  collect() |>
#  as.data.frame()

# atlas |> 
#  filter(kingdom %in% c("Plantae")) |>
#  filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
#  filter(!dataset_name %in% c("Points d'observation écologique")) |>
#  filter(!dataset_name %in% c("Relevés écologiques terrestres nordiques")) |>
#  filter(!dataset_name %in% c("Réseau de suivi de la biodiversité du Québec")) |>
#  collect() |>
#  (\(x) x[!grepl("Placettes-échantillons", x$dataset_name), ])() |>
#  count(dataset_name) |>
#  arrange(-n) |>
#  as.data.frame() |> _[1:50, 1]
#  head(20)


background_poe <- atlas |> 
  filter(kingdom %in% c("Plantae")) |>
  filter(dataset_name %in% c("Points d'observation écologique")) |>
  distinct(longitude, latitude, .keep_all = TRUE) |>
  collect() |>
  (\(x) x[!grepl("Placettes-échantillons", x$dataset_name), ])()# |>

background_nonpoe <- atlas |> 
  filter(kingdom %in% c("Plantae")) |>
  filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
  filter(!dataset_name %in% c("Points d'observation écologique")) |>
  filter(!dataset_name %in% c("Relevés écologiques terrestres nordiques")) |>
  collect() |>
  (\(x) x[!grepl("Placettes-échantillons", x$dataset_name), ])()# |>

#background_atlas <- rbind(background_poe, background_nonpoe) 

background_atlas <- atlas |> 
  filter(kingdom %in% c("Plantae", "Fungi")) |>
  filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
  filter(!dataset_name %in% c("Points d'observation écologique")) |>
  filter(!dataset_name %in% c("Relevés écologiques terrestres nordiques")) |>
  filter(!dataset_name %in% c("Réseau de suivi de la biodiversité du Québec")) |>
  head(20000) |>
  #filter(!dataset_name %in% c("Points d'observation écologique")) |>
  collect() |>
  (\(x) x[!grepl("Placettes-échantillons", x$dataset_name), ])()# |>

#background_placettes <- atlas |> 
#  filter(kingdom %in% c("Plantae")) |>
#  rename(species = valid_scientific_name) |>
#  collect() |>
#  (\(x) x[grepl("Placettes-échantillons", x$dataset_name), ])() |>
#  count(dataset_name)
#  distinct(longitude, latitude, .keep_all = TRUE)

#background_atlas <- atlas |> 
#  filter(kingdom %in% c("Plantae")) |>
#  filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
#  collect() |>
#  (\(x) x[!grepl("Placettes-échantillons", x$dataset_name), ])()# |>
#  #count(dataset_name) |> arrange(-n)

#xx <- gbif |> 
#  filter(kingdom %in% c("Plantae")) |>
#  count(collectioncode, institutioncode) |>
#  arrange(-n) |>
#  collect() |>
#  as.data.frame() |>
#  head(50)
#  as.data.table()
#  filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
#  collect()

background_gbif <- gbif |> 
  filter(kingdom %in% c("Plantae")) |>
  head(100000) |>
  #filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
  collect()

### Observations ###############################################

obs_atlas <- atlas |> 
  filter(genus == !!genus) |> 
  filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
  filter(!dataset_name %in% c("Points d'observation écologique")) |>
  filter(!dataset_name %in% c("Relevés écologiques terrestres nordiques")) |>
  filter(!dataset_name %in% c("Réseau de suivi de la biodiversité du Québec")) |>
  collect() |>
  mutate(species = sapply(strsplit(valid_scientific_name, " "), function(i){paste(i[1:2], collapse = " ")})) |>
  filter(species == !!sp) |>
  (\(x) x[!grepl("Placettes-échantillons", x$dataset_name), ])()

obs_gbif <- gbif |>
  filter(species == !!sp) |>
  #filter(!dataset_name %in% c("Pl@ntNet automatically identified occurrences")) |>
  collect()