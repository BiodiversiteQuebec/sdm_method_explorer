
library(ebirdst)

"https://data.canadensys.net/vascan/checklist?lang=en&habit=all&taxon=0&combination=anyof&province=BC&province=AB&province=SK&province=MB&province=ON&province=QC&province=NB&province=PE&province=NS&province=NL_N&province=NL_L&province=YT&province=NT&province=NU&status=native&status=introduced&status=ephemeral&status=doubtful&status=extirpated&status=excluded&rank=class&rank=order&rank=family&rank=genus&rank=species&nolimit=false&sort=taxonomically&criteria_panel=selection"

#vascan <- read.csv("http://data.canadensys.net/downloads/vascan/TXT-b23b0de1-4c83-4136-88c3-7e1ce0918d5c.txt", sep = "\t")
vascan <- read.csv("data/vascan.txt", sep = "\t")
vascan <- vascan[vascan$Rank == "Species", ]
trees <- vascan$Scientific.name[grepl("Tree", vascan$Habit)]

atlas <- duckdbfs::open_dataset("data/atlas_2025-03-17.parquet", tblname = "atlas")
gbif <- duckdbfs::open_dataset("data/gbif_2025-03-01.parquet")
#ebird <- duckdbfs::open_dataset("/home/frousseu/data2/ebd_relJan-2025.parquet")
ebird <- duckdbfs::open_dataset("data/ebd_relJan-2025.parquet")

species_info <- atlas |>
  #head() |> collect()
  filter(observed_rank %in% c("species", "subspecies", "variety", "form")) |>
  count(group_en, kingdom, phylum, class, order, genus, valid_scientific_name) |>
  arrange(-n) |> 
  collect() |>
  rename(species = valid_scientific_name) |>
  mutate(species = sub("^(([^ ]+ )[^ ]+).*", "\\1", species)) |>
  group_by(across(-n)) |>
  summarise(n = sum(n), .groups = "drop") |>
  mutate(group = tolower(group_en)) |>
  mutate(group = recode(group, "amphibians" = "reptiles")) |>
  mutate(group = ifelse(species %in% trees, "trees", group)) |>
  mutate(group = ifelse(species %in% plants, "plants", group)) |>
  select(-group_en) |>
  #count(group) |>
  filter(group %in% c("birds", "reptiles", "mammals", "trees", "plants")) |>
  as.data.frame()


eb <- ebirdst_runs |>
         mutate(start = ifelse(is.na(breeding_start), "01-01", substr(breeding_start, 6, 10))) |>
         mutate(end = ifelse(is.na(breeding_end), "12-31", substr(breeding_end, 6, 10))) |>
         rename(species = scientific_name) |>
         mutate(species = case_match(species, 
          "Botaurus exilis" ~ "Ixobrychus exilis", 
          "Acanthis flammea" ~ "Acanthis hornemanni",
          "Astur atricapillus" ~ "Accipiter atricapillus",
          "Astur cooperii" ~ "Accipiter cooperii",
          "Ardea ibis" ~ "Bubulcus ibis",
          "Botaurus exilis" ~ "Ixobrychus exilis",
          "Larus smithsonianus" ~ "Larus argentatus",
          "Nannopterum auritum" ~ "Phalacrocorax auritus",
          "Corthylio calendula" ~ "Regulus calendula",
          "Troglodytes aedon/musculus" ~ "Troglodytes aedon",
          .default = species)) |>
         select(species, start, end) |>
         unique() |>
         as.data.frame()

species_info <- merge(species_info, eb, all.x = TRUE)
species_info <- species_info[-which(species_info$group == "birds" & species_info$n <= 500), ]
species_info <- species_info[rev(order(species_info$n)), ]

# species_info[species_info$group == "birds", ]
#species_info[grepl("villosus", species_info$species), ]

#atlas |>
#  filter(valid_scientific_name %in% c("Dryobates villosus", "Leuconotopicus villosus")) |>
#  count(valid_scientific_name) |>
#  collect()

#atlas |>
#  filter(grepl("Dryobates|Leuconotopicus", valid_scientific_name)) |>
#  count(valid_scientific_name) |>
#  collect()




species_vars <- list(
`Pseudacris triseriata` = c("wetland", "marais", "marecage", "geomflat"), 
`Hemidactylium scutatum` = c("tourbiere", "marais", "organique"), 
`Gyrinophilus porphyriticus` = c("elevation", "ruggedness", "forest", "geomflat", "twi"), 
`Desmognathus ochrophaeus` = c("elevation", "ruggedness", "forest", "geomflat", "twi"), 
`Emydoidea blandingii` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde"),
`Glyptemys insculpta` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde", "sand"),
`Nerodia sipedon` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde"),
`Lampropeltis triangulum` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde"), 
`Aquila chrysaetos` = c("elevation", "ruggedness"), 
`Catharus bicknelli` = c("elevation", "ruggedness"), 
`Setophaga cerulea` = c("forest", "ph", "silt", "nitrogen"), 
`Coturnicops noveboracensis` = c("flat", "marais", "wetland", "prairie_humide"), 
`Ixobrychus exilis` = c("geomflat", "wetland", "marais", "eau_peu_profonde"), 
`Glaucomys volans` = c("forest")
)

species_info$vars <- NA
ma <- match(names(species_vars), species_info$species)
species_info$vars[ma] <- species_vars

species_info$vars <- lapply(species_info$vars, function(i){
  add <- c("urban", "cropland")
  if(!all(is.na(i))){
    c(i, add)
  } else {
    add
  }
})



set.seed(1234)
#species <- c("Pseudacris triseriata", "Hemidactylium scutatum", "Gyrinophilus porphyriticus", "Desmognathus ochrophaeus", "Emydoidea blandingii", "Glyptemys insculpta", "Nerodia sipedon", "Lampropeltis triangulum", "Aquila chrysaetos", "Catharus bicknelli", "Setophaga cerulea", "Coturnicops noveboracensis", "Ixobrychus exilis", "Glaucomys volans")
#species <- sample(species_info$species[species_info$group %in% "birds"], 10)
#species <- sample(species_info$species[species_info$group %in% "trees"], 2)
#species <- c("Catharus guttatus", "Ammospiza leconteii")
#species <- c("Poecile atricapillus", "Dryobates pubescens")
#species <- c("Turdus migratorius", "Poecile atricapillus")
species <- c("Ammospiza nelsoni", "Ammospiza leconteii")
print(species)
species_info <- species_info[species_info$species %in% species, ]


#breeding_periods <- list(
#    `Aquila chrysaetos` = c("06-07", "08-23"), 
#    `Catharus bicknelli` = c("06-07", "07-12"), 
#    `Setophaga cerulea` = c("05-24", "07-12"), 
#    `Coturnicops noveboracensis` = c("06-07", "08-24"), 
#    `Ixobrychus exilis` = c("06-07", "07-19")
#)

bb <- species_info[species_info$group == "birds", ]
breeding_periods <- lapply(1:nrow(bb), function(i){
  c(bb$start[i], bb$end[i])
})
names(breeding_periods) <- bb$species

species_target_groups <- as.list(species_info$group)
names(species_target_groups) <- species_info$species

species_vars<- species_info$vars
names(species_vars) <- species_info$species

run_model <- 1

