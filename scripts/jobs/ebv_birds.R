
message(paste("Running: inputs","/",Sys.time(),"\n"))

library(ebirdst)

################################################################################
### Paramaters #################################################################
################################################################################

data <- c("gbif", "ebird", "atlas")[1:3]

group <- "birds"
period <- c("breeding", "yearround", "nonbreeding", "prebreeding", "postbreeding")[1]
target_group <- c("birds")

vars_pool<-c("conifers", "taiga", "deciduous", "mixed", "temperate_shrubland", 
"temperate_grassland", "polar_shrubland", "polar_grassland", 
"polar_barren", "wetland", "cropland", "barren", "urban", "water", 
"snow", "distfsl", "tmean", "prec", "geomflat", "elevation", 
"distroads", "sand")
vars_pool <- vars_pool[c(1, 4, 17)]

rerun <- TRUE

years <- list( # year wanted or a vector of years, has to be a range for gbif data
  1950:2024
)

### minimal coordinate precision
th <- 20000 
th_small <- th # for local scale model if any


### Modeling ##################################################################

algorithms<-c("ewlgcpSDM","randomForest","brt","maxent")[c(2:4)]
bias<-c("Bias","noBias")[1]
usepredictors<-c("Predictors","noPredictors")[1]
spatial<-c("Spatial","noSpatial")[2]

### background parameters
background_prop <- 0.9 # targeted proportion of background points for the model 
background_cap <- TRUE # if TRUE, will cap the nb of background points with the min/max 
#background_n <- 10000 # number of background points
background_min <- 5000 # overall min nb of background points
background_max <- 10000000 # overall max nb of background points

add_effort_buffer <- TRUE # add an effort buffer or not
effort_buffer_radius <- 500000 # in meters
effort_buffer_n <- 5000 # number of observations in the outside buffer

dmesh_resolution <- 0.01

### Variables ###################################################################

species_vars <- list(
#`Pseudacris triseriata` = c("wetland", "marais", "marecage", "geomflat"), 
#`Hemidactylium scutatum` = c("tourbiere", "marais", "organique"), 
#`Gyrinophilus porphyriticus` = c("elevation", "ruggedness", "forest", "geomflat", "twi"), 
#`Desmognathus ochrophaeus` = c("elevation", "ruggedness", "forest", "geomflat", "twi"), 
#`Emydoidea blandingii` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde"),
#`Glyptemys insculpta` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde", "sand"),
#`Nerodia sipedon` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde"),
#`Lampropeltis triangulum` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde"), 
`Aquila chrysaetos` = c("elevation", "ruggedness"), 
`Catharus bicknelli` = c("elevation", "ruggedness"), 
`Setophaga cerulea` = c("forest", "ph", "silt", "nitrogen"), 
`Coturnicops noveboracensis` = c("flat", "marais", "wetland", "prairie_humide"), 
`Ixobrychus exilis` = c("geomflat", "wetland", "marais", "eau_peu_profonde")#, 
#`Glaucomys volans` = c("forest")
)

set.seed(1234)
#species <- sample(species_info$species[species_info$group %in% "birds"], 300)
#species <- sample(species_info$species[species_info$group %in% "trees"], 2)
#species <- c("Turdus migratorius", "Poecile atricapillus")
species <- c("Catharus bicknelli", "Catharus ustulatus") #
#species <- NULL # leave NULL if all species should be used
print(species)

############################################################################################
############################################################################################

atlas <- duckdbfs::open_dataset("data/atlas_2025-03-17.parquet", tblname = "atlas")
gbif <- duckdbfs::open_dataset("data/gbif_2025-03-01.parquet")
ebird <- duckdbfs::open_dataset("data/ebd_relJan-2025.parquet")

species_info <- atlas |>
  filter(observed_rank %in% c("species", "subspecies", "variety", "form")) |>
  mutate(group = tolower(group_en)) |>
  filter(group %in% c("birds")) |>
  rename(species = valid_scientific_name) |>
  count(group, order, genus, species) |>
  arrange(-n) |> 
  collect() |>
  mutate(species = sub("^(([^ ]+ )[^ ]+).*", "\\1", species)) |>
  group_by(across(-n)) |>
  summarise(n = sum(n), .groups = "drop") |>
  arrange(-n) |> 
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
         dplyr::select(species, start, end) |>
         unique() |>
         as.data.frame()

species_info <- merge(species_info, eb, all.x = TRUE)
species_info <- species_info[species_info$n >= 500, ]
species_info <- species_info[rev(order(species_info$n)), ]


#atlas |>
#  filter(valid_scientific_name %in% c("Dryobates villosus", "Leuconotopicus villosus")) |>
#  count(valid_scientific_name) |>
#  collect()

#atlas |>
#  filter(grepl("Dryobates|Leuconotopicus", valid_scientific_name)) |>
#  count(valid_scientific_name) |>
#  collect()

ma <- match(species_info$species, names(species_vars))
species_info$vars <- species_vars[ma]

## common vars
species_info$vars <- lapply(species_info$vars, function(i){
  add <- vars_pool
  if(!is.null(i)){
    unique(c(i, add))
  } else {
    add
  }
})


species_info <- species_info[species_info$species %in% species, ]

breeding_periods <- lapply(1:nrow(species_info), function(i){
  c(species_info$start[i], species_info$end[i])
})
names(breeding_periods) <- species_info$species

species_target_groups <- as.list(species_info$group)
names(species_target_groups) <- species_info$species

species_vars<- species_info$vars
names(species_vars) <- species_info$species


###################################
###################################
###################################

yearparams <- sapply(years, function(y){
  ifelse(length(y) > 1, paste(min(y), max(y), sep = "-"), y) 
})

if((group == "birds" | any(data %in% "ebird")) & period != "yearround"){
  eb <- get_ebirdst()
  ma <- match(species, eb$scientific_name)
  if(any(is.na(ma))){
    warning(paste(paste(species[is.na(ma)], collapse = " "), "have no matches in ebirdst"))
  }
  periodparams <- eb[[period]][ma]
  periodparams <- ifelse(is.na(periodparams), "0101-1231", periodparams)
} else {
  periodparams <- "0101-1231"
}


results <- expand.grid(job = job,
                       group = group,
                       species = species, 
                       target_group = target_group, 
                       years = yearparams, 
                       algorithm = algorithms, 
                       bias = bias,
                       usepredictors = usepredictors, 
                       spatial = spatial, 
                       stringsAsFactors = FALSE)

results$period <- period
results$period_dates <- periodparams[match(results$species, species)]
o <- c("job", "group", "species", "years", "period", "period_dates", "target_group", "algorithm", "bias", "usepredictors", "spatial")
results <- results[ , o]

#for file in *.tif; do
#  new=$(echo "$file" | sed -E 's/^(([^_]*_){4})/\1yearround_0101-1231_/')
#  mv "$file" "$new"
#done

results <- results[apply(results[,c("usepredictors","spatial")],1,function(i){!all(c("noPredictors","noSpatial")==i)}),] # remove nopredictors and nospatial

### randomize cases
if(FALSE){
  set.seed(1234)
  results <- results[sample(1:nrow(results)), ]
}

if(!rerun){
  #x <- fromJSON("results.json") |> setDT()
  #results <- fsetdiff(setDT(results), x[, names(results), with = FALSE], all = FALSE) |> as.data.frame()

  x <- list.files("outputs", pattern = ".tif") |>
          gsub(".tif", "", x = _) |>
    strsplit("_") |>
    lapply(function(i){
      c(i[1], sub("^(\\w)", "\\U\\1", paste(i[2:3], collapse = " "), perl = TRUE), i[4:length(i)])
    }) |>
    do.call("rbind", args = _) |>
    as.data.table() |>
    setnames(c("group", "species", "years", "period", "period_dates", "algorithm", "usepredictors", "bias", "spatial"))
    
  results <- fsetdiff(setDT(results[, names(x)]), x, all = FALSE) |> as.data.frame()

}

results <- merge(results, species_info)



#library(jsonlite)
#new <- toJSON(results)
#write_json(results, "x")

#dplyr::bind_rows(results, old)
#xx <- rbindlist(list(results, old), fill = TRUE)
#write_json(results, "results.json")

#if(file.exists("results.json")){
#  old <- fromJSON("results.json")
#  res <- rbindlist(list(results, old), fill = TRUE)
#}






