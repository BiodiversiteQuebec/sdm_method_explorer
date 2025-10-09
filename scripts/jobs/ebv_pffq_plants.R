

# wget https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Foret/DONNEES_FOR_ECO_PROV/Aires_de_repartition_des_especes_PFFQ/02-Donnees/PROV/AIRES_REPARTITION_PFFQ.gpkg.zip

# ogr2ogr -f GPKG aires_repartition_pffq.gpkg -nln aires_repartition_pffq -nlt CONVERT_TO_LINEAR -nlt PROMOTE_TO_MULTI AIRES_REPARTITION_PFFQ.gpkg aires_repartition_pffq

#

message(paste("Running: inputs","/",Sys.time(),"\n"))

################################################################################
### Paramaters #################################################################
################################################################################

data <- c("gbif", "ebird", "atlas")[1:3]

group <- "plants"
period <- c("breeding", "yearround", "nonbreeding", "prebreeding", "postbreeding")[2]
target_group <- c("plants")

#vars_pool<-c("conifers", "taiga", "deciduous", "mixed", "temperate_shrubland", "temperate_grassland", "polar_shrubland", "polar_grassland", "polar_barren", "wetland", "cropland", "barren", "urban", "water", "snow", "distfsl", "tmean", "prec", "geomflat", "elevation", "distroads", "sand")
#vars_pool <- vars_pool[c(1, 4, 17)]

rerun <- TRUE

years <- list( # year wanted or a vector of years, has to be a range for gbif data
  2000:2025
)

### minimal coordinate precision
th <- 2500 
th_small <- th # for local scale model if any


### Modeling ##################################################################

algorithms<-c("ewlgcpSDM","randomForest","brt","maxent")[c(4)]
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
effort_buffer_radius <- 250000 # in meters
effort_buffer_n <- 400000 # number of observations in the outside buffer

dmesh_resolution <- 0.002

### Variables ###################################################################

species_vars <- list(
`Aquila chrysaetos` = c("elevation", "ruggedness"), 
`Catharus bicknelli` = c("elevation", "ruggedness"), 
`Setophaga cerulea` = c("forest", "ph", "silt", "nitrogen"), 
`Coturnicops noveboracensis` = c("flat", "marais", "wetland", "prairie_humide"), 
`Ixobrychus exilis` = c("geomflat", "wetland", "marais", "eau_peu_profonde")#, 
#`Glaucomys volans` = c("forest")
)

set.seed(1234)
aires <- st_read("data/aires_repartition_pffq.gpkg") |>
  mutate(species = sub("^(([^ ]+ )[^ ]+).*", "\\1", NOM_SCI)) |>
  mutate(species = case_match(species, 
    "Lonicera villosa" ~ "Lonicera caerulea", 
    #"Athyrium filix-femina" ~ "Athyrium angustum",
    #"Osmunda regalis" ~ "Osmunda spectabilis",
    #"Matteuccia struthiopteris" ~ "Matteuccia pensylvanica",
    .default = species))
species <- unique(aires$species)

vascan <- read.csv("data/vascan.txt", sep = "\t")
vascan <- vascan[vascan$Rank == "Species", ]
#plants <- vascan$Scientific.name[!grepl("Tree", vascan$Habit)]
plants <- vascan$Scientific.name

species <- species[!species %in% plants] # Just keep what is not a tree in VASCAN

#species <- sample(species_info$species[species_info$group %in% "birds"], 300)
#species <- sample(species_info$species[species_info$group %in% "trees"], 2)
#species <- c("Aralia hispida", "Solidago rugosa")
#species <- c("Trillium erectum", "Vitis riparia", "Allium tricoccum", "Medeola virginiana", "Uvularia sessilifolia", "Oclemena acuminata", "Polystichum braunii", "Veratrum viride", "Solidago macrophylla", "Ageratina altissima", "Viola labradorica") #
#species <- c("Viola labradorica")
set.seed(1234)
#species <- sample(species, 40)
#species <- NULL # leave NULL if all species should be used
print(species)

############################################################################################
############################################################################################

#atlas |> 
#  filter(kingdom == "Plantae") |> 
#  rename(species = valid_scientific_name) |> 
#  count(dataset_name) |>
#  arrange(-n) |>
#  collect() |>
#  as.data.frame() |>
#  head(10)

#atlas |> 
#  filter(kingdom == "Plantae") |> 
#  filter(dataset_name == "Points d'observation écologique") |> 
#  rename(species = valid_scientific_name) |> 
#  filter(species == "Oclemena acuminata") |>
#  count(dataset_name, year_obs) |>
#  arrange(dataset_name, -n) |>
#  collect() |>
#  as.data.frame()
 
#atlas |> 
#  filter(kingdom == "Plantae") |> 
#  rename(species = valid_scientific_name) |> 
#  count(dataset_name) |>
#  arrange(-n) |>
#  collect() |>
#  #(\(x) x[grepl("Placettes-échantillons", x$dataset_name), ])() |>
#  #group_by(species) |>
#  #summarise(n = sum(n), .groups = "drop") |>
#  #arrange(-n) |>
#  as.data.frame() |>
#  head(100)


atlas <- duckdbfs::open_dataset("data/atlas_2025-09-11.parquet", tblname = "atlas")
gbif <- duckdbfs::open_dataset("data/gbif_2025-03-01.parquet")
ebird <- duckdbfs::open_dataset("data/ebd_relJan-2025.parquet")


species_info <- atlas |>
  filter(kingdom %in% c("Plantae", "Fungi")) |>
  mutate(taxon = tolower(group_en)) |>
  mutate(start = "01-01") |>
  mutate(end = "12-31") |>
  mutate(group = group) |>
  rename(species = valid_scientific_name) |>
  count(group, taxon, species) |>
  arrange(-n) |> 
  collect() |>
  mutate(species = sub("^(([^ ]+ )[^ ]+).*", "\\1", species)) |>
  group_by(across(-n)) |>
  summarise(n = sum(n), .groups = "drop") |>
  arrange(-n) |> 
  as.data.frame()         


species_info <- species_info[species_info$species %in% species, ]

species_info <- species_info[species_info$n >= 5, ]
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
results$period_dates <- periodparams#[match(results$species, species)]
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







