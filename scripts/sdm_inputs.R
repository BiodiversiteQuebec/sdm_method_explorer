

### List of inputs for models

message(paste("Running: inputs","/",Sys.time(),"\n"))

library(data.table)

data <- c("gbif", "ebird", "atlas")[2]

epsg <- 32618 #32198 quebec lambert conforme #paste0("+units=km +init=epsg:",32198) #32198 # 32618 the original

species<-c("Bonasa umbellus", "Catharus bicknelli", "Catharus fuscescens", 
           "Catharus guttatus", "Catharus ustulatus", "Falcipennis canadensis", 
           "Junco hyemalis", "Melospiza georgiana", "Melospiza lincolnii", 
           "Melospiza melodia", "Poecile atricapillus", "Poecile hudsonicus", 
           "Setophaga americana", "Setophaga caerulescens", "Setophaga castanea", 
           "Setophaga cerulea", "Setophaga coronata", "Setophaga fusca", 
           "Setophaga magnolia", "Setophaga palmarum", "Setophaga pensylvanica", 
           "Setophaga petechia", "Setophaga pinus", "Setophaga ruticilla", 
           "Setophaga striata", "Setophaga tigrina", "Setophaga virens")

#sp<-species[10:length(species)]
#sp<-c("Bonasa umbellus","Falcipennis canadensis","Setophaga americana", "Catharus fuscescens")
#sp<-c("Melospiza melodia")
sp <- species[26]#[1:20]
#sp<-c("Catharus bicknelli")
#sp <- species#[11]

rerun <- FALSE

years <- list( # year wanted or a vector of years, has to be a range for gbif data
  1950:2024
)

yearparams <- sapply(years, function(y){
  ifelse(length(y) > 1, paste(min(y), max(y), sep = "-"), y) 
})

target_group <- c("birds")

#vars_pool<-c("tmax","prec","trange","elevation","truggedness","deciduous_esa","mixed_esa","conifers_esa","shrubs_esa","crop_esa","grass_esa","builtup_esa","water_esa","sparse_esa","harsh_esa","wettree_esa","wetherbaceous_esa")

vars_pool<-c("conifers", "taiga", "deciduous", "mixed", "temperate_shrubland", 
"temperate_grassland", "polar_shrubland", "polar_grassland", 
"polar_barren", "wetland", "cropland", "barren", "urban", "water", 
"snow", "distfsl", "tmean", "prec", "geomflat", "elevation", 
"distroads", "sand")

algorithms<-c("ewlgcpSDM","randomForest","brt","maxent")[c(1)]
bias<-c("Bias","noBias")[1]
usepredictors<-c("Predictors","noPredictors")[1]
spatial<-c("Spatial","noSpatial")[2]

### background parameters
background_prop <- 0.9 # targeted proportion of background points for the model 
background_cap <- TRUE # if TRUE, will cap the nb of background points with the min/max 
#background_n <- 10000 # number of background points
background_min <- 5000 # overall min nb of background points
background_max <- 1000000 # overall max nb of background points

add_effort_buffer <- TRUE # add an effort buffer or not
effort_buffer_radius <- 500000 # in meters
effort_buffer_n <- 5000 # number of observations in the outside buffer

dmesh_resolution <- 0.005

results <- expand.grid(species = sp, 
                       target_group = target_group, 
                       years = yearparams, 
                       algorithm = algorithms, 
                       bias = bias,
                       usepredictors = usepredictors, 
                       spatial = spatial, 
                       stringsAsFactors = FALSE)

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
      c(sub("^(\\w)", "\\U\\1", paste(i[1:2], collapse = " "), perl = TRUE), i[3:length(i)])
    }) |>
    do.call("rbind", args = _) |>
    as.data.table() |>
    setnames(c("species", "years", "algorithm", "usepredictors", "bias", "spatial"))
    
  results <- fsetdiff(setDT(results[, names(x)]), x, all = FALSE) |> as.data.frame()


}





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
