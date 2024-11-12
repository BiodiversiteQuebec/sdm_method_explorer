
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

sp<-species[10:length(species)]
#sp<-c("Bonasa umbellus","Falcipennis canadensis","Setophaga americana", "Catharus fuscescens")
#sp<-c("Melospiza melodia")
sp <- species#[1]
#sp<-c("Catharus bicknelli")
#sp <- species#[11]

rerun<-TRUE

years <- list( # year wanted or a vector of years, has to be a range for gbif data
  2006:2010,
  2011:2015,
  2016:2020
)

yearparams <- sapply(years, function(y){
  ifelse(length(y) > 1, paste(min(y), max(y), sep = "-"), y) 
})

target_group <- c("birds")

vars_pool<-c("tmax","prec","trange","elevation","truggedness","deciduous_esa","mixed_esa","conifers_esa","shrubs_esa","crop_esa","grass_esa","builtup_esa","water_esa","sparse_esa","harsh_esa","wettree_esa","wetherbaceous_esa")

algorithms<-c("ewlgcpSDM","randomForest","brt","maxent")[c(1:4)]
bias<-c("Bias","noBias")[1:2]
usepredictors<-c("Predictors","noPredictors")[1:2]
spatial<-c("Spatial","noSpatial")[1:2]

### background parameters
background_prop <- 0.9 # targeted proportion of background points for the model 
background_cap <- TRUE # if TRUE, will cap the nb of background points with the min/max 
#background_n <- 10000 # number of background points
background_min <- 5000 # overall min nb of background points
background_max <- 1000000 # overall max nb of background points


add_effort_buffer <- TRUE # add an effort buffer or not
effort_buffer_radius <- 500000 # in meters
effort_buffer_n <- 5000 # number of observations in the outside buffer


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
if(TRUE){
  set.seed(1234)
  results <- results[sample(1:nrow(results)), ]
}

if(!rerun){
  x <- fromJSON("results.json") |> setDT()
  results <- fsetdiff(setDT(results), x[, names(results), with = FALSE], all = FALSE) |> as.data.frame()
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
