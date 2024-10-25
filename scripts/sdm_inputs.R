
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
#sp<-species
sp<-c("Bonasa umbellus")


rerun<-TRUE

years <- list( # year wanted or a vector of years, has to be a range for gbif data
  2017:2018,
  2019:2020
)

yearparams <- sapply(years, function(y){
  ifelse(length(y) > 1, paste(min(y), max(y), sep = "-"), y) 
})

target_group <- c("birds")

vars_pool<-c("tmax","prec","trange","elevation","truggedness","deciduous_esa","mixed_esa","conifers_esa","shrubs_esa","crop_esa","grass_esa","builtup_esa","water_esa","sparse_esa","harsh_esa","wettree_esa","wetherbaceous_esa")

algorithms<-c("ewlgcpSDM","randomForest","brt","maxent")[c(1, 2, 3, 4)]
bias<-c("Bias","noBias")[1]
usepredictors<-c("Predictors","noPredictors")[1]
spatial<-c("Spatial","noSpatial")[2]

nbackground <- 10000 # number of background points
mult <- 2                   # multiply by this value to get more background points from which to resample to get the desired value (in cases of NAs


results <- expand.grid(species = sp, 
                       target_group = target_group, 
                       years = yearparams, 
                       algorithm = algorithms, 
                       bias = bias,
                       usepredictors = usepredictors, 
                       spatial = spatial, 
                       stringsAsFactors = FALSE)

results <- results[apply(results[,c("usepredictors","spatial")],1,function(i){!all(c("noPredictors","noSpatial")==i)}),] # remove nopredictors and nospatial



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
