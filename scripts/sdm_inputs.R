
### List of inputs for models

message(paste("Running: inputs","/",Sys.time(),"\n"))

library(data.table)

data <- c("gbif","ebird")[2]

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
sp<-c("Bonasa umbellus","Falcipennis canadensis","Setophaga americana", "Catharus fuscescens")
#sp<-c("Melospiza melodia")
sp<-species


rerun<-FALSE

year<-2017:2018 # year wanted or a vector of years, has to be a range for gbif data

target_group <- c("birds")

vars_pool<-c("tmax","prec","trange","elevation","truggedness","deciduous_esa","mixed_esa","conifers_esa","shrubs_esa","crop_esa","grass_esa","builtup_esa","water_esa","sparse_esa","harsh_esa","wettree_esa","wetherbaceous_esa")

algorithms<-c("ewlgcpSDM","randomForest","brt","maxent")[3:4]
bias<-c("Bias","noBias")
usepredictors<-c("Predictors","noPredictors")
spatial<-c("Spatial","noSpatial")

nbackground<-10000 # number of background points
mult<-2                   # multiply by this value to get more background points from which to resample to get the desired value (in cases of NAs

results<-expand.grid(species=sp,algorithm=algorithms,bias=bias,usepredictors=usepredictors,spatial=spatial,stringsAsFactors=FALSE)
results<- results[apply(results[,c("usepredictors","spatial")],1,function(i){!all(c("noPredictors","noSpatial")==i)}),] # remove nopredictors and nospatial

if(!rerun){
  x<-fread("results.csv")
  results<-fsetdiff(setDT(results),x[,names(results),with=FALSE],all=FALSE) |> as.data.frame()
}




#params<-list(algorithm=algorithms[1],bias=bias[1],usepredictors=usepredictors[1],spatial=spatial[2])
