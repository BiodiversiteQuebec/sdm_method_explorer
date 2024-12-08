
cat(paste("Running: background","/",Sys.time(),"\n"))

library(sf)
library(terra)
library(data.table)
library(concaveman)


nbackground <- round((nobs * background_prop) / (1 - background_prop), 0)
if(background_cap){
  if(nbackground < background_min){
    nbackground <- background_min
  }
  if(nbackground > background_max){
    nbackground <- background_max
  }
}
  
mult <- 50 # multiply by this value to get more background points from which to resample to get the desired value (in cases of NAs)

if(data == "gbif"){

  gbif<-rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/gbif_heatmaps/gbif_plants_density_06-2022.tif")
  gbif<-crop(gbif,vect(st_buffer(st_transform(region,4326),1)))
  gbif<-mask(gbif,vect(st_buffer(st_transform(region,4326),1)))
  gbif<-subst(gbif,NA,0) # replace NAs with 0
  
  ### Simulate effort surface/background obs from gbif density raster
  if( params$bias == "Bias"){
    p<-spatSample(gbif, nbackground * mult, as.points = TRUE, method = "weights")
    tb<-st_as_sf(p) |> st_transform(st_crs(region))
    tb<-tb[, "geometry"]
  }else{
    tb <- st_sample(region, nbackground * mult)  
  }
  tb <- tb[region,] # keep what's in region
  tb <- get_period(tb)
  tb <- tb[sample(1:nrow(tb), min(c(nbackground, nrow(tb)))), ] # keep the specified number of background points

}



if(data == "ebird"){

  y <- as.integer(strsplit(params$years, "-")[[1]])
  if(length(y) > 1){
    y <- paste(min(y):max(y), collapse = ",")
  }
  
  if(params$bias == "Bias"){
    tb <- st_read("data/ebird_sampling_events.gpkg",
                  query = paste0("SELECT * FROM ebird_sampling_events WHERE year IN ", paste0("(", y, ")"),paste0(" ORDER BY RANDOM() LIMIT ",nbackground * mult)), quiet = T
    )
    tb<-st_transform(tb, st_crs(region))
  }else{
    tb<-st_sample(region, nbackground*mult) |> st_as_sf()
  }
  tb <- tb[region, ]
  tb <- get_period(tb)
  tb <- tb[sample(1:nrow(tb), min(c(nbackground, nrow(tb)))), ]
  st_geometry(tb) <- "geometry"
  
  ### if using csv and fread
  #x<-checklists[year%in%year & month_day>="06-01" & month_day<="08-15",]
  #x<-checklists[year%in%year,]
  #x<-x[sample(.N,min(nbackground*mult,nrow(x)))]
  #tb<-st_as_sf(x,coords=c("longitude","latitude"),crs=4326)
  #tb<-st_transform(tb,epsg)
  #tb<-tb[region,]
  #tb<-tb[sample(1:nrow(tb),min(c(nbackground,nrow(tb)))),]
  
  
}





if(add_effort_buffer){
  b <- st_buffer(concaveman(obs), effort_buffer_radius) |> st_union() # concavemanning before the buffer makes it much faster
  diff <- st_difference(region, b)
  if(nrow(diff)){
    add <- st_sample(diff, effort_buffer_n) |> st_as_sf()
    st_geometry(add) <- "geometry"
    tb <- rbind(tb[, "geometry"], add)
  }
}


bg <- rbind(obs[,"geometry"], tb[,"geometry"])

d <- cbind(presence = c(rep(1, nrow(obs)), rep(0, nrow(tb))), bg)
e <- terra::extract(unwrap(predictors), vect(d), ID = FALSE)
nas <- !apply(e, 1, function(i){any(is.na(i))})
d <- cbind(d, e)
d <- d[nas, ]
w <- which(d$presence == 0)
s <- sample(w, min(c(length(w), nbackground * mult))) 
k <- which(d$presence == 1)
d <- d[c(k,s), ]
dat <- st_drop_geometry(d)

vars <- adjust_vars(vars_pool, params)
dat <- dat[, c("presence", vars)]
