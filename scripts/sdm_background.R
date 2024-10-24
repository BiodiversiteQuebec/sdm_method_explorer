
cat(paste("Running: background","/",Sys.time(),"\n"))

library(sf)
library(terra)
library(data.table)
library(concaveman)

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
  tb <- tb[sample(1:nrow(tb), min(c(nbackground, nrow(tb)))), ] # keep the specified number of background points

}

#ese<-st_layers("C:/Users/rouf1703/Downloads/ebird_sampling_events.gpkg")
#ese<-st_read("C:/Users/rouf1703/Downloads/ebird_sampling_events.gpkg")


if(data == "ebird"){
  #tb <- st_read("C:/Users/rouf1703/Downloads/vbellavance/ebird_sampling_events.gpkg",
  #              query = paste0("SELECT * ebird_sampling_events WHERE year=", year,paste0(" ORDER BY RANDOM() LIMIT ",nbackground*mult)), quiet = T
  #)
  if(params$bias == "Bias"){
    tb <- st_read("data/ebird_sampling_events.gpkg",
                  query = paste0("SELECT * FROM ebird_sampling_events WHERE year IN", paste0("(",paste(year,collapse=","),")"),paste0(" ORDER BY RANDOM() LIMIT ",nbackground*mult)), quiet = T
    )
    tb<-st_transform(tb,st_crs(region))
  }else{
    tb<-st_sample(region,nbackground*mult) |> st_as_sf()
  }
  tb<-tb[region,]
  tb<-tb[sample(1:nrow(tb),min(c(nbackground,nrow(tb)))),]
  st_geometry(tb)<-"geometry"
  
  ### if using csv and fread
  #x<-checklists[year%in%year & month_day>="06-01" & month_day<="08-15",]
  #x<-checklists[year%in%year,]
  #x<-x[sample(.N,min(nbackground*mult,nrow(x)))]
  #tb<-st_as_sf(x,coords=c("longitude","latitude"),crs=4326)
  #tb<-st_transform(tb,epsg)
  #tb<-tb[region,]
  #tb<-tb[sample(1:nrow(tb),min(c(nbackground,nrow(tb)))),]
  
  
}


if(TRUE){
  b<-st_buffer(concaveman(obs),500000) |> st_union() # concavemanning before the buffer makes it much faster
  add<-st_sample(st_difference(region,b),5000) |> st_as_sf()
  st_geometry(add)<-"geometry"
  tb<-rbind(tb[,"geometry"],add)
}

#plot(st_geometry(region))
#plot(st_geometry(tb),add=TRUE,cex=0.5)
#plot(st_geometry(add),add=TRUE,cex=0.5,col="red")




#plot(st_geometry(tb),col=adjustcolor("black",0.99),pch=16)
#plot(st_geometry(obs),col=adjustcolor("forestgreen",0.5),pch=16,add=TRUE)


bg<-rbind(obs[,"geometry"],tb[,"geometry"])

d<-cbind(presence=c(rep(1,nrow(obs)),rep(0,nrow(tb))),bg)
e<-terra::extract(unwrap(predictors),vect(d),ID=FALSE)
nas<-!apply(e,1,function(i){any(is.na(i))})
d<-cbind(d,e)
d<-d[nas,]
w<-which(d$presence==0)
s<-sample(w,min(c(length(w),nbackground*mult))) 
k<-which(d$presence==1)
d<-d[c(k,s),]
dat<-st_drop_geometry(d)


vars<-adjust_vars(vars_pool,params)
dat<-dat[,c("presence",vars)]
