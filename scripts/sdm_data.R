
cat(paste("Running: data","/",Sys.time(),"\n"))

library(sf)
library(terra)
library(rgbif)
library(rmapshaper)

## Get observations

if(data == "gbif"){

  wkt<-st_union(st_buffer(region,20000))
  wkt<-ms_simplify(wkt,0.005)
  wkt<-st_transform(wkt,4326)
  wkt<-st_as_text(wkt)
  
  years<-if(length(year)>1){paste(range(year),collapse=",")}else{year}
  obs<-rgbif::occ_data(scientificName = params$species, hasCoordinate = TRUE,limit=5000,geometry=wkt, year=years)$data
  rem<-which(obs$coordinateUncertaintyInMeters>30000) # removes locations with high uncertainty
  if(any(rem)){
    obs<-obs[-rem,]
  }
  obs<-st_as_sf(as.data.frame(obs),coords=c("decimalLongitude","decimalLatitude"),crs=4326)
  obs<-st_transform(obs,st_crs(region))
  
  plotQC()
  plot(st_geometry(obs),pch=21,bg=adjustcolor("forestgreen",0.5),color=adjustcolor("forestgreen",0.9),lwd=1,add=TRUE)

  ### remove observer/spatial duplicates
  ### keeps a single obs per observer/cell
  bb<-st_bbox(region)
  fg<-rast(ext(bb),resolution=5000)
  values(fg)<-1:ncell(fg)
  e<-terra::extract(fg,vect(obs))
  dups<-duplicated(data.frame(cell=e[,2],observer=obs$recordedBy))
  table(dups)
  obs<-obs[!dups,]

}


if( data == "ebird") {
# SELECT * FROM table WHERE id IN (SELECT id FROM table ORDER BY RANDOM() LIMIT x) # possibly faster if there is a row id
# https://stackoverflow.com/questions/4114940/select-random-rows-in-sqlite

  spp<-tolower(gsub(" ","_",params$species))
  obs <- st_read(file.path(path_write,"total_occ_pres_only_versionR_UTM.gpkg"),
                  query = paste0("SELECT * FROM total_occ_pres_only_versionR WHERE year_obs IN", paste0("(",paste(year,collapse=","),")")," AND species=\"",tolower(spp),"\""), quiet = T
  )
  obs<-st_transform(obs,st_crs(region))
  obs<-obs[region,]
  st_geometry(obs)<-"geometry"
  obs$date<-ISOdate(obs$year,obs$month,obs$day) |> as.Date() |> as.character()
  table(substr(obs$date,6,10))
  #obs<-obs[sample(1:nrow(obs),2000),]
  

}


if(data == "atlas"){
  
  library(dplyr)
  library(duckdb)
  
  atlas_local <- function(parquet_file,
                          tblname="atlas"){
    requireNamespace("duckdbfs")
    atlas<-duckdbfs::open_dataset(parquet_file, tblname = tblname)
    atlas
  }
  
  atlas <- atlas_local(file.path(path_write,"atlas.parquet"),'atlas')
  
  x <- atlas |>
    filter(valid_scientific_name == 'Dendroica cerulea') |>
    #group_by()
    collect() |>
    #sample_n(20000) |>
    as.data.table()
  
  x[,date:=as.character(as.Date(paste(year_obs,month_obs,day_obs,sep="-"),format="%Y-%m-%d"))]
  x<-x[!is.na(date),]
  x[,jul:=as.integer(format(as.Date(date),"%j"))]
  x[,year:=as.integer(substr(date,1,4))]
  x[,species:=valid_scientific_name]
  
  x<-st_as_sf(x,coords=c("longitude","latitude"),crs=4326)
  x<-st_transform(x,epsg)
  
  
  buff<-concaveman(x) |> st_buffer(200000)

  plot(crop(predictors[["elevation"]],buff,mask=TRUE),mar=c(0,0,0,0))
  plot(st_geometry(qc),add=TRUE)
  plot(st_geometry(x),add=TRUE)
  
  
}




