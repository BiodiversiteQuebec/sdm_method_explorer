
cat(paste("Running: prelim","/",Sys.time(),"\n"))

epsg <- 6624

# Downloads polygons using package geodata
#can<-gadm("CAN",level=1,path="data") |> st_as_sf()
#usa<-gadm("USA",level=1,path="data") |> st_as_sf()
can <- readRDS("data/gadm/gadm41_CAN_1_pk.rds") |> st_as_sf()
usa <- readRDS("data/gadm/gadm41_USA_1_pk.rds") |> st_as_sf()
na <- rbind(can, usa)
na <- st_transform(na, epsg)

# keep Québec and bordering provinces/states as a buffer
#region <- na[na$NAME_1%in%c("Québec", "New Brunswick", "Maine", "Vermont", "New Hampshire", "New York", "Ontario", "Nova Scotia", "Prince Edward Island", "Massachusetts", "Connecticut", "Rhode Island"),]
region <- na[na$NAME_1 %in% c("Québec"), ]
#region<-na[na$NAME_1%in%c("Québec","Manitoba","Nunavut","New Brunswick","Maine","Vermont","New Hampshire","New York","Ontario","Nova Scotia","Prince Edward Island","Massachusetts","Connecticut","Rhode Island"),]

# split NF into different polygons
labrador <- ms_explode(na[na$NAME_1%in%c("Newfoundland and Labrador"),]) 
labrador <- labrador[which.max(st_area(labrador)), ] # keep Labarador
#region <- rbind(region, labrador)
qc <- na[na$NAME_1 %in% c("Québec"), ]
qc <- ms_simplify(qc, 0.05)

# Add it to the study region
#region <- rbind(region, labrador) 

# Simplify polygons to make things faster
region <- ms_simplify(region, 0.05)
region <- st_union(region) |> st_as_sf()

# lakes
#lakes<-ne_download(scale="medium",type="lakes",destdir="data",category="physical",returnclass="sf") |> st_transform(epsg) |> st_write("data/ne_50m_lakes.gpkg")
lakes <- st_read("data/ne_50m_lakes.gpkg")
lakes <- st_filter(lakes, region)

#if(data=="ebird"){
  #checklists<-fread("data/ebird_sampling_events.csv")
#}



#x<-checklists[sample(.N,min(100000))]
#tb<-st_as_sf(x,coords=c("longitude","latitude"),crs=4326)
#tb<-st_transform(tb,epsg)
#plot(st_geometry(tb),add=TRUE)

#tb<-st_read("C:/Users/rouf1703/Downloads/vbellavance/total_occ_pres_only_versionR_UTM.gpkg",
#               query = "SELECT * FROM total_occ_pres_only_versionR ORDER BY RANDOM() LIMIT 100000", quiet = T)



#poly<-readRDS("C:/Users/rouf1703/Downloads/spacePoly.rds")

#l<-lapply(poly@polygons[[1]]@Polygons,function(i){
 # #i@hole
#  i@coords
#})

#slotNames(poly)


#x<-st_polygon(l)
#x<-st_sfc(x,crs=st_crs(poly@proj4string))
#x<-st_as_sf(x)
#x<-st_transform(x,st_crs(region))

#st_write(x,"C:/Users/rouf1703/Documents/BiodiversitéQuébec/sdm_explorer/spacePoly.gpkg")
#spacePoly<-st_read("C:/Users/rouf1703/Documents/BiodiversitéQuébec/sdm_explorer/spacePoly.gpkg")
#spacePoly<-ms_simplify(x,0.01,keep_shapes=TRUE)

#plot(st_geometry(region))
#plot(st_geometry(x),border="red",lwd=2,add=TRUE)
#plot(st_transform(st_geometry(tb),st_crs(region)),pch=16,cex=1,col="darkgreen",add=TRUE)
#plot(st_geometry(region),add=TRUE,lwd=1)
#plot(st_geometry(x),border="red",lwd=2,add=TRUE)




