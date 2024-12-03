
### This script gathers predictors for modeling the distribution of emv species for the finance indicators


library(terra)
library(ranger)
#library(mapview)
library(rmapshaper)
#library(knitr)
#library(gdalcubes)
library(rstac)
library(stars)
library(FNN)
library(data.table)

terraOptions(memfrac = 0.8)
terraOptions(verbose = TRUE)
tmpFiles(TRUE, TRUE, TRUE, TRUE)

##########################################################
### CEC Landcover ########################################

source("scripts/sdm_utils.R")
source("scripts/sdm_inputs.R")
source("scripts/sdm_prelim.R")


if(TRUE){
  
  #cec <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/io/CEC_land_cover/NA_NALCMS_landcover_2020_30m.tif")
  
  cec <- rast("data/NA_NALCMS_landcover_2020_30m.tif")
  cec <- terra::crop(cec, st_transform(region, st_crs(cec)))
  
  r <- rast(resolution = 300, ext = terra::ext(region), crs = crs(region))
  

  cec_classes <- c("conifers", "taiga", "evergreen", "tropical", "deciduous", "mixed", "tropical_shrubland", "temperate_shrubland", "tropical_grassland", "temperate_grassland", "polar_shrubland", "polar_grassland", "polar_barren", "wetland", "cropland", "barren", "urban", "water", "snow")
  
  chosen <- cec_classes[c(1:2, 5:6, 8, 10:19)]
  #chosen <- cec_classes[c(1, 2, 8, 10:19)]
  
  p <- lapply(chosen, function(i){
    subst(cec, match(i, cec_classes), 1, 0) |>
      project(r)
  })
  p <- rast(p)
  names(p) <- chosen
  
  writeRaster(p, "data/cec300.tif", overwrite = TRUE, filetype = "COG")
  
p <- rast("data/cec300.tif")


##########################################################
### Distance au Fleuve St-Laurent ########################

### tunr CEC into single layer water raster %
system(sprintf(
  'cd %s
    gdal_translate -projwin %s NA_NALCMS_landcover_2020_30m.tif cropped.tif
    #cats=$cats
    gdal_calc.py -A cropped.tif --outfile=cat_cropped.tif --calc="%s" --format=GTiff --overwrite
    rm cropped.tif
    gdal_translate -ot Float32 cat_cropped.tif num_cropped.tif
    rm cat_cropped.tif
    gdalwarp -tr %s --config -wm 8000 -wo 8 -r average -overwrite num_cropped.tif output.tif
    rm num_cropped.tif
    # https://trac.osgeo.org/gdal/wiki/CloudOptimizedGeoTIFF
    # compress to COG
    gdal_translate output.tif %s.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW
    rm output.tif
   ', 
  "data", 
  "1750000 650000 2250000 250000", 
  "(A==18)", 
  "30 30", 
  "fsl"
))

#fsl <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/fsl.tif")
fsl <- rast("data/fsl.tif")
fsl <- aggregate(fsl, 2)
fsl <- ifel(fsl >= 0.75, 1, NA)
plot(fsl)

pols <- as.polygons(fsl, values = FALSE) |> st_as_sf() |> ms_explode()
plot(st_geometry(pols), col = "cyan", lwd = 0.1)
o <- rev(order(st_area(pols)))[1:3000]
sl <- pols[o, ]
sl <- st_buffer(sl, 150) |> st_union() |> st_buffer(-150) |> st_as_sf() |> ms_explode()
sl <- sl[rev(order(st_area(sl)))[1], ]

p2 <- aggregate(p, 5)
coo <- xyFromCell(p2, 1:ncell(p2)) |>
  as.data.frame() |>
  st_as_sf(coords = c("x", "y"), crs = st_crs(p)) |>
  st_transform(st_crs(sl))

dis <- st_distance(coo, ms_simplify(sl, 0.01)) |> as.matrix()
coo$distfl <- dis[, 1]
coo <- st_transform(coo, st_crs(p))

distfl <- setValues(p2[[1]], coo$distfl)
dfl <- project(distfl, p[[1]])
th <- 10
dfl[dfl < th] <- th
dfl <- subst(dfl, NA, th)
names(dfl) <- "distfsl"

#plot(crop(log(dfl), st_transform(st_buffer(sl, 20), crs = st_crs(p))))
#plot(st_geometry(st_transform(sl, crs = st_crs(p2))), add = TRUE)
#test <- c(p, log(dfl))
#plot(mask(test, st_transform(region, st_crs(p)))[[3]])

p <- c(p, log(dfl))


##########################################################
### WorldClim ############################################

wc <- worldclim_country("CAN", path = "data", var = "bio")
wc <- wc[[c(1, 4, 5, 7, 10, 12)]]
wc <- project(wc, p[[1]])
names(wc) <- c("tmean", "tseason", "tmax", "trange", "twarm", "prec")
wc <- wc[[c("tmean", "tmax", "prec")]]
p <- c(p, wc)



##########################################################
### Topography ###########################################

flat <- rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/earthenv/topography_derived/geomflat_1KMperc_GMTEDmd.tif")
flat <- terra::crop(flat, st_transform(region, st_crs(flat)))

flat <- project(flat, p[[1]])
names(flat) <- "geomflat"

p <- c(p, flat)

##########################################################
### Elevation ############################################

el <- elevation_30s("CAN", "data", mask=FALSE)
el <- terra::crop(el, st_transform(region, st_crs(el)))
el <- project(el, p[[1]])
names(el) <- "elevation"

p <- c(p, el)

##########################################################
### Distance to road #####################################

#roads <- rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/distance_to_roads/GRIP_Roads_Dist_300.tif")

io <- stac("https://io.biodiversite-quebec.ca/stac/")

url <- io |>
  stac_search(collections = "distance_to_roads") |>
  post_request() |> 
  items_fetch() |>
  _$features[[1]]$assets$data$href

roads <-read_stars(paste0('/vsicurl/', url), proxy = TRUE) |>
           rast() |>
           project(p[[1]])

names(roads) <- "distroads"

#roads <- terra::crop(roads, st_transform(region, st_crs(roads)))

mind <- values(roads)[,1]       
mind[mind == 0] <- NA
mind <- min(mind, na.rm = TRUE)

p <- c(p, log(roads+mind)) # add minimum non-zero value to avoid log(0)




##########################################################
### Soils ################################################

#soil <- rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/soilgrids/silt_0-5cm.tif")

ids <- io |>
  stac_search(collections = "soilgrids") |>
  post_request() |> 
  items_fetch() |>
  _$features |>
  sapply(X = _, function(i){i$id})

url <- io |>
  stac_search(collections = "soilgrids") |>
  post_request() |> 
  items_fetch() |>
  _$features[[which(ids == "sand_0-5cm")]]$assets[[1]]$href

soil <-read_stars(paste0('/vsicurl/', url), proxy = TRUE) |>
  rast() |>
  project(p[[1]])

names(soil) <- "sand"

p <- c(p, soil)

##########################################################
### lat/lon ##############################################

y <- init(p[[1]], "y")/100000
names(y) <- "y"
x <- init(p[[1]], "x")/100000
names(x) <- "x"
p <- c(p, x, y)



##########################################################
### Scale, fill, mask and write ##########################

### Nearest neighbour filling (memory limited...)
xy <- xyFromCell(p, 1:ncell(p))
xy <- cbind(xy, values(p))
xynotna <- xy |> as.data.table() |> na.omit() |> as.matrix()
nn <- knnx.index(xynotna[, 1:2], xy[, 1:2], k = 1)
rm(xy);gc();gc()
p2 <- setValues(p, xynotna[nn, -(1:2)])
rm(xynotna, nn)
gc();gc()

p <- mask(p2, st_transform(region, st_crs(p)))

writeRaster(p, "data/predictors_300.tif", overwrite = TRUE)


}

#options(terra.pal=colo.scale(1:200, c("grey95", "grey80","grey60","grey40","grey20", "lavenderblush", "brown", "grey5")))
p <- rast("data/predictors_300.tif")


#r <- ifel(!is.na(r), 1, NA)

# check if distfsl is well computed in relatin to water, nn filling and region. 

#r <- rast(matrix(1:25, ncol = 5))

#p <- crop(p, region, mask = TRUE)

#colSums(is.na(values(p)))

#plot(p$sand)
#plot(st_geometry(region), add = TRUE)
#plot(st_geometry(lakes), col = "cyan", add = TRUE)
