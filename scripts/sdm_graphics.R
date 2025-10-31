
library(sdmtools)
library(ggplot2)


plot_background <- function(){
  plot(st_geometry(bregion), border = NA, col = "grey90") 
}

plot_foreground <- function(observations = FALSE, echelle = "large"){
  if(observations){
    points(st_geometry(obs[[echelle]]), bg = adjustcolor("orange", 0.90), col = "black", pch = 21, cex = 0.4, lwd= 0.10)
  }
  plot(st_geometry(na), lwd = 0.1, border = adjustcolor("black", 0.75), add = TRUE)
  plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
}


topng <- function(x){
  if(any(grepl(".tif", x))){
    res <- gsub("/rasters/", "/graphics/", gsub(".tif", ".png", x))
  } else {
    res <- gsub("/rasters/", "/graphics/", gsub(".gpkg", ".png", x))
  }
  spf <- gsub(" ", "_", sp)
  gsub(spf, paste(spf, names(models)[i], sep = "_"), res)
}

add_range <- function(){
   if(exists("aires")){
     w <- which(aires$species == sp)
     if(any(w)){
       plot(st_geometry(aires[w, ]), border = adjustcolor("black", 0.25), lwd = 1.5, col = NA, add = TRUE) 
     }
   }   
   #plot(st_geometry(ran), col = adjustcolor("black", 0.10), border = NA, add = TRUE)
   #plot(st_geometry(b), col = NA, border = adjustcolor("black", 0.30), lty = 3, add = TRUE)
}

xxx <- ext(preds)$xmin + 0.80 * abs((ext(preds)$xmax - ext(preds)$xmin))
yyy <- ext(preds)$ymin + 0.95 * abs((ext(preds)$ymax - ext(preds)$ymin))

plg <- list(x = xxx, y = yyy, size = c(0.33, 1.25), tic.box.col = "#ddd", tic.lwd = 0.5, tic.col = "#777", tic = "out")
#plg <- list(size = c(0.5, 1.5))#, tic.box.col = "#ddd", tic.lwd = 0.5, tic.col = "#ccc", tic = "out")
#sdm_cols <- terrain.colors(200)
sdm_cols <- coloScale(1:200, c("grey90", "palegreen3", "forestgreen", "darkgreen","black"))[1:170]
range_cols <- adjustcolor("forestgreen", 0.35)

basefilename <- paste(params$group, gsub(" ","_",params$species), params$model, params$years, params$period, params$dates, params$algorithm, params$usepredictors, params$bias, params$spatial, sep="_")# |> paste0(".png")

write_results(basefilename, "model", "json")

filename <- write_results(basefilename, "sdm", "png")
png(file.path("outputs/graphics", filename), units = "in", height = 6, width = 5, res = 300)
plot(preds, axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, mar = c(0.5, 0.5, 0.5, 0.5))
plot(st_geometry(st_intersection(region, na)), lwd = 0.1, border = adjustcolor("black", 0.75), add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
#points(st_geometry(obs), bg = adjustcolor("orange", 0.80), col = "black", pch = 21, cex = 0.3, lwd= 0.1)
#add_range()
dev.off()

filename <- write_results(basefilename, "sdmObs", "png")
png(file.path("outputs/graphics", filename), units = "in", height = 6, width = 5, res = 300)
plot(preds, axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, mar = c(0.5, 0.5, 0.5, 0.5))
plot(st_geometry(st_intersection(region, na)), lwd = 0.1, border = adjustcolor("black", 0.75), add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
points(st_geometry(obs), bg = adjustcolor("orange", 0.80), col = "black", pch = 21, cex = 0.3, lwd= 0.1)
#add_range()
dev.off()

filename <- write_results(basefilename, "sdmRange", "png")
png(file.path("outputs/graphics", filename), units = "in", height = 6, width = 5, res = 300)
plot(preds, axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, mar = c(0.5, 0.5, 0.5, 0.5))
plot(st_geometry(st_intersection(region, na)), lwd = 0.1, border = adjustcolor("black", 0.75), add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
#points(st_geometry(obs), bg = adjustcolor("orange", 0.80), col = "black", pch = 21, cex = 0.3, lwd= 0.1)
add_range()
dev.off()

filename <- write_results(basefilename, "sdmObsRange", "png")
png(file.path("outputs/graphics", filename), units = "in", height = 6, width = 5, res = 300)
plot(preds, axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, mar = c(0.5, 0.5, 0.5, 0.5))
plot(st_geometry(st_intersection(region, na)), lwd = 0.1, border = adjustcolor("black", 0.75), add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
points(st_geometry(obs), bg = adjustcolor("orange", 0.80), col = "black", pch = 21, cex = 0.3, lwd= 0.1)
add_range()
dev.off()

filename <- write_results(basefilename, "range", "png")
png(file.path("outputs/graphics", filename), units = "in", height = 6, width = 5, res = 300)
par(mar = c(0, 0, 0, 0))
plot(st_geometry(region), col = sdm_cols[1], border = NA)
plot(ran, col = range_cols, border = NA, add = TRUE)
plot(st_geometry(st_intersection(region, na)), lwd = 0.1, border = adjustcolor("black", 0.75), add = TRUE)
plot(st_geometry(st_intersection(region, lakes)), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
#points(st_geometry(obs), bg = adjustcolor("orange", 0.80), col = "black", pch = 21, cex = 0.3, lwd= 0.1)
#add_range()
dev.off()

filename <- write_results(basefilename, "rangeObs", "png")
png(file.path("outputs/graphics", filename), units = "in", height = 6, width = 5, res = 300)
par(mar = c(0, 0, 0, 0))
plot(st_geometry(region), col = sdm_cols[1], border = NA)
plot(ran, col = range_cols, border = NA, add = TRUE)
plot(st_geometry(st_intersection(region, na)), lwd = 0.1, border = adjustcolor("black", 0.75), add = TRUE)
plot(st_geometry(st_intersection(region, lakes)), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
points(st_geometry(obs), bg = adjustcolor("orange", 0.80), col = "black", pch = 21, cex = 0.3, lwd= 0.1)
#add_range()
dev.off()

filename <- write_results(basefilename, "rangeRange", "png")
png(file.path("outputs/graphics", filename), units = "in", height = 6, width = 5, res = 300)
par(mar = c(0, 0, 0, 0))
plot(st_geometry(region), col = sdm_cols[1], border = NA)
plot(ran, col = range_cols, border = NA, add = TRUE)
plot(st_geometry(st_intersection(region, na)), lwd = 0.1, border = adjustcolor("black", 0.75), add = TRUE)
plot(st_geometry(st_intersection(region, lakes)), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
#points(st_geometry(obs), bg = adjustcolor("orange", 0.80), col = "black", pch = 21, cex = 0.3, lwd= 0.1)
add_range()
dev.off()

filename <- write_results(basefilename, "rangeObsRange", "png")
png(file.path("outputs/graphics", filename), units = "in", height = 6, width = 5, res = 300)
par(mar = c(0, 0, 0, 0))
plot(st_geometry(region), col = sdm_cols[1], border = NA)
plot(ran, col = range_cols, border = NA, add = TRUE)
plot(st_geometry(st_intersection(region, na)), lwd = 0.1, border = adjustcolor("black", 0.75), add = TRUE)
plot(st_geometry(st_intersection(region, lakes)), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
points(st_geometry(obs), bg = adjustcolor("orange", 0.80), col = "black", pch = 21, cex = 0.3, lwd= 0.1)
add_range()
dev.off()

filename <- write_results(basefilename, "habitatAssociation", "png")
h <- habitatAssociation(raster = ha, sdm = preds, n = 10000, col = TRUE)
ggsave(file.path("outputs/graphics", filename), plot = h$plot, width = 5, height = 6, dpi = 300)


#x <- list.files("json", full = TRUE) |>
#  lapply(fromJSON, flatten = TRUE)# |>
  #lapply(as.data.frame) |>
  #do.call("rbind", args = _)

################################################################
########## habitat associations ################################
################################################################

if(FALSE){


im1 <- image_read(file.path("outputs/graphics", basefilename))
im2 <- image_border(image_read("habitat.png"), "white", "100")
im <- c(im1, im2)
image_write(image_append(image_scale(im, "x1500")), "magick.png")



### habitat association

 library(terra)
 library(sf)
 library(rstac)
 library(exactextractr)
 library(rstac)
 library(ggplot2)
 library(geodata)
 library(rmapshaper)

 # Downloads polygons using package geodata
 can <- gadm("CAN", level = 1, path = "data") |> st_as_sf()
 labrador <- ms_explode(can[can$NAME_1 %in% c("Newfoundland and Labrador"), ])
 labrador <- labrador[which.max(st_area(labrador)), ] # keep Labarador
 qc <- can[can$NAME_1 %in% c("Québec"),] |>
   rbind(labrador) |>
   ms_simplify(0.01)



 # https://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/

 category <- c("Temperate or sub-polar needleleaf forest",
              "Sub-polar taiga needleleaf forest",
              "Tropical or sub-tropical broadleaf evergreen forest",
              "Tropical or sub-tropical broadleaf deciduous forest",
              "Temperate or sub-polar broadleaf deciduous forest",
             "Mixed Forest",
              "Tropical or sub-tropical shrubland",
              "Temperate or sub-polar shrubland",
              "Tropical or sub-tropical grassland",
              "Temperate or sub-polar grassland",
             "Sub-polar or polar shrubland-lichen-moss",
              "Sub-polar or polar grassland-lichen-moss",
              "Sub-polar or polar barren-lichen-moss",
              "Wetland",
             "Cropland",
              "Barren lands",
              "Urban",
              "Water",
              "Snow and Ice")

 dat <- data.frame(code = c(0, seq_along(category)), category = c("No data", category))

 cec <- rast("data/NA_NALCMS_landcover_2020_30m.tif")
 levels(cec) <- dat

 cec <- crop(cec, st_transform(qc, st_crs(cec)))
 raster <- aggregate(cec, 10, fun = "modal") |>
  crop(st_transform(qc, st_crs(cec)), mask = TRUE) |>
  droplevels()

 x <- habitatAssociation(raster = raster, sdm = preds, n = 10000, col = TRUE)
 ggsave("habitat.png", plot = x$plot, width = 5, height = 6, dpi = 300)

 io <- stac("https://acer.biodiversite-quebec.ca/stac/")


 coll <- "oiseaux"
 #coll <- "sdm_emv_finance"

 urls <- io |>
   stac_search(collections = coll) |>
  post_request() |>
  items_fetch() |>
  _$features |>
  sapply(function(i){i$assets[[1]]$href})

 sdm <- rast(paste0('/vsicurl/', grep("(?=.*1900-2024)(?=.*setophaga_cer)", urls, perl = TRUE, value = TRUE)))

 x <- habitatAssociation(raster = raster, sdm = sdm, n = 10000, col = TRUE)
 ggsave("habitat.png", plot = x$plot, width = 5, height = 6, dpi = 300)


}











if(FALSE){

  png(topng(file_sdm), units = "in", height = 6, width = 7.5, res = 300)
  #par(mar = c(0, 0, 0, 8))
  #plot_background()
  plot(crop(predictions, bregion), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, mar = c(0, 0, 0, 0))
  #plot(crop(predictions, st_buffer(obs[[echelle]], buffd)), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, mar = c(0, 0, 0, 0))
  #plot(predictions, axes = FALSE, plg = plg, col = sdm_cols)
  plot_foreground(observation = TRUE, echelle = echelle)
  dev.off()


  png(topng(file_range), units = "in", height = 6, width = 6.5, res = 300)
  par(mar = c(0, 0, 0, 0))
  plot_background()
  plot(st_geometry(polran), col = range_cols, border = NA, add = TRUE)
  plot_foreground(observations = TRUE, echelle = echelle)
  legend("topright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = range_cols, bty = "n", xjust = 1, xpd = TRUE)
  dev.off()

}