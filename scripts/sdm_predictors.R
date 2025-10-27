
cat(paste("Running: predictors","/",Sys.time(),"\n"))

library(terra)
library(sf)
library(ewlgcpSDM)

#predictors <- rast("data/predictors_300.tif")
#predictors <- rast("data/predictors_500_QC.tif")
#predictors <- rast("data/predictors_100_QC.tif")
predictors <- rast("data/predictors_200_QC.tif")
predictors <- resample(predictors, rast(ext(st_buffer(region, 25000)), resolution = 1000), method = "average")
#writeRaster(p, "data/predictors_200_QC.tif", overwrite = TRUE)
#predictors <- aggregate(predictors, 2, na.rm = TRUE)
#predictors <- predictors[[vars_pool]]
predictors <- crop(predictors, vect(st_transform(st_buffer(region, 25000), st_crs(predictors))), mask = TRUE, touches = FALSE)
if(crs(predictors) != crs(region)){
    predictors <- project(predictors, crs(region), threads = 4)
}
#predictors <- aggregate(predictors, 4, na.rm = TRUE)

x <- init(predictors[[1]], "x")
names(x) <- "x"
y <- init(predictors[[1]], "y")
names(y) <- "y"
xy <- x * y
names(xy) <- "xy"
dummy <- init(predictors[[1]], fun = 1)
names(dummy) <- "dummy"

predictors <- c(predictors, x, y, xy, dummy)


### Habitat associations raster
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

ha <- rast("data/NA_NALCMS_landcover_2020_30m.tif")
levels(ha) <- dat

ha <- crop(ha, st_transform(qc, st_crs(ha)))
ha <- aggregate(ha, 10, fun = "modal") |>
  crop(st_transform(qc, st_crs(ha)), mask = TRUE) |>
  droplevels()






if(FALSE){
    png("predictors.png", width = 12, height = 10, units = "in", res = 300)
    plot(predictors, mar = c(0, 0, 1, 0), axes = FALSE)
    dev.off()
}