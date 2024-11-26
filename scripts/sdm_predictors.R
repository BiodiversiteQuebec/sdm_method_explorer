
cat(paste("Running: predictors","/",Sys.time(),"\n"))

library(terra)
library(sf)
library(ewlgcpSDM)

predictors <- rast("data/predictors_300.tif")
predictors <- predictors[[vars_pool]]
predictors <- crop(predictors, vect(st_transform(region, st_crs(predictors))), mask = TRUE, touches = FALSE)
if(crs(predictors) != crs(region)){
    predictors <- project(predictors, crs(region), threads = 4)
}
predictors <- aggregate(predictors, 4, na.rm = TRUE)

x <- init(predictors[[1]], "x")
names(x) <- "x"
y <- init(predictors[[1]], "y")
names(y) <- "y"
xy <- x * y
names(xy) <- "xy"
dummy <- init(predictors[[1]], fun = 1)
names(dummy) <- "dummy"


predictors <- c(predictors, xy, dummy)

predictors <- wrap(predictors)


###########################################
### Build mesh for ewlgcpSDM ##############

if(any(results$algorithm == "ewlgcpSDM")){

    cat(paste("Running: dmesh","/",Sys.time(),"\n"))

    ### Build mesh
    domain <- st_sample(st_buffer(region, 5000), 5000)
    domain <- inla.nonconvex.hull(st_coordinates(domain), convex = -0.015, resolution = 75)

    edge <- min(c(diff(st_bbox(region)[c(1, 3)]) * dmesh_resolution, diff(st_bbox(region)[c(2, 4)]) * dmesh_resolution))
    edge

    mesh <- inla.mesh.2d(loc.domain = NULL,
                        max.edge = c(edge, edge * 3),
                        min.angle = 21,
                        cutoff = edge / 1,
                        offset = c(edge, edge * 3),
                        boundary = domain,#inla.mesh.segment(domainloc),
                        crs = st_crs(region))

    plan(multisession, workers = 15)
    dmesh <- dmesh_mesh(mesh)
    plan(sequential)

    ### Compute weights
    dmesh <- dmesh_weights(dmesh, region)

    ### Summarize predictors
    plan(multisession, workers = 6)
    dmesh <- dmesh_predictors(dmesh, predictors, progress = FALSE)
    plan(sequential)

}




### download from stac catalogue

#deciduous<-crop(rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/earthenv/landcover/consensus_full_class_3.tif"),vect(st_transform(region,4326)))
#crops<-crop(rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/earthenv/landcover/consensus_full_class_7.tif"),vect(st_transform(region,4326)))
#maxtemp<-crop(rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/CHELSA/climatologies/CHELSA_bio5_1981-2010_V.2.1.tif"),vect(st_transform(region,4326)))
#bb<-st_bbox(region)
#predictors<-rast(ext(bb),resolution=2000,crs=crs(region))

#deciduous<-project(deciduous,predictors)
#crops<-project(crops,predictors)
#maxtemp<-project(maxtemp,predictors)

#predictors<-c(deciduous,crops,maxtemp)
#names(predictors)<-c("deciduous","crops","maxtemp")