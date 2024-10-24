
cat(paste("Running: predictors","/",Sys.time(),"\n"))

library(terra)
library(sf)

predictors<-rast("data/predictors.tif")
predictors<-predictors[[vars_pool]]
predictors<-crop(predictors,vect(st_transform(region,st_crs(predictors))),mask=TRUE,touches=FALSE)
#vars<-c(c("tmax","prec","trange","elevation","truggedness"),grep("_esa",names(predictors),value=TRUE))
predictors<-project(predictors,crs(region),threads=4)
predictors<-aggregate(predictors,4,na.rm=TRUE)

x<-init(predictors[[1]],"x")
names(x)<-"x"
y<-init(predictors[[1]],"y")
names(y)<-"y"
xy<-x*y
names(xy)<-"xy"
dummy<-init(predictors[[1]],fun=1)
names(dummy)<-"dummy"


predictors<-c(predictors,x,y,xy,dummy)

predictors<-wrap(predictors)





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