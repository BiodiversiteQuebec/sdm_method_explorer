
message(paste("Running: utils","/",Sys.time(),"\n"))

library(raster)
library(dismo)


### plot preds
plot_preds<-function(occs=TRUE){
  #opar<-par(read.only=TRUE)
  #on.exit(par(opar))
  plot(mask(preds[[1]],lakes,inverse=TRUE),mar=c(0,0,0,0),axes=FALSE)
  if(occs){
    plot(st_geometry(obs),cex=0.5,pch=16,col=adjustcolor("black",0.25),add=TRUE)
  }
  plot(st_geometry(qc),border=adjustcolor("black",0.5),add=TRUE)
}


write_preds<-function(preds){
  filename<-paste(tolower(gsub(" ","_",params$species)),params$algorithm,params$usepredictors,params$bias,params$spatial,sep="_") |> paste0(".tif")
  filepath<-file.path("outputs",filename)
  res<-crop(preds,vect(region),mask=TRUE)
  res<-mask(res,vect(lakes),inverse=TRUE)
  writeRaster(res,filepath,overwrite=TRUE)    
}

plot_vb<-function(occs=TRUE){
  lf<-list.files("C:/Users/rouf1703/Downloads/vbellavance/terra_converted_maps",pattern="maps_pocc.tif",full=TRUE,recursive=TRUE)
  r<-rast(lf[grep(gsub(" ","_",tolower(params$species)),lf)])
  r<-trim(r)
  r<-crop(r,vect(st_transform(qc,st_crs(r))))
  r<-mask(r,vect(st_transform(qc,st_crs(r))))
  plot(sum(r[[(nlyr(r)-4):(nlyr(r))]]))
  if(occs){
    plot(st_transform(st_geometry(obs),st_crs(r)),cex=0.5,pch=16,col=adjustcolor("black",0.25),add=TRUE)
  }
  plot(st_transform(st_geometry(qc),st_crs(r)),border=adjustcolor("black",0.5),add=TRUE)
}
#plot_vb()



### Adjust variables used with params selected
adjust_vars<-function(vars,params){
  if(params$usepredictors=="noPredictors" & params$algorithm %in% c("ewlgcpSDM")){
    vars<-c("dummy")  
  }
  if(params$algorithm %in% c("maxent","brt","randomForest")){
    if(params$spatial=="Spatial" & params$usepredictors=="noPredictors"){
      vars<-c("x","y","xy")
    }
    if(params$spatial=="Spatial" & params$usepredictors=="Predictors"){
      vars<-c(vars,"x","y","xy")
    }
      
  }
  vars
}

add_results<-function(){
  x<-fread("results.csv")
  info<-params
  info$auc<-auc
  info$I<-I
  info$time<-Sys.time()
  a<-as.data.table(info)
  a<-a[,names(x),with=FALSE]
  x<-rbind(x,a)
  x<-x[rev(order(time)),]
  x<-unique(x, by = c("species","algorithm","bias","usepredictors","spatial"))
  fwrite(x,"results.csv",append=TRUE)
  print(x[1,])
}

clean_results<-function(){
  x<-fread("results.csv")
  x<-x[rev(order(time)),]
  x<-unique(x, by = c("species","algorithm","bias","usepredictors","spatial"))
  fwrite(x,"results.csv",append=FALSE)
  #print(x)
}
#clean_results()

#add_results()

niche_overlap<-function(){
  lf<-list.files("/home/frousseu/data/ebird",pattern=gsub(" ","_",params$species),full=TRUE)
  if(length(lf)){
    r<-rast(lf)
    r<-project(r,preds,mask=TRUE)
    r<-mask(r,preds)
    nicheOverlap(raster(r),raster(preds),checkNegatives=FALSE)
  }else{
    NA
  }
}
#niche_overlap()

#x<-fread("results.csv")
#x$I<-NA
#x<-x[,c("species", "algorithm", "bias", "usepredictors", "spatial", 
#     "auc","I", "time"),with=FALSE]

checkpoint<-function(msg="Starting:"){
  message(paste(msg,paste0(params,collapse=" - "),"/",Sys.time(),"\n"))
}

get_repo_snapshot <- function(repo, user, tokenpath, n = 10){
  token <- readLines(tokenpath)
  githubapi <- paste0("https://", user, ":", token, "@api.github.com/repos/", repo, "/commits")
  x <- fromJSON(paste0(githubapi,"?per_page=", n, "&files=false"))
  latest_commit <- x$sha[1]
  repo_snapshot <- file.path("https://github.com", repo, "tree", latest_commit)
  repo_snapshot
}




