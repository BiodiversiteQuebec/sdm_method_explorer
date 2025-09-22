
message(paste("Running: utils","/",Sys.time(),"\n"))

library(raster)
library(dismo)
library(ebirdst)


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

write_preds <- function(preds){
  filename <- paste(params$group, tolower(gsub(" ","_",params$species)), params$years, params$period, params$period_dates, params$algorithm, params$usepredictors, params$bias,params$spatial,sep="_") |> paste0(".tif")
  filepath <- file.path("outputs/rasters",filename)
  res <- crop(preds,vect(region), mask = TRUE)
  res <- mask(res, vect(lakes), inverse = TRUE)
  metags(res) <- paste(names(params), params, sep = "=")
  writeRaster(res, filepath, overwrite = TRUE)    
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

write_results<-function(){
  #x<-fread("results.csv")
  res <- params
  res$auc <- auc
  res$I <- I
  res$time <- Sys.time()
  setDT(res)
  write_json(res, paste0("json/results", formatC(i, width = 6, flag = 0), ".json"))
  #a <- a[, names(x), with = FALSE]
  #x <- rbind(x, a)
  #x <- x[rev(order(time)),]
  #x <- unique(x, by = c("species", "algorithm", "bias", "usepredictors", "spatial", "reposnapshot"))
  #fwrite(x, "results.csv", append = TRUE)
  #print(x[1, ])
}

add_results <- function(){
  lf <- list.files("json", full = TRUE)
  x <- lapply(lf, fromJSON) |>
    rbindlist(fill = TRUE)
  if(file.exists("results.json")){
    old <- fromJSON("results.json")
    x <- rbindlist(list(x, old), fill = TRUE)
  }
  x <- x[rev(order(time)), ]
  #fwrite(x, "results.csv", append = FALSE)
  write_json(x, "results.json")
  #print(x)
}

clean_results <- function(){
  lf <- list.files("json", full = TRUE)
  if(length(lf)){
    unlink(lf)
  }
}


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


get_ebirdst <- function(){
  eb <- ebirdst_runs |> as.data.table()
  eb[ , breeding := paste(gsub("-", "", substr(breeding_start, 6, 10)), gsub("-", "", substr(breeding_end, 6, 10)), sep = "-")]
  eb[ , postbreeding := paste(gsub("-", "", substr(postbreeding_migration_start, 6, 10)), gsub("-", "", substr(postbreeding_migration_end, 6, 10)), sep = "-")]
  eb[ , prebreeding := paste(gsub("-", "", substr(prebreeding_migration_start, 6, 10)), gsub("-", "", substr(prebreeding_migration_end, 6, 10)), sep = "-")]
  eb[ , nonbreeding := paste(gsub("-", "", substr(nonbreeding_start, 6, 10)), gsub("-", "", substr(nonbreeding_end, 6, 10)), sep = "-")]
  eb[breeding == "NA-NA", breeding := NA]
  eb[prebreeding == "NA-NA", prebreeding := NA]
  eb[postbreeding == "NA-NA", postbreeding := NA]
  eb[nonbreeding == "NA-NA", nonbreeding := NA]
  eb <- eb[ ,.(species_code, scientific_name, common_name, is_resident, breeding, prebreeding, postbreeding, nonbreeding)]
  eb
}


get_period <- function(x){
  #period_dates <- "0201-0916"
  period_dates <- params$period_dates
  d <- strsplit(period_dates, "-")[[1]]
  d <- paste(substr(d, 1, 2), substr(d, 3, 4), sep = "-")
  if(d[1] < d[2]){
    dates <- seq.Date(as.Date(paste0("2000-", d[1])), as.Date(paste0("2000-", d[2])), by = 1) |>
      as.character() |>
      substr(6, 10)
  } else {
    dates <- seq.Date(as.Date(paste0("1999-", d[1])), as.Date(paste0("2000-", d[2])), by = 1) |>
      as.character() |>
      substr(6, 10)
  }
  x[substr(x$date, 6, 10) %in% dates, ]
}
