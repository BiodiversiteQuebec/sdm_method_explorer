
library(sdmtools)


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
       plot(st_geometry(aires[w, ]), border = adjustcolor("black", 0.25), lwd = 0.75, col = NA, add = TRUE) 
     }
   }   
   plot(st_geometry(ran), col = adjustcolor("black", 0.10), border = NA, add = TRUE)
}


plg <- list(size = c(0.33, 1.25), tic.box.col = "#ddd", tic.lwd = 0.5, tic.col = "#777", tic = "out")
#plg <- list(size = c(0.5, 1.5))#, tic.box.col = "#ddd", tic.lwd = 0.5, tic.col = "#ccc", tic = "out")
#sdm_cols <- terrain.colors(200)
sdm_cols <- coloScale(1:200, c("grey90", "palegreen3", "forestgreen", "darkgreen","black"))[1:170]
range_cols <- adjustcolor("forestgreen", 0.75)

filename <- paste(params$group, tolower(gsub(" ","_",params$species)), params$years, params$period, params$period_dates, params$algorithm, params$usepredictors, params$bias,params$spatial,sep="_") |> paste0(".png")

png(file.path("outputs/graphics", filename), units = "in", height = 6, width = 7.5, res = 300)
plot(preds, axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, mar = c(0, 0, 0, 4))
add_range()
plot(st_geometry(st_intersection(region, na)), lwd = 0.1, border = adjustcolor("black", 0.75), add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
points(st_geometry(obs), bg = adjustcolor("orange", 0.80), col = "black", pch = 21, cex = 0.3, lwd= 0.1)

dev.off()


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