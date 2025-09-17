
library(blockCV)



blocks <- cv_spatial(
  x = d,
  column = "presence", # the response column (binary or multi-class)
  r = unwrap(predictors)[[1]],
  k = 5, # number of folds
  size = 500000, # size of the blocks in metres
  selection = "random", # random blocks-to-fold
  iteration = 50, # find evenly dispersed folds
  progress = TRUE, # turn off progress bar
  biomod2 = FALSE, # also create folds for biomod2
  raster_colors = terrain.colors(10, rev = TRUE) # options from cv_plot for a better colour contrast
) 

plot(st_geometry(region),col="grey85",lwd=0.1)
plot(st_geometry(qc),lwd=0.2,add=TRUE)
plot(st_geometry(obs),cex=0.1,pch=16,col="forestgreen",add=TRUE)
plot(st_geometry(blocks$blocks),border=adjustcolor("black",0.5),lwd=0.2,add=TRUE)
text(st_coordinates(st_centroid(blocks$blocks)),labels=blocks$blocks$folds,font=2,cex=1.5,col=adjustcolor("black",0.25),add=TRUE)
