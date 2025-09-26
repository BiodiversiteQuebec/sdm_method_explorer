

library(smoothr)

bbox <- st_bbox(obs)

rangewidth <- sqrt(abs(bbox$ymax - bbox$ymin)*abs(bbox$ymax - bbox$ymin) + abs(bbox$xmax - bbox$xmin)*abs(bbox$xmax - bbox$xmin)) * 0.015


threshold <- 0.98
bufferdist <- rangewidth #20000
rangedist <- rangewidth #50000
e <- extract(preds, obs)
e <- e[rev(order(e[,2])), ]
val <- e[round(threshold * nrow(e)), 2]

ran <- ifel(preds > val, 1, 0)

pol <- ifel(ran == 1, 1, NA) |>
  as.polygons() |>
  st_as_sf() |>
  st_union() |>
  ms_explode() |>
  st_buffer(bufferdist) |>
  st_union()

pol <- ms_explode(pol) 
polwith <- st_union(pol[as.logical(lengths(st_intersects(pol, obs)))]) 
dis <- as.numeric(st_distance(pol, polwith))
polfinal <- st_as_sf(st_union(pol[dis <= rangedist]))  

polsmooth <- smooth(polfinal, method = "ksmooth", smoothness = 50) |> 
  st_make_valid()
ran <- st_intersection(polsmooth, region)

#png("range.png",width=16,height=10,res=400,units="in")
#plot(crop(preds, st_buffer(pol, 250000)), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, mar = c(0, 0, 0, 4))
#plot(st_geometry(pol), border = NA, col = adjustcolor("black", 0.15), add = TRUE)
#plot(st_geometry(ran), border = adjustcolor("black", 0.5), col = NA, add = TRUE)
#dev.off()

filename <- paste(params$group, tolower(gsub(" ","_",params$species)), params$years, params$period, params$period_dates, params$algorithm, params$usepredictors, params$bias,params$spatial,sep="_") |> paste0(".gpkg")

st_write(ran, file.path("outputs/polygons", filename), layer = params$species, append = FALSE, delete_dsn = TRUE, delete_layer = TRUE)

