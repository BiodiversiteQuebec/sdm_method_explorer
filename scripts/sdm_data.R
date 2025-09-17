

abortif0 <- function(){
  if(nrow(obs) == 0){
    stop(sprintf("0 obs for %s", sp))
    quit("no")
  }
}

### Observations #####################################################

obs_atlas <- obs_atlas |>
  filter(observation_value != "0") |>
  mutate(date = as.character(as.Date(paste(year_obs, month_obs, day_obs, sep="-"), format = "%Y-%m-%d"))) |>
  mutate(source = "atlas") |>
  mutate(coordinate_uncertainty = as.numeric(coordinate_uncertainty)) |>
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg)

obs_gbif <- obs_gbif |>
  filter(!is.na(decimallatitude) & !is.na(decimallatitude)) |>
  mutate(date = as.character(as.Date(eventdate, format = "%Y-%m-%d"))) |>
  mutate(coordinate_uncertainty = as.numeric(coordinateuncertaintyinmeters)) |>
  #filter(coordinate_uncertainty <= 50000 | is.na(coordinate_uncertainty)) |>
  mutate(recordedby = sapply(recordedby, paste, collapse = "; ")) |>
  mutate(source = "gbif") |>
  mutate(dataset_name = institutioncode) |>
  as.data.frame() |>
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) |>
  st_transform(epsg)# |>

#x$recordedby <- sapply(x$recordedby, paste, collapse = "; ")
  
obs_gbif <- obs_gbif[!lengths(st_intersects(obs_gbif, qc)), ]


### Background #######################################################

background_atlas <- background_atlas |> 
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg) |>
  mutate(species = valid_scientific_name) |>
  mutate(source = "atlas") |>
  mutate(coordinate_uncertainty = as.numeric(coordinate_uncertainty)) |>
  mutate(date = paste(year_obs, formatC(month_obs, width = 2, flag = 0), formatC(day_obs, width = 2, flag = 0), sep = "-"))

background_gbif <- background_gbif |> 
  filter(!is.na(decimallongitude) &  !is.na(decimallatitude)) |>
  mutate(date = as.character(as.Date(eventdate, format = "%Y-%m-%d"))) |>
  mutate(coordinate_uncertainty = as.numeric(coordinateuncertaintyinmeters)) |>
  mutate(source = "gbif") |>
  mutate(dataset_name = institutioncode) |>
  as.data.frame() |>
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) |>
  st_transform(epsg)

background_gbif <- background_gbif[!lengths(st_intersects(background_gbif, qc)), ]



if(species_target_groups[[sp]] == "birds"){
  n <- intersect(intersect(names(background_atlas), names(background_gbif)), names(background_ebird))
  background <- rbind(background_atlas[, n], background_gbif[, n], background_ebird[, n])
  n <- intersect(intersect(names(obs_atlas), names(obs_gbif)), names(obs_ebird))
  obs <- rbind(obs_atlas[, n], obs_gbif[, n], obs_ebird[, n])
} else {
  n <- intersect(names(background_atlas), names(background_gbif))
  background <- rbind(background_atlas[, n], background_gbif[, n])
  n <- intersect(names(obs_atlas), names(obs_gbif))
  obs <- rbind(obs_atlas[, n], obs_gbif[, n])
}

### remove what is obviously out
obs <- obs[region, ]
background <- background[region, ]

#nab <- st_buffer(na, 100000)
#obs <- obs[nab, ]
#background <- background[nab, ]

obs <- obs[which(obs$coordinate_uncertainty <= th | is.na(obs$coordinate_uncertainty)), ]
background <- background[which(background$coordinate_uncertainty <= th | is.na(background$coordinate_uncertainty)), ]

if(species_target_groups[[sp]] == "birds"){
  obs <- obs[obs$source %in% "ebird", ]
  background <- background[background$source %in% "ebird", ]
}

abortif0()


### Subsample observations and background

bg <- background[sample(1:nrow(background), min(c(nrow(background), 100000000))), ]
obs <- obs[sample(1:nrow(obs), min(c(nrow(obs), 100000000))), ]

#nobs <- nrow(obs)

nbackground <- round((nrow(obs) * background_prop) / (1 - background_prop), 0)
if(background_cap){
  if(nbackground < background_min){
    nbackground <- background_min
  }
  if(nbackground > background_max){
    nbackground <- background_max
  }
}
  
bg <- bg[sample(1:nrow(bg), nbackground), ]


mult <- 50 # multiply by this value to get more background points from which to resample to get the desired value (in cases of NAs)


if(add_effort_buffer){
  b <- st_buffer(concaveman(obs), effort_buffer_radius) |> st_union() # concavemanning before the buffer makes it much faster
  diff <- st_difference(region, b)
  if(nrow(diff)){
    add <- st_sample(diff, effort_buffer_n) |> st_as_sf()
    st_geometry(add) <- "geometry"
    bg <- rbind(bg[, "geometry"], add)
  }
}


presence <- c(rep(1, nrow(obs)), rep(0, nrow(bg)))
n <- intersect(names(obs), names(bg))
bg <- rbind(obs[, n], bg[, n])

d <- cbind(presence, bg)
e <- terra::extract(unwrap(predictors), d, ID = FALSE)
nas <- !apply(e, 1, function(i){any(is.na(i))})
d <- cbind(d, e)
d <- d[nas, ]
w <- which(d$presence == 0)
s <- sample(w, min(c(length(w), nbackground * mult))) 
k <- which(d$presence == 1)
d <- d[c(k, s), ]
dat <- st_drop_geometry(d)

vars <- adjust_vars(vars_pool, params)
dat <- dat[, c("presence", vars)]



####################################################################

####################################################################
### The following may be useful but is not currently used ##########
####################################################################

####################################################################

if(FALSE){


cols <- adjustcolor(c("forestgreen", "gold2", "tomato2"), 0.85)
ring <- adjustcolor("black", 0.5)

if(sp %in% aires$species){
  ran <- aires[aires$species == sp, ]# |>
    #st_transform(epsg)
} else {
  ran <- NULL
}

add_range <- function(){
  if(!is.null(ran)){
    plot(st_geometry(ran), border = NA, col = adjustcolor("black", 0.10), add = TRUE)
  }
}

png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_na.png")), width = 8, height = 8, units = "in", res = 300)
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(st_geometry(na))
plot(st_geometry(na), col = "grey90", border = "white", lwd = 1, add = TRUE)
text(st_coordinates(st_centroid(st_buffer(na, -50000))), labels = na$NAME_1, col = "white", lwd = 0.25, cex = 0.75)
plot(st_geometry(lakes), col = "white", border = "grey80", add = TRUE, lwd = 0.5)
obsna <- st_geometry(obs[is.na(obs$coordinate_uncertainty), ])
obsoverth <- st_geometry(obs[which(obs$coordinate_uncertainty >= th), ])
obsunderth <- st_geometry(obs[which(obs$coordinate_uncertainty < th), ])
points(obsunderth, pch = 21, lwd = 0.25, col = ring, bg = cols[1])
points(obsna, pch = 21, lwd = 0.25, col = ring,, bg = cols[2])
points(obsoverth, pch = 21, lwd = 0.25, col = ring, bg = cols[3])
legend("bottomright", pch = 21, pt.lwd = 0.25, pt.bg = cols[c(1, 3, 2)], col = ring, legend = c(paste("<", th, "/ n =", length(obsunderth)), paste("\u2265", th, "/ n =", length(obsoverth)), paste("NA", "/ n =", length(obsna))), cex = 1.25, bty = "n", title = "Précision (en m)")
#mtext(side = 3, line = -2.5, text = sp, font = 2, cex = 2, adj = 0.02)
dev.off()

obs_qc <- obs[qc, ]

counts <- obs_qc |> 
  st_drop_geometry() |>
  count(dataset_name) |>
  arrange(-n)

obs_qc$dataset_name <- factor(obs_qc$dataset_name, levels = counts$dataset_name)

png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_quebec.png")), width = 8, height = 8, units = "in", res = 300)
par(mar = c(0.5, 0.5, 0.5, 0.5))
if(nrow(obs_qc) == 0){
  plot(st_geometry(st_crop(na, qc)))
} else {
  plot(st_geometry(st_crop(na, obs_qc)))
}
plot(st_geometry(na), col = "grey90", border = "white", lwd = 1, add = TRUE)
add_range()
text(st_coordinates(st_centroid(st_buffer(na, -50000))), labels = na$NAME_1, col = "white", lwd = 0.25, cex = 0.75)
plot(st_geometry(lakes), col = "white", border = "grey80", add = TRUE, lwd = 0.5)
plot(st_geometry(obs_qc), col = obs_qc$dataset_name, add = TRUE, pch = 16)
legend("topright", inset = c(0.025, 0.025), pch = 16, col = 1:nrow(counts), legend = paste0(counts$dataset_name, " (n = ", counts$n, ")"), cex = 0.75, bty = "n", title = "Dataset")
#mtext(side = 3, line = -2.5, text = sp, font = 2, cex = 2, adj = 0.02)
dev.off()


obs_all <- obs

obs <- obs[region, ]
background <- background[region, ]



###



buff <- st_buffer(obs, 250000) |> st_union()
#buff <- concaveman(obs) 
nbuff <- 1000000
x <- st_difference(region, buff) |> st_sample(size = nbuff)# |> st_as_sf()
x <- st_as_sf(cbind(st_drop_geometry(bg[rep(1, nbuff), ]), geometry = x), crs = epsg)
bg <- rbind(bg, x)

obs_small <- obs[qc, ]
obs_small <- obs_small[which(obs_small$coordinate_uncertainty <= th_small | is.na(obs_small$coordinate_uncertainty)), ]
bg_small <- bg[qc, ]
bg_small <- bg_small[which(bg_small$coordinate_uncertainty <= th_small | is.na(bg_small$coordinate_uncertainty)), ]

print(sprintf("Large: %s observations, %s background", nrow(obs), nrow(bg)))
print(sprintf("Small: %s observations, %s background", nrow(obs_small), nrow(bg_small)))

obs <- list(large = obs, small = obs_small)
bg <- list(large = bg, small = bg_small)


png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_na_used.png")), width = 8, height = 8, units = "in", res = 300)
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(st_geometry(na))
plot(st_geometry(na), col = "grey90", border = "white", lwd = 1, add = TRUE)
text(st_coordinates(st_centroid(st_buffer(na, -50000))), labels = na$NAME_1, col = "white", lwd = 0.25, cex = 0.75)
plot(st_geometry(lakes), col = "white", border = "grey80", add = TRUE, lwd = 0.5)
points(obs_all, pch = 21, lwd = 0.25, col = ring, bg = cols[3])
points(obs$large, pch = 21, lwd = 0.25, col = ring, bg = cols[1])
legend("bottomright", pch = 21, pt.lwd = 0.25, pt.bg = cols[c(3, 1)], col = ring, legend = c(paste0("all ", "(n = ", nrow(obs_all), ")"), paste0("used ", "(n = ", nrow(obs$large), ")")), cex = 1.25, bty = "n", title = "Occurrences utilisées ?")
#mtext(side = 3, line = -2.5, text = sp, font = 2, cex = 2, adj = 0.02)
dev.off()


png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_quebec_used.png")), width = 5, height = 5, units = "in", res = 300)
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(st_geometry(st_crop(na, obs_all[qc, ])))
plot(st_geometry(na), col = "grey90", border = "white", lwd = 1, add = TRUE)
add_range()
text(st_coordinates(st_centroid(st_buffer(na, -50000))), labels = na$NAME_1, col = "white", lwd = 0.25, cex = 0.75)
plot(st_geometry(lakes), col = "white", border = "grey80", add = TRUE, lwd = 0.5)
points(obs_all[qc, ], pch = 21, lwd = 0.25, col = ring, bg = cols[3])
points(obs$small, pch = 21, lwd = 0.25, col = ring, bg = cols[1])
legend("topright", pch = 21, pt.lwd = 0.25, pt.bg = cols[c(3, 1)], col = ring, legend = c(paste0("all ", "(n = ", nrow(obs_all[qc, ]), ")"), paste0("used ", "(n = ", nrow(obs$small), ")")), cex = 0.75, bty = "n", title = "Occurrences utilisées ?")
#mtext(side = 3, line = -2.5, text = sp, font = 2, cex = 2, adj = 0.02)
dev.off()


uncertainty_plot <- function(x){
  brks <- 10
  if(all(is.na(x))){
   h <- list(counts = rep(0, brks), breaks = seq(0, 20000, length.out = brks + 1))
  } else {
   h <- hist(x, breaks = brks, plot = FALSE)
  }

  nas <- sum((is.na(x)))
  barplot(c(nas, h$counts), space = 0, col = "forestgreen", border = "white", lwd = 0.1)
  axis(1, at = c(0, seq_along(h$breaks)), labels = c("NA", h$breaks), las = 2, cex.axis = 0.75, col = "grey70", mgp = c(0.5, 0.25, 0), tcl = -0.2)
  grid()
}


png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_uncertainty.png")), width = 8, height = 4, units = "in", res = 300)
par(mfrow = c(1, 2), oma = c(1, 1, 0, 0), mar = c(2.5, 2.5, 0.5, 0.5))
uncertainty_plot(obs$large$coordinate_uncertainty)
mtext("Amérique du Nord", side = 3, line = -2)
uncertainty_plot(obs$small$coordinate_uncertainty)
mtext("Québec", side = 3, line = -2)
mtext("Incertitude des coordonnées (m)", outer = TRUE, side = 1, font = 2)
mtext("Nb d'observations", outer = TRUE, side = 2, font = 2)
dev.off()




obs_file <- file.path("results/rasters", paste0(gsub(" ", "_", sp), "_observations.gpkg"))

#st_write(obs_all, obs_file, layer = "NA", delete_dsn = TRUE)

#st_write(obs_all[qc, ], obs_file, layer = "QC", append = TRUE)

#st_write(obs$large, obs_file, layer = "NAused", append = TRUE)

#st_write(obs$small, obs_file, layer = "QCused", append = TRUE)

}




