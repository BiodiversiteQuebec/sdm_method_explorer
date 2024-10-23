
library(terra)
library(geodata)
library(sf)
library(rmapshaper)
library(data.table)
library(rnaturalearth)
library(smile)
library(INLA)
library(inlabru)
library(spBayes)
library(spdep)
library(INLAspacetime)

#options(vsc.dev.args = list(width = 1200, height = 800))

logit <- function(p){
  log(p / (1 - p))
}

invlogit <- function(x){
  exp(x) / (1+exp(x))
}

vectors <- function(obs = TRUE, background = FALSE){
  plot(st_geometry(qc), lwd = 0.75, add = TRUE)
  plot(st_geometry(lakes), add = TRUE, col = "white", lwd = 0.5)
  if(background){
    plot(st_geometry(s[s$pres == 0, ]), pch = 16, cex = 0.5, add = TRUE)
  }
  if(obs){
    plot(st_geometry(s[s$pres == 1, ]), pch = 16, cex = 0.5, col ="red", add = TRUE)
  }
}


can <- gadm(country=c("CAN","USA"),path="data")
qc <- can[can$NAME_1%in%c("Québec","Ontario","Vermont","New York","New Hampshire","Maine","New Brunswick","Nova Scotia","Rhode Island","Massachusetts","Connecticut","Newfoundland and Labrador")] |> st_as_sf()
qc <- ms_simplify(qc,0.001)
region <- st_union(qc) |> st_as_sf()

temp <- rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/CHELSA/climatologies/CHELSA_bio1_1981-2010_V.2.1.tif")


# lf <- list.files("/data/obs-datasets/ebird/raw",full=TRUE,pattern=".csv")
# cols <- c("scientific_name", "protocol_type", "sampling_event_identifier","observation_date", "duration_minutes", "longitude", "latitude", "all_species_reported")
# l <- lapply(lf,function(i){
#   x <- fread(i, select = cols , dec = ",")
# })
# d <- rbindlist(l)
# d <- d[(all_species_reported),]
# setnames(d, "observation_date","date")
# d[, c("protocol_type", "all_species_reported", "duration_minutes") := NULL]
# d[, date := as.character(date)]
# d[, year := as.integer(substr(date, 1, 4))]
# d[, month_day := substr(date, 6, 10)]

# fwrite(d, "/data/sdm_rbq/ebird_vb.csv")
d <- fread("/home/frousseu/data/ebird_vb.csv")

sp <- "Antigone canadensis"
year <- 2000
year <- (year-5):(year+5)
year <- seq(1970, 2020, by = 1)
#year <- 1996:2019
md <- format(c(as.Date(paste0("2000-",c("05-31","08-16")))+c(-10,10)),"%m-%d")
y <- year

obs <- d[scientific_name == sp & year %in% y & month_day >= md[1] & month_day <= md[2], ]

s <- d[year %in% y & month_day >= md[1] & month_day <= md[2], ]
s[, c("scientific_name") := NULL]
s <- unique(s, by = "sampling_event_identifier")
s[, pres := as.integer(sampling_event_identifier %in% obs$sampling_event_identifier)]
s[, occurrence := pres]

s <- st_as_sf(as.data.frame(s), coords = c("longitude", "latitude"), crs = 4326)
obs <- st_as_sf(as.data.frame(obs), coords = c("longitude", "latitude"), crs = 4326)

obs <- st_transform(obs, 6623)
s <- st_transform(s, 6623)
qc <- st_transform(qc, 6623)
region <- st_transform(region, 6623)
#region <- obs[region, ] |> st_union() |> st_convex_hull() |> st_as_sf() # |> st# |> st_geometry() |> plot(add = TRUE)

s$longitude <- st_coordinates(s)[,1]
s$latitude <- st_coordinates(s)[,2]

lakes <-
  ne_download(
    scale = "medium",
    type = "lakes",
    destdir = getwd(),
    category = "physical",
    returnclass = "sf"
  ) |> st_transform(6623)
lakes <- st_filter(lakes, region)

# plot(st_geometry(qc))
# plot(st_geometry(s[s$pres == 0, ]), pch = 16, add = TRUE)
# plot(st_geometry(s[s$pres == 1, ]), pch = 16, col = "red", add=TRUE)


########################################################
### On aggregated data #################################
########################################################

g <- rast(ext = st_bbox(region), res = c(50, 50) * 1000, crs = crs(region))

l <- lapply(year, function(i){

  counts <- rasterize(vect(s[s$year == i, ]), g, fun = "count")
  pres <- rasterize(vect(s[s$year == i, ]), g, field = "pres", fun = sum)
  scounts <- as.polygons(counts, dissolve = FALSE) |> st_as_sf()
  
  vals <- values(project(temp, g))
  
  s2 <- crds(g) |>
    as.data.frame() |>
    st_as_sf(coords = c("x", "y"), crs = st_crs(region))
  s2$pres <- values(pres)[,1]  
  s2$counts <- values(counts)[,1]      
  s2$longitude <- st_coordinates(s2)[,1]/1000/1000
  s2$latitude <- st_coordinates(s2)[,2]/1000/1000
  s2$longitude2 <- s2$longitude^2
  s2$latitude2 <- s2$latitude^2
  s2$temp <- scale(vals[,1])
  s2$temp2 <- s2$temp^2
  s2$year <- i
  s2$x <- st_coordinates(s2)[, 1]
  s2$y <- st_coordinates(s2)[, 2]
  s2$raw <- s2$pres/s2$counts
  s2

})
s2 <- do.call("rbind", l)


raw <- lapply(split(s2, s2$year),function(i){
  preds <- rasterize(i, g, field = "raw", fun = mean, na.rm = TRUE)
  mask(preds, vect(qc))
}) |> rast()
plot(aggregate(raw,5, na.rm = TRUE), fun = function(){vectors(obs = FALSE)})

years <- 2000
plot(raw[[as.character(years)]])
plot(st_geometry(s2[which(s2$pres >= 1 & s2$year == years), ]), cex = 0.75, lwd = 0.4, add = TRUE)
plot(st_geometry(qc), border = adjustcolor("black", 0.15), add = TRUE)



edge <- min(abs(c(diff(st_bbox(region)[c(3,1)]),diff(st_bbox(region)[c(4,2)]))))
pedge <- 0.03
edge <- edge * pedge

mesh <- fm_mesh_2d_inla(
  boundary = region, max.edge = c(edge, 3 * edge), # km inside and outside
  cutoff = edge, offset = c(edge, 3 * edge),
  crs = fm_crs(region)
) # cutoff is min edge

tmesh <- inla.mesh.1d(loc = sort(unique(s2$year)))

matern <-
  inla.spde2.pcmatern(mesh,
                      prior.sigma = c(0.2, 0.1),
                      prior.range = c(100000, 0.1)
  )


stmodel <- stModel.define(
  smesh = mesh, ## spatial mesh
  tmesh = tmesh, ## temporal mesh
  model = '220', ## model, see the paper
  control.priors = list(
    prs = c(100000, 0.1), ## P(spatial range < 1) = 0.1
    prt = c(50, 0.1), ## temporal range fixed to 5
    psigma = c(0.2, 0.1) ## P(sigma > 1) = 0.1
  )
)

linpred <- ~ Intercept(1) + field(list(space = cbind(x, y), time = year), model = stmodel)

ctrlf <- list(
  hyper = list(
    prec = list(
      initial = 10, 
      fixed = TRUE)    
  )
)

datalike <- like(
  formula = pres ~ Intercept + field, 
  family = "binomial",
  #control.family = ctrlf, 
  Ntrials = s2$counts,
  data = s2)


result <- 
  bru(
    components = linpred,
    datalike,
    options = list(
      control.inla = list(
        int.strategy = "eb"
      ),
      verbose = TRUE)
  )

pred <- predict(
  result, s2,
  ~ Intercept + 
    #latitude + latitude2 + 
    #temp + temp2 +
    field
)


pred$mean <- invlogit(pred$mean)
r<-lapply(split(pred,pred$year),function(i){
  preds <- rasterize(i["mean"], g, field = "mean", fun = mean)
  mask(preds, vect(qc))
})
r <- rast(r)
plot(r, fun = function() {vectors(obs = FALSE)}, mar = c(0, 0, 0, 1), axes= FALSE, range = range(values(r), na.rm = TRUE))



v <- "range"

years <- 1992:2017#year
paths <- paste0("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/TdeB_benchmark_SDM/oiseaux-nicheurs-qc/",paste0(paste(gsub(" ","_",tolower(sp)), v, years, sep = "_"), ".tif"))
vb <- lapply(paths, rast) |> rast()
names(vb) <- gsub("X","",names(vb))
vb <- trim(vb)
vb <- project(vb, crs(region))
vb <- mask(vb, vect(region))
vb <- vb[[names(vb)%in%year]]

years <- 2000
plot(vb[[as.character(years)]])
plot(st_geometry(s2[which(s2$pres >= 1 & s2$year %in% year), ]), cex = 0.75, lwd = 0.4, add = TRUE)
plot(st_geometry(qc), border = adjustcolor("black", 0.15), add = TRUE)



par(mfrow = n2mfrow(nlyr(vb)))
lapply(names(vb), function(i) {
  plot(vb[[i]], mar = c(0,0,3,3), axes = FALSE)
  plot(st_geometry(s[s$pres >= 1 & s$year %in% i, ]), add = TRUE)
  plot(st_geometry(qc), lwd = 0.2, add = TRUE)
})
par(mfrow=c(1,1))



### samples

samp <- generate(result, s2,
                 ~ Intercept + field,
                 n.samples = 200
) |> invlogit() |> as.data.frame() 

names(samp) <- paste0("samp", 1:ncol(samp))
samp <- cbind(pred, samp)


ss<-"samp25"
sims<-lapply(split(samp,samp$year),function(i){
  samps <- rasterize(i[ss], g, field = ss, fun = mean)
  mask(samps, vect(qc))
})
sims <- rast(sims)
plot(sims, fun = function() {vectors(obs = FALSE)}, mar = c(0, 0, 0, 1), axes= FALSE, range = range(values(sims), na.rm = TRUE))






#years <- 1992:2017#year
#paths <- paste0("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/TdeB_benchmark_SDM/oiseaux-nicheurs-qc/",paste0(paste(gsub(" ","_",tolower("Setophaga petechia")), v, years, sep = "_"), ".tif"))
#vb <- lapply(paths, rast) |> rast()

v <- "range" # range
sps <- list.files("/home/frousseu/data/terra_converted_maps", pattern = paste0("maps_", v,".tif"), recursive = TRUE, full = TRUE) |>
       dirname() |>
       basename()
lsps <- lapply(sps, function(i) {
  print(i)
  paths <- paste0("/home/frousseu/data/terra_converted_maps/", i, "/maps_", v, ".tif")
  vb <- rast(paths)
  names(vb) <- gsub("X","",names(vb))
  vb <- trim(vb)
  #vb <- project(vb, crs(region))
  #vb <- mask(vb, vect(region))
  vals <- values(vb)
  areas <- colSums(vals, na.rm = TRUE)
  areas <- areas/areas[1]
  areas
})
names(lsps) <- sps
#at <- c(1, 2, 5, 10, 30, 100)
#at <- unique(c(rev(1/at), at))
at <- c(0.01, 0.1, 0.5, 1, 2, 5, 10, 30, 100)
plot(0, 0, type = "l", xaxt = "n", yaxt = "n", xlim = range(as.integer(names(lsps[[1]]))), ylim = range(log2(at)))
axis(2, at = log2(at), label = at, las = 2)
atx <- c(1992, 2000, 2010, 2017)
axis(1, at = atx, label = atx)
abline(v = atx, col = adjustcolor("black", 0.2), lty = 3); abline(h = log2(at), col = adjustcolor("black", 0.2), lty = 3)
abline(h = log2(1), lty = 3)
lapply(seq_along(lsps), function(j) {
  i <- lsps[[j]]
  lines(as.integer(names(i)), log2(i), col = adjustcolor("black", 0.15))
  text(tail(as.integer(names(i)), 1), tail(log2(i), 1), label = sps[j], adj = c(-0.1, 0.5), cex = 0.45, xpd = TRUE)
})
sel <- "melospiza_georgiana"
lines(as.integer(names(lsps[[sel]])), log2(lsps[[sel]]), col = adjustcolor("red", 0.75), lwd = 4)


source("https://raw.githubusercontent.com/VincentBellavance/sdm/main/R/find_threshold.R")


yy <- "1970"
sdm <- r[yy]
th <- find_threshold(raster(sdm), s[s$year == yy, ], type = "sensitivity", sensitivity = 0.99)
plot(sdm)
plot(st_geometry(s[s$year == yy, ]), add = TRUE)
sdm[sdm >= th] <- 1
sdm[sdm < th] <- 0
patches(sdm, zeroAsNA = FALSE)

plot(clump(raster(sdm)))



