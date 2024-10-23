library(terra)
library(geodata)
library(sf)
library(rmapshaper)
library(data.table)
library(rnaturalearth)
#library(smile)
library(INLA)
library(inlabru)
library(spdep)
library(INLAspacetime)
library(readxl)
#library(FRutils)
library(concaveman)
library(mapview)

source("https://raw.githubusercontent.com/frousseu/FRutils/master/R/colo.scale.R")


strate_veg <- read_excel("/home/frousseu/Downloads/DICTIONNAIRE_POE.xlsx", sheet = "STRATE_VEG", skip = 2) |> as.data.frame()
classe_de_densite <- read_excel("/home/frousseu/Downloads/DICTIONNAIRE_POE.xlsx", sheet = "CLASSE_DE_DENSITE", skip = 2) |> as.data.frame()
poesp <- read_excel("/home/frousseu/Downloads/DICTIONNAIRE_POE.xlsx", sheet = "ESPECE_POE", skip = 2) |> as.data.frame()

names(poesp) <- c("code", "common", "botanic")
poesp$botanic <- gsub("\\s\\s+/g", " ", poesp$botanic) |> trimws()

add <- do.call("rbind", lapply(strsplit(poesp$botanic, " "), function(i){
  x <- if(length(i) < 3){
         c(i[1], "genus")
       } else {
         if(any(grep("\\.", i[1])) || (substr(i[2], 1, 1) %in% c(LETTERS, "("))){
           c(i[1], "genus")
         } else {
           c(paste(i[1:2], collapse = " "), "species")
         }
       }
  x
})) |> as.data.frame() |> setNames(c("species", "level"))

poesp <- cbind(poesp, add)


path <- "/home/frousseu/Downloads/POE_PROV.gpkg"
layers <- st_layers(path)

#l <- lapply(layers$name, function(i){
#  st_read(path, layer = i)
#})

lapply(layers$name, function(i){
  head(st_read(path, layer = i))
})

es <- st_read(path, layer = "espece_strate") |> as.data.frame()

classe_de_densite <- classe_de_densite[classe_de_densite$Code %in% es$cl_rec, ]
classe_de_densite$cover <- c(0.005, 0.9, 0.7, 0.5, 0.325, 0.15, 0.03, NA)

dens <- table(es$cl_rec, useNA = "always")
dens <- dens[c(2:7, 1)]

es <- merge(es, classe_de_densite[ , c("Code", "cover")], by.x = "cl_rec", by.y = "Code")
es <- merge(es, poesp[ , c("code", "species", "level")], by.x = "espece", by.y = "code")

#agg <- aggregate(cover ~ espece + id_poe, data = x, "sum")

p <- st_read(path, layer = "placette")
mlat <- mean(p$latitude)
sdlat <- sd(p$latitude)
p$latitude <- (p$latitude - mlat) / sdlat

### Predictors

lf <- list.files("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020")
classes <- read.csv("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/utilisation_territoire_2020.csv")
r <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/utilisation_territoire_2020.tif")
ut <- cats(r)[[1]]
e <- ext(c(-2.75e+05,-2.55e+05, 1.55e+05, 1.7e+05))
#e <- ext(c(-4.15e+05,-4.12e+05, 6.13e+05, 6.16e+05))
#r <- crop(r, st_transform(p, st_crs(r)))
r <- crop(r, e)
plot(r)


gdal_calc.py -A utilisation_territoire_2020.tif --outfile=result.tif --calc="A==26" --NoDataValue=0


bindOR <- function(x) {
  paste(paste0("(A==", x, ")"), collapse = " | ")  
}
bindOR(c(21,23,45))




# Define the coordinates of the subset
MINX=-275000
MINY=155000
MAXX=-255000
MAXY=170000

# Extract the subset
gdal_translate -projwin $MINX $MAXY $MAXX $MINY utilisation_territoire_2020.tif subset.tif

gdal_translate -projwin -275000 170000 -255000 155000 utilisation_territoire_2020.tif subset.tif
#gdal_translate -projwin -850000 1000000 800000 110000 utilisation_territoire_2020.tif subset.tif

# Run gdal_calc.py on the subset
'gdal_calc.py -A subset.tif --outfile=cat_subset.tif --calc="(A==20) | (A==21) | (A==22) | (A==23) | (A==25) | (A==26)" --NoDataValue=0 --type=Byte --format=GTiff --overwrite'

'gdal_calc.py -A subset.tif --outfile=cat_subset.tif --calc="(A==20) | (A==21) | (A==22) | (A==23) | (A==25) | (A==26)" --NoDataValue=0 --type=Byte --format=GTiff --overwrite'

'gdal_translate -ot Byte -co COMPRESS=LZW result_subset.tif num_subset.tif'

 
'gdal_calc.py -A subset.tif --outfile=result_subset.tif --calc="(A==20) | (A==21) | (A==22) | (A==23) | (A==25) | (A==26)" --type=Byte --format=GTiff --overwrite'
'gdal_calc.py -A subset.tif --outfile=result_subset.tif --calc="(A==20) | (A==21) | (A==22) | (A==23) | (A==25) | (A==26)" --type=Byte --format=GTiff --overwrite'

'gdalwarp -tr 500 500 -r average -overwrite num_subset.tif output_500m.tif'

ras <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/subset.tif")
ras <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/result_subset.tif")
ras <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/output_500m.tif")
plot(ras)
plot(st_geometry(p), cex = 0.01, add = TRUE)


#rev(sort(table(x$espece)))[1:50]
#res <- merge(p, unique(x[ x$species == "GRS", c("id_poe", "espece")]), all.x = TRUE)
#res$pres <- ifelse(is.na(res$espece), 0, 1)
#plot(st_geometry(res[res$pres == 0, ]), pch = 16, col = "black")
#plot(st_geometry(res[res$pres == 1, ]), pch = 16, col = "red", add = TRUE)

sp <- "Maianthemum canadense"
rev(sort(table(es$espece)))[1:50]
res <- merge(p, unique(es[ es$species == sp, c("id_poe", "espece", "cover")]), all.x = TRUE)
res$cover <- ifelse(is.na(res$cover), 0, res$cover)
res <- res[order(res$cover), ]
plot(st_geometry(res[res$cover<=0.001, ]), pch = 16, cex = 1, col = "grey90")
plot(st_geometry(res[res$cover>0.001, ]), pch = 16, cex = 1, col = colo.scale(res$cover[res$cover>0.001], c("grey70", "forestgreen", "darkgreen")), add= TRUE)

par(mar = c(0, 0, 0, 0))
plot(st_geometry(res), pch = 16, cex = 1, col = "grey90")
plot(st_geometry(res), pch = 1, cex = res$cover*2, col = "black", lwd = 0.5, add= TRUE)




#r <- rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/forets-cc-landis/baseline_BudwormBaselineFire_PICE.MAR_0_merged.tif")
#r <- aggregate(r, 2)
#mapview(r) + mapview(res[res$cover >= 0.001, ], cex = 4, color = "black", col.regions = "forestgreen")




#options(vsc.dev.args = list(width = 1200, height = 800))



can <- gadm(country=c("CAN","USA"),path="data")
qc <- can[can$NAME_1%in%c("Québec","Ontario","Vermont","New York","New Hampshire","Maine","New Brunswick","Nova Scotia","Rhode Island","Massachusetts","Connecticut","Newfoundland and Labrador")] |> st_as_sf()
qc <- ms_simplify(qc,0.001)
region <- concaveman(p, concavity = 3) |> st_buffer(dist = 10000)
plot(st_geometry(region))
plot(st_geometry(p), add = TRUE)





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

#res$longitude <- st_coordinates(res)[,1]
res$latitude2 <- res$latitude^2 # st_coordinates(res)[,2]

d <- res
d <- st_transform(d, 6623)
region <- st_transform(region, 6623)
qc <- st_transform(qc, 6623)



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

g <- rast(ext = st_bbox(region), res = c(5, 5) * 1000, crs = crs(region))
pgrid <- crds(g) |>
  as.data.frame() |>
  st_as_sf(coords = c("x", "y"), crs = st_crs(region))
pgridll <- st_transform(pgrid, 4326) |> 
  st_coordinates() |> 
  as.data.frame() |> 
  setNames(c("longitude", "latitude"))
pgrid <- cbind(pgrid, pgridll)
pgrid$latitude <- (pgrid$latitude - mlat) / sdlat
pgrid$latitude2 <- pgrid$latitude^2
pgrid <- pgrid[, c("latitude", "latitude2", "geometry")]


edge <- min(abs(c(diff(st_bbox(region)[c(3,1)]),diff(st_bbox(region)[c(4,2)]))))
pedge <- 0.01
edge <- edge * pedge

mesh <- fm_mesh_2d_inla(
  boundary = region, max.edge = c(edge, 3 * edge), # km inside and outside
  cutoff = edge, offset = c(edge, 3 * edge),
  crs = fm_crs(region)
) # cutoff is min edge
smesh <- mesh |> fm_as_sfc() |> st_as_sf()
pgrid <- pgrid[smesh, ]



matern <-
  inla.spde2.pcmatern(mesh,
                      prior.sigma = c(1, 0.1),
                      prior.range = c(100000, 0.1)
  )


logit <- function(p){
  log(p / (1 - p))
}

invlogit <- function(x){
  exp(x) / (1+exp(x))
}



beta0 <- function(p, C = 2) {
  (p * (length(p) - 1) + (1 / C)) / length(p)
}

invbeta0 <- function(pp, n, C = 2) {
  (pp * n - 1 / C) / (n - 1)
}

x <- c(0, runif(10), 1)
x
invbeta0(beta0(x)[1:3], n = length(x))


#n = 1000
#w = runif(n, min = 0.25, max = 0.75)
#phi = 0.5 * w
#z = rnorm(n, sd=0.2)
#eta = 1 + z
#mu = exp(eta)/(1+exp(eta))
#a = mu * phi
#b = -mu * phi + phi
#y = rbeta(n, a, b)
#hist(y)
#hist(rbeta(10000, 0.02, 10), breaks = 100)



#d$coverm <- beta0(d$cover)
d$coverm <- d$cover + 0.00001
#d$coverm <- rbeta(nrow(d), 0.1, 200)

cens <- 0.0049
d$coverm[d$coverm <= cens] <- 0
d$coverm[d$coverm >= 1-cens] <- 1


comps <- ~ Intercept(1) + latitude(d$latitude, model = "linear") + latitude2(d$latitude2, model = "linear") +
  #temp(s2$temp, model = "linear") + temp2(s2$temp2, model = "linear") +
  field(geometry, model = matern) 


fit <- bru(
  comps,
  inlabru::like(
    family = "beta", 
    data = d,
    formula = coverm ~ Intercept + latitude + latitude2 + field,
      #temp + temp2 +
    #E = NULL,
    #weights = 1,#s2$pres/s2$counts, #ifelse(s2$counts>250,250,s2$counts)/250,
    #Ntrials = s2$counts,
    #bru_options = bru_options(verbose = TRUE),
    control.family = list(beta.censor.value = cens),
    options = list(
      bru_verbose = TRUE,
     
      control.inla = list(int.strategy = "eb"), 
      control.predictor = list(link = 1)

    )
  )
)

pgrid$x <- st_coordinates(pgrid)[, 1]
pgrid$y <- st_coordinates(pgrid)[, 2]

x <- seq(-3, 3, by = 0.01)
x2 <- x^2 
#pgrid2 <- pgrid[c("latitude")]

predictions <- predict(
  fit, 
  pgrid,#newdata = data.frame(elev = x),#pgrid, 
  ~ Intercept + latitude_eval(pgrid$latitude) + latitude2_eval(pgrid$latitude2) + field
  #~ list(
  #    x1 = Intercept + latitude_eval(pgrid$latitude) + latitude2_eval(pgrid$latitude2) + field,
  #    x2 = Intercept + latitude_eval(x) + latitude2_eval(x2) 
  #)
  ##~ Intercept + latitude_eval(elev) + field
  #  #"latitude33" = field + Intercept

  ##~ Intercept + latitude + latitude2 + field
)
# samp <- generate(fit, s2,
#   ~ field + Intercept,
#   n.samples = 20
# )

#preds <- predictions[[1]]
preds <- predictions["mean"]


#plot((pred$elev * sdlat) + mlat, invlogit(pred$mean), type = "l")


#preds$mean <- preds$mean |> invlogit()# |> invbeta0(n = nrow(d))

preds <- rasterize(preds["mean"], g, field = "mean", fun = mean)
preds <- mask(preds, vect(qc[qc$NAME_1 == "Québec",]))
#plot(preds, fun = function() {vectors(obs = FALSE)})
preds <- invlogit(preds)


plot(preds, axes = FALSE, main = sp)
plot(st_geometry(qc), add = TRUE)
#plot(st_geometry(d), pch = 16, cex = 0.1 + (d$cover * 2), col = gray(0, 0.25), lwd = 0.1, add = TRUE)
offset <- 0.01
plot(st_geometry(d), pch = 16, cex = log((d$cover + offset)/max(d$cover + offset) * 100)/3, col = gray(0, 0.25), lwd = 0.1, add = TRUE)
plot(st_geometry(lakes), border = "black", col = "white", lwd = 0.01, add = TRUE)



#mapview(preds, map.types = c("Esri.WorldImagery"), col.regions = adjustcolor(terrain.colors(100), 0.5))
#mapview(map.types = c("Esri.WorldImagery")) + mapview(preds, col.regions = adjustcolor(terrain.colors(100), 0.5))



hist(rgamma(100000, 1, 0.1), breaks = 100)

mu = 0.0001
phi = fit$summary.hyperpar[1, 1]
phi = 10

a = mu * phi
b = -mu * phi + phi

hist(rbeta(10000, a, b), breaks = 50)

hist(rbeta(100000, 1, 100000))



lf <- list.files("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020")
classes <- read.csv("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/utilisation_territoire_2020.csv")
r <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/utilisation_territoire_2020.tif")
e <- data.frame(cbind(lon = -71.9186849, lat = 45.377099)) |> 
        st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
        st_transform(st_crs(r)) |>
        st_buffer(dist = 800) |>
        st_as_sf() |>
        ext()
#e <- ext(c(-2.70e+05,-2.60e+05, 1.57e+05, 1.68e+05))
#e <- ext(c(-4.75e+05,-0.55e+05, -1.55e+05, 3.7e+05))
#e <- ext(c(-4.15e+05,-4.12e+05, 6.13e+05, 6.16e+05))
r <- crop(r, e)
levels(r) <- cats(r)[[1]][, c("CODE_UT", "descriptio")]
plot(r, mar = c(1,1,1,12), plg = list(cex = 0.65))

col <- "DESC_CAT"
v <- values(r)[, 1]
cat <- classes[, col][match(v, classes$CODE_UT)]
rev(sort(table(cat)))
ch <- unique(v[grep("Humide", cat, ignore.case = TRUE)])

rr <- subst(as.numeric(r), from = ch, to = 1, others = NA)
plot(r)
plot(as.polygons(rr), col = "red", border = NA, add = TRUE)


r <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/cog.tif")
#e <- ext(c(-2.75e+05,-2.55e+05, 1.55e+05, 1.7e+05))
#r <- crop(r, e)
plot(r)

#table(values(r)[,1],useNA="always")

#library(terra)

#ndvi <- rast("/home/frousseu/Documents/uds/consultation/ZacharyCloutier/wetransfer_hillshade_clip_nad27utm111-tfw_2024-05-30_1738/MYD13A1_NDVI_2022_185.tif")

#hillshade <- rast("/home/frousseu/Documents/uds/consultation/ZacharyCloutier/wetransfer_hillshade_clip_nad27utm111-tfw_2024-05-30_1738/HIllShade_Clip_NAD27UTM111.tif")

#crs(hillshade) <- "epsg:26711"
#hillshade2 <- project(hillshade, "epsg:4326")

#ndvi2 <- project(ndvi, hillshade2, method = "bilinear")
#ndvi2 <- project(ndvi, "epsg:4326")


#plot(ndvi2)
#plot(hillshade)

r <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/utilisation_territoire_2020.tif")


classes <- read.csv("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/utilisation_territoire_2020.csv")

categories <- split(classes, classes$DESC_RCL_A) |>
                  lapply(function(i){
                    paste0("(A==", i$CODE_UT, ")") |>
                      paste(collapse = " | ")
                  })

path <- "/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/"
extent <- "-200000 250000 50000 115000"
res <- "500 500"
category <- 


system(cat(sprintf(
         'cd %s
          #gdal_translate -projwin -850000 1050000 800000 115000 utilisation_territoire_2020.tif cropped.tif
          gdal_translate -projwin %s utilisation_territoire_2020.tif cropped.tif
          cats=%s
          gdal_calc.py -A cropped.tif --outfile=cat_cropped.tif --calc=$cats --format=GTiff --overwrite
          rm cropped.tif
          gdal_translate -ot Float32 cat_cropped.tif num_cropped.tif
          rm cat_cropped.tif
          gdalwarp -tr %s -wm 8000 -wo 8 -r average -overwrite num_cropped.tif output.tif
          rm num_cropped.tif
          # https://trac.osgeo.org/gdal/wiki/CloudOptimizedGeoTIFF
          # compress to COG
          gdal_translate output.tif cog.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW
          rm output.tif
         ',
         path, extent , categories[[length(categories)]], res
       ))
      )
#        bash gdal.sh')

r <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/cog.tif")
#e <- ext(c(-2.75e+05,-2.55e+05, 1.55e+05, 1.7e+05))
#r <- crop(r, e)
plot(r)




