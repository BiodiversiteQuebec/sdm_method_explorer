
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

sp <- "Picoides dorsalis"
year <- 2017
year <- (year-2):(year+2)
md <- format(c(as.Date(paste0("2000-",c("01-01","12-31")))+c(-0,0)),"%m-%d")
y <- year

obs <- d[scientific_name == sp & year %in% y & month_day >= md[1] & month_day <= md[2], ]

s <- d[year %in% y & month_day >= md[1] & month_day <= md[2], ]
s[, c("scientific_name") := NULL]
s <- unique(s, by = "sampling_event_identifier")
s[, pres := as.integer(sampling_event_identifier %in% obs$sampling_event_identifier)]

s <- st_as_sf(as.data.frame(s), coords = c("longitude", "latitude"), crs = 4326)
obs <- st_as_sf(as.data.frame(obs), coords = c("longitude", "latitude"), crs = 4326)

obs <- st_transform(obs, 6623)
s <- st_transform(s, 6623)
qc <- st_transform(qc, 6623)
region <- st_transform(region, 6623)
#region <- obs[region, ] |> st_union() |> st_convex_hull() |> st_as_sf()

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



vb<-rast(paste0("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/TdeB_benchmark_SDM/oiseaux-nicheurs-qc/",paste0(paste(gsub(" ","_",tolower(sp)),"pocc",median(year),sep="_"),".tif")))
vb <- trim(vb)
vb <- project(vb, crs(region))
vb <- mask(vb, vect(region))
#plot(vb, fun = vectors)


########################################################
### On aggregated data #################################
########################################################

g <- rast(ext = st_bbox(region), res = c(20, 20) * 1000, crs = crs(region))

### to subsample observations
# e <- extract(setValues(g,runif(ncell(g))), vect(s), cells = TRUE)
# s$cell <- e$cell
# s2 <- s
# s2 <- s2[!is.nan(s2$cell), ]
# s2 <- do.call("rbind", lapply(split(s2, s2$cell), function(i){i[1:min(c(nrow(i),5)), ]}))
# counts <- rasterize(vect(s2), g, fun = "count")
# pres <- rasterize(vect(s2), g, field = "pres", fun = sum)

counts <- rasterize(vect(s), g, fun = "count")
pres <- rasterize(vect(s), g, field = "pres", fun = sum)
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


edge <- min(abs(c(diff(st_bbox(region)[c(3,1)]),diff(st_bbox(region)[c(4,2)]))))
pedge <- 0.03
edge <- edge * pedge

mesh <- fm_mesh_2d_inla(
  boundary = region, max.edge = c(edge, 3 * edge), # km inside and outside
  cutoff = edge, offset = c(edge, 3 * edge),
  crs = fm_crs(region)
) # cutoff is min edge

matern <-
  inla.spde2.pcmatern(mesh,
                      prior.sigma = c(0.2, 0.1),
                      prior.range = c(100000, 0.1)
  )

matern2 <-
  inla.spde2.pcmatern(mesh,
                      prior.sigma = c(0.000001, NA),
                      prior.range = c(100000, 0.1)
  )


comps <- ~ Intercept(1) + longitude(s2$longitude, model = "linear") +
  latitude(s2$latitude, model = "linear") + latitude2(s2$latitude2, model = "linear") +
  temp(s2$temp, model = "linear") + temp2(s2$temp2, model = "linear") +
  field(geometry, model = matern) 

comps2 <- ~ Intercept(1) + longitude(s2$longitude, model = "linear") +
  latitude(s2$latitude, model = "linear") + latitude2(s2$latitude2, model = "linear") +
  temp(s2$temp, model = "linear") + temp2(s2$temp2, model = "linear") +
  field(geometry, model = matern2) 


fit <- bru(
  comps,
  like(
    family = "binomial", 
    data = s2,
    formula = pres ~ Intercept +
      #latitude + latitude2 +
      #temp + temp2 +
      field,
    E = NULL,
    weights = NULL,#s2$pres/s2$counts, #ifelse(s2$counts>250,250,s2$counts)/250,
    Ntrials = s2$counts,
    options = list(control.inla = list(int.strategy = "eb"), control.predictor = list(link = 1))
  )
)

pred <- predict(
  fit, s2,
  ~ Intercept + 
    #latitude + latitude2 + 
    #temp + temp2 +
    field
)
# samp <- generate(fit, s2,
#   ~ field + Intercept,
#   n.samples = 20
# )

pred$mean <- invlogit(pred$mean)
preds <- rasterize(pred["mean"], g, field = "mean", fun = mean)
preds <- mask(preds, vect(qc))
predSpatial <- preds
#plot(preds, fun = function() {vectors(obs = FALSE)})


fit <- bru(
  comps,
  like(
    family = "binomial", 
    data = s2,
    formula = pres ~ Intercept +
      #latitude + latitude2 +
      temp + temp2 +
      field,
    E = NULL,
    weights = NULL,#ifelse(s2$counts<20,0.1,1),
    Ntrials = s2$counts,
    options = list(control.inla = list(int.strategy = "eb"), control.predictor = list(link = 1))
  )
)

pred <- predict(
  fit, s2,
  ~ Intercept + 
    #latitude + latitude2 + 
    temp + temp2 +
    field
)

pred$mean <- invlogit(pred$mean)
preds <- rasterize(pred["mean"], g, field = "mean", fun = mean)
predSpatialPredictors <- mask(preds, vect(qc))

fit <- bru(
  comps2,
  like(
    family = "binomial", 
    data = s2,
    formula = pres ~ Intercept +
      #latitude + latitude2 +
      temp + temp2 +
      field,
    E = NULL,
    weights = NULL,#ifelse(s2$counts<20,0.1,1),
    Ntrials = s2$counts,
    options = list(control.inla = list(int.strategy = "eb"), control.predictor = list(link = 1))
  )
)

pred <- predict(
  fit, s2,
  ~ Intercept + 
    #latitude + latitude2 + 
    temp + temp2 +
    field
)

pred$mean <- invlogit(pred$mean)
preds <- rasterize(pred["mean"], g, field = "mean", fun = mean)
predPredictors <- mask(preds, vect(qc))


l <- list("Vincent" = project(vb, predSpatial), 
          "inlabru spatial" = predSpatial, 
          "inlabru spatial + temp + temp^2" = predSpatialPredictors,
          "inlabru temp + temp^2" = predPredictors
     )


counts <- rasterize(vect(s), g, fun = "count")
pres <- rasterize(vect(s), g, field = "pres", fun = sum)



title <- paste(sp, paste(min(year), max(year), sep = "/"), paste(md, collapse = "/"), collapse = "__")
png(paste0(gsub(" |/|-","_",title),".png"),units="in",width=12,height=10,res=400)
mar <- c(0,0,3,5)
par(mfrow = c(3,3), mar = mar, oma = c(0,0,2,0))
plot(st_geometry(qc), axes = FALSE, lwd = 0.75, main = "Checklists and presence")
vectors(background = TRUE)

spres <- aggregate(pres, 5, fun ="sum", na.rm = TRUE)
scounts <- aggregate(counts, 5, fun = "sum", na.rm = TRUE)

raw <- spres / scounts
plot(raw, fun = function(){vectors(obs = FALSE, background = FALSE)}, axes = FALSE, mar = mar, main = "Raw proportions")

ebcounts <- as.polygons(scounts, dissolve = FALSE) |> st_as_sf()
ebpres <- as.polygons(spres, dissolve = FALSE) |> st_as_sf()
x <- cbind(ebpres, ebcounts)
nb <- poly2nb(x)
res <- EBlocal(x$sum,  x$count, nb)

eb <- scounts
values(eb)[!is.na(values(eb)[,1])]<- res$est
plot(eb, fun = function(){vectors(obs = FALSE, background = FALSE)}, range = 0:1, axes = FALSE, mar = mar, main = "Empirical Bayes proportions")


invisible(lapply(seq_along(l),function(i){
  rang <- if(i == 4){ NULL } else { 0:1 }
  plot(l[[i]], main = names(l)[i], fun = function(){vectors(obs = FALSE)}, axes = FALSE, range = rang, mar = mar)
}))
mtext(side = 3, line = 0, outer = TRUE, text = title, font = 2, cex = 1.25)
dev.off()



png(paste0(gsub(" |/|-","_",title),"_raw.png"),units="in",width=12,height=10,res=600)
plot(l[[1]])
plot(st_geometry(obs),add=TRUE,pch=16,col=adjustcolor("red",0.5),cex=0.5)
spres <- aggregate(pres, 2, fun ="sum", na.rm = TRUE)
scounts <- aggregate(counts, 2, fun = "sum", na.rm = TRUE)
raw <- spres / scounts
sraw <- as.polygons(raw, dissolve = FALSE) |> st_as_sf() |> st_centroid()
scounts <- as.polygons(scounts, dissolve = FALSE) |> st_as_sf() |> st_centroid()
plot(st_geometry(sraw), cex = sraw$sum*5, add = TRUE, lwd = 0.2)
text(st_coordinates(st_geometry(scounts)), labels = scounts$count, cex = 0.1, lwd = 0.2)
dev.off()



plot(pres/counts)
plot(st_geometry(region),add=TRUE)
plot(st_geometry(smesh), lwd = 0.05, add = TRUE)


xypres <- xyFromCell(pres, 1:ncell(pres)) |> as.data.frame() |> st_as_sf(coords = c("x", "y"), crs = st_crs(pres))
xypres$pres <- values(pres)[,1]
xypres$counts <- values(counts)[,1]
xypres$raw <- xypres$pres / xypres$counts 


smesh <- mesh |> fm_as_sfc() |> st_as_sf()
int <- st_intersects(smesh, xypres)
spres <- do.call("rbind", lapply(int, function(i) {
  cat(length(i), "\n\n")
  if(length(i)){
    c(sum(xypres$pres[i], na.rm = TRUE), sum(xypres$counts[i], na.rm = TRUE))
  } else {
    c(0, 0)
  }
})) |> as.data.frame() |> setNames(c("pres", "counts"))
spres$raw <- spres$pres / spres$counts
sprops <- cbind(smesh, spres)

sprops <- st_centroid(sprops)

plot(l[[3]])
plot(st_geometry(smesh), lwd = 0.05, add = TRUE)
plot(st_geometry(sprops), pch = 16, cex = sprops$raw * 5, lwd = 0.25, col = adjustcolor("red", 0.5), add = TRUE)
k <- sprops$counts > 0
samps <- st_sample(smesh[k,], size = ifelse(sprops$counts[k]>100, 100, sprops$counts[k]), type = "regular")
plot(st_geometry(samps), pch = 16, cex = 0.1, add = TRUE)

#plot(st_geometry(sprops), col = ifelse(is.na(sprops$pres), "red", "blue"), add = TRUE)

# samples <- rep(g, ncol(samp))
# values(samples) <- exp(samp)/(1+exp(samp))
# samples <- mask(samples, vect(region))
# plot(samples, fun = vectors, range = 0:1)



#hist(rbinom(1000, size = 100, p = rnorm(1000, 0.5, 0.025)) / 100, breaks = (0:20)/20)

#############################################################
### spBayes on previous model ###############################
#############################################################

#g <- rast(ext = st_bbox(region), res = c(10, 10) * 1000, crs = crs(region))
g3 <- rast(ext = st_bbox(region), res = c(200, 200) * 1000, crs = crs(region)) 
counts <- rasterize(vect(s), g3, fun = "count")
pres <- rasterize(vect(s), g3, field = "pres", fun = sum)
scounts <- as.polygons(counts, dissolve = FALSE) |> st_as_sf()

vals <- values(project(temp, g3))

s3 <- crds(g3) |>
  as.data.frame() |>
  st_as_sf(coords = c("x", "y"), crs = st_crs(region))
s3$pres <- values(pres)[,1]  
s3$counts <- values(counts)[,1]      
s3$longitude <- st_coordinates(s3)[,1]/1000/1000
s3$latitude <- st_coordinates(s3)[,2]/1000/1000
s3$longitude2 <- s3$longitude^2
s3$latitude2 <- s3$latitude^2
s3$temp <- scale(vals[,1])
s3$temp2 <- s3$temp^2
s3<-na.omit(as.data.frame(s3))


coords <- cbind(s3$longitude, s3$latitude)

weights <- s3$counts
y <- s3$pres

##Collect samples
fit <- glm(cbind(pres,counts) ~ 1, weights=NULL, family="binomial", data = na.omit(as.data.frame(s2)))
beta.starting <- coefficients(fit)

n.batch <- 200
batch.length <- 50
n.samples <- n.batch*batch.length

# find_phi(
#   d,
#   family = "pexp",
#   range = c(1e-04, 1000),
#   cut = 0.05
# )
knots <- crds(g3)/1000/1000

m.1 <- spGLM(y~1, family="binomial", coords=coords, weights=weights, 
             starting=list("beta"=beta.starting, "phi"=1,"sigma.sq"=0.5, "w"= rep(0,nrow(knots))),
             tuning=list("beta"=0.5, "phi"=0.5, "sigma.sq"=0.25, "w"= rep(0.5,nrow(knots))),
             priors=list("beta.Normal"=list(0,10), "phi.Unif"=c(0.02,5), "sigma.sq.IG"=c(2, 1)),
             amcmc=list("n.batch"=n.batch, "batch.length"=batch.length, "accept.rate"=0.43),
             cov.model="exponential", verbose=TRUE, n.report=10, knots = knots)

burn.in <- 0.9*n.samples
sub.samps <- burn.in:n.samples
print(summary(window(m.1$p.beta.theta.samples, start=burn.in)))
beta.hat <- m.1$p.beta.theta.samples[sub.samps,"(Intercept)"]
w.hat <- m.1$p.w.knots.samples[,sub.samps]
#p.hat <- 1/(1+exp(beta.hat+w.hat))
p.hat <- matrix(rep(beta.hat,nrow(w.hat)),ncol=ncol(w.hat),byrow=TRUE)+w.hat
#y.hat <- apply(p.hat, 2, function(x){rbinom(n, size=weights, prob=p.hat)})
y.hat.mu <- apply(p.hat, 1, mean)
#y.hat.var <- apply(y.hat, 1, var)


gs <- setValues(g3, invlogit(y.hat.mu))
gs <- mask(gs, vect(qc))
gs <- disagg(gs, 5, method = "bilinear")
plot(gs)
plot(st_geometry(qc), lwd = 0.75, add = TRUE)
plot(st_geometry(s[s$pres == 1, ]), cex = 0.1, add = TRUE)
plot(st_geometry(lakes), add = TRUE, col = "white", lwd = 0.5)

plot(m.1$p.beta.theta.samples)

curve(1*exp(-(x/2)),ylim=c(0,1))

# vectors<-function(){
#   plot(st_geometry(qc), lwd = 0.75, add = TRUE)
#   #plot(st_geometry(s[s$pres == 0, ]), cex = 0.25, col = "black", add = TRUE)
#   plot(st_geometry(s[s$pres == 1, ]), cex = 0.25, col = "red", add = TRUE)
#   plot(st_geometry(lakes), add = TRUE, col = "white", lwd = 0.5)
# }
predictions<-c(resample(pres,g,method="sum")/resample(counts,g,method="sum"),inlabru1, inlabru2)
plot(predictions, range = range(values(predictions), na.rm = TRUE), fun = vectors)

#surf <- mba.surf(cbind(knots,y.hat.mu),no.X=300, no.Y=300, extend=TRUE)$xyz.est
#image(surf, main="Interpolated mean of posterior rate\n(observed rate)")
#text(coords,labels=round(y/weights,2))


#############################################################
### Empirical Bayes #########################################
#############################################################

scounts <- as.polygons(counts, dissolve = FALSE) |> st_as_sf()
spres <- as.polygons(pres, dissolve = FALSE) |> st_as_sf()
x <- cbind(spres, scounts)
nb <- poly2nb(x)
res <- EBlocal(x$sum,  x$count, nb)
x$est <- x$sum / x$count
x$est000 <- res$est


plot(pres/counts)

eb1 <- counts
values(eb1)[!is.na(values(eb1)[,1])]<- x$est
names(eb1) <- "raw"

eb2 <- counts
values(eb2)[!is.na(values(eb2)[,1])]<- x$est000
names(eb2) <- "eb"

eb <-c(eb1,eb2)
plot(eb[[2]])
plot(st_geometry(qc), add = TRUE)
plot(st_geometry(s[s$pres == 1, ]), cex = 0.1, add = TRUE)

plot(eb, range = c(0,0.25))














