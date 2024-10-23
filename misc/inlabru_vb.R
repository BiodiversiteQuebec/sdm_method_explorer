
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

#options(vsc.dev.args = list(width = 1200, height = 800))

logit <- function(p){
  log(p / (1 - p))
}

invlogit <- function(x){
  exp(x) / (1+exp(x))
}

vectors <- function(){
  plot(st_geometry(qc), lwd = 0.75, add = TRUE)
  plot(st_geometry(s[s$pres == 1, ]), pch = 16, cex = 0.75, add = TRUE)
  plot(st_geometry(lakes), add = TRUE, col = "white", lwd = 0.5)
}


can <- gadm(country=c("CAN","USA"),path="data")
qc <- can[can$NAME_1%in%c("Québec","Ontario","Vermont","New York","New Hampshire","Maine","New Brunswick","Nova Scotia","Rhode Island","Massachusetts","Connecticut","Newfoundland and Labrador")] |> st_as_sf()
qc <- ms_simplify(qc,0.001)
region <- st_union(qc) |> st_as_sf()

temp <- rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/CHELSA/climatologies/CHELSA_bio1_1981-2010_V.2.1.tif")

#temp <- crop(rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/CHELSA/climatologies/CHELSA_bio5_1981-2010_V.2.1.tif"),vect(st_transform(region,4326)))




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

sp <- "Zonotrichia leucophrys"
year <- 2005
year <- (year-2):(year+2)
md <- format(c(as.Date(paste0("2000-",c("06-21","08-02")))+c(-10,10)),"%m-%d")
y <- year

obs <- d[scientific_name == sp & year %in% y & month_day >= md[1] & month_day <= md[2], ]

### keep the range of dates seen in Quebec
# keep <- st_as_sf(obs, coords = c("longitude", "latitude"), crs = 4326) |>
#   st_transform(st_crs(region)) |>
#   st_intersects(qc[qc$NAME_1 == "Québec",]) |>
#   lengths() |>
#   as.logical()
# md <- range(obs[keep, ]$month_day)
# obs <- obs[month_day >= min(md) & month_day <= max(md)]


s <- d[year %in% y & month_day >= md[1] & month_day <= md[2], ]
s[, c("scientific_name") := NULL]
s <- unique(s, by = "sampling_event_identifier")
s[, pres := as.integer(sampling_event_identifier %in% obs$sampling_event_identifier)]



s <- st_as_sf(as.data.frame(s), coords = c("longitude", "latitude"), crs = 4326)

s <- st_transform(s, 6623)
qc <- st_transform(qc, 6623)
region <- st_transform(region, 6623)
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



plot(st_geometry(qc))
plot(st_geometry(s[s$pres == 0, ]), pch = 16, add = TRUE)
plot(st_geometry(s[s$pres == 1, ]), pch = 16, col = "red", add=TRUE)



#############################################################
### inlabru #################################################
#############################################################

library(inlabru)
library(ggplot2)
library(INLA)


edge <- min(abs(c(diff(st_bbox(region)[c(3,1)]),diff(st_bbox(region)[c(4,2)]))))
pedge <- 0.02
edge <- edge * pedge


#hull <- fm_extensions(
#  s,
#  convex = c(100, 200)*1000,
#  concave = c(100, 200)*1000
#)

mesh <- fm_mesh_2d_inla(
  boundary = region, max.edge = c(edge, 3 * edge), # km inside and outside
  cutoff = edge, offset = c(edge, 3 * edge),
  crs = fm_crs(region)
) # cutoff is min edge


ggplot() +
  geom_fm(data = mesh)


matern <-
  inla.spde2.pcmatern(mesh,
                      prior.sigma = c(1, 0.01),
                      prior.range = c(100000, 0.01)
  )

cmp <- pres ~ field(geometry, model = matern) + Intercept(1)

fit <- bru(cmp, s, family = "binomial")
summary(fit)



#pix <- fm_pixels(mesh, dims = c(200, 200))
r <- rast(ext = st_bbox(region), res = c(5, 5) * 1000)
xy <- xyFromCell(r, 1:ncell(r)) |> as.data.frame()
pix <- st_as_sf(xy, coords = c("x", "y"), crs = st_crs(region))



pred <- predict(
  fit, pix,
  ~ field + Intercept,
  keep = TRUE
)
#samp <- generate(fit, pix,
#  ~ field + Intercept,
#  n.samples = 1
#)
#pred$sample <- samp[, 1]

pred$mean <- boot::inv.logit(pred$mean)
preds <- rasterize(pred["mean"], r, field = "mean", fun = mean)
preds <- mask(preds, vect(qc))
plot(preds)
plot(st_geometry(qc), add = TRUE)
plot(st_geometry(s[s$pres == 1, ]), cex = 0.1, add = TRUE)


g <- rast(ext = st_bbox(region), res = c(40, 40) * 1000)
g <- rasterize(vect(s), g, field = "pres", fun = mean)
plot(g)
plot(st_geometry(qc), add = TRUE)
counts <- rasterize(vect(s), g, fun = "count")
pres <- rasterize(vect(s), g, field = "pres", fun = sum)
coords <- xyFromCell(counts, 1:ncell(counts)) |> as.data.frame()
coords$counts <- values(counts)[,1]
coords$pres <- values(pres)[,1]

png("/data/sdm_rbq/vbmap.png", width = 12, height = 10, res = 500, units = "in")
plot(preds)

#k <- which(coords$pres >= 0)
points(coords$x, coords$y, cex = log(coords$counts)+1, col = adjustcolor("black",0.25))
points(coords$x, coords$y, cex = log(coords$pres)+1, col = "red")


text(coords[k, ], labels = coords$counts[k], cex = 0.5)
dev.off()



points(coords, cex = log(values(counts)[,1])+1, col = ifelse(values(pres)[,1] >= 1, "red", "black"))
k <- which(coords$pres >= 0)
text(coords[k, ], labels = coords$counts[k], cex = 0.5)
dev.off()
#system("xdg-open /data/sdm_rbq/vbmap.png")



library(spdep)

g <- rast(ext = st_bbox(region), res = c(400, 400) * 1000)
g <- rasterize(vect(s), g, field = "pres", fun = mean)
plot(g)
plot(st_geometry(qc), add = TRUE)
counts <- rasterize(vect(s), g, fun = "count")
pres <- rasterize(vect(s), g, field = "pres", fun = sum)
scounts <- as.polygons(counts, dissolve = FALSE) |> st_as_sf()
spres <- as.polygons(pres, dissolve = FALSE) |> st_as_sf()
x <- cbind(spres, scounts)
nb <- poly2nb(x)
res <- EBlocal(x$sum,  x$count, nb)
x$est <- x$sum / x$count
x$est000 <- res$est

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




### GAM

dat <- cbind(st_coordinates(s),as.data.frame(s))
m <- gam(pres ~ s(X, Y, k = 40), data = dat, family = binomial)

newdata <- xy
names(newdata) <- c("X", "Y")

p <- predict(m, newdata, type = "response")

preds_gam <- r
preds_gam[] <- p

plot(mask(preds_gam,qc))
plot(st_geometry(qc), add = TRUE)
plot(st_geometry(s[s$pres == 0, ]), cex = 0.1, add = TRUE)
plot(st_geometry(s[s$pres == 1, ]), cex = 0.1, col = "red", add = TRUE)








pl_posterior_mean <- ggplot() +
  gg(pred, geom = "tile") +
  #gg(bnd, alpha = 0) +
  ggtitle("Posterior mean")
pl_posterior_sample <- ggplot() +
  gg(pred, aes(fill = sample), geom = "tile") +
  #gg(bnd, alpha = 0) +
  ggtitle("Posterior sample")



# Common colour scale for the truth and estimate:
csc <- colsc(pred$mean, pred$sample)
multiplot(pl_posterior_mean + csc,
          pl_posterior_sample + csc,
          cols = 2
)

multiplot(pl_posterior_mean + csc,
          cols = 1
) + geom_sf(data=s)



xs<-st_as_sf(as.data.frame(x),coords=c("longitude","latitude"),crs=4326)
xs<-st_transform(xs,32618)
xs$date<-as.character(xs$date)


x<-unique(d,by="sampling_event_identifier")





checklists<-fread("/data/sdm_rbq/ebird_sampling_events.csv")

can<-gadm(country="CAN",path="C:/Users/rouf1703/Downloads")
qc<-can[can$NAME_1=="Québec",] |> st_as_sf()
qc<-ms_simplify(qc,0.01)

cat("\014")
sp<-"Leiothlypis_ruficapilla"
year<-2016
y<-year

occs <- st_read("/data/sdm_rbq/total_occ_pres_only_versionR_UTM.gpkg",
                query = paste0("SELECT * FROM total_occ_pres_only_versionR WHERE year_obs=", year," AND species=\"",tolower(sp),"\""), quiet = T
)
occs$date <- with(occs, format(as.Date(paste0(year_obs, "-", month_obs, "-", day_obs),format = "%Y-%m-%d")))
occs$month_day <- substr(occs$date, 6, 10)

obs <- occs
setDT(obs)


x <- checklists[year == y & month_day >= min(occs$month_day) & month_day <= max(occs$month_day),]




########################################################
### On aggregated data #################################
########################################################


g <- rast(ext = st_bbox(region), res = c(10, 10) * 1000, crs = crs(region))
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

#s2 <- na.omit(s2)



edge <- min(abs(c(diff(st_bbox(region)[c(3,1)]),diff(st_bbox(region)[c(4,2)]))))
pedge <- 0.035
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



comps <- ~ Intercept(1) + longitude(s2$longitude, model = "linear") +
  latitude(s2$latitude, model = "linear") + latitude2(s2$latitude2, model = "linear") +
  temp(s2$temp, model = "linear") + temp2(s2$temp2, model = "linear") +
  field(geometry, model = matern) 


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
    weights = NULL,#ifelse(s2$counts<20,0.1,1),
    Ntrials = s2$counts,
    options = list(control.inla = list(int.strategy = "eb"), control.predictor = list(link = 1))
  )
)

# #pix <- fm_pixels(mesh, dims = c(200, 200))
# r <- rast(ext = st_bbox(region), res = c(20, 20) * 1000)
# xy <- crds(r) |> as.data.frame()
# pix <- st_as_sf(xy, coords = c("x", "y"), crs = st_crs(region))
# pix$longitude <- st_coordinates(pix)[,1]/1000/1000
# pix$latitude <- st_coordinates(pix)[,2]/1000/1000 # needs to give the same size newdata as the input data?????


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
plot(preds, fun = vectors)


ras<-rast("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/TdeB_benchmark_SDM/oiseaux-nicheurs-qc/zonotrichia_leucophrys_pocc_2005.tif")
ras <- trim(ras)
ras <- project(ras, crs(s2))
ras <- mask(ras, vect(region))
plot(ras, fun = vectors)


# samples <- rep(g, ncol(samp))
# values(samples) <- exp(samp)/(1+exp(samp))
# samples <- mask(samples, vect(region))
# plot(samples, fun = vectors, range = 0:1)



#hist(rbinom(1000, size = 100, p = rnorm(1000, 0.5, 0.025)) / 100, breaks = (0:20)/20)

#############################################################
### spBayes on previous model ###############################
#############################################################

s3<-na.omit(as.data.frame(s2))
#s3<-as.data.frame(s2)

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
knots <- crds(g)/1000/1000

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
y.hat.var <- apply(y.hat, 1, var)


gs <- setValues(g, boot::inv.logit(y.hat.mu))
gs <- mask(gs, vect(qc))
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














