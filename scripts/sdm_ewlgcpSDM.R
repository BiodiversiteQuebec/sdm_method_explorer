
#remotes:::install_github("frousseu/ewlgcpSDM",auth_token=readLines("C:/Users/rouf1703/github_token"))


checkpoint("Running:")

library(ewlgcpSDM)


###########################################
### Build mesh for ewlgcpSDM ##############

if(any(params$algorithm == "ewlgcpSDM")){

    cat(paste("Running: dmesh","/",Sys.time(),"\n"))

    ### Build mesh
    domain <- st_sample(st_buffer(region, 5000), 5000)
    domain <- inla.nonconvex.hull(st_coordinates(domain), convex = -0.015, resolution = 75)

    edge <- min(c(diff(st_bbox(region)[c(1, 3)]) * dmesh_resolution, diff(st_bbox(region)[c(2, 4)]) * dmesh_resolution))
    edge

    mesh <- inla.mesh.2d(loc.domain = NULL,
                        max.edge = c(edge, edge * 3),
                        min.angle = 21,
                        cutoff = edge / 1,
                        offset = c(edge, edge * 3),
                        boundary = domain,#inla.mesh.segment(domainloc),
                        crs = st_crs(region))

    plan(multisession, workers = 5)
    dmesh <- dmesh_mesh(mesh)
    #plan(sequential)

    ### Compute weights
    dmesh <- dmesh_weights(dmesh, region)

    ### Summarize predictors
    #plan(multisession, workers = 10)
    dmesh <- dmesh_predictors(dmesh, predictors, progress = TRUE)
    plan(sequential)

}


### Summarize observations and effort
buff <- st_buffer(obs, effort_buffer_radius) |> st_union()
dmesh <- dmesh_effort(dmesh, obs = d[d$presence == 1, ], background = d[d$presence %in% c(0, 1), ], buffer = buff, adjust = FALSE)

if(params$usepredictors == "Predictors"){
  subvars <- c("conifers", "taiga", "deciduous", "temperate_grassland", "wetland", "cropland", "urban", "water", "distfsl", "tmean", "geomflat", "elevation", "sand")
  #subvars <- vars_pool
  form <- paste("y ~", paste(c(subvars, "tmean2"), collapse = " + ")) |> as.formula()
} else {
  form <- paste("y ~", "dummy") |> as.formula()
}


## Run model
m<-ewlgcp(
  formula = form,
  dmesh = dmesh,
  effort = if(params$bias == "Bias") {TRUE} else {FALSE},
  adjust = FALSE,
  buffer = FALSE,
  orthogonal = TRUE,
  prior.beta = list(prec = list(default = 1/(0.5)^2, Intercept = 1/(20)^2, fixed = 1/(0.5)^2), mean = list(default = 0, Intercept = 0, fixed = 0)),
  prior.range = c(5000, 0.01),
  prior.sigma = if(params$spatial == "Spatial") {c(1, 0.01)} else {c(0.000001, NA)},
  smooth = 2,
  num.threads = 1:1,
  #blas.num.threads=2,
  control.inla = list(
    strategy = "adaptive", # "adaptive"
    int.strategy = "eb", # "eb"
    huge = FALSE, # apparently ignored
    control.vb = list(
      enable = TRUE,
      verbose = TRUE
    )
  ),# adaptive, eb
  inla.mode = "experimental",
  control.compute = list(config = TRUE, openmp.strategy = "pardiso"),
  verbose = TRUE,
  safe = FALSE
)


## Map results

sdm <- ewlgcpSDM::map(model = m,
        dmesh = dmesh,
        #dims = c(1000, 1000),
        region = region,
        mask = TRUE
)

#preds<-exp(sdm$linkmean-sdm$spacemean)
#preds<-exp(sdm$linkmean)
#preds<-exp(sdm$"link0.5quant")
#preds<-sdm[[c("0.025quant","mean","0.975quant")]]
preds <- sdm[[c("mean")]]
#preds <- mask(preds, vect(region))
#preds <- crop(preds, vect(region), mask = TRUE)
#plot(preds)

#plot_preds()

write_preds(preds)

auc<-NA
I<-niche_overlap()

params$performance <- list(auc = auc, I = I)

params$production_date <- Sys.time()

checkpoint("Done:")

#plot(sqrt(crop(preds,vect(qc[qc$NAME_1=="Québec",]),mask=TRUE)))
#plot(st_geometry(lakes),col="white",border=NA,add=TRUE)
#plot(st_geometry(qc),border=adjustcolor("black",0.15),add=TRUE)
#plot(st_geometry(obs),border=adjustcolor("black",0.5),pch=16,cex=0.2,add=TRUE)


#plot(preds)
#plot(st_geometry(lakes),col="white",border=NA,add=TRUE)
#plot(st_geometry(qc),border=adjustcolor("black",0.15),add=TRUE)
#plot(st_geometry(tb),border=adjustcolor("black",0.15),lwd=2,add=TRUE)
#plot(st_geometry(obs),col=adjustcolor("red",0.55),lwd=2add=TRUE)
#plot(st_geometry(obs),col=adjustcolor("red",0.55),lwd=2,add=TRUE)
#plot(st_geometry(tb),border=adjustcolor("black",0.15),lwd=2,add=TRUE)





