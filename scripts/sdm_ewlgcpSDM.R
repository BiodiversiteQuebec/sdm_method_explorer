
#remotes:::install_github("frousseu/ewlgcpSDM",auth_token=readLines("C:/Users/rouf1703/github_token"))


checkpoint("Running:")

library(ewlgcpSDM)



### Build mesh
domain<-st_sample(st_buffer(region,5000),5000)
domain <- inla.nonconvex.hull(st_coordinates(domain),convex = -0.015,resolution=75)


pedge<-0.005
edge<-min(c(diff(st_bbox(region)[c(1,3)])*pedge,diff(st_bbox(region)[c(2,4)])*pedge))
edge

mesh <- inla.mesh.2d(loc.domain = NULL,
                     max.edge = c(edge,edge*3),
                     min.angle = 21,
                     cutoff = edge/1,
                     offset = c(edge,edge*3),
                     boundary = domain,#inla.mesh.segment(domainloc),
                     crs = st_crs(region))


#plan(multisession,workers=10)
dmesh<-dmesh_mesh(mesh)
#plan(sequential)

### Compute weights
dmesh<-dmesh_weights(dmesh,region)

### Summarize predictors
dmesh<-dmesh_predictors(dmesh,unwrap(predictors)[[vars]])


#dm<-params$dmesh
#dm$deciduous<-params$predictors$deciduous
#plot(dm[,"deciduous"],border=NA,nbreaks=100,pal=function(x){rev(terrain.colors(x))},reset=FALSE,key.pos=NULL)
#plot(st_geometry(qc),border=adjustcolor("black",0.25),add=TRUE)
#plot(st_geometry(lakes),col="white",border=adjustcolor("black",0.25),add=TRUE)


### Summarize observations and effort

buff<-st_buffer(obs,500000) |> st_union()
#plotQC()
#plot(st_geometry(buff),add=TRUE)
#plot(st_geometry(obs),add=TRUE)


dmesh<-dmesh_effort(dmesh,obs=d[d$presence==1,],background=d[d$presence%in%c(0,1),],buffer=buff,adjust=FALSE)

#form<-paste("y ~",paste(c("tmax","tmax2","deciduous_esa","deciduous_esa2","conifers_esa","conifers_esa2"),collapse=" + ")) |> as.formula()

if(params$usepredictors=="Predictors"){
  form<-paste("y ~",paste(c(vars,"tmax2"),collapse=" + ")) |> as.formula()
}else{
  form<-paste("y ~","dummy") |> as.formula()
}


## Run model

#dmesh$predictors[,1]<-runif(nrow(dmesh$predictors))

#x<-dmesh$dmesh

#x<-st_transform(x, paste0("+units=km +init=epsg:",epsg))
#x<-st_transform(x,st_crs(x)$input)

m<-ewlgcp(
  formula=form,
  dmesh=dmesh,
  effort = if(params$bias=="Bias"){TRUE}else{FALSE},
  adjust = FALSE,
  buffer = FALSE,
  orthogonal = TRUE,
  prior.beta = NULL,#prior.beta<-list(prec=list(default=1/(0.000000001)^2,Intercept=1/(20)^2),mean=list(default=0,Intercept=0)),
  prior.range = c(5000,0.01),
  prior.sigma = if(params$spatial=="Spatial"){c(1,0.01)}else{c(0.00001,NA)},
  smooth = 2,
  num.threads=1:1,
  #blas.num.threads=2,
  control.inla=list(
    strategy="adaptive", # "adaptive"
    int.strategy="eb", # "eb"
    huge=FALSE, # apparently ignored
    control.vb=list(
      enable=TRUE,
      verbose=TRUE
    )
  ),# adaptive, eb
  inla.mode="experimental",
  control.compute=list(config=TRUE,openmp.strategy="pardiso"),
  verbose=FALSE,
  safe=FALSE
)


## Map results

sdm<-ewlgcpSDM::map(model = m,
                    dmesh = dmesh,
                    dims = c(1000,1000),
                    region= region
)

#preds<-exp(sdm$linkmean-sdm$spacemean)
#preds<-exp(sdm$linkmean)
#preds<-exp(sdm$"link0.5quant")
#preds<-sdm[[c("0.025quant","mean","0.975quant")]]
preds<-sdm[[c("mean")]]
#preds <- mask(preds, vect(region))
#plot(preds)

#plot_preds()

write_preds(preds)

auc<-NA
I<-niche_overlap()

write_results()

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





