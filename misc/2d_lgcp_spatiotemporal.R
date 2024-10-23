library(INLA)
library(INLAspacetime)
library(inlabru)

set.seed(1)
n <- 5
dataf <- data.frame(
  s1   = runif(n, -1, 1),
  s2   = runif(n, -1, 1),
  time = runif(n, 1, 4),
  y    = rnorm(n, 0, 1))
str(dataf)


smesh <- inla.mesh.2d(
  loc = cbind(0,0), 
  max.edge = 5, 
  offset = 2)
tmesh <- inla.mesh.1d(
  loc = 0:5)


stmodel <- stModel.define(
  smesh = smesh, ## spatial mesh
  tmesh = tmesh, ## temporal mesh
  model = '121', ## model, see the paper
  control.priors = list(
    prs = c(1, 0.1), ## P(spatial range < 1) = 0.1
    prt = c(5, 0), ## temporal range fixed to 5
    psigma = c(1, 0.1) ## P(sigma > 1) = 0.1
  )
)


linpred <- ~ 1 +
  field(list(space = cbind(s1, s2), 
             time = time),
        model = stmodel)

ctrlf <- list(
  hyper = list(
    prec = list(
      initial = 10, 
      fixed = TRUE)    
  )
)


datalike <- like(
  formula = y ~ ., 
  family = "gaussian",
  control.family = ctrlf, 
  data=dataf)


result <- 
  bru(
    components = linpred,
    datalike,
    options = list(
      control.inla = list(
        int.strategy = "eb"
      ),
      verbose = !TRUE)
  )
