
library(spBayes)
library(MBA)
library(coda)

#set.seed(32746)

rmvn <- function(n, mu=0, V = matrix(1)){
  p <- length(mu)
  if(any(is.na(match(dim(V),p))))
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n*p), ncol=p) %*% D + rep(mu,rep(n,p)))
}

################################
##Spatial binomial
################################

##Generate binary data
coords <- as.matrix(expand.grid(seq(0,100,length.out=10), seq(0,100,length.out=10)))
n <- nrow(coords)

phi <- 3/50
sigma.sq <- 0.000001

R <- sigma.sq*exp(-phi*as.matrix(dist(coords)))
w <- rmvn(1, rep(0,n), R)

x <- as.matrix(rep(1,n))
beta <- 0
p <- 1/(1+exp(-(x%*%beta+w)))
truth <- cbind(coords, p)

weights <- rep(1, n)
weights[coords[,1]>mean(coords[,1])] <- 50
#weights <- ceiling((coords[,1] + 1) / 5)


y <- rbinom(n, size=weights, prob=p)

##Collect samples
fit <- glm((y/weights)~x-1, weights=weights, family="binomial")
beta.starting <- coefficients(fit)
beta.tuning <- t(chol(vcov(fit)))

n.batch <- 200
batch.length <- 50
n.samples <- n.batch*batch.length

w<-which(!coords[, 1]>mean(coords[,1]))
w<-setdiff(w, sample(w,10))
y<-y[-w]
coords<-coords[-w,]
weights<-weights[-w]
x<-x[-w, ,drop = FALSE]

m.1 <- spGLM(y~1, family="binomial", coords=coords, weights=weights, 
             starting=list("beta"=beta.starting, "phi"=0.06,"sigma.sq"=1, "w"=0),
             tuning=list("beta"=beta.tuning, "phi"=0.5, "sigma.sq"=0.5, "w"=0.5),
             priors=list("beta.Normal"=list(0,10), "phi.Unif"=c(0.03, 0.3), "sigma.sq.IG"=c(2, 1)),
             amcmc=list("n.batch"=n.batch, "batch.length"=batch.length, "accept.rate"=0.43),
             cov.model="exponential", verbose=TRUE, n.report=10)

burn.in <- 0.9*n.samples
sub.samps <- burn.in:n.samples

print(summary(window(m.1$p.beta.theta.samples, start=burn.in)))

beta.hat <- m.1$p.beta.theta.samples[sub.samps,"(Intercept)"]
w.hat <- m.1$p.w.samples[,sub.samps]

p.hat <- 1/(1+exp(-(x%*%beta.hat+w.hat)))

y.hat <- apply(p.hat, 2, function(x){rbinom(n, size=weights, prob=p.hat)})

y.hat.mu <- apply(p.hat, 1, mean)
y.hat.var <- apply(y.hat, 1, var)

##Take a look
#par(mfrow=c(1,2))
surf <- mba.surf(cbind(coords,y.hat.mu),no.X=300, no.Y=300, extend=TRUE)$xyz.est
image(surf, main="Interpolated mean of posterior rate\n(observed rate)")
#contour(surf, add=TRUE)
#text(coords, label=paste("(",y,")",sep=""))
#points(coords,cex=(y/weights)*5)
#points(coords,cex=weights)
#points(coords,cex=(y/weights)*5,pch=16,col=adjustcolor("black",0.5))
text(coords,labels=round(y/weights,2))


###############################################
###############################################
### inlabru ###################################
###############################################
###############################################

bnd <- fm_segm(rbind(c(0, 0), c(100, 0), c(100, 100), c(0, 100)), is.bnd = TRUE)
edge <- 2
mesh <- fm_mesh_2d_inla(
  boundary = bnd, max.edge = c(edge, 3 * edge), # km inside and outside
  cutoff = edge, offset = c(edge, 3 * edge),
  crs = NA
)

matern <-
  inla.spde2.pcmatern(mesh,
                      prior.sigma = c(1, 0.01), # 0.02 - 0.005
                      prior.range = c(5, 0.01)
  )

dat <- data.frame(y = y, weights = weights, x = coords[, 1], X = coords[, 1], Y = coords[, 2] ) |> st_as_sf(coords =c("X", "Y"))

comps <- ~ Intercept(1) + x(dat$x, model = "linear") + 
  field(geometry, model = matern) 

fit <- bru(
  comps,
  like(
    family = "binomial", 
    data = dat,
    formula = y ~ Intercept + x + field,
    E = NULL,
    Ntrials = dat$weights,
    options = list(control.inla = list(int.strategy = "eb"), control.predictor = list(link = 1))
  )
)

pred <- predict(
  fit, dat,
  ~ Intercept + x + field,
)
#samp <- generate(fit, pix,
#  ~ field + Intercept,
#  n.samples = 1
#)
#pred$sample <- samp[, 1]

pred$mean <- boot::inv.logit(pred$mean)
surf2 <- mba.surf(cbind(coords,pred$mean),no.X=300, no.Y=300, extend=TRUE)$xyz.est
image(surf2, main="Interpolated mean of posterior rate\n(observed rate)")
#contour(surf, add=TRUE)
#text(coords, label=paste("(",y,")",sep=""))
#points(coords,cex=(y/weights)*5)
#points(coords,cex=weights)
#points(coords,cex=(y/weights)*5,pch=16,col=adjustcolor("black",0.5))
text(coords,labels=round(y/weights,2))



par(mfrow=c(1,3),mar=c(2,2,3,0))
image(surf, main="Interpolated mean of posterior probability\n(spBayes)")
text(coords,labels=round(y/weights,2),cex=0.5,col=adjustcolor("black",0.5))
image(surf2, main="Interpolated mean of posterior probability\n(inlabru)")
text(coords,labels=round(y/weights,2),cex=0.5,col=adjustcolor("black",0.5))
#par(mfrow=c(1,1))
surf3 <- mba.surf(truth,no.X=300, no.Y=300, extend=TRUE)$xyz.est
image(surf3, main="Interpolated mean of posterior rate\n(Truth)")
