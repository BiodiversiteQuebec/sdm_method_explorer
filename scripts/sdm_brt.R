
checkpoint("Running:")

library(dismo)
library(lightgbm)

dat$presence<-as.integer(as.character(dat$presence))

X <- data.matrix(dat[ , -match(c("presence"), names(dat))])
Y <- dat$presence

m <- lightgbm(
  data = X
  , label = Y
  , params = list(
    num_leaves = 10L
    , learning_rate = 0.02
    , objective = "binary"
  )
  , nrounds = 2000L
  , verbose = -1L
)

inv_logit <- function(x) {
  1 / (1 + exp(-x))
}


newdata <- as.matrix(unwrap(predictors)[[vars]])
p <- predict(m, newdata, type = "raw")
p <- inv_logit(p)

preds <- unwrap(predictors)[[1]]
preds <- setValues(preds, p)
preds <- mask(preds, vect(region))
#plot(preds)

write_preds(preds)

auc<-mean(m$cv.roc)
I<-niche_overlap()

write_results()

checkpoint("Done:")

