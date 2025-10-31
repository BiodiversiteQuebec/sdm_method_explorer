
checkpoint("Running:")

library(dismo)
library(lightgbm)

dat$presence <- as.integer(as.character(dat$presence))
dat2 <- dat#[c(sample(which(dat$presence == 0), sum(dat$presence == 1) * 4), which(dat$presence == 1)), ]

X <- data.matrix(dat2[ , -match(c("presence"), names(dat2))])
Y <- dat2$presence

m <- lightgbm(
  data = X
  , label = Y
  , params = list(
    max_depth = 4, # none4
    num_leaves = 5L, # 10L5
    learning_rate = 0.02, # 0.02
    is_unbalance = TRUE,
    max_bin = 100,
    #bagging_fraction = 0.005,
    #max_bin = ,
    objective = "binary"
  )
  , nrounds = 2000L
  , verbose = 1L
)

inv_logit <- function(x) {
  1 / (1 + exp(-x))
}


newdata <- as.matrix(unwrap(predictors)[[vars]])
p <- predict(m, newdata, type = "raw")
p <- inv_logit(p)

preds <- unwrap(predictors)[[1]]
preds <- setValues(preds, p)
preds <- mask(preds, region)
#plot(preds)

write_preds(preds)

auc <- mean(m$cv.roc)
I <- niche_overlap()

params$performance <- list(auc = auc, I = I)

params$production_date <- Sys.time()

checkpoint("Done:")

