
checkpoint("Running:")

library(dismo)

dat$presence <- as.integer(as.character(dat$presence))
dat2 <- dat#[c(sample(which(dat$presence == 0), sum(dat$presence == 1) * 1), which(dat$presence == 1)), ]

#X <- data.matrix(dat2[ , -match(c("presence"), names(dat2))])
#Y <- dat2$presence

dat2 <- dat2[sample(1:nrow(dat2), min(c(nrow(dat2), 20000))), ]

m <- gbm.step(data = dat2, gbm.x = 2:ncol(dat2), gbm.y = 1, family = "bernoulli", max.trees = 2000, plot.main = FALSE)#,
       #tree.complexity = 5, learning.rate = 0.01, bag.fraction = 0.5)



inv_logit <- function(x) {
  1 / (1 + exp(-x))
}


#ewdata <- as.matrix(predictors[[vars]])
p <- predict(m, predictors, progress = "text")
#p <- predict(m, newdata, type = "raw")
p <- inv_logit(p)

preds <- unwrap(predictors)[[1]]
preds <- setValues(preds, p)
preds <- mask(preds, region)
#png("sdm.png", width = 6, height = 6, units = "in", res = 300); plot(preds); dev.off();
#png("sdm.png", width = 6, height = 6, units = "in", res = 300); plot(st_geometry(obs)); dev.off();

write_preds(preds)

auc <- NA #mean(m$cv.roc)
I <- NA #niche_overlap()

params$performance <- list(auc = auc, I = I)

params$production_date <- Sys.time()

checkpoint("Done:")

