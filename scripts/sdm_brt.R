
checkpoint("Running:")

library(gbm)
library(dismo)

dat$presence<-as.integer(as.character(dat$presence))

#blocks$folds_ids

m <- gbm.step(data=dat, gbm.x = vars, gbm.y = "presence",#, fold.vector = blocks$folds_ids,
                            family = "bernoulli", tree.complexity = 5,
                            learning.rate = 0.005, bag.fraction = 0.5, step.size = 50, tolerance.method = "fixed", n.folds = 2, tolerance = 0.001, max.trees = 3000)

#gbm.plot(m, n.plots=11, plot.layout=c(3, 4), write.title = FALSE, x.label = NULL)
#gbm.plot.fits(m)

p <- predict(unwrap(predictors)[[vars]], m, n.trees=m$gbm.call$best.trees, type="response")
preds <- mask(p, vect(region))

#plot_preds(F)

write_preds(preds)

auc<-mean(m$cv.roc)
I<-niche_overlap()

write_results()

checkpoint("Done:")

