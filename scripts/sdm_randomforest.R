
checkpoint("Running:")

library(ranger)

dat$presence<-factor(dat$presence)

m <- ranger(presence ~ ., data = dat, probability = TRUE)
newdata <- as.matrix(unwrap(predictors)[[vars]])
notna <- apply(newdata, 1, function(i){!any(is.na(i))})
ps <- predict(m, newdata[notna, , drop = FALSE])
p <- rep(NA, nrow(newdata))
p[notna] <- ps$predictions[,2]

preds <- unwrap(predictors)[[1]]
values(preds) <- p#[,2]
#plot(preds)

#plot_preds()

write_preds(preds)

auc<-NA
I<-niche_overlap()

write_results()

checkpoint("Done:")
