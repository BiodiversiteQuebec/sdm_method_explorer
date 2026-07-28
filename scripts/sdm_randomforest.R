
checkpoint("Running:")

library(ranger)

dat$presence<-factor(dat$presence)

#https://nsojournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fecog.05615&file=ecog12810-sup-0001-AppendixS1.pdf
prNum <- sum(dat$presence == "1") # number of presences
bgNum <-  sum(dat$presence == "0") # number of backgrounds # prNum * 2 
casewts <- ifelse(dat$presence == 1, 1, prNum / bgNum)


m <- ranger(presence ~ ., data = dat, probability = TRUE, sample.fraction = prNum / bgNum, case.weights = casewts)#, max.depth = 2)
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

params$performance <- list(auc = auc, I = I)

params$production_date <- Sys.time()

checkpoint("Done:")
