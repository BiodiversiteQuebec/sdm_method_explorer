
checkpoint("Running:")



# https://lightgbm.readthedocs.io/en/latest/Parameters-Tuning.html#use-bagging
# bagging_freq, neg_bagging_fraction
# pos_bagging_fraction = 1 #pos_samples * pos_bagging_fraction
# neg_bagging_fraction = prNum / bgNum
# bagging_freq = 5
# bagging_fraction = 0.5
# scale_pos_weight

library(dismo)
library(lightgbm)

dat$presence <- as.integer(as.character(dat$presence))
#dat2 <- dat
dat2 <- dat[c(sample(which(dat$presence == 0), sum(dat$presence == 1) * 5), which(dat$presence == 1)), ]

X <- data.matrix(dat2[ , -match(c("presence"), names(dat2))])
Y <- dat2$presence

prNum <- sum(dat2$presence == "1") # number of presences
bgNum <-  sum(dat2$presence == "0") # number of backgrounds # prNum * 2 
#casewts <- ifelse(dat$presence == 1, 1, prNum / bgNum)

m <- lightgbm(
  data = X
  , label = Y
  , params = list(
    max_depth = 4, # 4
    num_leaves = 10L, # 10
    learning_rate = 0.01, # 0.0001
    #is_unbalance = TRUE,
    #max_bin = 100,
    #bagging_fraction = 0.005,
    #max_bin = ,
    pos_bagging_fraction = 1, #pos_samples * pos_bagging_fraction
    neg_bagging_fraction = prNum / bgNum,
    bagging_freq = 5,
    bagging_fraction = 0.5, # 0.5
    min_data_in_leaf = 5, # 10
    objective = "binary"
  )
  , nrounds = 500L
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

