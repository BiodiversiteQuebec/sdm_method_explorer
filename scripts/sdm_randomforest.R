
checkpoint("Running:")

library(randomForest)




dat$presence<-factor(dat$presence)
m<-randomForest(presence ~ .,
                data = dat,
                ntree = 500,
                do.trace = TRUE) # number of trees


#p<-predict(m,dat,type="response")
p<-predict(m,as.matrix(unwrap(predictors)[[vars]]),type="prob")

preds<-unwrap(predictors)[[1]]
values(preds)<-p[,2]
#plot(preds)

#plot_preds()

write_preds(preds)

auc<-NA
I<-niche_overlap()

add_results()

checkpoint("Done:")