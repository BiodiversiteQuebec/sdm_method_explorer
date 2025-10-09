
checkpoint("Running:")

options(java.parameters = "-Xmx100g")

library(predicts)

#predictors2<-aggregate(predictors,2,na.rm=TRUE)



m <- MaxEnt(unwrap(predictors)[[vars]], 
             vect(d[d$presence==1,]), vect(d[d$presence==0,]),
             removeDuplicates=TRUE,
             silent=FALSE,
             #args=c("replicatetype=bootstrap","replicates=1","threads=4")
             #args=c("linear","quadratic","product","hinge","nothreshold","replicatetype=bootstrap","replicates=1","threads=4")
             args=c("linear", "quadratic", "noproduct", "hinge", "nothreshold", "replicatetype=bootstrap", "replicates=1", "threads=4")
     )

#newdat<-predictors[[vars]] |> as.data.frame() |> colMeans(na.rm=TRUE) |> as.list() |> data.frame() 

#p<-partialResponse(m, newdat, var=1, rng=c(9,30), nsteps=50)

#plot(p[,1],p[,2],type="l")

preds <- mask(predict(m, unwrap(predictors)[[vars]], args = c("outputformat=raw","replicatetype=bootstrap")), region)

#plot(preds)
#plot_preds(F)

write_preds(preds)

auc<-unname(m@results[row.names(m@results)=="Training.AUC", 1])
I<-niche_overlap()

write_results()

checkpoint("Done:")


