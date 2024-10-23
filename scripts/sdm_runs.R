

# scp "C:/Users/rouf1703/Documents/BiodiversitéQuébec/sdm_explorer/sdm_*" frousseu@vmbq:/home/frousseu

# scp -p frousseu@sdm:'/home/frousseu/sdm_*' C:/Users/rouf1703/Documents/BiodiversitéQuébec/sdm_explorer 


library(foreach)
library(doParallel)
library(future)
library(future.apply)
library(data.table)

### Run scripts
#save.image(file.path(path_write,"sdm.RData"))

path<-"/home/frousseu"
path_write<-"/home/frousseu/data"

source(file.path(path,"sdm_utils.R"),local=TRUE)
source(file.path(path,"sdm_inputs.R"),local=TRUE)
runs<-1:nrow(results)
#i<-6 # ou 9
#runs<-c(2:4,6:8)
#runs<-c(2,6,10,14,18,22)
#runs<-c(1:2)

#cl<-makeCluster(4)
#registerDoParallel(cl)

nworkers<-min(c(nrow(runs),12))

plan(multisession,workers=nworkers)
future_lapply(runs,function(i){
#foreach(i=runs) %dopar% {
  
  message("TESTING")
  
  path<-"/home/frousseu"
  path_write<-"/home/frousseu/data"
  
  source(file.path(path,"sdm_utils.R"),local=TRUE)
  source(file.path(path,"sdm_inputs.R"),local=TRUE)
  source(file.path(path,"sdm_prelim.R"),local=TRUE)
  source(file.path(path,"sdm_predictors.R"),local=TRUE)
  
  
  #load(file.path(path_write,"sdm.RData"))
  
  params<-lapply(as.list(results),"[",i)
  
  source(file.path(path,"sdm_data.R"),local=TRUE)
  source(file.path(path,"sdm_background.R"),local=TRUE)
  
  switch(params$algorithm,
         maxent={
           source(file.path(path,"sdm_maxent_predicts.R"),local=TRUE)
         },
         randomForest={
           source(file.path(path,"sdm_randomforest.R"),local=TRUE)
         },
         brt={
           source(file.path(path,"sdm_brt.R"),local=TRUE)
         },
         ewlgcpSDM={
           source(file.path(path,"sdm_ewlgcpSDM.R"),local=TRUE)
         }
  )
},future.conditions="message")
plan(sequential)
clean_results()
#stopCluster(cl)

