

# scp "C:/Users/rouf1703/Documents/BiodiversitéQuébec/sdm_explorer/sdm_*" frousseu@vmbq:/home/frousseu

# scp -p frousseu@sdm:'/home/frousseu/sdm_*' C:/Users/rouf1703/Documents/BiodiversitéQuébec/sdm_explorer 


library(foreach)
library(doParallel)
library(future)
library(future.apply)
library(data.table)
library(jsonlite)
library(jsonlite)

### Run scripts
#save.image(file.path(path_write,"sdm.RData"))

if(!dir.exists("outputs")){
  dir.create("outputs")
}

if(!dir.exists("data")){
  stop("Missing data folder")
}


### Get a current snapshot of the repo when running the pipeline 
github_user <- "frousseu"
github_token_path <- "/home/frousseu/.ssh/github_token"
repo <- "BiodiversiteQuebec/sdm_method_explorer"


source("scripts/sdm_utils.R", local = TRUE)
source("scripts/sdm_inputs.R",local = TRUE)
runs <- 1:nrow(results)
#i<-6 # ou 9
#runs<-c(2:4,6:8)
#runs<-c(2,6,10,14,18,22)
#runs<-c(1:2)

#cl<-makeCluster(4)
#registerDoParallel(cl)

nworkers<-min(c(length(runs),12))

plan(multisession,workers=nworkers)
future_lapply(runs,function(i){
#foreach(i=runs) %dopar% {
  
  message("TESTING")
  
  source("scripts/sdm_utils.R",local = TRUE)
  source("scripts/sdm_inputs.R",local = TRUE)
  
  reposnapshot <- get_repo_snapshot(repo, github_user, github_token_path)
  results$reposnapshot <- reposnapshot
  
  source("scripts/sdm_prelim.R", local = TRUE)
  source("scripts/sdm_predictors.R",local = TRUE)
  
  #load(file.path(path_write,"sdm.RData"))
  
  params <- lapply(as.list(results),"[",i)
  
  source("scripts/sdm_data.R",local=TRUE)
  source("scripts/sdm_background.R",local=TRUE)
  
  switch(params$algorithm,
         maxent={
           source("scripts/sdm_maxent_predicts.R",local=TRUE)
         },
         randomForest={
           source("scripts/sdm_randomforest.R",local=TRUE)
         },
         brt={
           source("scripts/sdm_brt.R",local=TRUE)
         },
         ewlgcpSDM={
           source("scripts/sdm_ewlgcpSDM.R",local=TRUE)
         }
  )
},future.conditions="message")
plan(sequential)
clean_results()
#stopCluster(cl)



