

# scp "C:/Users/rouf1703/Documents/BiodiversitéQuébec/sdm_explorer/sdm_*" frousseu@vmbq:/home/frousseu

# scp -p frousseu@sdm:'/home/frousseu/sdm_*' C:/Users/rouf1703/Documents/BiodiversitéQuébec/sdm_explorer 

# rsync -avzu --dry-run --info=progress2 --exclude '.*' --exclude='data/*' --exclude='outputs/*' --exclude='json/*' -e ssh /home/frousseu/Documents/github/sdm_method_explorer/ frousseu@sdm:/home/frousseu/data/sdm_method_explorer/

# rsync -avzu --dry-run --info=progress2 --exclude '.*' --exclude='data/*' --exclude='outputs/*' --exclude='json/*' -e ssh /home/frousseu/Documents/github/sdm_method_explorer/scripts/*.R frousseu@sdm:/home/frousseu/data/sdm_method_explorer/scripts/

# nohup R CMD BATCH --no-save scripts/sdm_runs.R log.out &

# nohup R CMD BATCH --no-save scripts/sdm_runs.R "log_$(TZ='America/New_York' date +'%Y-%m-%d_%H-%M-%S').out" &

library(foreach)
library(doParallel)
library(future)
library(future.apply)
library(data.table)
library(jsonlite)
#library(future.callr) # not really working for some reason

### Run scripts
#save.image(file.path(path_write,"sdm.RData"))

source("scripts/sdm_utils.R")

if(!dir.exists("outputs")){
  dir.create("outputs")
}

if(!dir.exists("json")){
  dir.create("json")
}
clean_results()

if(!dir.exists("data")){
  stop("Missing data folder")
}


### Get a current snapshot of the repo when running the pipeline 
github_user <- "frousseu"
github_token_path <- "/home/frousseu/.ssh/github_token"
repo <- "BiodiversiteQuebec/sdm_method_explorer"


source("scripts/sdm_inputs.R")
runs <- 1:nrow(results)

reposnapshot <- get_repo_snapshot(repo, github_user, github_token_path)
results$reposnapshot <- reposnapshot

source("scripts/sdm_prelim.R")
source("scripts/sdm_predictors.R")

nworkers<-min(c(length(runs), 3))
#options(mc.cores=nworkers)
options(future.globals.maxSize = 5000 * 1024 ^ 2)
plan(multisession, workers = nworkers)
future_lapply(runs,function(i){
#foreach(i=runs) %dopar% {
  
  t1 <- Sys.time()
  
  source("scripts/sdm_utils.R", local = TRUE)

  params <- lapply(as.list(results),"[",i)
  
  source("scripts/sdm_data.R",local=TRUE)
  
  if(nrow(obs) < 5){
    checkpoint("Aborting:")
    cat("\nToo few observations, returning NULL")
    return(NULL)
  }
  
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
  
  t2 <- Sys.time() 
  message(paste("Minutes:", round(as.numeric(difftime(t2, t1, units = "mins")), 2), "\n"))
  
},future.conditions="message", future.globals = ls(), future.packages = names(sessionInfo()$otherPkgs))

plan(sequential)

add_results()

clean_results()




