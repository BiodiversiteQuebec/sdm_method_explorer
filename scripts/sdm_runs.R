
# rsync -avzu --dry-run --info=progress2 --exclude '.*' --exclude='data/*' --exclude='outputs/*' --exclude='json/*' -e ssh /home/frousseu/Documents/github/sdm_method_explorer/ frousseu@sdm:/home/frousseu/data/sdm_method_explorer/

# rsync -avzu --dry-run --info=progress2 --exclude '.*' --exclude='data/*' --exclude='outputs/*' --exclude='json/*' -e ssh /home/frousseu/Documents/github/sdm_method_explorer/scripts/*.R frousseu@sdm:/home/frousseu/data/sdm_method_explorer/scripts/

# nohup R CMD BATCH --no-save scripts/sdm_runs.R log.out &

# nohup R CMD BATCH --no-save scripts/sdm_runs.R "log_$(TZ='America/New_York' date +'%Y-%m-%d_%H-%M-%S').out" &

t1 <- Sys.time()

library(foreach)
library(doParallel)
library(future)
library(future.apply)
library(data.table)
library(jsonlite)
library(terra)
library(sf)
library(geodata)
library(rmapshaper)
library(concaveman)
library(INLA)
library(rnaturalearth)
library(ewlgcpSDM)
library(dplyr)
library(sdmtools)

i <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
i <- 1

args <- commandArgs(trailingOnly=TRUE)
print(args[2])
#args <- c("bq_invascular.R", "commit")
#args <- "ebv_pffq_trees.R"
args <- "bq_invascular.R"

source("scripts/sdm_utils.R")

if(!dir.exists("outputs")){
  dir.create("outputs")
}

if(!dir.exists("json")){
  dir.create("json")
}
#clean_results()

if(!dir.exists("data")){
  stop("Missing data folder")
}

source("scripts/sdm_prelim.R")
source("scripts/sdm_predictors.R")
source("scripts/sdm_variables.R")

job <- gsub("\\.R|\\.r", "", args[1])
source(file.path("scripts/jobs", args[1]))
sprintf("Species: %s", results$species[i])

#sp <- species

results$reposnapshot <- args[2]

#nworkers<-min(c(length(runs), 3))
#options(mc.cores=nworkers)
#options(future.globals.maxSize = 5000 * 1024 ^ 2)
#plan(multisession, workers = nworkers)
#future_lapply(runs,function(i){
#lapply(runs,function(i){
#foreach(i=runs) %dopar% {
  
#source("scripts/sdm_utils.R")

params <- lapply(as.list(results),"[", i)
params$vars <- unlist(params$vars, use.names = FALSE)

sp <- params$species
genus <- strsplit(sp, " ")[[1]][1]

source(file.path("scripts/groups", paste0("sdm_", group, ".R")))
source("scripts/sdm_data.R")

#png("bg.png", width = 6, height = 6, units = "in", res = 200)
#plot(st_geometry(qc))
#plot(st_geometry(bg), add = TRUE)
#plot(st_geometry(obs), col = "orange", add = TRUE)
#dev.off()

if(nrow(obs) < 5){
  checkpoint("Aborting:")
  cat("\nToo few observations, returning NULL")
  return(NULL)
}

switch(params$algorithm,
        maxent={
          source("scripts/sdm_maxent_predicts.R")
        },
        randomForest={
          source("scripts/sdm_randomforest.R")
        },
        brt={
          source("scripts/sdm_brt.R")
        },
        ewlgcpSDM={
          source("scripts/sdm_ewlgcpSDM.R")
        }        
)

source("scripts/sdm_binarize.R")
source("scripts/sdm_graphics.R")

t2 <- Sys.time() 
message(paste("Minutes:", round(as.numeric(difftime(t2, t1, units = "mins")), 2), "\n"))




  
#},future.conditions="message", future.globals = ls(), future.packages = names(sessionInfo()$otherPkgs))
#plan(sequential)

#add_results()
#clean_results()

#ll <- list(repo = "test", performance = list(AUC = 42, I = 45))
#ll
#write_json(ll, "test.json")
