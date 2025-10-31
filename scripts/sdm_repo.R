

### Get a current snapshot of the repo when running the pipeline 
github_user <- "frousseu"
#github_token_path <- "/home/frousseu/.ssh/github_token"
github_token_path <- ".ssh/github_token"
repo <- "BiodiversiteQuebec/sdm_method_explorer"

get_repo_snapshot <- function(repo, user, tokenpath, n = 10){
  #token <- readLines(tokenpath)
  token <- readLines(file.path(Sys.getenv("HOME"), tokenpath))
  githubapi <- paste0("https://", user, ":", token, "@api.github.com/repos/", repo, "/commits")
  x <- jsonlite::fromJSON(paste0(githubapi,"?per_page=", n, "&files=false"))
  latest_commit <- x$sha[1]
  repo_snapshot <- file.path("https://github.com", repo, "tree", latest_commit)
  repo_snapshot
}

reposnapshot <- get_repo_snapshot(repo, github_user, github_token_path)

cat(reposnapshot)

#PARAM=$(Rscript scripts/sdm_repo.R) | echo "$PARAM"