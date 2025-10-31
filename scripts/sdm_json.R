library(jsonlite)

# Folder containing your JSON files
folder <- "json"

# List all JSON files
files <- list.files(folder, pattern = "\\.json$", full.names = TRUE)

# Read and normalize JSON
x <- lapply(files, function(f) {
  z <- fromJSON(f, flatten = TRUE)
  if (is.list(z$vars)) z$vars <- unlist(z$vars)
  #if (is.list(z$performance)) z$performance <- unlist(z$performance)
  #if (is.null(z$I)) z$I <- NA
  z
})

# Combine by group, species, and model
combine_by_model <- function(lst) {
  # Create unique key per group/species/model
  keys <- vapply(lst, function(z) paste(z$group, z$species, z$model, sep = "::"), character(1))
  grouped <- split(lst, keys)
  
  # Combine each set (same group/species/model)
  lapply(grouped, function(items) {
    base <- items[[1]]
    products <- lapply(items, function(z) {
      list(product = z$product, ext = z$ext, file = z$file)
    })
    base[c("product", "ext", "file")] <- NULL
    base$products <- products
    base
  })
}

# Combine
combined_models <- combine_by_model(x)

# Nest by group → species → model
nested <- list()
for (entry in combined_models) {
  g <- entry$group
  s <- entry$species
  m <- entry$model
  if (is.null(nested[[g]])) nested[[g]] <- list()
  if (is.null(nested[[g]][[s]])) nested[[g]][[s]] <- list()
  nested[[g]][[s]][[m]] <- entry
}

write_json(nested, "metadata.json", pretty = TRUE, auto_unbox = TRUE)



#####################################################
### Get group, species, models ######################
x <- fromJSON("metadata.json", simplifyVector = FALSE)

df <- do.call(rbind, lapply(names(x), function(group) {
  do.call(rbind, lapply(names(x[[group]]), function(species) {
    data.frame(
      group = group,
      species = species,
      model = names(x[[group]][[species]])
    )
  }))
}))
df

#####################################################
### Get group, species, models ######################
x <- fromJSON("metadata.json", simplifyVector = FALSE)

df <- do.call(rbind, lapply(names(x), function(group) {
  do.call(rbind, lapply(names(x[[group]]), function(species) {
    data.frame(
      group = group,
      species = species,
      model = names(x[[group]][[species]])
    )
  }))
}))
df

l <- split(df, df$group) |>
        lapply(function(i){unique(i$species)})

write_json(l, "species.json", pretty = TRUE, auto_unbox = TRUE)
rl <- readLines("species.json")
rl[1] <- paste("speciesByGroup =", rl[1])
rl[length(rl)] <- paste0(rl[length(rl)], ";")
write(rl, "species.js")
unlink("species.json")

#####################################################
### Split to species ################################
x <- fromJSON("metadata.json", simplifyVector = FALSE)

# Loop through groups → species
for (group in names(x)) {
  for (species in names(x[[group]])) {
    
    # Extract only the model-level content
    species_data <- x[[group]][[species]]
    
    # Clean filename (replace spaces with underscores)
    species_file <- paste0(
      gsub(" ", "_", species),
      ".json"
    )
    
    # Write to file
    write_json(species_data, file.path("json_species", species_file), pretty = TRUE, auto_unbox = TRUE)
  }
}