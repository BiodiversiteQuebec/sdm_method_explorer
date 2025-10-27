library(jsonlite)

# Folder containing your JSON files
folder <- "json"

# List all JSON files
files <- list.files(folder, pattern = "\\.json$", full.names = TRUE)

# Read and normalize JSON
x <- lapply(files, function(f) {
  z <- fromJSON(f, flatten = TRUE)
  if (is.list(z$vars)) z$vars <- unlist(z$vars)
  if (is.null(z$I)) z$I <- NA
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

# Write final JSON
write_json(nested, "combined_nested_models.json", pretty = TRUE, auto_unbox = TRUE)

