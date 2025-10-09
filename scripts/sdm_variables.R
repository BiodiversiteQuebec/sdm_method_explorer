
remove <- c("twi", "mhc", "eau_peu_profonde", "marais", "marecage", "indifferencie", "prairie_humide", "tourbiere_boisee", "tourbiere_indifferenciee", "tourbiere_minerotrophe", "tourbiere_ombrotrophe", "marin", "snow", "eolien") # partial cover of QC
keep <- c("alluvion", "annual_precipitation_amount", "annual_range_of_air_temperature", 
"anthropogenique", "barren", "bulk_density", "clay", "coniferous", 
"cropland", "deciduous", "depot", "distance_to_roads", 
"elevation", "eolien", "geomflat", "geomfootslope", "glaciaire", 
"glaciolacustre", "glaciomarin", "human_modification", 
"isothermality", "lacustre", "lai", "lichen", "mean_annual_air_temperature", "mean_daily_maximum_air_temperature_of_the_warmest_month", 
"mean_daily_mean_air_temperatures_of_the_coldest_quarter", "mean_daily_mean_air_temperatures_of_the_driest_quarter", 
"mean_daily_mean_air_temperatures_of_the_warmest_quarter", "mean_daily_mean_air_temperatures_of_the_wettest_quarter", 
"mean_daily_minimum_air_temperature_of_the_coldest_month", "mean_diurnal_air_temperature_range", 
"mean_monthly_precipitation_amount_of_the_coldest_quarter", "mean_monthly_precipitation_amount_of_the_driest_quarter", 
"mean_monthly_precipitation_amount_of_the_warmest_quarter", "mean_monthly_precipitation_amount_of_the_wettest_quarter", "mixed", "ndvi", "nitrogen", "organic_carbon_density", 
"organique", "ph", "polar_grass", "polar_shrub", 
"precipitation_amount_of_the_driest_month", "precipitation_amount_of_the_wettest_month", 
"precipitation_seasonality", "quaternaire", "roche", "ruggedness", 
"sand", "silt", "snow", "soil_organic_carbon", "taiga", "temperate_grass", 
"temperate_shrub", "temperature_seasonality", "till", "urban", "water", "wetland")

keep <- c("alluvion", "annual_precipitation_amount", "annual_range_of_air_temperature", 
"anthropogenique", "barren", "bulk_density", "clay", "coniferous", 
"cropland", "deciduous", "depot", "distance_to_roads", 
"elevation", "eolien", "geomflat", "geomfootslope", "glaciaire", 
"glaciolacustre", "glaciomarin", "human_modification", "lacustre", "lai", "lichen", "mean_annual_air_temperature", "mixed", "ndvi", "nitrogen", "organic_carbon_density", 
"organique", "ph", "polar_grass", "polar_shrub", "quaternaire", "roche", "ruggedness", 
"sand", "silt", "snow", "soil_organic_carbon", "taiga", "temperate_grass", 
"temperate_shrub", "till", "urban", "water", "wetland")

vars_pool <- setdiff(names(predictors), remove)
vars_pool <- vars_pool[vars_pool %in% keep]


filter_vars <- function(vars, data, th = 3){
    v <- vars
    vifs <- car::vif(lm(y ~ ., data = cbind(y = 1, data[ , v])))
    while(any(vifs >= th)){
      v <- v[!v %in% names(vifs)[which.max(vifs)]]
      vifs <- car::vif(lm(y ~ ., data = cbind(y = 1, data[ , v])))
    }
    v
}


#library(vegan)
r <- aggregate(predictors[[vars_pool]], 5, na.rm = TRUE)
#rr <- aggregate(predictors, 5, na.rm = TRUE)
#screeplot(rda(values(rr), scale = TRUE))
#pca <- rda(values(r), scale = TRUE)
#barplot(cumsum(eigenvals(pca)/sum(eigenvals(pca))))

#chosen <- climate
#chosen <- vars_pool
chosen <- names(r)
#chosen <- use_small
#chosen <- setdiff(use_small, c("lacustre", "human_modification", "organique"))
v <- values(r) |> as.data.frame()
#co <- cor(v[, chosen], use = "complete.obs")
#round(co, 2)

#car::vif(lm(y ~ ., data = cbind(y = 1, v[ , chosen])))

vars_pool <- filter_vars(chosen, v)
car::vif(lm(y ~ ., data = cbind(y = 1, v[ , vars_pool])))




#vars_pool<-c("conifers", "taiga", "deciduous", "mixed", "temperate_shrubland", "temperate_grassland","polar_shrubland", "polar_grassland", "polar_barren", "wetland", "cropland", "barren", "urban", "water", "snow", "distfsl", "tmean", "prec", "geomflat", "elevation", "sand")



if(FALSE){

  library(vegan)
  r <- predictors
  climate <- grep("temperature|precipitation|isothermality", names(r), value = TRUE)
  r <- aggregate(r[[vars_pool]], 5, na.rm = TRUE)
  #rr <- aggregate(predictors, 5, na.rm = TRUE)
  #screeplot(rda(values(rr), scale = TRUE))
  pca <- rda(values(r), scale = TRUE)
  barplot(cumsum(eigenvals(pca)/sum(eigenvals(pca))))
  
  #chosen <- climate
  #chosen <- vars_pool
  chosen <- names(r)
  #chosen <- use_small
  #chosen <- setdiff(use_small, c("lacustre", "human_modification", "organique"))
  v <- values(r) |> as.data.frame()
  co <- cor(v[, chosen], use = "complete.obs")
  round(co, 2)
  car::vif(lm(y ~ ., data = cbind(y = 1, v[, chosen])))
  car::vif(lm(y ~ ., data = cbind(y = 1, v[, filter_vars(chosen, v)])))

}