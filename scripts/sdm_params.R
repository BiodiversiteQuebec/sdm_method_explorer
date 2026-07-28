#####################################################
### Data parameters #################################
### minimal coordinate precision
th <- min(res(predictors))#2500 
th_small <- th # for local scale model if any

keep_na <- FALSE

####################################################
### Background parameters ##########################

background_prop <- 0.9 # targeted proportion of background points for the model 
background_cap <- TRUE # if TRUE, will cap the nb of background points with the min/max 
#background_n <- 10000 # number of background points
background_min <- 5000 # overall min nb of background points
background_max <- 10000000 # overall max nb of background points

add_effort_buffer <- TRUE # add an effort buffer or not
effort_buffer_radius <- 250000 # in meters
effort_buffer_n <- 400000 # number of observations in the outside buffer

dmesh_resolution <- 0.002