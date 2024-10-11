#install.packages("tidymodels")
#install.packages("rworldmap")
#install.packages("oz")
library(broom)
library(recipes)
library(dials)
library(rsample)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(infer)
library(tune)
library(modeldata)
library(workflows)
library(parsnip)
library(workflowsets)
library(purrr)
library(sf)
library(terra)
library (tidymodels)
library (tidysdm)
library (pastclim)
library(rworldmap)
library(readr)
library(oz)
library(DALEX)
library(car)
#pastclim::download_dataset("Krapp2021")

setwd("C:/github/SNR_SDM/Data/processed")
conditor <- read_csv("conditor.csv")
print(conditor)

#We convert our dataset into an sf data.frame so that we can easily plot it (here tidyterra shines):

# Convert data frame to spatial features
# Set the Coordinate reference system (CRS) to GDA2020 for use in Australia

conditor <- st_as_sf(conditor, coords = c("longitude", "latitude")) |>
  mutate(time_bp = time_bp)

st_crs(conditor) <- 4326



#As a background to our presences, we will use the land mask for the present, taken from pastclim, and cut to cover Oceania:
land_mask <- pastclim::get_land_mask(time_bp =, dataset = "Krapp2021")
Aust_extent <- terra::ext(109.4919, 152.6379, -42.8198, -8.1955)


land_mask <- crop (land_mask,vect(Aust_extent))
land_mask_layer <- land_mask[[1]] 

#tidyterra to plot:
library(tidyterra)

ggplot() +
  geom_spatraster(data = land_mask_layer, aes()) +  # Add the land mask as a spatial raster layer
  scale_fill_terrain_d() +  # Use a discrete terrain color scale for the land mask
  geom_sf(data = conditor, aes(col = time_bp))  # Plot conditor occurrence points, colored by time_bp





#We now need a time series of palaeoclimate reconstructions. In this vignette, we will use the example dataset from pastclim. This dataset only has reconstructions every 5k years for the past 20k years at 1 degree resolution, with 3 bioclimatic variables. It will suffice for illustrative purposes, but we recommend that you download higher quality datasets with pastclim for real analysis. As for the land mask, we will cut the reconstructions to cover Europe only:
# library(pastclim)
# climate_vars <- c('bio01','bio05','bio06','bio12','bio13','bio14')
# climate_full <- pastclim::region_series(
#   bio_variables = climate_vars,
#   data = "Krapp2021",
#   crop = vect(Aust_extent)
# )


# #####VIF
# 
# 
# conditor<-data.frame(as.numeric(conditor$class),conditor$bio01,conditor$bio05,conditor$bio06,conditor$bio12,conditor$bio13,conditor$bio14)
# colnames(conditor)<-c('presence','bio01','bio05','bio06','bio12','bio13','bio14')
# 
# 
# vif(lm(presence~bio01+bio05+bio06+bio12+bio13+bio14,data = conditor))
# vif(lm(presence~bio01+bio05+bio06+bio13+bio14,data = conditor))
# vif(lm(presence~bio05+bio06+bio13+bio14,data = conditor))


# climate_vars <- c('bio05','bio06','bio13','bio14')
# climate_full <- pastclim::region_series(
#   bio_variables = climate_vars,
#   data = "Krapp2021",
#   crop = vect(Aust_extent)
##
climate_vars <- c('bio05','bio06','bio12')
climate_full <- pastclim::region_series(
  bio_variables = climate_vars,
  data = "Krapp2021",
  crop = vect(Aust_extent) )

#Now we sample pseudo-absences (we will constraint them to be at least 70km away from any presences), selecting three times the number of presences

set.seed(123)
conditor <- sample_pseudoabs_time(conditor,
                                  n_per_presence = 3,
                                  raster = climate_full,
                                  time_col = "time_bp",
                                  lubridate_fun = pastclim::ybp2date,
                                  method = c("dist_min", km2m(70)))

#Let’s see our presences and absences:


ggplot() +
  geom_spatraster(data = land_mask_layer) +
  scale_fill_terrain_d() +
  geom_sf(data = conditor, aes(col = class))


#Now let’s get the climate for these location. pastclim requires a data frame with two columns with coordinates and a column of time in years before present (where negative values represent time in the past). We manipulate the sf object accordingly:

conditor_df <- conditor %>%
  dplyr::bind_cols(sf::st_coordinates(conditor)) %>%
  mutate(time_bp = date2ybp(time_step)) %>%
  as.data.frame() %>%
  select(-geometry)
# get climate
conditor_df <- location_slice_from_region_series(conditor_df,
                                                 region_series = climate_full)

# add the climate reconstructions to the sf object, and remove the time_step
# as we don't need it for modelling
conditor <- conditor %>%
  bind_cols(conditor_df[, climate_vars]) %>%
  select(-time_step) %>%
  filter(!is.na(.$bio05))


#Fit the model by crossvalidation
# Next, we need to set up a recipe to define how to handle our dataset. We don’t want to transform our data, so we just need to define the formula (class is the outcome, all other variables are predictors; note that, for sf objects, geometry is automatically ignored as a predictor):

conditor_rec <- recipe(conditor, formula = class ~ .)
conditor_rec

#We can quickly check that we have the variables that we want with:

conditor_rec$var_info

#We now build a workflow_set of different models, defining which hyperparameters we want to tune. We will use glm, gam, random forest and boosted trees as our models, so only random forest and boosted trees have tunable hyperparameters. For the most commonly used models, tidysdm automatically chooses the most important parameters, but it is possible to fully customise model specifications.

conditor_models <-
  # create the workflow_set
  workflow_set(
    preproc = list(default = conditor_rec),
    models = list(
      # the standard glm specs  (no params to tune)
      glm = sdm_spec_glm(),
      # the standard sdm specs (no params to tune)
      gam = sdm_spec_gam(),
      # rf specs with tuning
      rf = sdm_spec_rf(),
      # boosted tree model (gbm) specs with tuning
      gbm = sdm_spec_boost_tree()
    ),
    # make all combinations of preproc and models,
    cross = TRUE
  ) %>%
  # set formula for gams
  update_workflow_model("default_gam",
                        spec = sdm_spec_gam(),
                        formula = gam_formula(conditor_rec)
  ) %>%
  # tweak controls to store information needed later to create the ensemble
  option_add(control = control_ensemble_grid())
#Note that gams are unusual, as we need to specify a formula to define to which variables we will fit smooths. By default, gam_formula() fits a smooth to every continuous predictor, but a custom formula can be provided instead.
#We now want to set up a spatial block cross-validation scheme to tune and assess our models:

library(tidysdm)
set.seed(1005)
conditor_cv <- spatial_block_cv(conditor, v = 5)
autoplot(conditor_cv)

##########We can now use the block CV folds to tune and assess the models:

set.seed(123)
conditor_models <- conditor_models %>%
  workflow_map("tune_grid",
               resamples = conditor_cv, grid = 5,
               metrics = sdm_metric_set(), verbose = TRUE)
#Note that workflow_set correctly detects that we have no tuning parameters for glm and gam. We can have a look at the performance of our models with:
autoplot(conditor_models)

#Now let’s create an ensemble, selecting the best set of parameters for each model (this is really only relevant for the random forest, as there were not hype-parameters to tune for the glm and gam). We will use the Boyce continuous index as our metric to choose the best random forest and boosted tree. When adding members to an ensemble, they are automatically fitted to the full training dataset, and so ready to make predictions.

conditor_ensemble <- simple_ensemble() %>% add_member(conditor_models, metric = "boyce_cont")

autoplot(conditor_ensemble)
#We can now make predictions with this ensemble (using the default option of taking the mean of the predictions from each model) for the Last Glacial Maximum (LGM, 21,000 years ago).

climate_lgm <- pastclim::region_slice(
  time_bp = -1000,
  bio_variables = climate_vars,
  data = "Krapp2021",
  crop = vect(Aust_extent))

#And predict using the ensemble:
prediction_lgm <- predict_raster(conditor_ensemble, climate_lgm)
ggplot() +
  geom_spatraster(data = prediction_lgm, aes(fill = mean)) +
  scale_fill_terrain_c() 

# Create a Stack of Predictions for All Time Steps

# Create a Stack of Predictions for All Time Steps

# Loop over 40 time steps (40ka in 1ka timesteps) to create and save a map of predictions for each time step
walk(760:800, function(timestep) {
  year_bp <- time(climate_full[[1]])[timestep] - 1950  # Calculate years before present for each timestep
  
  # Create a climate snapshot for the current timestep
  climate_snapshot <- rast(list(climate_full[[1]][[timestep]], 
                                climate_full[[2]][[timestep]], 
                                climate_full[[3]][[timestep]]))
  names(climate_snapshot) <- c("bio05", "bio06", "bio12")  # Rename layers to match the climate variables
  
  # Predict species occurrence for the current climate snapshot
  prediction1 <- predict_raster(conditor_ensemble, climate_snapshot)
  
  # Plot the predictions using ggplot2
  p <- ggplot() +
    geom_spatraster(data = prediction1, aes(fill = mean)) +  # Add the prediction raster
    scale_fill_terrain_c(name = "Probability\nof occurrence") +  # Use a terrain color scale
    ggtitle(paste0("2 Krapp2021 Model conditor Years BP: ", year_bp))  # Add a title with the year before present
  
  # Save the plot to a folder with filenames indicating the year BP
  ggsave(path= "C:/github/SNR_SDM/Results/3conditorKrapp2021",
         plot = p,
         filename = paste0("3conditor_map_", year_bp, ".png"))
})

# Partial Dependence Profiles and Variable Importance
## PDP for bio12

# Explain the ensemble model to generate partial dependence profiles and variable importance
explainer_conditor_ens <- explain_tidysdm(conditor_ensemble)  # Create an explainer object for the ensemble model

# Compute variable importance using the DALEX package
vip_ensemble <- model_parts(explainer = explainer_conditor_ens)  # Calculate variable importance
plot(vip_ensemble)  # Plot the variable importance results

# Generate a partial dependence profile for bio12
pdp_bio12 <- model_profile(explainer_conditor_ens, N = 500, variables = "bio12")  # Generate PDP for bio12
plot(pdp_bio12)  # Plot the partial dependence profile for bio12


## PDP for bio05

# Generate and plot a partial dependence profile for bio05
pdp_bio05 <- model_profile(explainer_conditor_ens, N = 500, variables = "bio05")
plot(pdp_bio05)


## PDP for bio06

# Generate and plot a partial dependence profile for bio06
pdp_bio06 <- model_profile(explainer_conditor_ens, N = 500, variables = "bio06")
plot(pdp_bio06)

