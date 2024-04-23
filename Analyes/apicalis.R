install.packages("tidymodels")
install.packages("rworldmap")
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
pastclim::download_dataset("Beyer2020")

setwd("C:/Users/annie/OneDrive/Desktop/tidymodel")
apicalis <- read_csv("apicalis.csv")
print(apicalis)

#We convert our dataset into an sf data.frame so that we can easily plot it (here tidyterra shines):

# Convert data frame to spatial features
# Set the CRS to GDA2020 for use in Australia

apicalis <- st_as_sf(apicalis, coords = c("longitude", "latitude"))
st_crs(apicalis) <- 4326



#As a background to our presences, we will use the land mask for the present, taken from pastclim, and cut to cover Oceania:
land_mask <- pastclim::get_land_mask(time_bp =, dataset = "Beyer2020")
Aust_extent <- terra::ext(109.4919, 152.6379, -42.8198, -8.1955)


land_mask <- crop (land_mask,vect(Aust_extent))

#tidyterra to plot:
library(tidyterra)

ggplot() +
  geom_spatraster(data = land_mask, aes()) +
  scale_fill_terrain_c() +
  geom_sf(data = apicalis, aes(col = time_bp))


#We now need a time series of palaeoclimate reconstructions. In this vignette, we will use the example dataset from pastclim. This dataset only has reconstructions every 5k years for the past 20k years at 1 degree resolution, with 3 bioclimatic variables. It will suffice for illustrative purposes, but we recommend that you download higher quality datasets with pastclim for real analysis. As for the land mask, we will cut the reconstructions to cover Europe only:
library(pastclim)
climate_vars <- c("bio01", "bio05", "bio06","bio07","bio12")
climate_full <- pastclim::region_series(
  bio_variables = climate_vars,
  data = "Beyer2020",
  crop = vect(Aust_extent)
)

#Now we sample pseudo-absences (we will constraint them to be at least 70km away from any presences), selecting three times the number of presences

set.seed(123)
apicalis <- sample_pseudoabs_time(apicalis,
                                  n_per_presence = 3,
                                  raster = climate_full,
                                  time_col = "time_bp",
                                  lubridate_fun = pastclim::ybp2date,
                                  method = c("dist_min", km2m(70))
)

#Let’s see our presences and absences:

ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_0)) +
  scale_fill_terrain_c() +
  geom_sf(data = apicalis, aes(col = class))


#Now let’s get the climate for these location. pastclim requires a data frame with two columns with coordinates and a column of time in years before present (where negative values represent time in the past). We manipulate the sf object accordingly:

apicalis_df <- apicalis %>%
  dplyr::bind_cols(sf::st_coordinates(apicalis)) %>%
  mutate(time_bp = date2ybp(time_step)) %>%
  as.data.frame() %>%
  select(-geometry)
# get climate
apicalis_df <- location_slice_from_region_series(apicalis_df,
                                                 region_series = climate_full
)

# add the climate reconstructions to the sf object, and remove the time_step
# as we don't need it for modelling
apicalis <- apicalis %>%
  bind_cols(apicalis_df[, climate_vars]) %>%
  select(-time_step)

#Fit the model by crossvalidation
# Next, we need to set up a recipe to define how to handle our dataset. We don’t want to transform our data, so we just need to define the formula (class is the outcome, all other variables are predictors; note that, for sf objects, geometry is automatically ignored as a predictor):

apicalis_rec <- recipe(apicalis, formula = class ~ .)
apicalis_rec

#We can quickly check that we have the variables that we want with:

apicalis_rec$var_info

#We now build a workflow_set of different models, defining which hyperparameters we want to tune. We will use glm, gam, random forest and boosted trees as our models, so only random forest and boosted trees have tunable hyperparameters. For the most commonly used models, tidysdm automatically chooses the most important parameters, but it is possible to fully customise model specifications.

apicalis_models <-
  # create the workflow_set
  workflow_set(
    preproc = list(default = apicalis_rec),
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
                        formula = gam_formula(apicalis_rec)
  ) %>%
  # tweak controls to store information needed later to create the ensemble
  option_add(control = control_ensemble_grid())
#Note that gams are unusual, as we need to specify a formula to define to which variables we will fit smooths. By default, gam_formula() fits a smooth to every continuous predictor, but a custom formula can be provided instead.
#We now want to set up a spatial block cross-validation scheme to tune and assess our models:

library(tidysdm)
set.seed(1005)
apicalis_cv <- spatial_block_cv(apicalis, v = 5)
autoplot(apicalis_cv)

#We can now use the block CV folds to tune and assess the models:

set.seed(123)
apicalis_models <-
  apicalis_models %>%
  workflow_map("tune_grid",
               resamples = apicalis_cv, grid = 5,
               metrics = sdm_metric_set(), verbose = TRUE
  )
#Note that workflow_set correctly detects that we have no tuning parameters for glm and gam. We can have a look at the performance of our models with:
autoplot(apicalis_models)

#Now let’s create an ensemble, selecting the best set of parameters for each model (this is really only relevant for the random forest, as there were not hype-parameters to tune for the glm and gam). We will use the Boyce continuous index as our metric to choose the best random forest and boosted tree. When adding members to an ensemble, they are automatically fitted to the full training dataset, and so ready to make predictions.

apicalis_ensemble <- simple_ensemble() %>%
  add_member(apicalis_models, metric = "boyce_cont")

autoplot(apicalis_ensemble)
#We can now make predictions with this ensemble (using the default option of taking the mean of the predictions from each model) for the Last Glacial Maximum (LGM, 21,000 years ago).

climate_lgm <- pastclim::region_slice(
  time_bp = -20000,
  bio_variables = climate_vars,
  data = "Beyer2020",
  crop = vect(Aust_extent)
)
#And predict using the ensemble:
prediction_lgm <- predict_raster(apicalis_ensemble, climate_lgm)
ggplot() +
  geom_spatraster(data = prediction_lgm, aes(fill = mean)) +
  scale_fill_terrain_c()
