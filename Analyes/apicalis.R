# #install.packages("tidymodels")
#install.packages("rworldmap")
#install.packages("oz")
library(broom)
library(car)
library(DALEX)
library(dials)
library(dplyr)
library(ggplot2)
library(infer)
library(modeldata)
library(oz)
library(parsnip)
library(pastclim)
library(purrr)
library(readr)
library(recipes)
library(rsample)
library(rworldmap)
library(sf)
library(tidyr)
library(tidymodels)
library(tidysdm)
library(tidyterra)
library(tibble)
library(tune)
library(terra)
library(workflows)
library(workflowsets)

#pastclim::download_dataset("Krapp2021")

# #########import palaeoview variables
# #min temp
# "C:/github/SNR_SDM/Data/raw/Palaeoview_data/Minimum_Temperature_Annual_21000BP-100BP_step20_size30.nc" |> rast () -> min_temp
# plot (min_temp)
# 
# #max temp
# "C:/github/SNR_SDM/Data/raw/Palaeoview_data/Maximum_Temperature_Annual_21000BP-100BP_step20_size30.nc" |> rast () -> max_temp
# plot (max_temp)
# 
# #Mean Precipitation 
# "C:/github/SNR_SDM/Data/raw/Palaeoview_data/Mean_Precipitation_Annual_21000BP-100BP_step20_size30.nc" |> rast () -> mean_prec
# plot (mean_prec)

#set working directory
setwd("C:/github/SNR_SDM/Data/processed")

#Read in apicalis data
apicalis <- read_csv("apicalis.csv")
print(apicalis)


#We convert our dataset into an sf data.frame so that we can easily plot it (here tidyterra shines):

# Convert data frame to spatial and temporal features then convert age to time bp
apicalis2 <- st_as_sf(apicalis, coords = c("longitude", "latitude")) |>
  mutate(time_bp = time_bp*(-1))

# Set the Coordinate reference system (CRS) to GDA2020 for use in Australia
st_crs(apicalis2) <- 4326



# make a land mask based on present time, based on pastclim data
land_mask <- pastclim::get_land_mask(time_bp =, dataset = "Krapp2021")

#create a frame of Australia extent
Aust_extent <- terra::ext(109.4919, 152.6379, -42.8198, -8.1955)

#Use Australia frame to crop land mask
land_mask <- crop (land_mask,vect(Aust_extent))


#Plot
ggplot() +
  geom_spatraster(data = land_mask, aes()) +
  scale_fill_terrain_c() +
  geom_sf(data = apicalis2, aes(col = time_bp))

 #We now need a time series of palaeoclimate reconstructions. In this vignette, we will use the example dataset from pastclim. This dataset only has reconstructions every 5k years for the past 20k years at 1 degree resolution, with 3 bioclimatic variables. It will suffice for illustrative purposes, but we recommend that you download higher quality datasets with pastclim for real analysis. As for the land mask, we will cut the reconstructions to cover Europe only:
 library(pastclim)
#add bio05 palaeoview data
bio5 <- rast ("C:/github/SNR_SDM/Data/raw/Palaeoview_data/bio05.nc")
time_bp(bio5) <- seq(-21000, -100, by = 20)
writeCDF(bio5, "C:/github/SNR_SDM/Data/processed/paleoview/bio5_time.nc", overwrite = TRUE,
         zname = "time", varname = "bio05")
climate_bio05 <- pastclim::region_series(
 bio_variables = 'bio05',
 dataset = "custom",
 path_to_nc = "C:/github/SNR_SDM/Data/processed/paleoview/bio5_time.nc",
  crop = vect(Aust_extent)
 )
#add bio06 palaeoview data
bio6 <- rast ("C:/github/SNR_SDM/Data/raw/Palaeoview_data/bio06.nc")
time_bp(bio6) <- seq(-21000, -100, by = 20)
writeCDF(bio6, "C:/github/SNR_SDM/Data/processed/paleoview/bio6_time.nc", overwrite = TRUE,
         zname = "time", varname = "bio06")

climate_bio06 <- pastclim::region_series(
  bio_variables = 'bio06',
  dataset = "custom",
  path_to_nc = "C:/github/SNR_SDM/Data/processed/paleoview/bio6_time.nc",
  crop = vect(Aust_extent)
)
#add bio12 palaeoview data
bio12 <- rast ("C:/github/SNR_SDM/Data/raw/Palaeoview_data/bio12.nc")
time_bp(bio12) <- seq(-21000, -100, by = 20)
writeCDF(bio12, "C:/github/SNR_SDM/Data/processed/paleoview/bio12_time.nc", overwrite = TRUE,
         zname = "time", varname = "bio12")

climate_bio12 <- pastclim::region_series(
  bio_variables = 'bio12',
  dataset = "custom",
  path_to_nc = "C:/github/SNR_SDM/Data/processed/paleoview/bio12_time.nc",
  crop = vect(Aust_extent)
)


# # 
# # 
# # # #####VIF
# # #
# # #
# # # apicalis2<-data.frame(as.numeric(apicalis$class),apicalis$bio01,apicalis$bio05,apicalis$bio06,apicalis$bio12,apicalis$bio13,apicalis$bio14)
# # # colnames(apicalis2)<-c('presence','bio01','bio05','bio06','bio12','bio13','bio14')
# # #
# # #
# # # vif(lm(presence~bio01+bio05+bio06+bio12+bio13+bio14,data = apicalis2))
# # # vif(lm(presence~bio01+bio05+bio06+bio13+bio14,data = apicalis2))
# # # vif(lm(presence~bio05+bio06+bio13+bio14,data = apicalis2))
# 
# #We now need a time series of palaeoclimate reconstructions. In this vignette, we will use the example dataset from pastclim. This dataset only has reconstructions every 5k years for the past 20k years at 1 degree resolution, with 3 bioclimatic variables. It will suffice for illustrative purposes, but we recommend that you download higher quality datasets with pastclim for real analysis. As for the land mask, we will cut the reconstructions to cover Europe only:
# # library(pastclim)
# # climate_vars <- c('bio01','bio05','bio06','bio12','bio13','bio14')
# # climate_full <- pastclim::region_series(
# #   bio_variables = climate_vars,
# #   data = "Krapp2021",
# #   crop = vect(Aust_extent)
# # )
# 
# 
# # #####VIF
# #
# #
# # apicalis2<-data.frame(as.numeric(apicalis$class),apicalis$bio01,apicalis$bio05,apicalis$bio06,apicalis$bio12,apicalis$bio13,apicalis$bio14)
# # colnames(apicalis2)<-c('presence','bio01','bio05','bio06','bio12','bio13','bio14')
# #
# #
# # vif(lm(presence~bio01+bio05+bio06+bio12+bio13+bio14,data = apicalis2))
# # vif(lm(presence~bio01+bio05+bio06+bio13+bio14,data = apicalis2))
# # vif(lm(presence~bio05+bio06+bio13+bio14,data = apicalis2))



climate_data <- c('max_temp','min_temp', 'mean_prec')
##I couldnt get these ^^^ to work with the model code later so I swapped the Krapp files for these in the R data file
climate_vars <- c("bio05", "bio06","bio12")
climate_full <- pastclim::region_series(
  bio_variables = climate_vars,
  data = "Krapp2021",
  crop = vect(Aust_extent)
)

#Now we sample pseudo-absences (we will constraint them to be at least 70km away from any presences), selecting three times the number of presences

set.seed(123)
apicalis3 <- sample_pseudoabs_time(apicalis2,
                                  n_per_presence = 3,
                                  raster = climate_full,
                                  time_col = "time_bp",
                                  lubridate_fun = pastclim::ybp2date,
                                  method = c("dist_min", km2m(70)))

#Let’s see our presences and absences:

ggplot() +
  geom_spatraster(data = land_mask, aes(fill = land_mask_0)) +
  scale_fill_terrain_c() +
  geom_sf(data = apicalis3, aes(col = class))


#Now let’s get the climate for these location. pastclim requires a data frame with two columns with coordinates and a column of time in years before present (where negative values represent time in the past). We manipulate the sf object accordingly:

apicalis_df <- apicalis3 %>%
  dplyr::bind_cols(sf::st_coordinates(apicalis3)) %>%
  mutate(time_bp = date2ybp(time_step)) %>%
  as.data.frame() %>%
  select(-geometry)
# get climate
apicalis_df <- location_slice_from_region_series(apicalis_df,
                                                 region_series = climate_full)

# add the climate reconstructions to the sf object, and remove the time_step
# as we don't need it for modelling
apicalis4 <- apicalis3 %>%
  bind_cols(apicalis_df[, climate_vars]) %>%
  select(-time_step) %>%
  filter(!is.na(.$bio05))


#Fit the model by crossvalidation
# Next, we need to set up a recipe to define how to handle our dataset. We don’t want to transform our data, so we just need to define the formula (class is the outcome, all other variables are predictors; note that, for sf objects, geometry is automatically ignored as a predictor):

apicalis_rec <- recipe(apicalis4, formula = class ~ .)
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
apicalis_cv <- spatial_block_cv(apicalis4, v = 5)
autoplot(apicalis_cv)

##########We can now use the block CV folds to tune and assess the models:

set.seed(123)
apicalis_models <- apicalis_models %>%
  workflow_map("tune_grid",
               resamples = apicalis_cv, grid = 5,
               metrics = sdm_metric_set(), verbose = TRUE)
#Note that workflow_set correctly detects that we have no tuning parameters for glm and gam. We can have a look at the performance of our models with:
autoplot(apicalis_models)

#Now let’s create an ensemble, selecting the best set of parameters for each model (this is really only relevant for the random forest, as there were not hype-parameters to tune for the glm and gam). We will use the Boyce continuous index as our metric to choose the best random forest and boosted tree. When adding members to an ensemble, they are automatically fitted to the full training dataset, and so ready to make predictions.

apicalis_ensemble <- simple_ensemble() %>% add_member(apicalis_models, metric = "boyce_cont")

autoplot(apicalis_ensemble)
#We can now make predictions with this ensemble (using the default option of taking the mean of the predictions from each model) for the Last Glacial Maximum (LGM, 21,000 years ago).

climate_lgm <- pastclim::region_slice(
  time_bp = -21000,
  bio_variables = climate_vars,
  data = "Krapp2021",
  crop = vect(Aust_extent))

#And predict using the ensemble:
prediction_lgm <- predict_raster(apicalis_ensemble, climate_lgm)
ggplot() +
  geom_spatraster(data = prediction_lgm, aes(fill = mean)) +
  scale_fill_terrain_c() 

# ##explain importance of variables
# 
# explainer_apicalis_ens <- explain_tidysdm(apicalis_ensemble)
# 
# vip_ensemble <- model_parts(explainer = explainer_apicalis_ens)
# plot(vip_ensemble)
# 
# pdp_bio12 <- model_profile(explainer_apicalis_ens, N = 500, variables = "bio12")
# plot(pdp_bio12)
# 
# # ################  Step 2   ###################
#build and Australia shape vector
australia_shape <- st_read("C:/github/SNR_SDM/Data/raw/Australia_shape")
australia_shape <- st_transform(australia_shape, crs = crs(prediction_lgm))
#
plot(australia_shape["geometry"], col = NA, border = 'red', lwd = 2, main = "Outline of Australia")

# Create a raster template
rast_template <- rast(nrows=100, ncols=100, ext=ext(australia_shape))  # Adjust resolution as needed

# Rasterize the vector outline
Aust_raster <- rasterize(australia_shape, rast_template, field="AUS_CODE21", background=NA)

# Plot the prediction_lgm raster
plot(prediction_lgm, main = "apicalis -30ka")


  

# Check if the geometries are polygons and handle accordingly
 #if (any(st_geometry_type(australia_shape) %in% c("POLYGON", "MULTIPOLYGON"))) {
 plot(st_geometry(australia_shape), col = NA, border = 'red', lwd = 2, add = TRUE) 
 #else {
#  coords <- st_coordinates(st_geometry(australia_shape))
 # lines(coords, col = 'red', lwd = 2)
#} 
# Overlay the vector outline
#plot(st_geometry(australia_shape))
#

