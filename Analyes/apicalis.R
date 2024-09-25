# #install.packages("tidymodels")
#install.packages("rworldmap")
#install.packages("oz")
install.packages("fs")
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
library(gtools)
library(fs)
#pastclim::download_dataset("Krapp2021")

setwd("C:/github/SNR_SDM/Data/processed")

# Read in the 'apicalis.csv' file into a data frame called 'apicalis'
# Object type: data frame
apicalis <- read_csv("apicalis.csv")
print(apicalis)


# Convert the 'apicalis' data frame to an sf (simple features) object,specifying 'longitude' and 'latitude' columns as coordinates (X and Y dimensions). Also, create a new column 'time_bp' which is the negative of the existing 'time_bp' column to convert age to time before present. This is the Z dimension
# Object type: sf (simple features) object
apicalis2 <- st_as_sf(apicalis, coords = c("longitude", "latitude")) 

# Set the Coordinate Reference System (CRS) of 'apicalis2' to GDA2020, which has the EPSG code 4326 
st_crs(apicalis2) <- 4326

# Create a land mask for the present time using the 'Krapp2021' dataset from the 'pastclim' package
# Object type: SpatRaster where X =longitude, Y = latitude, Z = time
land_mask <- pastclim::get_land_mask(time_bp =, dataset = "Krapp2021")

# Create an extent object for Australia using specified minimum and maximum X (longitude) and Y (latitude) coordinates (saved in "C:/github/SNR_SDM/Data/raw/Australia_Extent.txt")
# Object type: Extent (terra package)
Aust_extent <- terra::ext(110, 152.5, -42.5, -7.5)

# Crop the 'land_mask' SpatRaster to the extent of Australia using the 'Aust_extent' object made in the previous step
# Object type: SpatRaster (cropped)
land_mask <- crop (land_mask,vect(Aust_extent))


##### Plot the data using ggplot2
ggplot() +
  # Add the land mask as a spatial raster layer
  geom_spatraster(data = land_mask, aes()) +
  # Use a terrain color scale for the land mask
  scale_fill_terrain_c() +
  # Add the 'apicalis2' sf object as spatial features, colouring by the 'time_bp' column
  geom_sf(data = apicalis2, aes(col = time_bp))


#Add Palaeoview Data
##-------------Add bio05 palaeoview data

# Load the bio05 raster data from the specified file path. "Bio5" is a SpatRaster Loaded from a NetCDF file. It contains raster data for the bio05 variable from the palaeoview dataset.
bio5 <- rast ("C:/github/SNR_SDM/Data/raw/Palaeoview_data/bio05.nc")

# Set the time_bp attribute for bio5, creating a sequence of time before present from -21000 to -100 in steps of 20. This represents the temporal dimension
time_bp(bio5) <- seq(-21000, -100, by = 20)

# Write the processed bio5 data (spatraster) to a new NetCDF file at the specified path. The file will be overwritten if it already exists. The z-dimension is named 'time', and the variable is named 'bio05'
writeCDF(bio5, "C:/github/SNR_SDM/Data/processed/paleoview/bio5_time.nc", overwrite = TRUE,
         zname = "time", varname = "bio05")

# Extract a time series of bio05 data for the specified region (Australia) using the region_series function from the pastclim package. Incorporate Paleoview data by setting 'dataset' to 'custom', and import it by specifying the path to the NetCDF file made in the previous step. The data is cropped to the extent of Australia defined by the Aust_extent object
climate_bio05 <- pastclim::region_series(
 bio_variables = 'bio05',
 dataset = "custom",
 path_to_nc = "C:/github/SNR_SDM/Data/processed/paleoview/bio5_time.nc",
  crop = vect(land_mask)
 )
#add bio06 palaeoview data (same as for Bio5)
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
#add bio12 palaeoview data (same as for Bio5 and bio6)
bio12 <- rast ("C:/github/SNR_SDM/Data/raw/Palaeoview_data/bio12.nc")
time_bp(bio12) <- seq(-21000, -100, by = 20)
#convert 94 infinite values into 0 so that the file will write
bio12[is.infinite(bio12)] <- 0
writeCDF(bio12, "C:/github/SNR_SDM/Data/processed/paleoview/bio12_time.nc", overwrite = TRUE,
         zname = "time", varname = "bio12")

climate_bio12 <- pastclim::region_series(
  bio_variables = 'bio12',
  dataset = "custom",
  path_to_nc = "C:/github/SNR_SDM/Data/processed/paleoview/bio12_time.nc",
  crop = vect(Aust_extent)
)

#import new landmask 
# Define the directory containing the landmask files and list all files that match the regular expression ".+ka_land.asc"
masks <- "C:/github/SNR_SDM/Data/raw/landmask/" %>% 
  dir_ls(regexp = ".+ka_land.asc") %>% 
  gtools::mixedsort() %>%  # Sort the filenames in a human-readable order
  .[1:24] %>%  # Select the first 24 files from the sorted list
  map(rast) %>%  # Load each selected file as a raster
  map(~ project(.x, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%  # Set the CRS for each raster to WGS84
  map(aggregate, fact = 10) %>%  # Reduce the resolution of each raster by a factor of 10
  map(crop, y = Aust_extent)  # Crop each raster to the extent of Australia defined by Aust_extent




# mask climate_full subdatasets to land_mask
climate_full$`Maximum_Temperature_Annual_21000BP-100BP_step20_size30` <- mask(climate_full$`Maximum_Temperature_Annual_21000BP-100BP_step20_size30`, land_mask2)

climate_full <- sds(bio5, bio6, bio12) |> terra::crop (land_mask)

saveRDS(climate_full, "C:/github/SNR_SDM/Data/processed/paleoview/climate_full.rds")

#VW: once you have finished one major work chunk, you can start a new file for the next discrete part of the workflow. In this case, you have downloaded everything and all the data are already in Data/processed/palaeoview, but sometimes you can just save an output by going save (climate_bio1, climate_bio2, file="Climate_bio.rda")


#sample pseudo-absences (we will constraint them to be at least 70km away from any presences), selecting three times the number of presences


apicalis3 <- sample_pseudoabs_time(apicalis2 |>
                                     #filter apicalis2 to within 21ka
                                     filter(time_bp > -21000, time_bp < -100),
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

