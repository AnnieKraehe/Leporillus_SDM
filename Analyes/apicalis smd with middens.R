# Load Required Libraries
install.packages("exactextractr")
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
library(tidymodels)
library(tidysdm)
library(pastclim)
library(rworldmap)
library(readr)
library(oz)
library(DALEX)
library(car)
library(tidyterra)
library(exactextractr)


# Set Working Directory
setwd("C:/github/SNR_SDM/Data/processed")

# Load Data
apicalis <- read_csv("apicalis.csv")
midden_data <- read_csv("datedmiddens.csv")

# Convert Apicalis Data to Spatial Features
apicalis <- st_as_sf(apicalis, coords = c("longitude", "latitude")) |>
  mutate(time_bp = time_bp)
st_crs(apicalis) <- 4326

# Process Midden Data
midden_data <- midden_data %>%
  mutate(time_bp = -time_bp,  # Convert to positive values for time BP
         time_step = round(time_bp, -3))  # Round to nearest 1,000 years

# Load Land Mask and Crop to Australia
land_mask <- pastclim::get_land_mask(time_bp = 0, dataset = "Krapp2021")
Aust_extent <- terra::ext(109.4919, 152.6379, -42.8198, -8.1955)
land_mask <- crop(land_mask, vect(Aust_extent))
land_mask_layer <- land_mask[[1]]

# Load Climate Data
climate_vars <- c('bio05', 'bio06', 'bio12')
climate_full <- pastclim::region_series(
  bio_variables = climate_vars,
  data = "Krapp2021",
  crop = vect(Aust_extent)
)

# Sample Pseudo-Absences
set.seed(123)
apicalis <- sample_pseudoabs_time(apicalis,
                                  n_per_presence = 3,
                                  raster = climate_full,
                                  time_col = "time_bp",
                                  lubridate_fun = pastclim::ybp2date,
                                  method = c("dist_min", km2m(70)))

# Extract Climate Data for Apicalis Points
apicalis_df <- apicalis %>%
  dplyr::bind_cols(sf::st_coordinates(apicalis)) %>%
  mutate(time_bp = date2ybp(time_step)) %>%
  as.data.frame() %>%
  select(-geometry)
apicalis_df <- location_slice_from_region_series(apicalis_df, region_series = climate_full)
apicalis <- apicalis %>%
  bind_cols(apicalis_df[, climate_vars]) %>%
  select(-time_step) %>%
  filter(!is.na(.$bio05))

# Define Recipe
apicalis_rec <- recipe(apicalis, formula = class ~ .)

# Define Models and Workflow
apicalis_models <- workflow_set(
  preproc = list(default = apicalis_rec),
  models = list(
    glm = sdm_spec_glm(),
    gam = sdm_spec_gam(),
    rf = sdm_spec_rf(),
    gbm = sdm_spec_boost_tree()
  ),
  cross = TRUE
) %>%
  update_workflow_model("default_gam",
                        spec = sdm_spec_gam(),
                        formula = gam_formula(apicalis_rec)
  ) %>%
  option_add(control = control_ensemble_grid())

# Cross-Validation and Model Tuning
set.seed(1005)
apicalis_cv <- spatial_block_cv(apicalis, v = 5)
set.seed(123)
apicalis_models <- apicalis_models %>%
  workflow_map("tune_grid",
               resamples = apicalis_cv, grid = 5,
               metrics = sdm_metric_set(), verbose = TRUE)

# Create Ensemble
apicalis_ensemble <- simple_ensemble() %>% add_member(apicalis_models, metric = "boyce_cont")

# Extract the subset of time steps we're using
time_steps_subset <- time(climate_full[[1]])[760:800]
time_steps_bp <- -(time_steps_subset - 1950)  # Convert to positive BP values

# Generate Maps for Each Time Step
walk(seq_along(time_steps_bp), function(idx) {
  year_bp <- time_steps_bp[idx]  # Get the correct year BP for the current index
  
  # Filter Midden Points for the Current Time Step
  current_middens <- midden_data %>% filter(time_step == round(year_bp, -3))
  
  # Create Climate Snapshot for the Current Time Step
  climate_snapshot <- rast(list(climate_full[[1]][[760 + idx - 1]], 
                                climate_full[[2]][[760 + idx - 1]], 
                                climate_full[[3]][[760 + idx - 1]]))
  names(climate_snapshot) <- c("bio05", "bio06", "bio12")
  
  # Predict Species Occurrence for the Current Climate Snapshot
  prediction <- predict_raster(apicalis_ensemble, climate_snapshot)
  
  # Plot Predictions with Midden Points
  p <- ggplot() +
    geom_spatraster(data = prediction, aes(fill = mean)) +
    scale_fill_terrain_c(name = "Probability\nof occurrence") +
    geom_point(data = current_middens, aes(x = longitude, y = latitude),
               color = "red", size = 2, alpha = 0.7) +
    ggtitle(paste0("Krapp2021 Apicalis + middens Years BP: ", round(year_bp, -3)))
  
  # Save the Plot
  ggsave(filename = paste0("C:/github/SNR_SDM/Results/SDM maps with middens/apicalis/apicalis_map_", 
                           round(year_bp, -3), ".png"),
         plot = p,
         width = 7, height = 7)
})


########################################3

