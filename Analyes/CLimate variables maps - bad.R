# Ensure necessary packages are loaded
library(terra)
library(ggplot2)
library(fs)

# Define the output directory
output_base_dir <- "C:/github/SNR_SDM/Results/Climate_Variables_Maps"

# Climate variables to map
climate_vars <- c("bio05", "bio06", "bio12")

# Create directories for each variable if they don't already exist
for (var in climate_vars) {
  dir_create(file.path(output_base_dir, var))
}

# Iterate over each climate variable
for (var_index in seq_along(climate_vars)) {
  variable <- climate_vars[var_index]
  climate_raster <- climate_full[[var_index]]  # Extract SpatRaster for the variable
  message("Processing variable: ", variable)
  
  for (timestep in 760:800) {  # Loop over time slices
    year_bp <- time(climate_raster)[timestep] - 1950  # Convert BP to calendar years
    message("Processing time slice: ", year_bp)
    
    # Extract raster for the current time slice
    climate_layer <- climate_raster[[timestep]]
    if (is.null(climate_layer)) {
      message("No data for timestep: ", timestep)
      next
    }
    
    # Plot using geom_spatraster
    p <- ggplot() +
      geom_spatraster(data = climate_layer) +
      scale_fill_terrain_c(name = paste(variable, "Value")) +
      ggtitle(paste(variable, "- Year BP:", year_bp)) +
      theme_minimal()
    
    # Save the plot
    output_path <- file.path(output_base_dir, variable, paste0(variable, "_", year_bp, "BP.png"))
    ggsave(filename = output_path, plot = p, width = 7, height = 7)
  }
}

# Final success message
message("Climate variable maps generated successfully!")
