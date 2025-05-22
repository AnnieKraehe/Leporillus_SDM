
# Load required libraries
library(terra)
library(ggplot2)
library(ggspatial)
library(rlang)
library(dplyr)

# Define file paths
base_path <- "C:/github/SNR_SDM/Data/raw/Krapp2021"
bio05_file <- file.path(base_path, "Krapp2021_bio05_v1.0.0.nc")
bio06_file <- file.path(base_path, "Krapp2021_bio06_v1.0.0.nc")
bio12_file <- file.path(base_path, "Krapp2021_bio12_v1.0.0.nc")

# Define Australia's geographic extent
Aust_extent <- ext(110, 152.5, -42.5, -7.5)

# Load raster stacks
bio05_stack <- rast(bio05_file)
bio06_stack <- rast(bio06_file)
bio12_stack <- rast(bio12_file)

# Clip each raster stack to Australia's extent
bio05_australia <- crop(bio05_stack, Aust_extent)
bio06_australia <- crop(bio06_stack, Aust_extent)
bio12_australia <- crop(bio12_stack, Aust_extent)

# Define the desired time slices (in years BP)
desired_times_bp <- seq(0, 40000, by = 1000)
desired_times_cal <- 1970 - desired_times_bp
time_info_cal <- time(bio05_australia)

# Match desired times with available time layers
time_indices <- sapply(desired_times_cal, function(year) which.min(abs(time_info_cal - year)))

# Extract the baseline layer for 1970
baseline_index <- which(time_info_cal == 1970)
if (length(baseline_index) == 0) stop("1970 (modern layer) not found.")
bio05_baseline <- bio05_australia[[baseline_index]]
bio06_baseline <- bio06_australia[[baseline_index]]
bio12_baseline <- bio12_australia[[baseline_index]]

# Define main folder
deviation_main_folder <- "C:/github/SNR_SDM/Results/Climate_Deviation"
dir.create(deviation_main_folder, recursive = TRUE, showWarnings = FALSE)

# Function to calculate ranges
calculate_range <- function(r_stack, baseline, indices) {
  range_vals <- sapply(indices, function(i) {
    dev <- r_stack[[i]] - baseline
    c(min = minmax(dev)[1], max = minmax(dev)[2])
  })
  c(min(range_vals[1, ]), max(range_vals[2, ]))
}

# Calculate ranges
bio05_range <- calculate_range(bio05_australia, bio05_baseline, time_indices)
bio06_range <- calculate_range(bio06_australia, bio06_baseline, time_indices)
bio12_range <- calculate_range(bio12_australia, bio12_baseline, time_indices)

# Improved plotting function
calculate_and_save_deviation <- function(r_stack, baseline, times_bp, indices, var_name, folder, range_vals) {
  out_dir <- file.path(folder, var_name)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  for (i in seq_along(indices)) {
    layer <- r_stack[[indices[i]]] - baseline
    names(layer) <- var_name
    df <- as.data.frame(layer, xy = TRUE, na.rm = TRUE)

    label <- case_when(
      var_name == "bio05" ~ "Max\nTemp (°C)",
      var_name == "bio06" ~ "Min\nTemp (°C)",
      var_name == "bio12" ~ "Annual\nRainfall (mm)"
    )

    p <- ggplot(df, aes(x = x, y = y, fill = .data[[var_name]])) +
      geom_tile() +
      scale_fill_gradientn(
        colours = rev(rainbow(7)),
        limits = range_vals,
        name = gsub("\\n", "\n", label)
      ) +
      labs(
        title = paste0("Deviation in ", gsub("\\n", " ", label), " – ", times_bp[i], " years BP"),
        x = "Longitude", y = "Latitude"
      ) +
      coord_fixed() +
      theme_classic() +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )

    ggsave(
      filename = file.path(out_dir, paste0(var_name, "_deviation_", times_bp[i], ".png")),
      plot = p,
      width = 7, height = 7
    )
  }
}

# Generate plots
calculate_and_save_deviation(bio05_australia, bio05_baseline, desired_times_bp, time_indices, "bio05", deviation_main_folder, bio05_range)
calculate_and_save_deviation(bio06_australia, bio06_baseline, desired_times_bp, time_indices, "bio06", deviation_main_folder, bio06_range)
calculate_and_save_deviation(bio12_australia, bio12_baseline, desired_times_bp, time_indices, "bio12", deviation_main_folder, bio12_range)

# Save metadata explainer in the root results folder
plot_path <- file.path(deviation_main_folder, "explainer.txt")  # Dummy file to anchor output location
assign("plot_path", plot_path, envir = .GlobalEnv)
source("C:/github/create_explainer/create_explainer.R")

