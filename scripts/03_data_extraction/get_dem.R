## David Klinges
## This script queries a DEM of desired resolution and extent

library(elevatr)
library(microclima)
library(raster)
library(terra)

# Confirm script should be run
if (program_rerun) {
  ans1 <- readline(paste0("program_rerun set to TRUE, so you should not need to re-run get_dem.R. Do you wish to continue? (Y/N): "))
  
  if (!tolower(ans1) %in% c("n", "no", "y", "yes")) {
    stop("Inappropriate input. Must be one of: yes, YES, Y, y, no, NO, N, n.\n")
  }
  
  if (tolower(ans1) %in% c("n", "no")) {
    cat("Skipping get_dem.R. \n")
    continue <- FALSE
  }
  if (tolower(ans1) %in% c("y", "yes")) {
    cat("Continuing program.\n")
    continue <- TRUE
  }
} else {
  continue <- TRUE
}

if (continue) {
  spatial_extentr <- terra::rast(ext(spatial_extent))
  crs(spatial_extentr) <- projection
  
  cat("Getting DEM....\n")
  dem <- rast(get_dem(r = raster(spatial_extentr), resolution = chosen_rez))
  
  # Specify string for naming output files
  if (complete.cases(landscape_name)) {
    filepattern <- landscape_name
  } else {
    filepattern <- paste(round(spatial_extent, 0), collapse = "_")
  }
  
  cat("writing DEM...\n")
  # Save DEM file. But first, check if already exists and ask user if they
  # want to overwrite
  save_raster(dem, 
              "data/spatial_drivers/topography/derivative/dem_", 
              paste0(round(chosen_rez, 4), "_", filepattern))
  
  ## Slope and Aspect -------------
  
  if ("slope" %in% chosen_layers) {
    cat("Calculating slope....\n")
    
    slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "degrees")
    
    cat("writing slope raster...\n")
    save_raster(slope, 
                "data/spatial_drivers/topography/derivative/slope_",
                paste0(round(chosen_rez, 4), "_", filepattern))
  }
  
  if ("aspect" %in% chosen_layers) {
    cat("Calculating aspect....\n")
    aspect <- terra::terrain(dem, v = "aspect")
    
    cat("writing aspect raster...\n")
    save_raster(aspect, 
                "data/spatial_drivers/topography/derivative/aspect_",
                paste0(round(chosen_rez, 4), "_", filepattern))
  }  
  
  cat("Topographic variables - OK!")
  
}
