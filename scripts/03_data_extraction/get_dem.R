## This script queries a DEM of desired resolution and extent

pkgs <- c("crayon", "elevatr", "raster", "terra")

options("rgdal_show_exportToProj4_warnings"="none") # suppresses rgdal deprecation warning

for (i in seq_along(pkgs)) {
  suppressPackageStartupMessages(
    suppressWarnings(library(pkgs[i], character.only = TRUE))
  )
  if (i == length(pkgs)) { rm(pkgs, i) }
}

# As querying DEM may be time consuming, first check to see if program_rerun, or
# if DEM of same name already exists, and if so then prompt user
if (program_rerun | file.exists(paste0("data/spatial_drivers/topography/derivative/dem_", 
                                       round(chosen_rez, 4), "_", filepattern, ".tif"))) {
  cat(red("Either `program_rerun` is set to TRUE and/or DEM file", paste0("data/spatial_drivers/topography/derivative/dem_", 
                                                                          round(chosen_rez, 4), "_", filepattern, ".tif"), 
          "already exists, so you should not need to re-run get_dem.R. Do you wish to continue? (Y/N): "))
  ans1 <- readline(" ")
  
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
  
  spatial_extentr <- terra::rast(ext(spatial_extent), resolution = chosen_rez)
  crs(spatial_extentr) <- projection
  
  cat("Getting DEM....\n")
  
  if (projection_units == "m") {
    dem <- get_dem(r = terra::rast(spatial_extentr), resolution = chosen_rez)
  }
  
  if (projection_units == "dd") {
    dem <- get_dem(r = terra::rast(spatial_extentr), resolution = chosen_rez * 120000)
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
