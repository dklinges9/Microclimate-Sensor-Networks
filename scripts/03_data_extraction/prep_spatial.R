# David Klinges
# This script pulls in and preps remote sensing data for the Ambalavero landscape, 
# in order to generate a semi-stratified set of points at which to deploy 
# microclimate loggers


## .... Load dependencies ---------
pkgs <- c("crayon", "readr", "raster", "terra", "landscapemetrics")

for (i in seq_along(pkgs)) {
  suppressPackageStartupMessages(
    suppressWarnings(library(pkgs[i], character.only = TRUE))
  )
  if (i == length(pkgs)) { rm(pkgs, i) }
}


# Confirm script should be run
if (program_rerun) {
  cat(red("program_rerun set to TRUE, so you should not need to re-run prep_spatial.R. Do you wish to continue? (Y/N): "))
  ans1 <- readline(" ")
  
  if (!tolower(ans1) %in% c("n", "no", "y", "yes")) {
    stop("Inappropriate input. Must be one of: yes, YES, Y, y, no, NO, N, n.\n")
  }
  
  if (tolower(ans1) %in% c("n", "no")) {
    cat("Skipping prep_spatial.R. \n")
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
  
  cat("Prepping spatial data...\n")
  
  
  ## Workspace prep -----------
  
  ## .... Temporary extent with buffer ---------
  
  if (projection_units == "dd") {
    spatial_extent_buffer <- ext(spatial_extent[1] - 0.5,
                                 spatial_extent[2] + 0.5,
                                 spatial_extent[3] - 0.5,
                                 spatial_extent[4] + 0.5) 
  }
  if (projection_units == "m") {
    
    spatial_extent_buffer <- rast(extent = ext(spatial_extent[1] - 10000,
                                               spatial_extent[2] + 10000,
                                               spatial_extent[3] - 10000,
                                               spatial_extent[4] + 10000) )
    crs(spatial_extent_buffer) <- projection
    spatial_extent_buffer <- terra::project(spatial_extent_buffer, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  }
  
  ## .... Import data layers ------------
  
  ## Topography 
  if ("elevation" %in% chosen_layers) {
    elevation <- terra::rast(paste0("data/spatial_drivers/topography/derivative/dem_", 
                                    round(chosen_rez, 4), 
                                    "_", filepattern, ".tif"))
  }
  
  if ("slope" %in% chosen_layers) {
    slope <- terra::rast(paste0("data/spatial_drivers/topography/derivative/slope_", 
                                round(chosen_rez, 4), 
                                "_", filepattern, ".tif"))
  }
  
  if ("aspect" %in% chosen_layers) {
    aspect <- terra::rast(paste0("data/spatial_drivers/topography/derivative/aspect_", 
                                 round(chosen_rez, 4), 
                                 "_", filepattern, ".tif"))
  }
  
  if ("landcover" %in% chosen_layers) {
    ## ESA CCI Land cover
    landcover_global <- terra::rast("data/spatial_drivers/landcover/original/esa_cci/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
    landcover_link <- read_csv("data/spatial_drivers/landcover/original/esa_cci/esa_cci_landcover_link.csv", show_col_types = FALSE)
  }
  
  ## ....Import custom layers ------------
  
  if (any(complete.cases(custom_layers_test))) {
    custom_layers_r <- lapply(custom_layers_test, function(l) {
      if (file.exists(l)) {
        cat("Reading in custom layers", l, "\n")
        return(rast(l))
      } else {
        cat("No file found for filepath", l, "so ignoring.\n")
        return(NA)
      }
    })
    custom_layers_r <- custom_layers_r[!is.na(custom_layers_r)]
    names(custom_layers_r) <- custom_layers_names
  }
  
  
  ## Data curation ---------
  
  ## .... Land cover ------------
  
  if ("landcover" %in% chosen_layers) {
    if (projection_units == "dd") {
      landcover <- terra::crop(landcover_global, spatial_extent_buffer)
      landcover <- terra::project(landcover, projection, method = "near")
      
    } else {
      landcover <- terra::crop(landcover_global, spatial_extent_buffer)
      landcover <- terra::project(landcover, projection, method = "near")
    }
    
    # Save landcover file. But first, check if already exists and ask user if they
    # want to overwrite
    save_raster(landcover, 
                "data/spatial_drivers/landcover/deriative/landcover_", 
                filepattern)
  }
  
  ## .... Custom layers -----------
  
  if (any(complete.cases(custom_layers_test))) {
    custom_layers_r <- lapply(custom_layers_r, function(r) {
      r <- terra::crop(r, spatial_extent_buffer)
      return(r)
    })
  }
  
  
  ## Combine layers into single spatRaster stack -----------
  
  # Get chosen layers (minus the custom layer names) as a list
  if (any(complete.cases(custom_layers_test))) {
    chosen_layers_list <- mget(chosen_layers[chosen_layers != custom_layers_names])
    chosen_layers_list <- c(chosen_layers_list, custom_layers_r)
  } else {
    chosen_layers_list <- mget(chosen_layers)
  }
  
  # Get resolutions of all chosen layers
  layer_resolutions <- lapply(chosen_layers_list, function(r) {
    return(res(r)[1])
  })
  
  # Find layer with the finest resolution
  finest_rez_layer <- chosen_layers_list[which.min(unlist(layer_resolutions))][[1]]
  
  # Resample all layers
  cat("Standardizing spatial information across chosen layers...\n")
  chosen_layers_list <- lapply(chosen_layers_list, function(x) {
    # Resample all layers to the layer with the finest resolution. But, use 
    # method = "near" so we aren't interpolating categorical data or making 
    # unsupported assumptions about smooth distributions
    return(terra::resample(x, finest_rez_layer, method = "near"))
  })
  
  layers <- rast(chosen_layers_list)
  
  layers <- crop(layers, spatial_extent)
  
  ## Write out layers
  save_raster(layers, 
              "data/spatial_drivers/combined/layers_",
              paste0(round(chosen_rez, 4), "_", filepattern))
}
