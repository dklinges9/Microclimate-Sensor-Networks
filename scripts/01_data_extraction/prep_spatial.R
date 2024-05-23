# David Klinges
# This script pulls in and preps remote sensing data for the Ambalavero landscape, 
# in order to generate a semi-stratified set of points at which to deploy 
# microclimate loggers


cat("Prepping spatial data...\n")


## Workspace prep -----------

## .... Load dependencies ---------
library(tidyverse)
library(raster)
library(terra)
library(landscapemetrics)

## ....Temporary extent with buffer ---------

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

if ("proportion_forest" %in% chosen_layers) {
  ## Hansen global forest change layers
  hansen_lastYear_scene <- terra::rast("data/environmental/vegetation/global_forest_change/original/Hansen_GFC-2020-v1.8_last_20S_040E.tif")
  hansen_firstYear_scene <- terra::rast("data/environmental/vegetation/global_forest_change/original/Hansen_GFC-2020-v1.8_treecover2000_20S_040E.tif")
  hansen_lossyear <- terra::rast("data/environmental/vegetation/global_forest_change/original/Hansen_GFC-2020-v1.8_lossyear_20S_040E.tif")
}


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
  landcover_link <- read_csv("data/spatial_drivers/landcover/original/esa_cci/esa_cci_landcover_link.csv")
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
  if (file.exists(paste0("data/spatial_drivers/landcover/deriative/landcover_",
                         filepattern, ".tif"))) {
    ans1 <- readline(paste0("File ", 
                            paste0("data/spatial_drivers/landcover/deriative/landcover_",
                                           filepattern, ".tif"), 
                            " already exists. Overwrite? (Y/N): "))
    
    if (tolower(ans1) %in% c("y", "yes")) {
      cat("File overwritten, continuing program.\n")
      writeRaster(landcover, paste0("data/spatial_drivers/landcover/deriative/landcover_",
                                    filepattern, ".tif"), overwrite = T)
    }
    if (tolower(ans1) %in% c("n", "no")) {
      cat("File not overwritten, continuing program.\n")
    }
  } else {
    writeRaster(landcover, paste0("data/spatial_drivers/landcover/deriative/landcover_",
                                  filepattern, ".tif"))
  }
}

## Combine layers into single spatRaster stack -----------

# Get all chosen layers as a list
chosen_layers_list <- mget(chosen_layers)
# Get resolutions of all chosen layers
layer_resolutions <- lapply(chosen_layers_list, res)

# Find layer with the finest resolution
finest_rez_layer <- chosen_layers_list[which.min(unlist(layer_resolutions))][[1]]

# Resample all layers
chosen_layers_list <- lapply(chosen_layers_list, function(x) {
  # Resample all layers to the layer with the finest resolution. But, use 
  # method = "near" so we aren't interpolating categorical data or making 
  # unsupported assumptions about smooth distributions
  return(terra::resample(x, finest_rez_layer, method = "near"))
})

layers <- rast(chosen_layers_list)


