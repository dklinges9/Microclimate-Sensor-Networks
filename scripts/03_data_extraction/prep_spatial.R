# David Klinges
# This script pulls in and preps remote sensing data for the Ambalavero landscape, 
# in order to generate a semi-stratified set of points at which to deploy 
# microclimate loggers


## .... Load dependencies ---------
pkgs <- c("crayon", "readr", "magrittr", "raster", "terra", "landscapemetrics", "geodata")

for (i in seq_along(pkgs)) {
  suppressPackageStartupMessages(
    suppressWarnings(library(pkgs[i], character.only = TRUE))
  )
  if (i == length(pkgs)) { rm(pkgs, i) }
}


# Confirm script should be run
if (program_rerun) { # running the code non-sequentially (e.g. running prep_spatial.R first) will break the program
  cat(red("program_rerun set to TRUE, so you should not need to re-run prep_spatial.R. Do you wish to continue? (Y/N): "))
  ans1 <- readline(" ")
  
  if (!tolower(ans1) %in% c("n", "no", "y", "yes")) {
    stop("Inappropriate input. Must be one of: yes, YES, Y, y, no, NO, N, n.\n")
  }
  
  if (tolower(ans1) %in% c("n", "no")) {
    cat(red("Skipping prep_spatial.R. \n"))
    continue <- FALSE
  }
  if (tolower(ans1) %in% c("y", "yes")) {
    cat(red("Continuing program.\n"))
    continue <- TRUE
  }
} else {
  continue <- TRUE
}

if (continue) {
  
  cat(red("Prepping spatial data...\n"))
  
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
    spatial_extent_buffer <- terra::project(spatial_extent_buffer, "EPSG:4326")
  }
  
  ## .... Import data layers ------------
  
  ## ......Topography -----------
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
  
  ## ......Landcover ------------
  if ("landcover" %in% chosen_layers) {
    ## ESA CCI Land cover
    landcover_global <- terra::rast("data/spatial_drivers/landcover/original/esa_cci/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
    landcover_link <- read_csv("data/spatial_drivers/landcover/original/esa_cci/esa_cci_landcover_link.csv", show_col_types = FALSE)
  }
  
  ## ......Macroclimate ---------------
  if ("macroclimate" %in% chosen_layers) {
    # Because WorldClim is made available in WGS84, need to use 
    # spatial_extent_buffer (which is in WGS84) for extracting

    # Download = F entails that the function first searches the specified `path`
    # to see if tile already downloaded
    macroclimate <- geodata::worldclim_tile(
      var = 'bio', lon = ext(spatial_extent_buffer)[1], 
      lat = ext(spatial_extent_buffer)[3], download = F, 
      path = "data/spatial_drivers/macroclimate/worldclim/")[[1]] %>% 
      terra::resample(spatial_extent_buffer)
    
    # Download global bioclimatic data from worldclim
    # Because WorldClim tiles are in 30-degree tiles, a spatial extent may fall
    # across multiple scenes. Max of 4 scenes. Pull out each corner of spatial_extent
    # and attempt to download each scene (if scene already downloaded, it will
    # just load a previously downloaded scene)
    macroclimate <- geodata::worldclim_tile(
      var = 'bio', lon = ext(spatial_extent_buffer)[2], 
      lat = ext(spatial_extent_buffer)[3], download = F, 
      path = "data/spatial_drivers/macroclimate/worldclim/")[[1]] %>% 
      terra::resample(spatial_extent_buffer) %>% 
      terra::merge(macroclimate)
       
    macroclimate <- geodata::worldclim_tile(
      var = 'bio', lon = ext(spatial_extent_buffer)[1], 
      lat = ext(spatial_extent_buffer)[4], download = F, 
      path = "data/spatial_drivers/macroclimate/worldclim/")[[1]] %>% 
      terra::resample(spatial_extent_buffer) %>% 
      terra::merge(macroclimate)

    macroclimate <- geodata::worldclim_tile(
      var = 'bio', lon = ext(spatial_extent_buffer)[2], 
      lat = ext(spatial_extent_buffer)[4], download = F, 
      path = "data/spatial_drivers/macroclimate/worldclim/")[[1]] %>% 
      terra::resample(spatial_extent_buffer) %>% 
      terra::merge(macroclimate)
  }
  
  ## ......Soil Temp ---------------
  if ("soiltemp" %in% chosen_layers) {
    # Only download if does not already exist
    if (!file.exists("data/spatial_drivers/microclimate/soil_bio/SBIO1_0_5cm_Annual_Mean_Temperature.tif")) {
      cat(red("Global soil temperature maps must be downloaded from Zenodo in order to include. Continue? (Y/N): "))
      ans1 <- readline(" ")
      if (!tolower(ans1) %in% c("n", "no", "y", "yes")) {
        stop("Inappropriate input. Must be one of: yes, YES, Y, y, no, NO, N, n.\n")
      }
      
      if (tolower(ans1) %in% c("n", "no")) {
        cat(red("NOT downloading global BIO1 soil temperature, and so removing soiltemp from `chosen_layers`\n"))
        chosen_layers <- chosen_layers[chosen_layers != "soiltemp"]
      }
      if (tolower(ans1) %in% c("y", "yes")) {
        cat(red("Downloading global BIO1 soil temperature.... \n"))
        if (!file.exists("data/spatial_drivers/microclimate/soil_bio")) {
          cat(red("Creating folder 'data/spatial_drivers/microclimate/soil_bio'\n"))
          dir.create("data/spatial_drivers/microclimate/soil_bio", recursive = TRUE)
        }
        zen4R::download_zenodo(doi = "10.5281/zenodo.7134169", 
                               path = "data/spatial_drivers/microclimate/soil_bio/", 
                               files = "SBIO1_0_5cm_Annual_Mean_Temperature.tif",
                               timeout = 1000)
        soiltemp <- rast("data/spatial_drivers/microclimate/soil_bio/SBIO1_0_5cm_Annual_Mean_Temperature.tif")
      }
    } else {
      soiltemp <- rast("data/spatial_drivers/microclimate/soil_bio/SBIO1_0_5cm_Annual_Mean_Temperature.tif")
    }
  }

  ## ....Import custom layers ------------
  
  if (any(complete.cases(custom_layers_test))) {
    custom_layers_r <- lapply(custom_layers_test, function(l) {
      if (file.exists(l)) {
        cat(red("Reading in custom layers", l, "\n"))
        return(rast(l))
      } else {
        cat(red("No file found for filepath", l, "so ignoring.\n"))
        return(NA)
      }
    })
    custom_layers_r <- custom_layers_r[!is.na(custom_layers_r)]
    names(custom_layers_r) <- custom_layers_names
  }
  
  
  ## Data curation ---------
  
  ## .... Land cover ------------
  
  if ("landcover" %in% chosen_layers) {
    landcover <- terra::crop(landcover_global, spatial_extent_buffer)
    landcover <- terra::project(landcover, projection, method = "near")
    
    # Save landcover file. But first, check if already exists and ask user if they
    # want to overwrite
    save_raster(landcover, 
                "data/spatial_drivers/landcover/derivative/landcover_", 
                filepattern)
  }
  
  ## .... Macroclimate ------------
  
  if ("macroclimate" %in% chosen_layers) {
    macroclimate <- terra::crop(macroclimate, spatial_extent_buffer)
    macroclimate <- terra::project(macroclimate, projection, method = "bilinear")
    
    # Save landcover file. But first, check if already exists and ask user if they
    # want to overwrite
    save_raster(macroclimate, 
                "data/spatial_drivers/macroclimate/derivative/macroclimate_", 
                filepattern)
  }
  
  ## .... Soil Temp ------------
  
  if ("soiltemp" %in% chosen_layers) {
    soiltemp <- terra::crop(soiltemp, spatial_extent_buffer)
    soiltemp <- terra::project(soiltemp, projection, method = "bilinear")
    
    # Save landcover file. But first, check if already exists and ask user if they
    # want to overwrite
    save_raster(soiltemp, 
                "data/spatial_drivers/soiltemp/derivative/soiltemp_", 
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
  cat(red("Standardizing spatial information across chosen layers...\n"))
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
