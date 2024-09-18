## Check to confirm that chosen parameters conform to program guidelines

cat("Checking parameters...\n")

## Naming parameters --------

# Specify string for naming output files
if (complete.cases(landscape_name)) {
  filepattern <- landscape_name
} else {
  filepattern <- paste(round(spatial_extent, 0), collapse = "_")
}

## Spatial parameters ------------
if (any(is.na(spatial_extent))) {
  if (all(is.na(c(extent_rast, extent_shp)))) {
    stop("If no spatial extent was provided, you need to provide a path to a raster or shapefile (set_parameters.R; L36).\n")
  }
}

if (!projection_units %in% c("dd", "m")) {
  stop("Your projection_units must either be 'dd' or 'm' (set_parameters.R; L47).\n")
}

if (projection_units == "dd") {
  
  ## Check spatial extent
  if (abs(spatial_extent[1] - spatial_extent[2]) > 10) {
    stop("Your projection unit is in decimal degrees, but the longitudes of your chosen extent are greater than 10 units apart. Perhaps extent was provided in meters? Please provide in decimal degrees (set_parameters.R; L36).\n")
  }
  if (abs(spatial_extent[3] - spatial_extent[4]) > 10) {
    stop("Your projection unit is in decimal degrees, but the latitudes of your chosen extent are greater than 10 units apart. Perhaps extent was provided in meters? Please provide in decimal degrees (set_parameters.R; L36).\n")  } 

  ## Confirm spatial extent is on land
  landmask <- terra::rast("data/base_layers/landmask.nc4")
  landmask <- terra::project(landmask, projection, method = "near")
  landcrop <- terra::crop(landmask, spatial_extent)
  if (all(terra::values(landcrop) < 1)) {
    stop("Your chosen spatial extent appears to include no land, only open water?\n")
  }
  
  ## Resolution
  if (chosen_rez > 1) {
    stop("If projection units are dd then chosen resolution must be as well (set_parameters.R; L52).\n")
  }
  
  ## Buffer radius
  # For calculating forest cover. Not included yet
  # if (buffer_radius > 1) {
  #   stop("If projection units are dd then buffer radius must be as well.")
  # }
  
  if (min_distance > 1) {
    stop("If projection units are dd then the minimum distance must be as well (set_parameters.R; L53).\n")
  }
  
  if (max_distance > 10) {
    stop("If projection units are dd then the maximum distance must be as well (set_parameters.R; L54).\n")
  }
}

if (projection_units == "m") {
  ## Check spatial extent
  if (abs(spatial_extent[1] - spatial_extent[2]) < 100) {
    stop("Your projection unit is in meters, but the longitudes of your chosen extent are less than 100 units apart. Perhaps extent was provided in decimal degrees? Please provide in meters (set_parameters.R; L36).\n")
  }
  if (abs(spatial_extent[3] - spatial_extent[4]) < 100) {
    stop("Your projection unit is in meters, but the latitudes of your chosen extent are less than 100 units apart. Perhaps extent was provided in decimal degrees? Please provide in meters (set_parameters.R; L36).\n")
  }
  
  ## Resolution
  if (chosen_rez < 1) {
    stop("If projection units are meters then chosen resolution must be as well (set_parameters.R; L52).\n")
  }
  
  ## Buffer radius
  # For calculating forest cover. Not included yet
  # if (buffer_radius < 1) {
  #   stop("If projection units are meters then buffer radius must be as well.")
  # }
  
  if (min_distance < 1) {
    stop("If projection units are meters then the minimum distance must be as well (set_parameters.R; L53).\n")
  }
  
  if (max_distance < 1) {
    stop("If projection units are meters then the maximum distance must be as well (set_parameters.R; L54).\n")
  }
}

## ....Check if spatial driver data are downloaded ------------

# Check if landcover file is available
if ("landcover" %in% chosen_layers) {
  if (!file.exists("data/spatial_drivers/landcover/original/esa_cci/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")) {
    cat(red("You've selected landcover as one of your chosen_layers, but do not have the corresponding ESA CCI landcover spatial file downloaded into the correct location (`data/spatial_drivers/landcover/original/esa_cci/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif`)\n"))
    
    cat("
    
    INSTRUCTIONS FOR DOWNLOADING ESA CCI LANDCOVER FILE:
        
        1. Visit this link:
        https://maps.elie.ucl.ac.be/CCI/viewer/download.php
        
        2. Under `Data Access` to the right, please input your Name, Organization, and email, agree to the Terms and Conditions, and press `Validate`.
        
        3. The page should then refresh. Scroll down to 'LC map 2015' and click on the down arrow next to '1 tif file, zip compression - 258Mo'. This should be the link:
        ftp://geo10.elie.ucl.ac.be/CCI/LandCover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.zip
        
        4. Depending on the Internet browser you are using, the file may begin to download. It is likely, however, that you are redirected to download the file via FTP (File Transfer Protocol). It is recommended to use FileZilla, which can be installed here:
        https://filezilla-project.org/
        
        Then, follow the instructions listed on the ESA CCA page under 'Download via FTP'. Specifically:
        
        4a. Open FileZilla
        4b. Paste 'geo10.elie.ucl.ac.be' into the Host option in the top-left of the window
        4c. Leave the username, password, and port empty, and press 'Quickconnect'
        4d. You will likely receive a pop-up concerning 'Insecure FTP connection'. Press OK.
        4e. The panel in the bottom-right labeled 'Filename' should now display a folder called 'CCI'. Click on that directory, then LandCover.
        4f. On the file 'ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.zip', double click, or right click > Download. The download should now commence. Once completed, the file will be stored at the path displayed on the tab 'Successful transfers' at the bottom of the window.
        
        5. Once the file is downloaded, unzip the .zip file, and place the corresponding .tif file into the following directory: 'data/spatial_drivers/landcover/original/esa_cci/'
        ")
    
    stop("See instructions above for downloading the landcover layer, or remove 'landcover' from your chosen_layers.")
  }
}

if ("macroclimate" %in% chosen_layers) {
  if (!dir.exists("data/spatial_drivers/macroclimate/worldclim/climate/")) {
    cat(red("You've selected macroclimate as one of your chosen_layers, but do not have any WorldClim spatial files downloaded into the correct location ('data/spatial_drivers/macroclimate/worldclim/climate/'). These will be automatically downloaded when you run scripts/03_data_extraction/prep_spatial.R \n"))
  }
}

if ("soiltemp" %in% chosen_layers) {
  if (!file.exists("data/spatial_drivers/microclimate/soil_bio/SBIO1_0_5cm_Annual_Mean_Temperature.tif")) {
    cat(red("You've selected soiltemp as one of your chosen_layers, but do not have the corresponding SoilTemp global map downloaded into the correct location ('data/spatial_drivers/microclimate/soil_bio/SBIO1_0_5cm_Annual_Mean_Temperature.tif'). This will be automatically downloaded when you run scripts/03_data_extraction/prep_spatial.R \n"))
  }
}

## Check custom layers and layer mask --------------------
if (any(complete.cases(custom_layers))) {
  custom_layers_test <- lapply(custom_layers, function(l) {
    if (file.exists(l)) {
      r <- try(terra::rast(l), silent = T)
      if (any(class(r) == "try-error")) {
        stop("Could not read file for custom layer ", l, ". Is this a geoTIFF?")
      }
      
      # Check projection of custom layer
      if (gsub(" \\+no_defs", "", terra::crs(r, proj = T)) != 
          gsub(" \\+no_defs", "", projection)) {
        stop("Projection for custom layer ", l, 
             " does not match the specified `projection`.")
      }
      
      # Check extent of custom layer
      foo <- terra::rast(ext = spatial_extent, crs = projection)
      
      i <- try(terra::intersect(foo, r), silent = T)
      
      if (any(class(i) == "try-error")) {
        stop("Extent for custom layer ", l, " does not seem to fall within your specified `spatial_extent`.")
      }
      
      return(l)
    } else {
      cat("No file found for filepath", l, "so ignoring.\n")
      return(NA)
    }
  })
  
  ## Subset custom_layers_names
  custom_layers_names <- custom_layers_names[!is.na(custom_layers_test)]
  ## Subset custom_layers
  custom_layers_test <- custom_layers_test[!is.na(custom_layers_test)]
  
  chosen_layers <- c(chosen_layers, custom_layers_names)
} else {
  custom_layers_test <- NA
}

if (any(class(layer_mask) != "SpatRaster")) {
  if (all(!is.na(layer_mask))) {
    stop("`layer_mask` must either by a SpatRaster or NA.")
  }
}

if (any(class(layer_mask) == "SpatRaster")) {
  if (!any(terra::values(layer_mask) %in% c(0,1))) {
    stop("All values of `layer_mask` must be either 0 or 1.")
  }
  
  # Check extent of layer_mask
  foo <- terra::rast(ext = spatial_extent, crs = projection)
  
  i <- try(terra::intersect(foo, layer_mask), silent = T)
  
  if (any(class(i) == "try-error")) {
    stop("Extent for `layer_mask` does not seem to fall within your specified `spatial_extent`.")
  }
}

## Power analysis parameters -----------
# Ensure values are numerical
l_objects <- list(
  budget = budget,
  cost_per_sensor= cost_per_sensor,
  n_sites = n_sites,
  chosen_rez = chosen_rez, 
  min_distance = min_distance,
  max_distance = max_distance
)

l_objects <- suppressWarnings(lapply(l_objects, as.numeric))

if (length(names(which(is.na(l_objects)))) > 0) {
  stop(
    "Following objects are non-numerical, please review user inputs in set_parameters.R: ",
    paste(names(which(is.na(l_objects))), collapse = ", ")
  )
}

rm(l_objects)

# Ensure all values are numerical
budget <- as.numeric(budget)
cost_per_sensor <- as.numeric(cost_per_sensor)
n_sites <- as.numeric(n_sites) # Perhaps integer?
chosen_rez <- as.numeric(chosen_rez) # Perhaps integer?
min_distance <- as.numeric(min_distance)
max_distance <- as.numeric(max_distance)



if (!is.null(power)) {
  if (is.na(power)) {
    power <- NULL
  }
}

if (!is.null(r2)) {
  if (is.na(r2)) {
    r2 <- NULL
  }
}

power_vars <- c(n_sites, r2, power, sum(c(ifelse(is.na(chosen_layers), NA, 1))))
power_vars <- power_vars[complete.cases(power_vars)]

  if (length(power_vars) != 3) {
    stop("You must provide values for three, and only three, of the following four inputs (i.e. one left as NA): `n_sites`, `power`, `r2`, and `chosen_layers` (set_parameters.R; L20, L72, L79, L82)")
  }

## Check program re-run specifications ---------

if (any(complete.cases(landscape_bins))) {
  if (!is.data.frame(landscape_bins)) {
    stop("If object `landscape_bins` is not NA, must be a data.frame")
  } 
}

if (program_rerun) {
  
  if (!is.data.frame(landscape_bins)) {
    stop("If program_rerun == TRUE, then `landscape_bins` must be a data.frame")
  } 
  if (!any(c("x", "y", "dim1_bin", "dim2_bin", "dim3_bin") %in% colnames(landscape_bins))) {
    stop("`landscape_bins` must have the following columns: x, y, dim1_bin, dim2_bin, dim3_bin")
  }
}

cat("Checking parameters - OK!\n")