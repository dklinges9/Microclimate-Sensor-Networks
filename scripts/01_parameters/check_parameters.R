## Check to confirm that chosen parameters conform to program guidelines

cat("Checking parameters...\n")

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
             "does not match the specified `projection`.")
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