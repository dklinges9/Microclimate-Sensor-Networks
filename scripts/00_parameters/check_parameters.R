## Check to confirm that chosen parameters conform to program guidelines

cat("Checking parameters...\n")

if (any(is.na(spatial_extent))) {
  if (all(is.na(c(extent_rast, extent_shp)))) {
    stop("If there is no spatial extent provided, you need to provide a path to a raster or shapefile.")
  }
}

if (projection_units == "dd") {
  
  ## Check spatial extent
  if (abs(spatial_extent[1] - spatial_extent[2]) > 10) {
    stop("Your projection unit is in decimal degrees, but the longitudes of your chosen extent are greater than 10 units apart. Perhaps extent was provided in meters? Please provide in decimal degrees.\n")
  }
  if (abs(spatial_extent[3] - spatial_extent[4]) > 10) {
    stop("Your projection unit is in decimal degrees, but the latitudes of your chosen extent are greater than 10 units apart. Perhaps extent was provided in meters? Please provide in decimal degrees.\n")  } 

  ## Resolution
  if (chosen_rez > 1) {
    stop("If projection units are dd then chosen resolution must be as well.")
  }
  
  ## Buffer radius
  # For calculating forest cover. Not included yet
  # if (buffer_radius > 1) {
  #   stop("If projection units are dd then buffer radius must be as well.")
  # }
  
  if (min_distance > 1) {
    stop("If projection units are dd then the minimum distance must be as well.")
  }
  
  if (max_distance > 10) {
    stop("If projection units are dd then the maximum distance must be as well.")
  }
}

if (projection_units == "m") {
  ## Check spatial extent
  if (abs(spatial_extent[1] - spatial_extent[2]) < 100) {
    stop("Your projection unit is in meters, but the longitudes of your chosen extent are less than 100 units apart. Perhaps extent was provided in decimal degrees? Please provide in meters.\n")
  }
  if (abs(spatial_extent[3] - spatial_extent[4]) < 100) {
    stop("Your projection unit is in meters, but the latitudes of your chosen extent are less than 100 units apart. Perhaps extent was provided in decimal degrees? Please provide in meters.\n")
  }
  
  ## Resolution
  if (chosen_rez < 1) {
    stop("If projection units are meters then chosen resolution must be as well.")
  }
  
  ## Buffer radius
  # For calculating forest cover. Not included yet
  # if (buffer_radius < 1) {
  #   stop("If projection units are meters then buffer radius must be as well.")
  # }
  
  if (min_distance < 1) {
    stop("If projection units are meters then the minimum distance must be as well.")
  }
  
  if (max_distance < 1) {
    stop("If projection units are meters then the maximum distance must be as well.")
  }
}
  
