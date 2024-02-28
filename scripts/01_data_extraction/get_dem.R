## David Klinges
## This script queries a DEM of desired resolution and extent


library(elevatr)
library(microclima)
library(raster)
library(terra)

spatial_extentr <- terra::rast(ext(spatial_extent))
crs(spatial_extentr) <- projection

cat("Getting dem....\n")
dem <- rast(get_dem(r = raster(spatial_extentr), resolution = chosen_rez))

# Specify string for naming output files
if (complete.cases(landscape_name)) {
  filepattern <- landscape_name
} else {
  filepattern <- paste(round(spatial_extent, 0), collapse = "_")
}

cat("writing dem...\n")
writeRaster(dem, paste0("data/spatial_drivers/topography/derivative/dem_", 
                        round(chosen_rez, 4), 
                        "_", filepattern, ".tif"),
            overwrite = T)

## Slope and Aspect -------------

if ("slope" %in% chosen_layers) {
  cat("Calculating slope....\n")
  
  slope <- terra::terrain(dem, v = "slope", neighbors = 8, unit = "degrees")
  
  writeRaster(slope, paste0("data/spatial_drivers/topography/derivative/slope_", 
                            round(chosen_rez, 4), 
                            "_", filepattern, ".tif"),
              overwrite = T)
}

if ("aspect" %in% chosen_layers) {
  cat("Calculating aspect....\n")
  aspect <- terra::terrain(dem, v = "aspect")
  writeRaster(aspect, paste0("data/spatial_drivers/topography/derivative/aspect_",
                             round(chosen_rez, 4), 
                             "_", filepattern, ".tif"),
              overwrite = T)
}  



