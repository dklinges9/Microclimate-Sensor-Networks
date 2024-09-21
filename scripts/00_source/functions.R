

# A wrapper function for saving rasters, but only after checking if already exists and 
# if user wants to overwrite. This prevents errors when running code out of the box,
# thus stopping the data pipeline.
save_raster <- function(rst, filepath, filepattern) {
  if (file.exists(paste0(filepath,
                         filepattern, ".tif"))) {
    cat(crayon::red("File ", 
                    paste0(filepath,
                           filepattern, ".tif"), 
                    " already exists. Overwrite? (Y/N): "))
    
    ans1 <- readline(paste0(" "))
    
    if (tolower(ans1) %in% c("y", "yes")) {
      cat("File overwritten, continuing program.\n")
      writeRaster(rst, paste0(filepath,
                              filepattern, ".tif"), overwrite = T)
    }
    if (tolower(ans1) %in% c("n", "no")) {
      cat("File not overwritten, continuing program.\n")
    }
  } else {
    if (!file.exists(dirname(filepath))) {
      cat(red("Directory `", dirname(filepath), "` does not yet exist.\n Create directory? (Y/N)\n"))
      ans2 <- readline(" ")
      
      if (tolower(ans2) %in% c("y", "yes")) {
        dir.create(dirname(filepath), recursive = TRUE)
        cat(red("Creating directory: ", dirname(filepath)), "\n")
      } else if (tolower(ans2) %in% c("n", "no")) {
        stop("Directory was not created, program ended.")
      }
      
    }
    writeRaster(rst, paste0(filepath,
                            filepattern, ".tif"))
  }
}


# Imported from microclima
get_dem <- function(r = NA, lat, long, resolution = 30, zmin = 0, xdims = 200, ydims = 200) {
  if (!curl::has_internet()) {
    message("Please connect to the internet and try again.")
    return(NULL)
  }
  if (resolution < 30) {
    warning("Higher resolution data only available for some locations. DEM
             may be derived by interpolation")
  }
  if (class(r)[1] != "SpatRaster") {
    xy <- data.frame(x = long, y = lat)
    xy <- sf::st_as_sf(xy, coords = c('x', 'y'), crs = 4326)
    if (lat >= -80 & lat <= 84)
      xy <- sf::st_transform(xy, 3395)
    if (lat > 84)
      xy <- sf::st_transform(xy, 3413)
    if (lat < -80)
      xy <- sf::st_transform(xy, 3976)
    e <- ext(c(sf::st_coordinates(xy)[1] - floor(xdims/2) * resolution,
               sf::st_coordinates(xy)[1] + ceiling(xdims/2) * resolution,
               sf::st_coordinates(xy)[2] - floor(ydims/2) * resolution,
               sf::st_coordinates(xy)[2] + ceiling(ydims/2) * resolution))
    r <- rast(e)
    res(r) <- resolution
    crs(r) <- sf::st_crs(xy)$wkt
    ll <- latlongfromraster(r)
  } else {
    ll <- latlongfromraster(r)
    lat <- ll$lat
    long <- ll$long
    if (is.na(resolution)) resolution<-res(r)[1]
  }
  z<-ceiling(log((cos(ll$lat*pi/180)*2*pi*6378137)/(256*resolution),2))
  if (z > 14) z <- 14
  prj<-sf::st_crs(r)
  r2<-elevatr::get_aws_terrain(r,z,prj)
  r2 <- resample(r2, r)
  r2[r2 < zmin] <- zmin
  r2[is.na(r2)] <- zmin
  return(r2)
}

# Imported from microclima
latlongfromraster <- function(r) {
  e <- ext(r)
  xy <- data.frame(x = (e$xmin + e$xmax)/2, y = (e$ymin + e$ymax)/2)
  xy <- sf::st_as_sf(xy, coords = c("x", "y"),
                     crs = crs(r))
  ll <- sf::st_transform(xy, 4326)
  ll <- data.frame(lat = sf::st_coordinates(ll)[2], long = sf::st_coordinates(ll)[1])
  return(ll)
}