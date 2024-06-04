

# Saving rasters, but only after checking if already exists and if user wants to overwrite
save_raster <- function(rst, filepath, filepattern) {
  if (file.exists(paste0(filepath,
                         filepattern, ".tif"))) {
    ans1 <- readline(paste0("File ", 
                            paste0(filepath,
                                   filepattern, ".tif"), 
                            " already exists. Overwrite? (Y/N): "))
    
    if (tolower(ans1) %in% c("y", "yes")) {
      cat("File overwritten, continuing program.\n")
      writeRaster(rst, paste0(filepath,
                                    filepattern, ".tif"), overwrite = T)
    }
    if (tolower(ans1) %in% c("n", "no")) {
      cat("File not overwritten, continuing program.\n")
    }
  } else {
    writeRaster(rst, paste0(filepath,
                                  filepattern, ".tif"))
  }
}
