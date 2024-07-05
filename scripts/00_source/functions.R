

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
      cat(red("Directory `", filepath, "` does not yet exist\n Create directory? (Y/N)\n"))
      ans2 <- readline(" ")

      if (tolower(ans2) %in% c("y", "yes")) {
        dir.create(dirname(filepath), recursive = TRUE)
        cat(red("Creating directory: ", filepath))
      } else if (tolower(ans2) %in% c("n", "no")) {
        stop("Directory was not created, program ended.")
      }
      
    }
    writeRaster(rst, paste0(filepath,
                                  filepattern, ".tif"))
  }
}
