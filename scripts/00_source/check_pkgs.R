## This script checks whether all required packages have been installed

cat("Checking installed packages\n")

# list all required packages for the scripts to run
pkgs <- c("tidyverse", "elevatr", "rnoaa", "microclima", "terra", "landscapemetrics",
          "pwr", "sf", "factoextra", "FactoMineR", "ggplot2", "RColorBrewer", "viridis", 
          "crayon", "geodata", "zen4R")

# find the packages that have not yet been installed on the device
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]

# end script if packages have already been installed
if (length(new_pkgs) == 0) {
  cat("All packages have been installed succesfully.\n")
} else {
    # prompt user to confirm whether or not to install required packages
  if (length(new_pkgs)) {
    
    cat("Following packages have not yet been installed:\n")
    cat(paste0(new_pkgs, "\n"))
    cat("Would you like to install these packages and their dependencies? (Y/N): ")
    ans1 <- readline(" ")

    # for microclim a, installation via github (requires devtools, Rtools, and rnoaa dependency)
    if (c("microclima", "zen4R") %in% new_pkgs) {
      if (!"devtools" %in% installed.packages()[,"Package"]) {
        ans2 <- readline("Package `devtools` is required to install packages from GitHub Install devtools? (Y/N): ")
        
        if (!tolower(ans2) %in% c("y", "yes")) {
          warning("Packages have not been installed. Scripts are not guaranteed to run.\n")
        }
        
        else {
          cat("")
          install.packages("devtools")
        }     
      }
      if ("microclima" %in% new_pkgs) {
        cat("Installing microclima from GitHub. NOTE: Rtools required (https://cran.rstudio.com/bin/windows/Rtools/).\n")
        
        if ("rnoaa" %in% new_pkgs) {
          devtools::install_github("ropensci/rnoaa") # required dependency for microclima
        }
        devtools::install_github("ilyamaclean/microclima")
      }
      
      if ("zen4R" %in% new_pkgs) {
        cat("Installing zen4R from GitHub. NOTE: Rtools required (https://cran.rstudio.com/bin/windows/Rtools/).\n")
        devtools::install_github("eblondel/zen4R")
      }
    }
    if (tolower(ans1) %in% c("y", "yes")) {
      cat("Installing packages...\n")
      install.packages(new_pkgs[!grepl(c("microclima", "rnoaa", "zen4R"), new_pkgs)])
    }
    else warning("Packages have not been installed. Scripts are not guaranteed to run.\n")
  }
  
  # if, for some reason, packages were not installed -> issue warning
  if (new_pkgs[(!new_pkgs %in% installed.packages()[,"Package"])] |> length() != 0) {
    warning("Not all required packages have been installed on your device.")
  } else cat("All required packages have been installed succesfully.\n")
  
  # Garbage collect
  gc()
  
  cat("We recommend restarting your R session (Session > Restart R).\n")
  
  # Attempt to remove both objects, suppress warnings
  suppressWarnings(rm(list = c("ans1", "ans2")))
}

# clean environment  
rm(list = c("pkgs", "new_pkgs"))
