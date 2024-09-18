## This script checks whether all required packages have been installed

cat("Checking installed packages\n")

# list all required packages for the scripts to run
pkgs <- c("tidyverse", "elevatr", "terra", "landscapemetrics",
          "pwr", "factoextra", "FactoMineR", "ggplot2", "RColorBrewer", "viridis", 
          "crayon", "geodata", "sf", "zen4R", "wesanderson", "gridExtra") # crayon required?

# find the packages that have not yet been installed on the device
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]

# end script if packages have already been installed
if (length(new_pkgs) == 0) {
  cat("All packages have been installed succesfully.\n")
} else {
    # prompt user to confirm whether or not to install required packages
  if (length(new_pkgs)) {
    
    cat("\033[0;31mFollowing packages have not yet been installed:\033[0m\n") # 41 = white text, red bg, 31 = red text
    cat(paste0(new_pkgs, "\n"))
    cat("\033[0;31mWould you like to install these packages and their dependencies? (Y/N): \033[0m\n")
    ans1 <- readline(" ")

    # for zen4R, installation via github (requires devtools, and Rtools dependency)
    if (sum(c("zen4R") %in% new_pkgs)>0) {
      cat("Package 'zen4R' not installed, and must be installed from GitHub...\n")
      if (!"devtools" %in% installed.packages()[,"Package"]) {
        ans2 <- readline("Package `devtools` is required to install packages from GitHub.\nInstall devtools? (Y/N): ")
        
        if (!tolower(ans2) %in% c("y", "yes")) {
          warning("Packages have not been installed. Scripts are not guaranteed to run.\n")
        }
        
        else {
          cat("")
          install.packages("devtools")
        }     
      }
      
      if ("zen4R" %in% new_pkgs) {
        cat("Installing zen4R from GitHub. NOTE: Rtools required (https://cran.rstudio.com/bin/windows/Rtools/).\n")
        devtools::install_github("eblondel/zen4R")
      }
    }
    if (tolower(ans1) %in% c("y", "yes")) {
      cat("Installing packages...\n")
      install.packages(new_pkgs[!grepl(c("zen4R"), new_pkgs)])
    }
    else warning("Packages have not been installed. Scripts are not guaranteed to run.\n")
  }
  
  # if, for some reason, packages were not installed -> issue warning
  if (new_pkgs[(!new_pkgs %in% installed.packages()[,"Package"])] |> length() != 0) {
    warning("Not all required packages have been installed on your device.")
  } else cat("All required packages have been installed succesfully.\n")
  
  # Garbage collect
  invisible(gc())
  
  cat(crayon::red("We recommend restarting your R session (For RStudio users: Session > Restart R or CTRL+SHIFT+F10).\n"))
  
  # Attempt to remove both objects, suppress warnings
  suppressWarnings(rm(list = c("ans1", "ans2", "pkgs", "new_pkgs")))
}