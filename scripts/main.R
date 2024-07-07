## Microclimate Sensor Networks
## Master script: running this script runs all sub-modules in sequence. It may 
## take a few minutes to complete! Running this script without modifications
## anywhere will assume all default values and parameterizations (here, for
## microclimate sensor site selection in Southeastern Madagascar).


## If running the program for the first time: run these lines of code
## Please execute each of these lines of code one at a time, as some scripts
## require user input in the console
source("scripts/00_source/check_pkgs.R")
source("scripts/00_source/functions.R")
source("scripts/set_parameters.R")
source("scripts/01_parameters/check_parameters.R")
source("scripts/02_power_analysis/power_analysis.R")
source("scripts/03_data_extraction/get_dem.R")
source("scripts/03_data_extraction/prep_spatial.R")
source("scripts/04_site_selection/site_selection.R")
source("scripts/05_visualize_sites/visualize_sites.R")

## If re-running the program iteratively after having already chosen sites
## via this program, then only run these lines of code
source("scripts/00_source/functions.R")
source("scripts/set_parameters.R")
source("scripts/01_parameters/check_parameters.R")
source("scripts/04_site_selection/site_selection.R")
source("scripts/05_visualize_sites/visualize_sites.R")
