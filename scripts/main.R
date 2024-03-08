## Microclimate Sensor Networks
## Master script: running this script runs all sub-modules in sequence. It may 
## take a few minutes to complete! Running this script without modifications
## anywhere will assume all default values and parameterizations (here, for
## microclimate sensor site selection in Southeastern Madagascar).


## Workplace prep ----------
source("scripts/00_parameters/check_pkgs.R")

source("scripts/00_parameters/set_parameters.R")

source("scripts/00_parameters/check_parameters.R")

source("scripts/01_data_extraction/get_dem.R")

source("scripts/01_data_extraction/prep_spatial.R")

source("scripts/02_power_analysis/power_analysis.R")

source("scripts/03_site_selection/site_selection.R")

source("scripts/04_visualize_sites/visualize_sites.R")
