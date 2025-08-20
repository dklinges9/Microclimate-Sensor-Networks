## Microclimate Sensor Networks
## Master script: running this script runs all sub-modules in sequence. It may 
## take a few minutes to complete! Running this script without modifications
## anywhere will assume all default values and parameterizations (here, for
## microclimate sensor site selection in Southeastern Madagascar).


## If running the program for the first time: run these lines of code
## Please execute each of these lines of code ONE AT A TIME, as some scripts
## require user input in the console
source("scripts/00_source/check_pkgs.R")
source("scripts/00_source/functions.R")
# For setting parameters, you can manually edit the script 'scripts/set_parameters.R',
# OR you can use our Excel template for specifying parameters. If you choose to
# use the Excel template (found in docs/parameters/parameter_template.xlsx),
# please specify `parameters_from_excel` as TRUE below. Otherwise keep FALSE.
parameters_from_excel <- FALSE # TRUE or FALSE
source("scripts/set_parameters.R")
# This script checks your parameters inputs to make sure they're valid
source("scripts/01_parameters/check_parameters.R")
# This script performs power analysis
source("scripts/02_power_analysis/power_analysis.R")
# This script downloads a DEM (digital elevation model) for your study area. 
# This step requires an Internet connection. If your spatial resolution is 
# fairly fine (<10m), or your spatial projection is in decimal degrees 
# (rather than meters), this step may take a few minutes.
source("scripts/03_data_extraction/get_dem.R")
# This script prepares the spatial data for site selection
source("scripts/03_data_extraction/prep_spatial.R")
# This script conducts the site selection, and saves outputs
# in data/chosen_sites/selected_sites_LANDSCAPENAME_*.csv
source("scripts/04_site_selection/site_selection.R")
# This script visualizes your selected sites in several ways, and saves the
# figures in figures/analysis/ and figures/point_selection/
source("scripts/05_visualize_sites/visualize_sites.R")


## RE-RUNNING THE PROGRAM ITERATIVELY --------------
## If re-running the program iteratively after having already chosen sites
## via this program, then only run these lines of code
source("scripts/00_source/functions.R")
source("scripts/set_parameters.R")
source("scripts/01_parameters/check_parameters.R")
source("scripts/04_site_selection/site_selection.R")
source("scripts/05_visualize_sites/visualize_sites.R")
