## Microclimate Sensor Networks: Site Selection and Visualization Program


#### Main
_scripts/main.R_  

Used to run all steps in the program, in their appropriate sequence.  

#### Set Parameters
_scripts/00_paremeters/set_parameters.R_  

Requires *user input* of parameters specifying your target landscape, budget, desired environmental drivers, and so on.  


#### Check Parameters
_scripts/00_parameters/check_parameters.R_

Checks all user parameter inputs to confirm they meet program standards.  

#### Get DEM
_scripts/01_data_extraction/get_dem.R_  

Downloads a DEM for a desired spatial extent and desired spatial resolution.  

#### Prep Spatial data
_scripts/01_data_extraction/prep_spatial.R_  

Preps spatial data for spatial environmental drivers to be used in point selection and visualization scripts.  


#### Power Analysis
_scripts/02_power_analysis/power_analysis.R_  

Performs a test of the statistical power of a study design based upon chosen number of spatial drivers and sample size (i.e. number of sensors).  


#### Site Selection
_scripts/03_site_selection/site_selection.R_  

Performs selection of sensor sites that adequately represent environmental space across the specified landscape.  

#### Visualize Sites
_scripts/04_visualize_sites/visualize_sites.R_

Generates maps of chosen sites over environmental layers, and other plots of distribution of environmental space and how it was sampled.  


