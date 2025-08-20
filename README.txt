Microclimate Sensor Networks: Site Selection and Visualization Program
(please see README.md for a Markdown-formatted version of this readme)

Questions? Please submit an issue here:  
https://github.com/dklinges9/Microclimate-Sensor-Networks/issues

If you don't have a GitHub account and can't submit issues, then reach out to Dave Klinges at: dklinges9@gmail.com. Responses will be faster for issues than emails, however!  

See our paper published in _Ecological Informatics_ for further info:
Klinges, D.H., Lembrechts, J.J., Van de Vondel, S., Greenlee, E.J., Hayles-Cotton, K., Senior, R.A., 2025. A workflow for microclimate sensor networks: Integrating geographic tools, statistics, and local knowledge. Ecological Informatics 91, 103376. https://doi.org/10.1016/j.ecoinf.2025.103376

https://www.sciencedirect.com/science/article/pii/S1574954125003851


INSTRUCTIONS

1. Set Parameters
(scripts/set_parameters.R)

*Requires user input* of parameters specifying your target landscape, budget, desired environmental drivers, and so on. By default provides sensor selection for a landscape in Madagascar. See instructions within this script.

2. Main Program
(scripts/main.R)  

*This file is your home base.* Used to run all steps in the program, in their appropriate sequence. Running scripts out of order may cause the data pipeline to be interrupted due to error messages.

3. Inspect Outputs

- CSV of chosen sensor locations, and environmental information corresponding to these locations, is provided in the directory `data/chosen_sites/`  
- CSVs and TIF rasters of landscape environmental variation and bins are provided in the directory `data/landscape_data/`  
- Visualizations of environmental variation in your landscape, locations of bins, and locations of chosen sensor locations are provided in `figures/point_selection/`.  
- TIF rasters of input environmental layers are queried and stored to the directory `data/spatial_drivers`.  
- Additional diagnostics about chosen sensor locations, if necessary, are provided in the directory `data/chosen_sites/diagnostics/`.  
   
_That's it!_ See below for descriptions of other source files, which should not be modified (unless further customization desired).  








INTERNAL FILES (do not modify!!)  

Source Code
(scripts/00_source/functions.R)

Source code used internally to support other scripts. **Do not modify.**

Check Packages
(scripts/00_source/check_pkgs.R)

Checks that the user has all necessary R packages, whom is prompted to install missing 
packages, if any. **Do not modify.**

Check Parameters
(scripts/01_parameters/check_parameters.R)

Checks all user parameter inputs to confirm they meet the program's standards. **Do not modify.**   

Power Analysis
(scripts/02_power_analysis/power_analysis.R)

Performs a test of the statistical power of a study design based upon the chosen number of spatial drivers and sample size (i.e. number of sensors). **Do not modify.**  

Get DEM
(scripts/03_data_extraction/get_dem.R)

Downloads a DEM for a desired spatial extent and desired spatial resolution, and uses this DEM to calculate elevation, slope, and aspect. **Do not modify.**   

Prep Spatial data
(scripts/03_data_extraction/prep_spatial.R)

Preps spatial data for spatial environmental drivers to be used in point selection and visualization scripts. **Do not modify.**  

Site Selection
(scripts/04_site_selection/site_selection.R)

Performs selection of sensor sites that adequately represent environmental space across the specified landscape. **Do not modify.**  

Visualize Sites
(scripts/05_visualize_sites/visualize_sites.R)

Generates maps of chosen sites over environmental layers, and other plots of distribution of environmental space and how it was sampled. **Do not modify.**  

