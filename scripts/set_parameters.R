## Set parameters for each step of the workflow


## Load libraries ---------------

cat("Setting parameters...\n")

## Project parameters -----------

# What is the name of the landscape? This will be appended to names of output files
# If left NA will name files according to the chosen spatial extent
landscape_name <- "flanders"

# Your budget for sensors. This will be used to constrain the amount of 
# environmental space that you can sample, and inform the power analysis
budget <- 15000
cost_per_sensor <- 100

# How many sensor sites do you want?
# The program will return a set of sites that will be no fewer than n_sites,
# but might be slightly higher
# If left as NA, program will calculate necessary and possible number from
# power analysis and chosen budget
# Also note that depending on your study design, you may want to place multiple
# sensors at a given spatial point (e.g. at different heights/depths).
n_sites <- 150

## Spatial parameters ------------

# What is the spatial extent of your target region? Provide this as a vector of
# 4 numbers corresponding to the bounding box of the spatial extent
# We suggest specifying an extent that is slightly larger than actual target extent,
# to account for lost edges when reprojecting and when calculating topographical
# variables 
# Examples:
# spatial_extent <- c(170939.777724312, 518527.501269666, 5355763.70341275, 5493712.58119482) 
# spatial_extent <- c(9686288, 9730000, -370468, -348855)
# spatial_extent <- c(102.583, 103.728, -3.44038, -3)

spatial_extent <- c(170939.777724312, 518527.501269666, 5355763.70341275, 5493712.58119482)
# Alternatively a path to a raster file or shapefile can be provided
# (I HAVEN'T YET MADE THESE OPTIONS FUNCTIONAL)
# extent_rast <- NA
# extent_shp <- NA

# Provide the projection of the specified spatial_extent
# Some example projections:
# "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# "+proj=longlat +datum=WGS84" 
# Note that projections with units in meters will entail MUCH faster access
# of a Digital Elevation Model (DEM) in get_dem.R
projection <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# What are the units of your projection? Must be either decimal degrees (dd) 
# or meters (m)
projection_units <- "m" # c("m", "dd") for 'meters' and 'decimal degrees'

# Your desired resolution of the Digital Elevation Model (DEM). Finer resolution
# means slower processing This resolution will also determine the resolution 
# at which all analyses are conducted (other layers resampled to this resolution)
# Can divide meters by 120000 for quick (rough) translation to decimal degrees
chosen_rez <- 100

# What is the maximum distance between any two locations that you will tolerate?
# Useful if you wish to reduce travel time between sensor locations. The program
# will attempt to find points below this max_distance (but shorter max_distance
# means program might take longer)
# Must be in same units as projection_units
# Can divide meters by 120000 for quick (rough) translation to decimal degrees
min_distance <- 100

# What is the minimum distance between any two points that you will tolerate?
# In theory, the program might choose two locations right next to each other.
# This parameter sets a minimum distance between all locations
# Must be in same units as projection_units
# Can divide meters by 120000 for quick (rough) translation to decimal degrees
max_distance <- 25000

## Environmental layers ---------
# What environmental layers do you wish to use to inform the sampling algorithm?
# chosen_layers must be a string of the names of a subset (or all) of the following:
# elevation, slope, aspect, landcover, macroclimate, soiltemp
chosen_layers <- c("elevation", "slope", "aspect", "landcover", 
                   "soiltemp", "macroclimate")
# NOTE: elevation, macroclimate, and soiltemp are all highly correlated. We 
# recommend choosing from just one of these layers depending on your needs

# Are there other spatial layers of your choice that you wish to be included
# to inform the site selection algorithm? Provide the filepaths of these layers
# as a vector below. Or, leave as NA
# Requirements:
# Must be a geoTIFF file with a single layer
# Must have the same projection as provided in `projection` above
# Must have an extent that is equal to or larger than the `spatial_extent` above
# Must have numeric values
# Can be any resolution (but will be resampled to resolution of DEM, either 
# via billinear interpolation for continuous rasters or nearest neighbor for
# categorical rasters)
custom_layers <- NA
# What are the desired names for each layer's variable?
custom_layers_names <- NA

# Example for Madagascar:
# custom_layers <- c("data/spatial_drivers/vegetation/global_forest_change/original/Hansen_GFC-2020-v1.8_treecover2000_20S_040E.tif", "PATH/TO/LAYER2", "PATH/TO/LAYER3")
# custom_layers_names <- c("treecover", "test1", "test2")

# Do you want to mask your target landscape so that only certain grid cells are
# eligible for selection? This may be because you are only interested in a certain
# habitat type (eg forest), or certain areas or inaccessible/impermissible.
# Requirements:
# Must be a geoTIFF file with a single layer
# The values of this layer must be 1 (eligible cells) or 0 (ineligible cells)
# Must have the same projection as provided in `projection` above
# Must have an extent that is equal to or larger than the `spatial_extent` above
# Must have numeric values
# Can be any resolution
layer_mask <- NA

# Example for Madagascar:
# layer_mask <- rast("data/spatial_drivers/topography/derivative/aspect_4e-04_madagascar1.tif")
# terra::values(layer_mask) <- round(runif(length(terra::values(layer_mask)), min= 0, max = 1))

# When selecting sites, do you want to give more weight to outliers within 
# the environmental space (e.g. more topographically complex areas)? TRUE or FALSE
favor_outliers <- FALSE

## Power analysis parameters ----------

# Estimate of power (1 minus Type II error probability). If unknown, keep at 0.8
# OR leave as NULL or NA to calculate power from other parameters
power <- NULL # 0.8

# Estimate of the explanatory power (r2) of chosen set of predictors for
# explaining response variable of interest (that is measured by sensors), OR
# desired r2. 
# Used for calculating f2 (effect size) in power analysis. If unknown, kept at 0.15
# (reasonably cautious for most environmental relationships). If `power` is NULL,
# `r2` must be provided a value. Cannot have NULL or NA for both
r2 <- 0.15

## Required sites, or sites from prior program run -----------

# Are there specific point locations at which you require sampling?
# This includes locations from a prior program run, and you are now updating
# locations based upon field visits
# This must be either a data.frame with the following columns:
# `x` -- longitude of required sites. Must match spatial/projection specifications above
# `y` -- latitude of required sites. Must match spatial/projection specifications above

# If you are running the program again to update locations based upon a prior
# program run, then this data.frame must also include the following columns (in other
# words, must be all of, or at subset of, the `"data/chosen_sites/selected_sites_*.csv` 
# generated from the prior program run):
# dim1_bin -- Dim. 1 bin that the coordinate falls into, as created on last program run
# dim2_bin -- Dim. 2 bin that the coordinate falls into, as created on last program run
# dim3_bin -- Dim. 3 bin that the coordinate falls into, as created on last program run
# (other columns are also allowed but ignored)
# If you include columns dim1_bin, dim2_bin, and dim3_bin, must set 
# program_rerun to TRUE and provide landscape_bins (see below)
required_sites <- NA
# For example, randomly choosing 10 locations from a prior run for Oman: 
# required_sites <- read_csv("data/chosen_sites/selected_sites_100_oman1.csv") %>%
#   dplyr::sample_n(10)

# Or, another example in which columns dim1_bin, dim2_bin, and dim3_bin are absent,
# and the program assumes the user has not yet run the program:
# required_sites <- read_csv("data/chosen_sites/selected_sites_100_oman1.csv") %>%
#   dplyr::sample_n(10) %>% 
#   dplyr::select(x, y)

## Program re-run parameters --------

# Are you re-running the program it update chosen sensor locations? TRUE or FALSE
program_rerun <- FALSE

# If TRUE, you need to provide the bin values from the last program run. This was
# saved as a CSV as "data/landscape_data/landscape_bins_*.csv"
# Otherwise, leave as NA
landscape_bins <- NA

# Example for Flanders:
# landscape_bins <- read_csv("data/landscape_data/landscape_bins_flanders_150.csv")

cat("Setting parameters - OK!\n")