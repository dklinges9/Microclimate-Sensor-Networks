## Set parameters for each step of the workflow


## Load libraries ---------------

library(tidyverse)

cat("Setting parameters...\n")

## These are parameters relevant to running a power analysis to determine statistical power

## Project parameters -----------

# What is the name of the landscape? Will be appended to names of output files
# If left NA will names files according to the chosen spatial extent
landscape_name <- "gnv"

# Your budget for sensors. This will be used to constrain the amount of 
# environmental space that you can sample
budget <- 5000
cost_per_sensor <- 100

# Approximate number of sites to select, at which a sensor will be placed
# The program will return a list of sites that will be no fewer than n_sites,
# but might be slightly higher

# If left as NA, program will calculate necessary and possible number from
# power analysis and chosen budget

# Also note that depending on your study design, you may want to place multiple
# sensors at a given spatial point (e.g. at different heights/depths).
n_sites <- 100

## Spatial parameters ------------

# Spatial extent in decimal degrees
# Suggest specifying an extent that is slightly larger than actual target extent,
# to account for lost edges when reprojecting and when calculating topographical
# variables 
spatial_extent <- c(-82.407035, -82.306098, 29.641339, 29.704579)
  # Ambalavero: c(47.4, 47.55, -21.6, -21.45)
# c(5042788.27696657, 5064493.44436001, 1817708.23946404, 1850265.99055419) # c(5042788.27696657, 5064493.44436001, 1817708.23946404, 1850265.99055419) # c(-83.8, -83.3, 35.3, 35.8) # c(748472.066251046, 764572.066251046, -2389957.46406275, -2373857.46406275) #  # provide also in meters 
# Alternatively a path to a raster file or shapefile can be provided
# (I HAVEN'T YET MADE THESE OPTIONS FUNCTIONAL)
extent_rast <- NA
extent_shp <- NA

# Provide the projection of the specified spatial_extent
# Some example projections:
# "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# "+proj=longlat +datum=WGS84" 
# Note that projections with unites in meters will entail much faster access
# of a Digital Elevation Model (DEM)
projection <-  "+proj=longlat +datum=WGS84" 
projection_units <- "dd" # c("m", "dd") for 'meters' and 'decimal degrees'

# Your desired resolution of the Digital Elevation Model (DEM). Must be at 
# least 30 meters. This will also determine the resolution at which all 
# analyses are conducted
# The following few parameters can be divided by 120000 for a quick
# translation from meters to decimal degrees
chosen_rez <- 100 / 120000 # in same units as projection in meters
min_distance <- 100 / 120000 # min tolerable distance between points, in projection_units
max_distance <- 3500 / 120000 # max tolerable distance between one point and the
# next nearest point, in projection_units

## Power analysis parameters ----------

# Estimate of power (1 minus Type II error probability). If unknown, keep at 0.8
# OR is calculated
power <- NA # 0.8 # NULL

# Estimate of the explanatory power (r2) of chosen set of predictors for
# explaining response variable of interest (that is measured by sensors), OR
# desired r2. 
# Used for calculating f2 (effect size) in power analysis. If unknown, kept at 0.15
# (reasonably cautious for most environmental relationships)
r2 <- 0.15

## Environmental layers ---------
# chosen_layers must be a string of the names of a subset (or all) of the following:
# elevation, slope, aspect, landcover, ndvi, proportion_forest, patch_area_4neighbr
# If you intend to add other spatial predictors, please add their names to this string.
# The length of this string is important for the power analysis.
chosen_layers <- c("elevation", "slope", "aspect", "landcover")

# Are there other spatial layers of your choice that you wish to be included
# to inform the site selection algorithm? Provide the filepaths of these layers
# as a vector below. Or, leave as NA
# Requirements:
# Must be a geoTIFF file with a single layer
# Must have the same projection as provided in `projection` above
# Must have an extent that is equal to or larger than the `spatial_extent` above
# Must have numeric values
# Can be any resolution

custom_layers <- NA
custom_layers_names <- NA
# custom_layers <- c("data/spatial_drivers/vegetation/global_forest_change/original/Hansen_GFC-2020-v1.8_treecover2000_20S_040E.tif", "PATH/TO/LAYER2", "PATH/TO/LAYER3")
# 
# custom_layers_names <- c("treecover", "test1", "test2")

# Do you want to mask your target landscape so that only certain grid cells are
# eligible for selection? This may be because you are only interestd in a certain
# habitat type (eg forest), or certain areas or inaccessible/impermissible.
# If so, provide a raster layer, of equal extent as the provided `spatial_extent`
# above. The values of this layer must be 1 (eligible cells) or 0 (ineligible cells)
# Requirements:
# Must be a geoTIFF file with a single layer
# Must have the same projection as provided in `projection` above
# Must have an extent that is equal to or larger than the `spatial_extent` above
# Must have numeric values
# Can be any resolution

layer_mask <- rast("data/spatial_drivers/topography/derivative/aspect_4e-04_amba2.tif")
terra::values(layer_mask) <- round(runif(length(terra::values(layer_mask)), min= 0, max = 1))
layer_mask <- NA

# When selecting sites, do you want to give more weight to outliers within 
# the environmental space (e.g. more topographically complex areas)?
favor_outliers <- FALSE

## Required sites -----------

# This must be either a data.frame with the following columns:
# `x` -- longitude of required sites. Must match spatial/projection specifications above
# `y` -- latitude of required sites. Must match spatial/projection specifications above
# dim1_bin -- Dim. 1 bin that the coordinate falls into, as created on last program run
# dim2_bin -- Dim. 2 bin that the coordinate falls into, as created on last program run
# dim3_bin -- Dim. 3 bin that the coordinate falls into, as created on last program run
# (other columns are also allowed but ignored)
# required_sites <- NA
required_sites <- read_csv("data/chosen_sites/selected_sites_30_gnv.csv") %>%
  dplyr::sample_n(10)
# For example:
# required_sites <- read_csv("data/chosen_points/selected_points_150_-83.8_-83.3_35.3_35.8.csv")

## Program re-run parameters --------

program_rerun <- FALSE

# A data.frame of sites required to be included in the analysis (either NA or 
# a dataframe of x-y coordinates in projection_units)

# If you are re-running this program after removing some sites, and intending to
# select new sites, you must set required_sites to the file path of your
# updated list of valid sites

# If re-running the program iteratively, you need to provide the bin values 
# from the last run. `bins` below is a data.frame that has the following columns:
# `x` -- longitude of required sites. Must match spatial/projection specifications above
# `y` -- latitude of required sites. Must match spatial/projection specifications above
# dim1_bin -- Dim. 1 bin that the coordinate falls into, as created on last program run
# dim2_bin -- Dim. 2 bin that the coordinate falls into, as created on last program run
# dim3_bin -- Dim. 3 bin that the coordinate falls into, as created on last program run
# (other columns are also allowed but ignored)

# Otherwise, leave as NA
landscape_bins <- NA
# landscape_bins <- read_csv("data/landscape_data/landscape_bins_30_gnv.csv")

cat("Setting parameters - OK!\n")