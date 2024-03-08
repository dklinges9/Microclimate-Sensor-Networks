## Set parameters for each step of the workflow


## Load libraries ---------------

library(tidyverse)

cat("Setting parameters...\n")

## These are parameters relevant to running a power analysis to determine statistical power

## Project parameters -----------
# What is the name of the landscape? Will be appended to names of output files
# If left NA will names files according to the chosen spatial extent
landscape_name <- "oman1"

# Number of sensors
# If left as NA, program will calculate necessary and possible number from
# power analysis and chosen budget
n_sensors <- NA_integer_
# Your budget for sensors
budget <- 5000
cost_per_sensor <- 100

# Approximate number of sites to select (which needs to be no smaller than n_sensors)
# The program will return a list of sites that will be no fewer than n_sites,
# but might be slightly higher
n_sites <- 75

## Spatial parameters ------------

# Spatial extent in decimal degrees
# Suggest specifying an extent that is slightly larger than actual target extent,
# to account for lost edges when reprojecting and when calculating topographical
# variables 
spatial_extent <- c(5042788.27696657, 5064493.44436001, 1817708.23946404, 1850265.99055419) # c(5042788.27696657, 5064493.44436001, 1817708.23946404, 1850265.99055419) # c(-83.8, -83.3, 35.3, 35.8) # c(748472.066251046, 764572.066251046, -2389957.46406275, -2373857.46406275) # c(47.4, 47.55, -21.6, -21.45) # provide also in meters 
# Alternatively a path to a raster file or shapefile can be provided
# (I HAVEN'T YET MADE THESE OPTIONS FUNCTIONAL)
extent_rast <- NA
extent_shp <- NA

# Provide the projection of the specified spatial_extent
# Some example projections:
# "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# "+proj=longlat +datum=WGS84" 
projection <-  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
projection_units <- "m" # c("m", "dd") for 'meters' and 'decimal degrees'
# Your desired resolution of the DEM (must be at least 30 meters), which will
# also determine the resolution at which all analyses are conducted
# The following few parameters can be divided by 120000 for a quick
# translation from meters to decimal degrees
chosen_rez <- 50 # / 120000 # in same units as projection in meters
min_distance <- 100 # / 120000 # min tolerable distance between points, in projection_units
max_distance <- 3500 # / 120000 # max tolerable distance between one point and the
# next nearest point, in projection_units

# A data.frame of sites required to be included in the analysis (either NA or 
# a dataframe of x-y coordinates in projection_units)

# If you are re-running this program after removing some sites, and intending to
# select new sites, it is important to set required_sites to the file path of your
# updated list of valid sites
required_sites <- NA

# For example:
# required_sites <- read_csv("data/chosen_points/selected_points_150_-83.8_-83.3_35.3_35.8.csv")

## Power analysis parameters ----------

# Estimate of power (1 minus Type II error probability). If unknown, kept at 0.8
# OR is calculated
power <- NULL

# Estimate of the explanatory power (r2) of chosen set of predictors for
# explaining response variable of interest (that is measured by sensors), OR
# desired r2. 
# Used for calculating f2 (effect size) in power analysis. If unknown, kept at 0.15
# (reasonably cautious for most environmental relationships)
r2 <- 0.15

## Environmental layers ---------
chosen_layers <- c("elevation", "slope", "aspect", "landcover") #  "ndvi", "proportion_forest", "patch_area_4neighbr"
# When selecting sites, do you want to give more weight to outliers within 
# the environmental space (e.g. more topographically complex areas)?
favor_outliers <- FALSE 
# These parameters will become important once I've added in functionality for
# NDVI and forest cover
ndvi_threshold <- .6
forestcover2000_threshold <- 65 # what is threshold cover to be "forest"

cat("Setting parameters - OK!\n")