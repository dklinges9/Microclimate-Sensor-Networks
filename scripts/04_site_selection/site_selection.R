## David Klinges
## Site selection algorithm

## 1. Workspace prep --------

cat("\n\nPrepping data for site selection....\n")


## .... Load dependencies ---------
pkgs <- c("crayon", "dplyr", "magrittr", "tibble", "readr", "sf", "terra", "factoextra", "FactoMineR")

for (i in seq_along(pkgs)) {
  suppressPackageStartupMessages(
    suppressWarnings(library(pkgs[i], character.only = TRUE))
  )
  if (i == length(pkgs)) { rm(pkgs, i) }
}

layers <- rast(paste0("data/spatial_drivers/combined/layers_",
                      round(chosen_rez, 4), "_", filepattern, ".tif"))


## Exclude any masked cells -------------

if (any(class(layer_mask) == "SpatRaster")) {
  # Resample 
  layer_mask2 <- terra::resample(layer_mask, layers, method = "near")
  
  # Mask `layers`
  layers <- terra::mask(layers, layer_mask2, maskvalues = 0, updatevalue=NA)
}

## Extract sites from raster grids -----------

layers_df <- as.data.frame(layers[[1]], xy = T)
layers_df <- layers_df %>% 
  dplyr::select(x, y)

## Add in required sites, if any
if (any(!is.na(required_sites))) {
  layers_df <- layers_df %>% 
    bind_rows(dplyr::select(required_sites, x, y)) %>% 
    distinct(x, y)
}

# Automatically removes cells that are NA across all layers
layers_df <- terra::extract(layers, layers_df, xy = T, ID = FALSE)

# Remove rows with NA
layers_df <- layers_df %>% 
  filter_all(~complete.cases(.))

## Recode landcover as factor -----------
if ("landcover" %in% chosen_layers) {
  
  landcover_link <- suppressMessages(read_csv("data/spatial_drivers/landcover/original/esa_cci/esa_cci_landcover_link.csv"))
  
  layers_df <- layers_df %>% 
    dplyr::rename(ungrouped_number = landcover) %>% 
    left_join(landcover_link %>% 
                dplyr::rename(landcover = grouped_number, landcover_class = grouped_class) %>% 
                dplyr::select(landcover, landcover_class, ungrouped_number), by = join_by(ungrouped_number)) %>%
    dplyr::select(-ungrouped_number) %>% 
    dplyr::select(x, y, landcover, landcover_class, everything()) %>% 
    mutate(landcover = as.factor(landcover))
}

## Exclude coordinates classified as water -----------
if ("landcover" %in% chosen_layers) {
  if (any(grepl("water", layers_df$landcover_class))) {
    cat("Removing spatial coordinates classified as water from selection process....\n")
    terrain_df <- layers_df %>% 
      dplyr::filter(!grepl("water", landcover_class))
    
    if (nrow(terrain_df) < 1) {
      stop("Removing spatial coordinates classified as water removed all locations. Check to confirm that there is dry land in your scene?\n")
    }
  } else {
    terrain_df <- layers_df
  }
} else {
  terrain_df <- layers_df
}

## square root correction of data that would heavily favour outliers (e.g. in mountains) ------------
# optional step determined by user inputs

if (favor_outliers) {
  cat("Favoring outlier values as requested....\n")
  # Applies to all spatial drivers except landcover
  terrain_df <- terrain_df %>% 
    mutate_at(vars(all_of(chosen_layers[!chosen_layers == "landcover"])), ~sqrt(. + 0.0001))
}


## Create functions to ensure min and max distance between sites -------

check_min_dist <- function(selected_sites, min_distance, attempts = 10, recur = 0) {
  # Initialize badcoordcount
  badcoordcount <- 0
  
  # Determine euclidean distance all sites and all other sites
  dis_spatial <- dist(dplyr::select(selected_sites, x, y), method = "euclidean")
  
  # Coerce to dataframe
  dis_spatial_df <- as.data.frame(as.matrix(dis_spatial, nrow = nrow(selected_sites),
                                            ncol = nrow(selected_sites)))
  
  # Find any coords that are too close to any other point
  bad_coord <- dis_spatial_df %>% 
    dplyr::filter_all(any_vars(. < min_distance & . > 0)) %>%
    # Identify what rows these were, as this will allow us to join back to selected_sites
    rownames_to_column()
  
  # If looped entirely through and did not find another point closer 
  # than min_distance and did not find a good coordinate...
  if (nrow(bad_coord) > 0) {
    cat("Found", nrow(bad_coord), "site(s) that were too close to another site, and so searching for new site(s)...\n")
    
    # Pull out the bad coords from selected_sites
    new_selected_sites <- selected_sites %>% 
      rownames_to_column() %>% 
      dplyr::filter(rowname %in% bad_coord$rowname)
    
    # Remove bad coords
    selected_sites <- selected_sites %>% 
      rownames_to_column() %>% 
      dplyr::filter(!rowname %in% bad_coord$rowname) %>% 
      dplyr::select(-rowname)
    
    # Select new sites
    for (i in 1:nrow(new_selected_sites)) {
      iter <- terrain_df %>% 
        dplyr::filter(dim1_bin %in% new_selected_sites$dim1_bin[i] & 
                        dim2_bin %in% new_selected_sites$dim2_bin[i] &
                        dim3_bin %in% new_selected_sites$dim3_bin[i])
      
      if (nrow(iter) > 0) {
        iter <- iter %>% 
          group_by(dim1_bin, dim2_bin, dim3_bin) %>% 
          sample_n(1) %>% 
          ungroup()
        
      } else {
        badcoordcount <- badcoordcount + 1
        # If no other points in this bin, then need to just use the prior selection
        iter <- new_selected_sites[i,] %>% 
          dplyr::select(-rowname)
      }
      
      if (i == 1) {
        out <- iter
      } else {
        out <- bind_rows(out, iter)
      }
    }
    
    # Add in new coords
    selected_sites <- selected_sites %>% 
      bind_rows(out)
    
    # Remove from environment
    rm("out")
    
    # Recursive call
    # But if attempts-th attempt, then cancel
    if (recur > attempts) {
      cat("Could not find sites that were not above min distance after ",
          attempts, " attempts. Choosing last site.\n")
    } else {
      selected_sites <- check_min_dist(selected_sites, min_distance, recur = recur + 1)    
    }
  } else {
    if (recur >= 1 & recur <= attempts) {
      if (badcoordcount > 0) {
        cat(badcoordcount, "of the chosen sites had to be closer than your specified min distance.\n")
      } else {
        cat("Successfully found all sites that are all far enough away from each other.\n")
      }
    }
  }
  
  # Then return selected_sites
  return(selected_sites)
}

check_max_dist <- function(selected_sites, max_distance, attempts = 10, recur = 0) {
  # Initialize badcoordcount
  badcoordcount <- 0
  
  # Determine euclidean distance all sites and all other sites
  dis_spatial <- dist(dplyr::select(selected_sites, x, y), method = "euclidean")
  
  # Coerce to dataframe
  dis_spatial_df <- as.data.frame(as.matrix(dis_spatial, nrow = nrow(selected_sites),
                                            ncol = nrow(selected_sites)))
  
  # Find any coords that are too far away from everything else
  bad_coord <- dis_spatial_df %>% 
    dplyr::filter_all(all_vars(. > max_distance | . == 0)) %>%
    # Identify what rows these were, as this will allow us to join back to selected_sites
    rownames_to_column()
  
  # If looped entirely through and did not find another point closer 
  # than max_distance and did not find a good coordinate...
  if (nrow(bad_coord) > 0) {
    cat("Found", nrow(bad_coord), "site(s) that were too far away from all other sites, and so searching for new site(s)...\n")
    
    # Pull out the bad coords from selected_sites
    new_selected_sites <- selected_sites %>% 
      rownames_to_column() %>% 
      dplyr::filter(rowname %in% bad_coord$rowname)
    
    # Remove bad coords
    selected_sites <- selected_sites %>% 
      rownames_to_column() %>% 
      dplyr::filter(!rowname %in% bad_coord$rowname) %>% 
      dplyr::select(-rowname)
    
    # Select new sites
    for (i in 1:nrow(new_selected_sites)) {
      iter <- terrain_df %>% 
        dplyr::filter(dim1_bin %in% new_selected_sites$dim1_bin[i] & 
                        dim2_bin %in% new_selected_sites$dim2_bin[i] &
                        dim3_bin %in% new_selected_sites$dim3_bin[i])
      
      if (nrow(iter) > 0) {
        iter <- iter %>% 
          group_by(dim1_bin, dim2_bin, dim3_bin) %>% 
          sample_n(1) %>% 
          ungroup()
        
        badcoordcount <- badcoordcount + 1
        
      } else {
        # If no other points in this bin, then need to just use the prior selection
        iter <- new_selected_sites[i,] %>% 
          dplyr::select(-rowname)
      }
      
      if (i == 1) {
        out <- iter
      } else {
        out <- bind_rows(out, iter)
      }
    }
    
    # Add in new coords
    selected_sites <- selected_sites %>% 
      bind_rows(out)
    
    # Recursive call
    # But if attempts-th attempt, then cancel
    if (recur > attempts) {
      cat("Could not find sites that were not below max distance after ",
          attempts, " attempts. Choosing last site.\n")
    } else {
      selected_sites <- check_max_dist(selected_sites, max_distance, recur = recur + 1)    
    }
  } else {
    if (recur >= 1 & recur <= attempts) {
      if (badcoordcount > 0) {
        cat(badcoordcount, "of the chosen sites had to be farther away than your specified max distance.\n")
      } else {
        cat("Successfully found all sites that are all close enough to each other.\n")
      }
    }
  }
  
  # Then return selected_sites
  return(selected_sites)
}


## Create selected_sites dataframe, add in any required or pre-visited sites -------------

if(any(!is.na(required_sites))) {
  cat("Adding in required sites....\n")
  
  if (all(c("dim1_bin", "dim2_bin", "dim3_bin") %in% colnames(required_sites))) {
    cat("Required sites have ordination bins from a prior run, so selecting for complementary sites...\n")
    
    selected_sites <- dplyr::select(required_sites, x, y, dim1_bin, dim2_bin, dim3_bin) %>% 
      mutate(bin_string = paste(dim1_bin, dim2_bin, dim3_bin)) %>% 
      left_join(terrain_df, by = join_by(x, y))
    
  } else {
    selected_sites <- dplyr::select(required_sites, x, y) %>% 
      left_join(terrain_df, by = join_by(x, y))
  }
} else {
  selected_sites <- c()
}

## Ordination algorithm ----------
# random selection in each bin is used; use set.seed to have the same selection in each run
set.seed(seed = 1)

cat("Conducting ordination algorithm....\n")

if (program_rerun) {
  cat("Re-running program, and therefore using ordination values from last run...\n")
  if (any(complete.cases(landscape_bins))) {
    terrain_df <- landscape_bins 
  }
} else {
  enviro_layers_df <- terrain_df %>% 
    dplyr::select(all_of(chosen_layers)) 
  
  if ("landcover" %in% chosen_layers) {
    ## FAMD (Factor Analysis of Mixed Data) if includes categorical vars ----------
    # FAMD allows inclusion of non-continuous data
    
    # Specifying number of dimensions (ncp) as 3 as this is ndims to be included
    # in selection below
    # While with PCA via `prcomp()` below you must specify to center and scale
    # continuous variables, this is done automatically in `FAMD()` (by necessity,
    # given the comparison to non-continuous variables)
    enviro_ord <- FAMD(na.omit(enviro_layers_df), ncp = 3, graph = FALSE)
    
    cat("Summary of FAMD:\n")
    print(enviro_ord)
    
    cat("\n
      \n")
    
    eigen_vals <- get_eigenvalue(enviro_ord)
    print(eigen_vals)
    
    cat("\n
      \n")
    
    cat("Saving Eigen values docs/analysis/....\n")
    write_csv(as.data.frame(eigen_vals), paste0("docs/analysis/", filepattern, 
                                                "_", n_sites, "_FAMD_eigen_vals.csv"))
    
    cat("\n
      \n")
    
    scree_plot <- fviz_screeplot(enviro_ord)
    
    cat("Saving scree plot to figures/analysis/....\n")
    ggsave(plot = scree_plot, filename = paste0("figures/analysis/", filepattern,
                                                "_", n_sites, "_FAMD_screeplot.png"),
           height = 5, width = 6)
    
    cat("Add FAMD coordinates to our dataframe of spatial drivers...\n")
    # Add FAMD coordinates to our big table
    terrain_df <- cbind(terrain_df,get_famd_ind(enviro_ord)$coord[,1:3]) 
    
  } else {
    ## PCA (if no categorical vars) -----------------
    
    cat("\nConducting PCA of environmental variables...\n")
    
    enviro_ord <- prcomp(na.omit(enviro_layers_df), center = TRUE, scale. = TRUE)
    
    cat("Summary of PCA:\n")
    print(summary(enviro_ord))
    
    cat("\n
      \n")
    
    eigen_vals <- get_eigenvalue(enviro_ord)
    print(eigen_vals)
    
    cat("\n
      \n")
    
    cat("Saving Eigen values docs/analysis/....\n")
    write_csv(as.data.frame(eigen_vals), paste0("docs/analysis/", filepattern, 
                                                "_", n_sites, "_FAMD_eigen_vals.csv"))
    
    cat("\n
      \n")
    
    scree_plot <- fviz_screeplot(enviro_ord)
    
    cat("Saving scree plot to figures/analysis/....\n")
    ggsave(plot = scree_plot, filename = paste0("figures/analysis/", filepattern,
                                                "_", n_sites, "_PCA_screeplot.png"),
           height = 5, width = 6)
    
    cat("Add PCA coordinates to our dataframe of spatial drivers...\n")
    # Add PCA coordinates to our big table
    terrain_df <- cbind(terrain_df,get_pca_ind(enviro_ord)$coord[,1:3]) 
    
  }
}

## Perform Hierarchical Selection ----------

if (!program_rerun) {
  #Selection of sample locations by dividing 3D environmental space into little cubes, 
  #and selecting same amount of locations in each cube
  #Note that this is just one possible way in which the selection can be made, 
  #alternatively, a more rigorous approach could be used that maximimizes the environmental distances between the 
  #selected sites (e.g., using Euclidian distances and the function dist() in the stats package)
  #such an approach would however take very long to run, especially for large areas, making it less user-friendly.
  
  # Note: algorithm does NOT cycle until all these sites have been achieved, 
  # although it has two optional clauses to select additional locations till this number is reached (see below)
  
  Groups <- round(n_sites^0.33) # 0.33 is used, as we are using environmental space as a cube, which we want to cut into little cubes, with selection of n_sites_1 in each cube. 0.33 gives a reasonable selection, but number can be changed to have a coarser or more strict selection
  
  # Divide each ordination axis in the same number of bins with equal sizes along each axis
  Steps_Dim.1<-(max(terrain_df$Dim.1)-min(terrain_df$Dim.1))/Groups
  Steps_Dim.2<-(max(terrain_df$Dim.2)-min(terrain_df$Dim.2))/Groups
  Steps_Dim.3<-(max(terrain_df$Dim.3)-min(terrain_df$Dim.3))/Groups
  
  cat(red("\n\nBEGINNING SITE SELECTION....\n\n"))
  
  # Sleep for 1.5 seconds so user can see messages
  Sys.sleep(1.5)
  
  # Cut terrain_df into bins
  terrain_df <- terrain_df %>% 
    dplyr::select(x, y, Dim.1, Dim.2, Dim.3) %>% 
    # Cut dimensions into bins. And, coerce to character to make joining
    # required_sites easier in the future
    mutate(dim1_bin = as.character(cut(Dim.1, Groups)),
           dim2_bin = as.character(cut(Dim.2, Groups)),
           dim3_bin = as.character(cut(Dim.3, Groups))) %>% 
    mutate(bin_string = paste(dim1_bin, dim2_bin, dim3_bin))
}

## ....If previous sites, remove these from ordination space --------

# This will ensure that new sites chosen will be complementary to the the sites
# already present in selected_sites

if(any(!is.na(selected_sites))) {
  # If required_sites already has bins, remove those from terrain_df
  if (all(c("dim1_bin", "dim2_bin", "dim3_bin") %in% colnames(selected_sites))) {
    selected_sites <- selected_sites %>% 
      mutate(bin_string = paste(dim1_bin, dim2_bin, dim3_bin))
    
    # Remove bins represented by required_sites
    terrain_toSample_df <- terrain_df %>% 
      dplyr::filter(!bin_string %in% selected_sites$bin_string)
  } else {
    # If required_sites does not already has bins, join the bins, then remove
    # them from terrain_df
    selected_sites <- selected_sites %>% 
      left_join(terrain_df %>% 
                  dplyr::select(x, y, dim1_bin, dim2_bin, dim3_bin),
                by = join_by(x, y, dim1_bin, dim2_bin, dim3_bin)) %>% 
      mutate(bin_string = paste(dim1_bin, dim2_bin, dim3_bin))
    
    # Remove bins represented by required_sites
    terrain_toSample_df <- terrain_df %>% 
      dplyr::filter(!bin_string %in% selected_sites$bin_string)
  }
} else {
  # If not re-running, then still need to create the terrain_toSample_df
  terrain_toSample_df <- terrain_df
}

# Count the number of unique bins
n_bins <- nrow(terrain_toSample_df %>% 
                 group_by(dim1_bin, dim2_bin, dim3_bin) %>% 
                 count() %>% 
                 ungroup())

# Determine number of new sites to be added (if some sites already required/selected)
if(any(!is.na(selected_sites))) {
  n_sites_new <- n_sites - nrow(selected_sites)
} else {
  n_sites_new <- n_sites 
}


# Determine the number of samples we can afford to take from each bin
sample_size <- floor(n_sites_new / n_bins)

if (sample_size < 1) {
  cat("You did not specify enough sites in order to sample evenly across your target landscape...provided locations will be uneven sampling. \n")
  
  # Don't let sample_size be 0
  sample_size <- 1
}

# Report to user
cat("There are", n_bins, "equal-volume bins of environmental space present within your landscape. With", n_sites_new, "sites, you are able to sample", sample_size, "sites per bin.\n")

# Perform sampling
selected_sites <- terrain_toSample_df %>% 
  group_by(dim1_bin, dim2_bin, dim3_bin) %>% 
  sample_n(sample_size, replace = TRUE) %>% 
  ungroup() %>% 
  bind_rows(selected_sites)

# Remove redundant coordinates
selected_sites <- distinct(selected_sites)

# Check that all sites are below maximum distance
selected_sites <- check_max_dist(selected_sites, max_distance, attempts = 10, recur = 0)
# Check that all sites are above minimum distance
selected_sites <- check_min_dist(selected_sites, min_distance, attempts = 10, recur = 0)

cat(red("COMPLETED SITE SELECTION.\n"))

## ....Checks on sites -------------

# Summarize distances between sites
dis_spatial <- dist(dplyr::select(selected_sites, x, y), method = "euclidean")
dist_df <- as.data.frame(as.matrix(dis_spatial))

# Find any rows (sites) that have any distances less than min_distance
close_df <- dist_df %>% 
  # Replace 0 (self_distance) with max dist
  mutate_all(~case_when(. != 0 ~ .,
                        . == 0 ~ min(dist_df)
  )) %>% 
  dplyr::filter_all(any_vars(. < min_distance & . != 0))

if (nrow(close_df) > 0) {
  cat("There are", nrow(close_df), "sites that are closer to at least one other site than your specified min_distance, as there were no other sites to choose from that represented environmental space as well as these.
  
You can investigate these sites separately in the file", paste0("data/chosen_sites/diagnostics/", filepattern, 
                                                                "_", n_sites, "_close_coords.csv"), "\n")
  
  # Find coordinates for too-near sites
  close_coords <- selected_sites %>% 
    dplyr::slice(which(colnames(close_df) %in% rownames(close_df)))
  
  if (!dir.exists("data/chosen_sites/diagnostics/")) {
    cat(red("Directory `data/chosen_sites/diagnostics/` does not exist.\n Create directory? (Y/N)\n"))
    ans0 <- readline(" ")
    
    if (!tolower(ans0) %in% c("n", "no", "y", "yes")) {
      stop("Inappropriate input. Must be one of: yes, YES, Y, y, no, NO, N, n.\n")
    }
    if (tolower(ans0) %in% c("y", "yes")) {
      cat(red("Creating directory: `data/chosen_sites/diagnostics/`"))
      dir.create("data/chosen_sites/diagnostics/", recursive = T)
    } 
    if (tolower(ans0) %in% c("n", "no")) {
      stop("Directory was not created, program ended.")
    }
  }
  
  write_csv(close_coords, paste0("data/chosen_sites/diagnostics/", filepattern, 
                                 "_", n_sites, "_close_coords.csv"))
}

# Find any rows (sites) that have all distances greater than max_distance
far_df <- dist_df %>% 
  # Replace 0 (self_distance) with max dist
  mutate_all(~case_when(. != 0 ~ .,
                        . == 0 ~ max(dist_df)
  )) %>% 
  dplyr::filter_all(all_vars(. > max_distance & . != 0))

if (nrow(far_df) > 0) {
  cat("There are", nrow(far_df), "sites that are farther away from all sites than your specified max_distance, as there were no other sites to choose from that represented environmental space as well as these.
  
You can investigate these sites separately in the file /data/chosen_sites/diagnostics/far_coords.csv.\n")
  
  # Find coordinates for too-near sites
  far_coords <- selected_sites %>% 
    dplyr::slice(which(colnames(far_df) %in% rownames(far_df)))
  
  write_csv(far_coords, paste0("data/chosen_sites/diagnostics/", filepattern, 
                               "_", n_sites, "_far_coords.csv"))
}


# If still not enough sites to go to the final requested number, just fill up with random sites
if(nrow(selected_sites) < n_sites_new){
  cat(red("NOTE: only need ", nrow(selected_sites), 
          " sites to represent environmental variance in this landscape. Do you want ", 
          n_sites_new - nrow(selected_sites), " more sites to bring your total to ",
          n_sites_new, "? (Y/N): "))
  ans1 <- readline(" ")
  
  if (tolower(ans1) %in% c("y", "yes")) {
    cat("Choosing", n_sites_new - nrow(selected_sites), "more sites.\n")
    
    # Perform sampling
    selected_sites <- terrain_toSample_df %>% 
      sample_n(n_sites_new - nrow(selected_sites)) %>% 
      ungroup() %>% 
      bind_rows(selected_sites)
    
    # Check that all sites are below maximum distance
    selected_sites <- check_max_dist(selected_sites, max_distance, attempts = 10, recur = 0)
    # Check that all sites are above minimum distance
    
  }
  if (tolower(ans1) %in% c("n", "no")) {
    cat("Not choosing any more sites, providing", nrow(selected_sites), "sites.\n")
  }
}


## Add environmental data back in to selected_sites -------------

selected_sites <- selected_sites %>% 
  # Must keep bin values
  dplyr::select(x, y, dim1_bin, dim2_bin, dim3_bin) %>% 
  # Also add in ordination dimension values
  left_join(terrain_df, by = join_by(x, y, dim1_bin, dim2_bin, dim3_bin))

# Given that the coordinates from terra::extract() may not be exact, we will
# join according to rownames, rather than coordinates
selected_sites <- selected_sites %>% 
  rownames_to_column() %>% 
  left_join(
    terra::extract(layers, selected_sites %>% dplyr::select(x, y), xy = T, ID = FALSE) %>% 
      rownames_to_column() %>% 
      dplyr::select(-x, -y), by = join_by(rowname)
  ) %>% 
  dplyr::select(-rowname)

# If landcover included, join back landcover class names
if ("landcover" %in% chosen_layers) {
  selected_sites <- selected_sites %>% 
    dplyr::rename(ungrouped_number = landcover) %>% 
    left_join(landcover_link %>% 
                dplyr::rename(landcover = grouped_number, landcover_class = grouped_class) %>% 
                dplyr::select(landcover, landcover_class, ungrouped_number), by = join_by(ungrouped_number)) %>%
    dplyr::select(-ungrouped_number)
}

## Reorder columns
selected_sites <- selected_sites %>% 
  dplyr::select(x, y, Dim.1, Dim.2, Dim.3, dim1_bin, dim2_bin, dim3_bin, everything())

## Create rasters from landscape bins -------

## All values that are NA for the bins are water pixels
bins_df <- as.data.frame(layers[[1]], xy = T) %>% 
  dplyr::select(x, y) %>% 
  distinct() %>% 
  full_join(terrain_df) %>% 
  # Remove x and y as we don't need those columns
  dplyr::select(-x, -y)

# Create a template raster from layers
bin_r <- layers[[1]]

dim1_r <- terra::setValues(bin_r, bins_df %>% 
                                dplyr::select(Dim.1))
dim2_r <- terra::setValues(bin_r, bins_df %>% 
                             dplyr::select(Dim.2))
dim3_r <- terra::setValues(bin_r, bins_df %>% 
                             dplyr::select(Dim.3))
# 3 layers, one for each of ordination dimensions 1, 2, and 3
dim_values_r <- c(dim1_r, dim2_r, dim3_r)
names(dim_values_r) <- c("Dim_1", "Dim_2", "Dim_3")

dim1_id_r <- terra::setValues(bin_r, bins_df %>% 
                             dplyr::select(dim1_bin))
dim2_id_r <- terra::setValues(bin_r, bins_df %>% 
                             dplyr::select(dim2_bin))
dim3_id_r <- terra::setValues(bin_r, bins_df %>% 
                             dplyr::select(dim3_bin))
binString_id_r <- terra::setValues(bin_r, bins_df %>% 
                             dplyr::select(bin_string))
# 4 layers, one for each of ordination dimensions 1, 2, and 3 AND the collective bin ID
bin_id_r <- c(dim1_id_r, dim2_id_r, dim3_id_r, binString_id_r)
names(bin_id_r) <- c("Dim_1", "Dim_2", "Dim_3", "Bins")

## Write out CSV of selected sites, and rasters of environmental bins -----------

# Check if CSV for selected sites already exists, if so confirm with user 
# before overwriting
if (file.exists(paste0("data/chosen_sites/selected_sites_",
                       filepattern, "_", n_sites, ".csv"))) {
  cat(red("File ", 
          paste0("data/chosen_sites/selected_sites_",
                 filepattern, "_", n_sites, ".csv"), 
          " already exists. Overwrite? (Y/N): "))
  ans1 <- readline(" ")
  
  if (!tolower(ans1) %in% c("n", "no", "y", "yes")) {
    stop("Inappropriate input. Must be one of: yes, YES, Y, y, no, NO, N, n.\n")
  }
  
  if (tolower(ans1) %in% c("y", "yes")) {
    
    write_csv(layers_df, paste0("data/landscape_data/layers_", 
                                filepattern, "_", n_sites, ".csv"))
    write_csv(terrain_df, paste0("data/landscape_data/landscape_bins_", filepattern,
                                 "_", n_sites, ".csv"))
    write_csv(selected_sites, paste0("data/chosen_sites/selected_sites_",
                                     filepattern, "_", n_sites, ".csv"))
    cat("\n\nSelected sites are saved to a CSV file:\n", paste0("data/chosen_sites/selected_sites_", filepattern, "_", n_sites, ".csv"))
  }
  if (tolower(ans1) %in% c("n", "no")) {
    cat("File not overwritten, but selected sites still available in object `selected_sites`.\n")
  }
} else {
  # Check if directories need to be created
  if (!dir.exists("data/landscape_data/")) {
    cat(red("Directory `data/landscape_data` does not exist.\n Create directory? (Y/N)\n"))
    ans2 <- readline(" ")
    
    if (!tolower(ans2) %in% c("n", "no", "y", "yes")) {
      stop("Inappropriate input. Must be one of: yes, YES, Y, y, no, NO, N, n.\n")
    }
    if (tolower(ans2) %in% c("y", "yes")) {
      cat(red("Creating directory: `data/landscape_data`"))
      dir.create("data/landscape_data", recursive = T)
    } 
    if (tolower(ans2) %in% c("n", "no")) {
      stop("Directory was not created, program ended.")
    }
  }
  
  # Now write out files
  write_csv(layers_df, paste0("data/landscape_data/layers_", 
                               filepattern, "_", n_sites, ".csv"))
  write_csv(terrain_df, paste0("data/landscape_data/landscape_bins_", 
                               filepattern, "_", n_sites, ".csv"))
  write_csv(selected_sites, paste0("data/chosen_sites/selected_sites_",
                                   filepattern, "_", n_sites, ".csv"))
}

# Save bin rasters
save_raster(dim_values_r, 
            "data/landscape_data/dim_values_", 
            filepattern)
save_raster(bin_id_r, 
            "data/landscape_data/bin_id_", 
            filepattern)
