## David Klinges
## Site selection algorithm

## 1. Workspace prep --------

cat("\n\nPrepping data for site selection....\n")

## .... Load dependencies ----------

library(tidyverse)
library(sf)
library(terra)
library(factoextra)

# Specify string for naming output files
if (complete.cases(landscape_name)) {
  filepattern <- landscape_name
} else {
  filepattern <- paste(round(spatial_extent, 0), collapse = "_")
}

## Extract sites from raster grids -----------

layers_df <- as.data.frame(layers[[1]], xy = T)
layers_df <- layers_df %>% 
  dplyr::select(x, y)

## Add in required sites, if any
if(any(!is.na(required_sites))) {
  layers_df <- layers_df %>% 
    bind_rows(dplyr::select(required_sites, x, y)) %>% 
    distinct(x, y)
}

layers_df <- terra::extract(layers, layers_df, xy = T, ID = FALSE)

# Remove rows with NA
layers_df<-layers_df[complete.cases(layers_df),]

## Recode landcover as character -----------
if ("landcover" %in% chosen_layers) {
  layers_df <- layers_df %>% 
    dplyr::rename(ungrouped_number = landcover) %>% 
    left_join(landcover_link %>% 
                dplyr::rename(landcover = grouped_number, landcover_class = grouped_class) %>% 
                dplyr::select(landcover, landcover_class, ungrouped_number), by = join_by(ungrouped_number)) %>%
    dplyr::select(-ungrouped_number) %>% 
    dplyr::select(x, y, landcover, landcover_class, everything())
}

## Exclude coordinates classified as water -----------
if ("landcover" %in% chosen_layers) {
  if (any(grepl("water", layers_df$landcover_class))) {
    cat("Removing spatial coordinates classified as water from selection process....\n")
    terrain_df <- layers_df %>% 
      dplyr::filter(!grepl("water", landcover_class))
  } else {
    terrain_df <- layers_df
  }
} else {
  terrain_df <- layers_df
}
  

## square root correction of data that would heavily favour outliers (e.g. in mountains) ------------
# optional step determined by user inputs

if (favor_outliers) {
  # Applies to all spatial drivers except landcover
  terrain_df <- terrain_df %>% 
    mutate_at(vars(all_of(chosen_layers[!chosen_layers == "landcover"])), ~sqrt(. + 0.0001))
}

## PCA environmental variables -----------------

cat("\nConducting PCA of environmental variables...\n")

Layers <- terrain_df %>% 
  dplyr::select(all_of(chosen_layers)) #For simplicity, a PCA is used, so non-continuous data is excluded. 
#Alternatively, a FAMD can be used that allows inclusion of non-continuous data. 
#X and Y are excluded here as well, but can be included as well to maximize spatial variability

enviro_pca <- prcomp(na.omit(Layers), center = TRUE, scale. = TRUE)

cat("Summary of PCA:\n")
print(summary(enviro_pca))

cat("Add PCA coordinates to our dataframe of spatial drivers...\n")

terrain_df <- cbind(terrain_df,get_pca_ind(enviro_pca)$coord[,1:3]) #add PCA coordinates to our big table


## Create functions to ensure min and max distance between sites -------

check_min_dist <- function(Selection, min_distance, select_fun, attempts = 10, recur = 0) {

  # Determine euclidean distance between new selection and all 
  # previous selections
  # Could use dist(), but takes too long. All we need to know is the
  # distance between the newest sites and all other sites, as opposed
  # to the distance between all sites and all other sites
  # dis_spatial <- dist(dplyr::select(Selection, x, y), method = "euclidean")
  
  # Find last row of Selection
  s <- dplyr::slice(Selection, nrow(Selection))
  dis_spatial <- apply(dplyr::select(Selection, x, y), 1, function(row) {
    # sqrt(sum((x_i - y_i)^2))
    sqrt((row[1] - s[,1])^2 + (row[2] - s[,2])^2)
  })

  # Remove last row of dis_spatial, as is self-distance
  dis_spatial <- dis_spatial[1:(length(dis_spatial)-1)]

  # If smallest distance is smaller than min_distance...
  if (suppressWarnings(min(unlist(dis_spatial), na.rm =T) < min_distance)) {
    cat("This site too close to another site, and so finding new one...\n")
    
    # Remove last row
    Selection <- dplyr::slice(Selection, 1:(nrow(Selection) - 1))
    
    # Select new sites
    Selection <- select_fun(Selection)

    # Recursive call
    # But if attempts-th attempt, then cancel
    if (recur > attempts) {
      cat("Could not find sites that were not above min distance after ",
              attempts, " attempts. Choosing last site.\n")
    } else {
      Selection <- check_min_dist(Selection, min_distance, select_fun(Selection), recur = recur + 1)
    }
  } else {
    if (recur >= 1 & recur <= attempts) {
      cat("Successfully found a new site that is far enough away from all others.\n")
    }
  }
  
  # Then return selection
  return(Selection)
}

check_max_dist <- function(Selection, max_distance, select_fun, attempts = 10, recur = 0) {
  # Determine euclidean distance between new selection and all 
  # previous selections
  # Could use dist(), but takes too long. All we need to know is the
  # distance between the newest sites and all other sites, as opposed
  # to the distance between all sites and all other sites
  # dis_spatial <- dist(dplyr::select(Selection, x, y), method = "euclidean")
  
  # Find last row of Selection
  s <- dplyr::slice(Selection, nrow(Selection))
  xs <- s$x
  ys <- s$y
  # Loop back through each prior coordinate (so not including last coordinate....
  good_coord <- FALSE
  for (coord in 1:((nrow(Selection)) - 1)) {
    xi <- Selection$x[coord]
    yi <- Selection$y[coord]
    dist <- sqrt((xi - xs)^2 + (yi - ys)^2)
    if (complete.cases(dist)) {
      if (dist <= max_distance) {
        good_coord <- TRUE
        break
      }
    }
  }
  # If looped entirely through and did not find another point closer 
  # than max_distance and did not find a good coordinate...
  if (!good_coord) {
    cat("This point too far away from all others, and so finding new one...\n")
    
    # Remove last row
    Selection <- dplyr::slice(Selection, 1:(nrow(Selection) - 1))
    
    # Select new sites
    Selection <- select_fun(Selection)   
    
    # Recursive call
    # But if attempts-th attempt, then cancel
    if (recur > attempts) {
      cat("Could not find sites that were not below max distance after ",
              attempts, " attempts. Choosing last site.\n")
    } else {
      Selection <- check_max_dist(Selection, max_distance, select_fun(Selection), recur = recur + 1)    
    }
  } else {
    if (recur >= 1 & recur <= attempts) {
      cat("Successfully found a new site that is close enough to at least one other site.\n")
    }
  }
  
  # Then return selection
  return(Selection)
}


## Add in any required or pre-visited sites -------------

if(any(!is.na(required_sites))) {
  cat("Adding in required sites....\n")
  Selection <- dplyr::select(required_sites, x, y) %>% 
    left_join(terrain_df)
} else {
  Selection <- c()
}

## Perform Hierarchical Selection ----------

#Selection of sample locations by dividing 3D environmental space into little cubes, 
#and selecting same amount of locations in each cube
#Note that this is just one possible way in which the selection can be made, 
#alternatively, a more rigorous approach could be used that maximimizes the environmental distances between the 
#selected sites (e.g., using Euclidian distances and the function dist() in the stats package)
#such an approach would however take very long to run, especially for large countries, making it less user-friendly.


#Note: algorithm does NOT cycle until all these sites have been achieved, 
#although it has two optional clauses to select additional locations till this number is reached (see below)

Groups <- ceiling(n_sites^0.33) #0.33 is used, as we are using environmental space as a cube, which we want to cut in little cubes,
#with selection of n_sites in each cube. 0.33 gives a reasonable selection, but number can be changed to have a coarser or more strict selection

#divide each PCA axis in the same number of bins with equal sizes along each axis
Steps_Dim.1<-(max(terrain_df$Dim.1)-min(terrain_df$Dim.1))/Groups
Steps_Dim.2<-(max(terrain_df$Dim.2)-min(terrain_df$Dim.2))/Groups
Steps_Dim.3<-(max(terrain_df$Dim.3)-min(terrain_df$Dim.3))/Groups

set.seed(1) #random selection in each bin is used; use set.seed to have the same selection in each run

cat("\n\nBEGINNING SITE SELECTION....\n\n")

# Sleep for 1 seconds so user can see messages
Sys.sleep(1.5)

for (i in 1:Groups) {
   #hierarchical selection procedure, first based on dimension 1, 
   #then on dim 2 and 3, each time within the groups of Dim. 1
  #select all sites within the i'th bin of axis 1
  Selection1 <- terrain_df[terrain_df$Dim.1>(suppressWarnings(min(terrain_df$Dim.1))+(i-1)*Steps_Dim.1) & 
                             terrain_df$Dim.1<(suppressWarnings(min(terrain_df$Dim.1))+i*Steps_Dim.1),]
  for (j in 1:Groups) {
    #select all sites from the i'th bin that fall in the j'th bin of axis 2
    Selection2 <- Selection1[Selection1$Dim.2>(suppressWarnings(min(Selection1$Dim.2))+(j-1)*Steps_Dim.2) & 
                               Selection1$Dim.2<(suppressWarnings(min(Selection1$Dim.2))+j*Steps_Dim.2),]
    for (k in 1:Groups) {
      #select all sites in that j'th bin that fall within the k'th bin of axis 3
      Selection3 <- Selection2[Selection2$Dim.3>(suppressWarnings(min(Selection2$Dim.3))+(k-1)*Steps_Dim.3) & 
                                 Selection2$Dim.3<(suppressWarnings(min(Selection2$Dim.3))+k*Steps_Dim.3),]
      
      #randomly select a point within that cube with dimensions i,j,k
      #depending on distribution of locations in the PCA space (and size of the bins suggested earlier), 
      #there will be more or fewer cubes with dimension i,j,k that have no sites, where thus no sites will be selected
      Selection <- rbind(Selection,Selection3[sample(1:nrow(Selection3), 1),])
      
      # Filter to non-NA
      Selection<-Selection[complete.cases(Selection[,1:5]),]
      
      # If not first iteration and this new bin has sites in it, BUT also has
      # more than just one point to choose from....
      if (nrow(Selection) > 1 & nrow(Selection3) > 1) {
        # Designate select_function
        select_fun <- function(Selection) {
          Selection3 <- Selection2[Selection2$Dim.3>(suppressWarnings(min(Selection2$Dim.3))+(k-1)*Steps_Dim.3) &
                                     Selection2$Dim.3<(suppressWarnings(min(Selection2$Dim.3))+k*Steps_Dim.3),]
          Selection <- rbind(Selection,Selection3[sample(1:nrow(Selection3), 1),])
          Selection<-Selection[complete.cases(Selection[,1:5]),]
          return(Selection)
        } 
        
        # Perform min distance check
        Selection <- check_min_dist(Selection, min_distance, select_fun = select_fun)
        # Perform max distance check
        Selection <- check_max_dist(Selection, max_distance, select_fun = select_fun)
      }
    }
  }
  
  if(nrow(Selection[complete.cases(Selection[,1:5]),])<(i*n_sites/Groups)){ 
    #if not enough potential sampling sites in the cubes, 
    #we can fill out with random selections from the main category, as many as are in there
    #this is optional and not done in the paper, as it only matters if a fixed amount of locations is required
    if((i*n_sites/Groups)-nrow(Selection[complete.cases(Selection[,1:5]),])< nrow(Selection1)){
      Selection4 <- Selection1[sample(1:nrow(Selection1), (i*n_sites/Groups)-nrow(Selection[complete.cases(Selection[,1:5]),])),]
      Selection <- rbind(Selection, Selection4)
    } else {
      Selection4 <- Selection1
      Selection <- rbind(Selection, Selection4)
    }
    # If not first iteration and this new bin (Selection4) has sites in it, BUT also has
    # more than just one point to choose from....
    if (k > 1 | j > 1 | i > 1 & nrow(Selection) > 1 & nrow(Selection4) > 1) {
      # Designate select_function
      select_fun <- function(Selection) {
        Selection4 <- Selection1[sample(1:nrow(Selection1), (i*n_sites/Groups)-nrow(Selection[complete.cases(Selection[,1:5]),])),]
        Selection <- rbind(Selection, Selection4)
        return(Selection)
      }
      # Perform min distance check
      Selection <- check_min_dist(Selection, min_distance = min_distance,
                                  select_fun = select_fun)
      # Perform max distance check
      Selection <- check_max_dist(Selection, max_distance = max_distance, select_fun = select_fun)
    }
  }
}
cat("COMPLETED SITE SELECTION.\n")

# Summarize distances between sites
dis_spatial <- dist(dplyr::select(Selection, x, y), method = "euclidean")
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
  
You can investigate these sites separately in the file /data/chosen_sites/diagnostics/close_coords.csv.\n")

  # Find coordinates for too-near sites
  close_coords <- Selection %>% 
    dplyr::slice(which(colnames(close_df) %in% rownames(close_df)))

  write_csv(close_coords, paste0("data/chosen_sites/diagnostics/", filepattern, "close_coords.csv"))
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
  far_coords <- Selection %>% 
    dplyr::slice(which(colnames(far_df) %in% rownames(far_df)))
  
  write_csv(far_coords, paste0("data/chosen_sites/diagnostics/", filepattern, "far_coords.csv"))
}

Selection<-Selection[complete.cases(Selection[,1:5]),]

#if  still not enough sites to go to the final requested number, just fill up with random sites
if(nrow(Selection[complete.cases(Selection[,1:5]),])<n_sites){
  cat("NOTE: you only need", nrow(Selection), "sites to represent environmental variance throughout this landscape, but choosing more to reach", n_sites, "sites.\n")
    Selection4 <- terrain_df[sample(1:nrow(terrain_df), n_sites-nrow(Selection[complete.cases(Selection[,1:5]),])),]
    Selection <- rbind(Selection, Selection4)
    # If not first iteration and this new bin (Selection4) has sites in it....
    if (k > 1 | j > 1 | i > 1 & nrow(Selection) > 1 & nrow(Selection4) > 0) {
      # Designated selection function
      select_fun <- function(Selection) {
        Selection4 <- terrain_df[sample(1:nrow(terrain_df), n_sites-nrow(Selection[complete.cases(Selection[,1:5]),])),]
        Selection <- rbind(Selection, Selection4)
        return(Selection)
      }
      
      # Perform min distance check
      Selection <- check_min_dist(Selection, min_distance, select_fun = select_fun)
      # Perform max distance check
      Selection <- check_max_dist(Selection, max_distance, select_fun = select_fun)
      
    }
}


## Write out CSV of selected sites -----------


write_csv(Selection, paste0("data/chosen_sites/selected_sites_", n_sites, "_",
                            filepattern, ".csv"))

cat("\n\nSelected sites are saved to a CSV file:\n", paste0("data/chosen_sites/selected_sites_", n_sites, "_",
                                                        filepattern, ".csv"))
