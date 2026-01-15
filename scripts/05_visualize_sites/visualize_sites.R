
cat("\n\nPrepping data for visualization....\n")

## Workspace prep --------------

## .... Load dependencies ---------
pkgs <- c("viridis", "tidyr", "dplyr", "readr", "RColorBrewer", "ggplot2", "terra", "gridExtra")

for (i in seq_along(pkgs)) {
  suppressPackageStartupMessages(
    suppressWarnings(library(pkgs[i], character.only = TRUE))
  )
  if (i == length(pkgs)) { rm(pkgs, i) }
}

# Specify string for naming output files
if (complete.cases(landscape_name)) {
  filepattern <- landscape_name
} else {
  filepattern <- paste(round(spatial_extent, 0), collapse = "_")
}

## .... Load in files ---------

layers <- rast(paste0("data/spatial_drivers/combined/layers_",
                      round(chosen_rez, 4), "_", filepattern, ".tif"))

dims <- rast(paste0("data/landscape_data/dim_values_", filepattern, ".tif"))
bin_id <- rast(paste0("data/landscape_data/bin_id_", filepattern, ".tif"))
landcover_link <- suppressMessages(read_csv("data/spatial_drivers/landcover/original/esa_cci/esa_cci_landcover_link.csv"))

if (!exists("layers_df")) {
  layers_df <- read_csv(paste0("data/landscape_data/layers_", 
                               filepattern, "_", n_sites, ".csv")) 
}

if (!exists("selected_sites")) {
  selected_sites <- read_csv(paste0("data/chosen_sites/selected_sites_",
                                    filepattern, "_", n_sites, ".csv"))
}

## Data Curation for visuals ----------

# Particular curation if landcover is one of the predictor layers
if ("landcover" %in% chosen_layers) {
  layers_df_tall <- layers_df %>% 
    mutate(landcover = as.factor(landcover)) %>% 
    left_join(landcover_link %>% 
                dplyr::rename(landcover = grouped_number, landcover_class = grouped_class) %>%
                mutate(landcover = as.factor(landcover)) %>% 
                dplyr::distinct(landcover, landcover_class, hex_color), by = join_by(landcover, landcover_class)) %>% 
    # Convert landcover to character to preserve numeric values, then back to 
    # numeric for plotting purposes
    # If you convert straight to numeric (without character) then the value will
    # equal the factor level, eg. 1, 2, 3, etc. rather than the landscale number
    mutate(landcover = as.character(landcover)) %>%
    mutate(landcover = as.double(landcover)) %>%
    pivot_longer(all_of(chosen_layers), names_to = "chsen_layers", values_to = "vals")
} else {
  layers_df_tall <- layers_df %>% 
    pivot_longer(all_of(chosen_layers), names_to = "chsen_layers", values_to = "vals")
}


# Curate bin layers
dims_df <- as.data.frame(dims, xy = T) %>% 
  dplyr::rename(`Dim 1` = Dim_1, `Dim 2` = Dim_2, `Dim 3` = Dim_3)

binID_df <- as.data.frame(bin_id, xy = T)

## Color palettes --------

if ("landcover" %in% chosen_layers) {
  # Filter to just landcover classes that are present 
  # Because landcover classes will be displayed alphabetically, first sort
  # according to landcover class
  landcover_colors <- layers_df_tall %>% 
    dplyr::arrange(landcover_class) %>% 
    distinct(landcover_class, hex_color) %>% 
    dplyr::pull(hex_color)
}

letters <- c("A", "C", "E", "F", "B", "D", "G", "H")
viridis_pals <- letters[1:length(chosen_layers[!chosen_layers %in% "landcover"])]

# Because chosen_layers might be longer than number of letters, just repeat letters
viridis_pals[is.na(viridis_pals)] <- letters[1:length(viridis_pals[is.na(viridis_pals)])]

## For bins
bin_pal <- c(brewer.pal(8, "Set2"),
             brewer.pal(12, "Paired"),
             brewer.pal(8, "Dark2"))

if (length(unique(binID_df$Bins))) {
  bin_pal <- c(brewer.pal(8, "Set2"),
               brewer.pal(12, "Paired"),
               brewer.pal(8, "Dark2"),
               brewer.pal(9, "YlOrRd"),
               brewer.pal(11, "PRGn"),
               brewer.pal(11, "BrBG"),
               brewer.pal(11, "RdBu"),
               brewer.pal(11, "PuOr"),
               brewer.pal(11, "PiYG"))
  # Randomize order
  bin_pal <- sample(bin_pal, size = length(bin_pal), replace = FALSE)
}
## Visualizations -------------
## .... Spatial Maps -------------
## ....** Designate plot dimensions --------

# Create dimensions according to the dimensions of input raster
# Take log of dims. This ensures that smaller rasters still have reasonable 
# plot sizes, while big rasters won't have excessively massive plots
plot_h <- log(dim(layers)[1])
plot_w <- log(dim(layers)[2]) 
# Make sure dimensions aren't too small
plot_h <- ifelse(plot_h < 3, 3, plot_h)
plot_w <- ifelse(plot_w < 3, 3, plot_w)

# Find bigger of two dimensions
max_d <- ifelse(plot_w > plot_h, plot_w, plot_h)
# We don't want to save any files as too big. If the bigger dimension is 
# greater than 10, than scale both dimensions so larger dimension equals 10
plot_h <- ifelse(max_d > 10, plot_h / (max_d / 10), plot_h)
plot_w <- ifelse(max_d > 10, plot_w / (max_d / 10), plot_w)

## ....** For environmental layers -----------
count <- 0
maps <- lapply(chosen_layers, function(foo) {
  if (foo != "landcover") {
    count <<- count + 1
  }
  df <- layers_df_tall %>% 
    dplyr::filter(chsen_layers == foo)
  
  if (foo == "landcover") {
    df <- df %>% 
      mutate(vals = as.factor(vals))
    map <- ggplot(data = df, aes(x, y)) +
      geom_raster(aes(fill = landcover_class)) +
      scale_fill_manual(values = landcover_colors) +
      geom_point(data = selected_sites, aes(x, y), size =3, pch=21, fill = "white", colour = "black") +
      labs(color = "") +
      theme_void()
    
    # Save out file
    cat("Saving map for", foo, "...\n")
    ggsave(plot = map, filename = paste0("figures/point_selection/", foo, 
                                         "_", filepattern, "_", n_sites, 
                                         "_map.png"),
           # Add some to width for large landcover legend
           # Landcover map wider width to provide room for class names
           height = plot_h, width = plot_w + (plot_w * 0.25))
  } else {
    map <- ggplot(data = df, aes(x, y)) +
      geom_raster(aes(fill = vals), alpha = 0.7) +
      scale_fill_viridis(option = viridis_pals[count]) +
      geom_point(data = selected_sites, aes(x, y), size =3, pch=21, fill = "white", colour = "black") +
      labs(color = "") +
      theme_void()
    
    # Save out file
    cat("Saving map for", foo, "...\n")
    ggsave(plot = map, filename = paste0("figures/point_selection/", foo, 
                                         "_", filepattern, "_", n_sites, 
                                         "_map.png"),
           # Create dimensions according to the dimensions of input raster
           # Add some to width for legend
           height = plot_h, width = plot_w + (plot_w * 0.08))
  }
})

## ....** For bins -----------

dim1_plot <- ggplot(data = dims_df, aes(x, y)) +
  geom_raster(aes(fill = `Dim 1`)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous")) +
  theme_void() +
  theme(text = element_text(size = 26))

dim2_plot <- ggplot(data = dims_df, aes(x, y)) +
  geom_raster(aes(fill = `Dim 2`)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("GrandBudapest1", 100, type = "continuous")) +
  theme_void() +
  theme(text = element_text(size = 26))

dim3_plot <- ggplot(data = dims_df, aes(x, y)) +
  geom_raster(aes(fill = `Dim 3`)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Rushmore1", 100, type = "continuous")) +
  theme_void() +
  theme(text = element_text(size = 26))

bin_plot <- ggplot(data = binID_df, aes(x, y)) +
  geom_raster(aes(fill = Bins)) +
  scale_fill_manual(values = bin_pal) +
  theme_void() +
  theme(legend.text = element_blank(),
        # Legend unit inversely proportional to number of bins (more bins, shrink 
        # legend unit)
        legend.key.size = unit((18 / length(unique(bin_pal))), "in"),
        text = element_text(size = 30))

## Arrange plots
dim_bin_plot <- arrangeGrob(dim1_plot, dim2_plot, dim3_plot,
                             bin_plot, layout_matrix = rbind(c(1,2,3),
                                                             c(4,4,4),
                                                             c(4,4,4)))

ggsave(plot = dim_bin_plot, filename = paste0("figures/point_selection/bin_maps_",
                                     filepattern, "_", n_sites, ".png"),
       # Create dimensions according to the dimensions of input raster
       # Add some to width for legend
       height = plot_h * 2, width = 2 * (plot_w + (plot_w * 0.08)))

## ....Distribution of Environmental Layers -----------

# Multiply selection by a scale factor
multi_factor <- dim(layers)[1] * dim(layers)[2] / n_sites * .75

selected_sites_multi <- do.call("rbind", replicate(multi_factor, 
                                                   selected_sites %>% 
                                                     mutate(landcover = as.double(landcover)),
                                                   simplify = FALSE))

selected_sites_multi_tall <- selected_sites_multi %>% 
  dplyr::select(all_of(chosen_layers)) %>% 
  pivot_longer(everything(), names_to = "chsen_layers", values_to = "vals")

histogram_plots <- ggplot() +
  geom_histogram(data = layers_df_tall, aes(vals), bins = 30) +
  geom_histogram(data = selected_sites_multi_tall, aes(vals), bins = 30, 
                 fill = "blue", alpha = 0.7) +
  scale_y_continuous(
    # Features of the first axis
    name = "Frequency (Background Variation)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~. / multi_factor, name="Frequency (Selected Sites)")) +
  labs(x = "Values") +
  theme_bw() +
  theme(axis.text.y.right = element_text(colour="blue"),
        axis.title.y.right = element_text(colour="blue")) +
  facet_wrap(~chsen_layers, scales = "free_x")

ggsave(histogram_plots, filename = paste0("figures/point_selection/", 
                                          filepattern, "_", n_sites, "_histograms.png"),
       height = 5, width = 5)

## ....Environmental space scatter plots ----------------

chosen_layers2 <- chosen_layers
# Everything except land cover
if ("landcover" %in% chosen_layers2) {
  chosen_layers2 <- chosen_layers2[!chosen_layers2 == "landcover"]
}

for (x_layer in chosen_layers2) {
  
  y_layers <- chosen_layers2[!chosen_layers2 == x_layer]
  
  for (y_layer in y_layers) {
    
    scatterplot <- ggplot(data = layers_df, aes(x = !!sym(x_layer), y = !!sym(y_layer))) +
      geom_point(size=0.2,pch=16,alpha=0.1) +
      geom_point(data=selected_sites,size=1,pch=3) +
      geom_point(data=selected_sites,size=0.5,col="red") +
      theme_classic() +
      labs(x = x_layer,
           y = y_layer)
    
    # Save out file
    cat("Saving scatterplot for", x_layer,  "vs", y_layer, "...\n")
    ggsave(plot = scatterplot, filename = paste0("figures/point_selection/", x_layer, "_vs_",
                                                 y_layer, "_", filepattern, 
                                                 "_", n_sites, ".png"),
           height = 5, width = 5)
  }
  
  chosen_layers2 <- chosen_layers2[!grepl(x_layer, chosen_layers2)]
}

cat(red("All figures have been saved to /figures/point_selection/. \n"))
