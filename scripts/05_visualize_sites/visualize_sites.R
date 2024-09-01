
cat("\n\nPrepping data for visualization....\n")

## Workspace prep --------------

## .... Load dependencies ---------
pkgs <- c("viridis", "tidyr", "readr", "RColorBrewer", "ggplot2", "terra")

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

## Data Curation for visuals ----------

layers_df_tall <- layers_df %>% 
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

## Color palettes --------

## .... For landcover ----------------
# Filter to just landcover classes that are present 
# Because landcover classes will be displayed alphabetically, first sort
# according to landcover class
landcover_colors <- layers_df_tall %>% 
  dplyr::arrange(landcover_class) %>% 
  distinct(landcover_class, hex_color) %>% 
  dplyr::pull(hex_color)

letters <- c("A", "C", "E", "F", "B", "D", "G", "H")
viridis_pals <- letters[1:length(chosen_layers[!chosen_layers %in% "landcover"])]

# Because chosen_layers might be longer than number of letters, just repeat letters
viridis_pals[is.na(viridis_pals)] <- letters[1:length(viridis_pals[is.na(viridis_pals)])]

## Visualizations -------------

## .... Spatial Maps -------------
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
      geom_point(aes(color = landcover_class)) +
      scale_color_manual(values = landcover_colors) +
      geom_point(data = selected_sites, aes(x, y), pch=21, fill = "white", colour = "black") +
      labs(color = "") +
      theme_void()
    
    # Save out file
    cat("Saving map for", foo, "...\n")
    ggsave(plot = map, filename = paste0("figures/point_selection/", foo, 
                                         "_", filepattern, "_", n_sites, 
                                         "_map.png"),
           # Landcover map wider width to provide room for class names
           height = 5, width = 8)
  } else {
    map <- ggplot(data = df, aes(x, y)) +
      geom_raster(aes(fill = vals), alpha = 0.7) +
      scale_fill_viridis(option = viridis_pals[count]) +
      geom_point(data = selected_sites, aes(x, y), pch=21, fill = "white", colour = "black") +
      labs(color = "") +
      theme_void()
    
    # Save out file
    cat("Saving map for", foo, "...\n")
    ggsave(plot = map, filename = paste0("figures/point_selection/", foo, 
                                         "_", filepattern, "_", n_sites, 
                                         "_map.png"),
           height = 5, width = 6)
  }
})

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
