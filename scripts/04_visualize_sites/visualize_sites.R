
cat("\n\nPrepping data for visualization....\n")

## Workspace prep --------------

library(ggplot2)
library(RColorBrewer)
library(viridis)

# Specify string for naming output files
if (complete.cases(landscape_name)) {
  filepattern <- landscape_name
} else {
  filepattern <- paste(round(spatial_extent, 0), collapse = "_")
}

## Data Curation for visuals ----------

layers_df_tall <- layers_df %>% 
  left_join(landcover_link %>% 
              dplyr::rename(landcover = grouped_number, landcover_class = grouped_class) %>% 
              dplyr::distinct(landcover, landcover_class, hex_color), by = join_by(landcover, landcover_class)) %>% 
  pivot_longer(all_of(chosen_layers), names_to = "chsen_layers", values_to = "vals")

## Color palettes --------


## ....For landcover ----------------
# Filter to just landcover classes that are present 
# Because landcover classes will be displayed alphabetically, first sort
# according to landcover class
landcover_colors <- layers_df_tall %>% 
  dplyr::arrange(landcover_class) %>% 
  distinct(landcover_class, hex_color) %>% 
  dplyr::pull(hex_color)

letters <- c("A", "C", "E", "F", "B", "D", "B", "H")
viridis_pals <- letters[1:length(chosen_layers[!chosen_layers %in% "landcover"])]

## Visualizations -------------

## ....Spatial Maps -------------
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
      geom_point(data = Selection, aes(x, y)) +
      labs(color = "") +
      theme_void()
    
    # Save out file
    cat("Saving map for", foo, "...\n")
    ggsave(plot = map, filename = paste0("figures/point_selection/", foo, 
                                         "_", filepattern,
                                         "_map.png"),
           # Landcover map wider width to provide room for class names
           height = 5, width = 8)
  } else {
    map <- ggplot(data = df, aes(x, y)) +
      geom_raster(aes(fill = vals), alpha = 0.7) +
      scale_fill_viridis(option = viridis_pals[count]) +
      geom_point(data = Selection, aes(x, y)) +
      labs(color = "") +
      theme_void()
    
    # Save out file
    cat("Saving map for", foo, "...\n")
    ggsave(plot = map, filename = paste0("figures/point_selection/", foo, 
                                         "_", filepattern,
                                         "_map.png"),
           height = 5, width = 6)
  }
})

## ....Distribution of Environmental Layers -----------


# Multiply selection by 100
multi_factor <- length(terra::values(elevation)) / n_sites * .75
selection_multi <- do.call("rbind", replicate(multi_factor, Selection, simplify = FALSE))

selection_multi_tall <- selection_multi %>% 
  dplyr::select(all_of(chosen_layers)) %>% 
  pivot_longer(everything(), names_to = "chsen_layers", values_to = "vals")

histogram_plots <- ggplot() +
  geom_histogram(data = layers_df_tall, aes(vals), bins = 30) +
  geom_histogram(data = selection_multi_tall, aes(vals), bins = 30, 
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
  facet_wrap(~chsen_layers, scales = "free_x"); 

ggsave(histogram_plots, filename = paste0("figures/point_selection/", filepattern, "histograms.png"),
       height = 5, width = 5)

## ....Environmental space scatter plots ----------------

chosen_layers2 <- chosen_layers
for (x_layer in chosen_layers2) {
  y_layers <- chosen_layers2[!chosen_layers2 == x_layer]
  
  for (y_layer in y_layers) {
    
    scatterplot <- ggplot(data = layers_df, aes(x = !!sym(x_layer), y = !!sym(y_layer))) +
      geom_point(size=0.2,pch=16,alpha=0.1) +
      geom_point(data=Selection,size=1,pch=3) +
      geom_point(data=Selection,size=0.5,col="red") +
      theme_classic() +
      labs(x = x_layer,
           y = y_layer)
    
    # Save out file
    cat("Saving scatterplot for", x_layer,  "vs", y_layer, "...\n")
    ggsave(plot = scatterplot, filename = paste0("figures/point_selection/", x_layer, "_vs_",
                                                 y_layer, "_", filepattern, ".png"),
           height = 5, width = 5)
  }
  
  chosen_layers2 <- chosen_layers2[!grepl(x_layer, chosen_layers2)]
}

