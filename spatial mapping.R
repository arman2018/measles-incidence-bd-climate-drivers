#My directory
setwd("D:/Research/Measles")

library(ggplot2)
library(sf)
library(gridExtra)
library(ggspatial)  # Add for north arrow and scale bar
library(ggrepel)

# Load the spatial data
spatial_data <- st_read("gadm40_BGD_1.shp")

# Load the incidence data
case_data <- read.csv(file.choose(), header = TRUE)

# Merge both spatial and disease data
merged_data <- merge(spatial_data, case_data, by.x = "NAME_1", by.y = "NAME_1")


# Ensure centroids for labeling
merged_data$centroid <- st_centroid(merged_data$geometry)
coords <- st_coordinates(merged_data$centroid)
merged_data$lon <- coords[,1]
merged_data$lat <- coords[,2]

p1 <- ggplot(data = merged_data) +
  
  # Map layer
  geom_sf(aes(fill = Cases), color = "white", size = 0.2) +
  
  # Labels (repelled to avoid overlap)
  geom_text_repel(
    aes(x = lon, y = lat, label = NAME_1),
    size = 3,
    color = "black",
    fontface = "bold",
    max.overlaps = 100
  ) +
  
  # Improved color scale 
  scale_fill_gradientn(
    name = "Cases",
    colours = c("#2e8b57", "#f4a460", "#ff4500", "#8b0000"),  # smoother gradient
    limits = c(0, 250),
    breaks = seq(0, 250, by = 50),
    labels = scales::comma
  ) +
  
  # Titles
  labs(
    title = "",
    subtitle = "",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  # Theme improvements
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  ) +
  
  # North arrow & scale bar
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  )
p1
