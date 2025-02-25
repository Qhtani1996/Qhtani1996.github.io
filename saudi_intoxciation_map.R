install.packages(c("sf", "tmap", "cartogram", "dplyr"))
library(sf)
library(tmap)
library(cartogram)
library(dplyr)

saudi_map <- st_read("01_GIS/healthdirectorate_final.shp")

# Check the column names
names(saudi_map)

# Ensure the data is numeric
saudi_map$population <- as.numeric(saudi_map$population)

drug_intoxication_data <- data.frame(
  Health_Dir = c("Riyadh", "Makkah", "Jeddah", "Taif", "Madina", "Al Qassim", "Eastern Province", "Al Ahsaa", "Hafr Albatin", "Asir", "Bishah", "Tabuk", "Hail", "Northern Borders", "Jizan", "Najran", "Al Bahah", "Al Jawf", "Qurrayat", "Qunfutha"),
  ChemicalDrugIntoxication = c(944, 147, 486, 131, 216, 53, 310, 222, 18, 152, 64, 203, 223, 248, 142, 111, 11, 119, 248, 27)
)


# Merge the new data with the shapefile
saudi_map <- merge(saudi_map, drug_intoxication_data, by.x = "name_en", by.y = "Health_Dir", all.x = TRUE)



# Transform to UTM Zone 37N (EPSG:32637)
saudi_map_projected <- st_transform(saudi_map, crs = 32637)
st_crs(saudi_map_projected)



library(cartogram)

# Create the Dorling cartogram using the projected shapefile
saudi_cartogram <- cartogram_dorling(saudi_map_projected, weight = "ChemicalDrugIntoxication", k = 5)



library(tmap)

tm_shape(saudi_cartogram) +
  tm_polygons(col = "ChemicalDrugIntoxication", palette = "Oranges", title = "Chemical Drug Intoxication") +
  tm_layout(legend.position = c("right", "bottom"))





library(ggplot2)

ggplot(saudi_cartogram) +
  geom_sf(aes(fill = ChemicalDrugIntoxication), color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal()













# Check for missing values
sum(is.na(saudi_map_projected$ChemicalDrugIntoxication))

# Check the range of values
summary(saudi_map_projected$ChemicalDrugIntoxication)


# Check the structure of the cartogram
str(saudi_cartogram)

# Check the first few rows
head(saudi_cartogram)


library(tmap)

tm_shape(saudi_cartogram) +
  tm_polygons(
    col = "ChemicalDrugIntoxication",
    palette = "Oranges",
    title = "Chemical Drug Intoxication",
    breaks = c(0, 200, 400, 600, 800, 1000) # Adjust breaks as needed
  ) +
  tm_layout(legend.position = c("right", "bottom"))









library(ggplot2)

ggplot(saudi_cartogram) +
  geom_sf(aes(fill = ChemicalDrugIntoxication), color = "white") +
  scale_fill_viridis_c(option = "plasma", breaks = c(0, 100, 200, 300, 500, 1000)) +
  theme_minimal()




st_geometry(saudi_cartogram)





tm_shape(saudi_map_projected) +
  tm_polygons(col = "ChemicalDrugIntoxication", palette = "Oranges") +
  tm_layout(legend.position = c("right", "bottom"))
















library(tmap)

tm_shape(saudi_map_projected) +
  tm_polygons(col = "ChemicalDrugIntoxication", palette = "Oranges", title = "Chemical Drug Intoxication") +
  tm_text(
    text = "name_ar", # Replace "NAME" with the column containing region names
    size = 0.8,    # Adjust the text size
    col = "black", # Text color
    auto.placement = TRUE, # Automatically adjust placement to avoid overlap
    remove.overlap = TRUE, # Remove overlapping labels
    just = "left" # Justification of the text (e.g., "center", "left", "right")
  ) +
  tm_layout(legend.position = c("right", "bottom"))



tm_shape(saudi_map_projected) +
  tm_polygons(col = "ChemicalDrugIntoxication", palette = "Oranges", title = "Chemical Drug Intoxication") +
  tm_text(
    text = "name_ar",
    size = 0.8,
    col = "black",
    xmod = 0.5, # Shift labels horizontally
    ymod = 0.5, # Shift labels vertically
    auto.placement = TRUE,
    remove.overlap = TRUE
  ) +
  tm_layout(legend.position = c("left", "bottom"))


library(tmap)

tm_shape(saudi_map_projected) +
  tm_polygons(col = "ChemicalDrugIntoxication", palette = "Oranges", title = "Chemical Drug Intoxication") +
  tm_text(
    text = "name_en", # Replace "NAME" with the column containing region names
    size = 0.8,    # Adjust the text size
    col = "black", # Text color
    fontfamily = "Times New Roman", # Set font to Times New Roman
    auto.placement = TRUE, # Automatically adjust placement to avoid overlap
    remove.overlap = TRUE # Remove overlapping labels
  ) +
  tm_layout(
    legend.position = c("right", "bottom"),
    fontfamily = "Times New Roman" # Set font for the entire map (including legend)
  )

tm_shape(saudi_map_projected) +
  tm_polygons(col = "ChemicalDrugIntoxication", palette = "Oranges", title = "Chemical Drug Intoxication") +
  tm_text(
    text = "name_en",
    size = 0.8,
    col = "black",
    xmod = 0.5, # Shift labels horizontally
    ymod = 0.5, # Shift labels vertically
    auto.placement = TRUE,
    remove.overlap = TRUE
  ) +
  tm_layout(legend.position = c("left", "bottom"))






# Register Times New Roman
windowsFonts(Times = windowsFont("Times New Roman"))

# Use in ggplot2
ggplot(saudi_map_df) +
  geom_sf(aes(fill = ChemicalDrugIntoxication), color = "black") +
  geom_text_repel(
    aes(X, Y, label = name_en),
    size = 3,
    color = "black",
    family = "Times", # Use the registered font
    box.padding = 0.5,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal(base_family = "Times") # Set font for the entire plot




library(ggplot2)
library(plotly)

# Register Times New Roman
windowsFonts(Times = windowsFont("Times New Roman"))

# Use in ggplot2
ggplot(saudi_map_df) +
  geom_sf(aes(fill = ChemicalDrugIntoxication), color = "black") +
  geom_text_repel(
    aes(X, Y, label = name_en),
    size = 3,
    color = "black",
    family = "Times", # Use the registered font
    box.padding = 0.5,
    max.overlaps = Inf
  ) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_family = "Times")+ # Set font for the entire plot
  labs(title = "Chemical and Drug Intoxication by Health Region, 2023")












p <- ggplot(saudi_map_df) +
  geom_sf(aes(fill = ChemicalDrugIntoxication), color = "black") +
  geom_text_repel(
    aes(X, Y, label = name_en),
    size = 3,
    color = "black",
    family = "Times", # Use the registered font
    box.padding = 0.5,
    max.overlaps = Inf
  ) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal(base_family = "Times") + # Set font for the entire plot
  ggtitle("Chemical and Drug Intoxication by Health Region, 2023") +
  theme(
    plot.title = element_text(
      size = 14,        # Title font size
      face = "bold",    # Title font face ("plain", "italic", "bold", "bold.italic")
      hjust = 0.5,      # Horizontal justification (0 = left, 0.5 = center, 1 = right)
      color = "blue"    # Title font color
    )
  )
