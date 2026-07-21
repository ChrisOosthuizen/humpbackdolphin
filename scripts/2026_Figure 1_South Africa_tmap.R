# ============================================================
# Required packages
# ============================================================
library(sf)
library(tmap)
library(dplyr)

tmap_mode("plot")  # static map

# ============================================================
# South Africa inset map
# ============================================================
library(rnaturalearth)
library(rnaturalearthdata)

sa_provinces <- ne_states(country = "South Africa", returnclass = "sf")
sa_provinces$highlight <- ifelse(sa_provinces$name == "KwaZulu-Natal",
                                 "KwaZulu-Natal", "Other")
sa_country <- ne_countries(country = "South Africa", returnclass = "sf", scale = "medium")

# Site dots
sites_sf <- data.frame(
  name = c("KwaZulu-Natal", "Study area", "Mossel Bay", "Algoa Bay", "False Bay", "Kosi Bay"),
  lon  = c(30.4,  23.046, 22.153, 25.585, 18.7,  32.87),
  lat  = c(-29.6, -34.086, -34.183, -33.958, -34.1, -26.93)
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Label positions (offset)
labels_sf <- data.frame(
  name = c("KwaZulu-Natal", "Study area", "Mossel Bay", "Algoa Bay", "False Bay", "Kosi Bay"),
  lon  = c(31.2,  23.4,  22.153, 26.6,  18.2,  32.05),
  lat  = c(-29.2, -33.5, -34.5,  -33.45, -33.6, -26.4)
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

sa_bbox <- st_bbox(c(xmin = 16, ymin = -35, xmax = 33, ymax = -22),
                   crs = st_crs(4326))

study_area_sf <- st_as_sfc(
  st_bbox(c(xmin = 23.0, ymin = -34.2, xmax = 24.0, ymax = -33.9),
          crs = st_crs(4326))
)

sa_map <-
  tm_shape(sa_provinces, bbox = sa_bbox) +
  tm_polygons(
    fill        = "highlight",
    fill.scale  = tm_scale_categorical(values = c("KwaZulu-Natal" = "grey55", "Other" = "grey85")),
    fill.legend = tm_legend_hide(),
    col         = "white",
    lwd         = 0.5
  ) +
  tm_shape(sa_country) +
  tm_borders(col = "black", lwd = 1.2) +
  
  tm_shape(study_area_sf) +
  tm_borders(col = "red", lwd = 1.5) +
  tm_fill(fill = "red", fill_alpha = 0.2) +
  
  tm_shape(sites_sf |> filter(!name %in% c("Study area", "KwaZulu-Natal"))) +
  tm_dots(
    col   = "black",
    size  = 0.8,
    shape = 21,
    fill  = "blue4") +
  
  tm_shape(labels_sf) +
  tm_text(
    text     = "name",
    size     = 1.2,
    fontface = "plain",
    col      = "black",
    options  = opt_tm_text(shadow = FALSE)
  ) +
  
  tm_add_legend(
    type       = "polygons",
    labels     = "Study area",
    fill       = "red",
    fill_alpha = 0.2,
    col        = "red",
    position   = tm_pos_in("left", "top"),
    frame      = FALSE,
    text.size  = 1.2) +
  
  tm_layout(
    frame    = TRUE,
    bg.color = "white")

sa_map

tmap_save(sa_map, "./figures/Figure1A.png",
           width  = 6000,
           height = 3000,
           units  = "px",
           dpi    = 600)

