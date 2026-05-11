# ============================================================
# Required packages
# ============================================================
library(terra)
library(ggplot2)
library(scico)
library(dplyr)
library(sf)

# ============================================================
# 1. Import and prepare bathymetry data
# ============================================================
r <- rast("gis/continental_shelf.grd")

e <- ext(22.6, 24.5, -34.3, -33.8)
r_crop <- crop(r, e)

# Cap deep water at -100 m
r_crop[r_crop < -100] <- -100

# Convert full raster to data frame (land + sea)
bathy_df <- as.data.frame(r_crop, xy = TRUE)
names(bathy_df) <- c("lon", "lat", "depth")

# Separate sea pixels only
sea_df <- bathy_df |> filter(depth <= 0)

# ============================================================
# 2. Extract coastline as depth = 0 contour
# ============================================================
coast_lines <- as.contour(r_crop, levels = 0) |>
  st_as_sf() |>
  st_set_crs(4326)

# ============================================================
# 3. Place labels
# ============================================================
places <- data.frame(
  name = c("Mossel Bay", "Knysna", "Plettenberg Bay", "Tsitsikamma"),
  lon  = c(22.153, 23.046, 23.368, 23.890),
  lat  = c(-34.183, -34.070, -34.058, -34.010)
)

# ============================================================
# 4. Study area section boundaries
# ============================================================
lat_line <- -34.15
lon_west <- 22.815
lon_div1 <- 23.37
lon_div2 <- 23.575
lon_east <- 24.195

# Horizontal line
h_line_df <- data.frame(
  x = c(lon_west, lon_east),
  y = c(lat_line, lat_line)
)

# Vertical dividers
v_lines_df <- data.frame(
  x    = c(lon_west, lon_west, lon_div1, lon_div1, lon_div2, lon_div2, lon_east, lon_east),
  y    = c(lat_line, lat_line + 0.15, lat_line, lat_line + 0.15,
           lat_line, lat_line + 0.15, lat_line, lat_line + 0.15),
  grp  = c(1, 1, 2, 2, 3, 3, 4, 4)
)

region_labels <- data.frame(
  name = c("Knysna", "Plettenberg Bay", "Tsitsikamma"),
  lon  = c((lon_west + lon_div1) / 2, (lon_div1 + lon_div2) / 2, (lon_div2 + lon_east) / 2),
  lat  = lat_line + 0.07
)

# ============================================================
# 5. Plot
# ============================================================
depth_colors <- scico(100, palette = "oslo", direction = 1)

ggplot() +
  # Bathymetry (sea only)
  geom_raster(data = sea_df, aes(x = lon, y = lat, fill = depth)) +
  scale_fill_gradientn(
    colours = depth_colors,
    limits  = c(-100, 0),
    name    = "Depth (m)"
  ) +
  # Land (grey fill above depth = 0)
  geom_raster(data = bathy_df |> filter(depth > 0),
              aes(x = lon, y = lat), fill = "grey80") +
  # Coastline contour
  geom_sf(data = coast_lines, colour = "grey30", linewidth = 0.4, inherit.aes = FALSE) +
  # Study area lines
  geom_line(data = h_line_df, aes(x = x, y = y), colour = "white", linewidth = 0.8) +
  geom_line(data = v_lines_df, aes(x = x, y = y, group = grp), colour = "white", linewidth = 0.8) +
  # Region labels
  geom_text(data = region_labels, aes(x = lon, y = lat, label = name),
            colour = "white", size = 3.5) +
  # Place names
  geom_text(data = places, aes(x = lon, y = lat + 0.03, label = name),
            colour = "black", size = 3, fontface = "plain") +
  geom_point(data = places, aes(x = lon, y = lat),
             colour = "black", size = 1.5) +
  # Coordinates
  coord_sf(xlim = c(22.6, 24.5), ylim = c(-34.3, -33.8), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid       = element_line(colour = "grey90", linewidth = 0.3),
    legend.position  = "right",
    axis.title       = element_text(size = 10)
  )

# ============================================================
# 6. Save
# ============================================================
#ggsave("./figures/figure1_dolphin_map_ggplot.png",
#       width = 20, height = 12, units = "cm", dpi = 300)

