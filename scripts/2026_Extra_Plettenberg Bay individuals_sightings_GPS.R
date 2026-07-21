
# Plettenberg Bay map

# Where were the Plettenberg Bay photo-ID animals seen? Overlap with 2002 and 2003 area?

library(sf)
library(tmap)
library(tidyverse)
library(maptiles)

tmap_mode("plot")  # static map

# ============================================================
# 1. Import and prepare bathymetry data
# ============================================================


# ============================================================
# 2. Place labels
# ============================================================

places <- data.frame(
  name = c("Jack’s Point", "Plettenberg Bay", "Robberg Point", "Nature's Point"),
  lon  = c(23.36629, 23.378, 23.405, 23.501),
  lat  = c(-34.103, -34.055, -34.1179, -33.9993)
)

places_sf <- st_as_sf(places, coords = c("lon", "lat"), crs = 4326)

places_points <- data.frame(
  name = c("Jack’s Point", "Plettenberg Bay", "Robberg Point", "Nature's Point"),
  lon  = c(23.36463, 23.378, 23.412254, 23.5284),
  lat  = c(-34.105844, -34.057, -34.108752,-33.9993)
)

places_sf_point <- st_as_sf(places_points, coords = c("lon", "lat"), crs = 4326)

# ============================================================
# 3. Dolphin kernel density overlay
# ============================================================
dat <- read.csv("./data/Plettenberg Bay individuals_sightings_GPS.csv")

pts_sf  <- st_as_sf(dat, coords = c("lon", "lat"), crs = 4326)

# # ============================================================
# # 4. Colour palettes
# # ============================================================
# depth_colors <- scico(100, palette = "oslo", direction = 1)
 heat_colors  <- RColorBrewer::brewer.pal(9, "YlOrRd")
# 
# ============================================================
# 5. Bounding box — edit the 4 borders here for Plettenberg Bay
# ============================================================
xmin_map <- 23.345   # west border
xmax_map <- 23.585   # east border
ymin_map <- -34.16  # south border
ymax_map <- -33.98  # north border

bbox_sf <- st_bbox(c(xmin = xmin_map,
                     ymin = ymin_map,
                     xmax = xmax_map,
                     ymax = ymax_map),
                   crs = st_crs(4326)) |> st_as_sfc()

basemap <- get_tiles(bbox_sf, provider = "CartoDB.Positron", zoom = 13,
                     crop = TRUE,
                     cachedir = "gis/tiles")   # To download tiles, remove this line of code. If not set, tiles are cached in a tempdir folder.


# ============================================================
# 5b. MPA areas
# ============================================================
mpas <- st_read("./gis/MPA/MPAs_adjustedHWM.shp")

mpa_names <- data.frame(
  name = c("Goukamma", "Robberg\nMarine Protected\nArea", "Tsitsikamma"),
  lon  = c(22.91, 23.438, 24.00),
  lat  = c(-34.11, -34.11, -34.11)  # moved up from -34.14 to sit inside new ymin of -34.20
)

mpa_names_sf <- st_as_sf(mpa_names, coords = c("lon", "lat"), crs = 4326)

# ============================================================
# 6. Study area section boundaries
# ============================================================
lat_line  <- -34.15
lon_west  <-  22.815   # nudged inward from 22.815 to sit inside west border
lon_div1  <-  23.37   # Knysna | Plettenberg Bay
lon_div2  <-  23.575  # Plettenberg Bay | Tsitsikamma
lon_east  <-  24.195   # nudged inward from 24.195 to sit inside east border of 24.20

# Horizontal line
h_line <- st_sfc(
  st_linestring(matrix(c(lon_west, lat_line, lon_east, lat_line), ncol = 2, byrow = TRUE)),
  crs = 4326
) |> st_as_sf()

# Vertical dividers
v_lines <- st_sfc(
  st_linestring(matrix(c(lon_west, lat_line, lon_west, lat_line + 0.12), ncol = 2, byrow = TRUE)),  # height reduced: map is shorter N-S
  st_linestring(matrix(c(lon_div1, lat_line, lon_div1, lat_line + 0.12), ncol = 2, byrow = TRUE)),
  st_linestring(matrix(c(lon_div2, lat_line, lon_div2, lat_line + 0.12), ncol = 2, byrow = TRUE)),
  st_linestring(matrix(c(lon_east, lat_line, lon_east, lat_line + 0.12), ncol = 2, byrow = TRUE)),
  crs = 4326
) |> st_as_sf()

# Region labels (sit above the horizontal line)
region_labels <- data.frame(
  name = c("Knysna", "Plettenberg Bay", "Tsitsikamma"),
  lon  = c((lon_west + lon_div1) / 2,
           (lon_div1 + lon_div2) / 2 ,
           (lon_div2 + lon_east) / 2),
  lat  = lat_line + 0.06   # reduced from 0.07 so labels stay below new north border
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

# ============================================================
# 7. Build tmap
# ============================================================
map =  
 # Basemap tiles — bbox_sf sets the rendered extent
  tm_shape(basemap, bbox = bbox_sf) +
  tm_rgb() +
  
  tm_shape(pts_sf) +
  tm_dots(fill = "blue",
  #tm_dots(fill        = "Individual",       # or whatever column
  # fill.scale  = tm_scale_categorical(),
  # fill.legend = tm_legend(title = "Species"),
    size  = 1) + 

#-----------------------------------
# MPA polygons
#-----------------------------------
# 
tm_shape(mpas, bbox = bbox_sf) +
  tm_polygons(
    fill       = "aquamarine1",
    fill_alpha = 0.3,
    col        = "aquamarine1",
    lwd        = 1.5,
    col.legend = tm_legend_hide()) +

  #-----------------------------------
# Study area section lines
#-----------------------------------
#tm_shape(h_line) +
#  tm_lines(col = "white", lwd = 1.5) +

tm_shape(v_lines) +
  tm_lines(col = "white", lwd = 1.9) +
  
#-----------------------------------
# Place names
#-----------------------------------
tm_shape(places_sf) +
  tm_text(
    text     = "name",
    size     = 1,
    fontface = "plain",
    col      = "black",
    options  = opt_tm_text(shadow = FALSE),
    ymod     = 0.3) +
  
  tm_shape(places_sf_point) +
  tm_symbols(
    shape = 24,          # filled triangle-up, often used as an arrow/marker substitute
    col   = "black",
    fill  = "hotpink",
    size  = 0.5) + 
  
  #-----------------------------------
# MPA names
#-----------------------------------
tm_shape(mpa_names_sf) +
  tm_text(
    text     = "name",
    size     = 1,
    fontface = "plain",
    col      = "aquamarine1",
    options  = opt_tm_text(shadow = FALSE, just = "left"),
    ymod     = 0.3) +
  

#-----------------------------------
# Scale bar — moved left to stay inside new east border
#-----------------------------------
tm_scalebar(breaks = c(0, 2.5, 5, 10), position = tm_pos_in(pos.h = 0.4, pos.v = 0.1), 
            color.dark  = "black",
            color.light = "white",
            text.size = 1,
            text.color  = "black")

map

# ============================================================
# 8. Save
# ============================================================

# plet_map <- plet_map +
#   tm_layout(outer.margins = c(0, 0, 0, 0))  # bottom, left, top, right — as fractions of plot size

tmap_save(map, "./Supplement/Extra_Plettenberg Bay individuals_sightings_GPS.png",
          width  = 6000,
          height = 3000,
          units  = "px",
          dpi    = 600)


