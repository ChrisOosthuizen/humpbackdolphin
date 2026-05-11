# ============================================================
# Required packages
# ============================================================

library(sf)
library(dplyr)
library(RColorBrewer)
library(terra)
library(scico)
library(spatstat.geom)
library(spatstat.core)
library(tmap)
library(viridis)
library(maptiles)

tmap_mode("plot")  # static map

# ============================================================
# 1. Import and prepare bathymetry data
# ============================================================
r <- rast("gis/continental_shelf.grd")   # too large for github - can load cropped data below.

r_crop <- crop(r, ext(20, 28, -35, -33.5))

# Cap deep water at -100m for better shallow colours
r_crop[r_crop < -100] <- -100

bathy_df <- as.data.frame(r_crop, xy = TRUE)
names(bathy_df) <- c("lon", "lat", "depth")

# save for github
saveRDS(bathy_df, "./gis/continental_shelf_cropped.rds")
# import for github
bathy_df <- readRDS("./gis/continental_shelf_cropped.rds")

# Mask land
bathy_raster <- r_crop
bathy_raster[bathy_raster > 0] <- NA


# ============================================================
# 2. Place labels
# ============================================================
# places <- data.frame(
#   name = c("Knysna", "Buffels Bay", "Plettenberg Bay", "Tsitsikamma"),
#   lon  = c(23.046, 23.05, 23.368, 23.890),
#   lat  = c(-34.070, -34.115, -34.058, -34.010)
# )

places <- data.frame(
  name = c("Knysna", "Plettenberg Bay", "Tsitsikamma"),
  lon  = c(23.046, 23.368, 23.890),
  lat  = c(-34.070, -34.058, -34.010)
)


places_sf <- st_as_sf(places, coords = c("lon", "lat"), crs = 4326)

# ============================================================
# 3. Dolphin kernel density overlay
# ============================================================
dat     <- read.csv("./gis/sightings_data.csv")
pts_sf  <- st_as_sf(dat, coords = c("lon", "lat"), crs = 4326)
pts_utm <- st_transform(pts_sf, 32734)

coords <- st_coordinates(pts_utm)
win    <- as.owin(st_bbox(pts_utm))
pp     <- ppp(coords[,1], coords[,2], window = win)

kde      <- density(pp, sigma = 100)
kde_rast <- rast(kde)
crs(kde_rast) <- "EPSG:32734"
kde_wgs  <- project(kde_rast, "EPSG:4326")

# Normalise 0-1
kde_vals <- values(kde_wgs)
kde_wgs  <- kde_wgs / max(kde_vals, na.rm = TRUE)

# Mask near-zero values
kde_wgs[kde_wgs < 0.02] <- NA

# ============================================================
# 4. Colour palettes
# ============================================================
depth_colors <- scico(100, palette = "oslo", direction = 1)
heat_colors  <- RColorBrewer::brewer.pal(9, "YlOrRd")

# ============================================================
# 5. Bounding box — edit the 4 borders here
# ============================================================
xmin_map <- 22.795   # west border
xmax_map <- 24.208   # east border
ymin_map <- -34.205  # south border
ymax_map <- -33.825  # north border

bbox_sf <- st_bbox(c(xmin = xmin_map,
                     ymin = ymin_map,
                     xmax = xmax_map,
                     ymax = ymax_map),
                   crs = st_crs(4326)) |> st_as_sfc()

basemap <- get_tiles(bbox_sf, provider = "CartoDB.Positron", zoom = 12,
                     crop = TRUE,
                     cachedir = "gis/tiles")   # To download tiles, remove this line of code. If not set, tiles are cached in a tempdir folder.


# ============================================================
# 5b. MPA areas
# ============================================================
mpas <- st_read("./gis/MPA/MPAs_adjustedHWM.shp")

mpa_names <- data.frame(
  name = c("Goukamma", "Robberg", "Tsitsikamma"),
  lon  = c(22.91, 23.5, 24.00),
  lat  = c(-34.14, -34.14, -34.14)  # moved up from -34.14 to sit inside new ymin of -34.20
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
           (lon_div1 + lon_div2) / 2,
           (lon_div2 + lon_east) / 2),
  lat  = lat_line + 0.06   # reduced from 0.07 so labels stay below new north border
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

# ============================================================
# 7. Build tmap
# ============================================================
m <-
  # Basemap tiles — bbox_sf sets the rendered extent
  tm_shape(basemap, bbox = bbox_sf) +
  tm_rgb() +
  
  #-----------------------------------
# Bathymetry raster
#-----------------------------------
tm_shape(bathy_raster) +
  tm_raster(
    col.scale  = tm_scale_continuous(values = depth_colors,
                                     limits = c(-100, 0)),
    col_alpha  = 1,
    col.legend = tm_legend(
      title    = "Depth (m)",
      frame    = FALSE,
      position = tm_pos_out("right", "center"))) +
  
  #-----------------------------------
# MPA polygons
#-----------------------------------
# tm_shape(mpas, bbox = bbox_sf) +
#   tm_polygons(
#     fill       = "aquamarine1",
#     fill_alpha = 0.3,
#     col        = "aquamarine1",
#     lwd        = 1.5,
#     col.legend = tm_legend_hide()) +
#   tm_add_legend(
#     type       = "polygons",
#     fill       = "aquamarine1",
#     fill_alpha = 0.3,
#     col        = "aquamarine1",
#     lwd        = 1.5,
#     title      = "Marine Protected Areas",
#     position   = tm_pos_in("left", "bottom"),
#     frame      = FALSE,
#     bg.color   = NA,
#     width      = 18,
#     height     = 2,
#     title.size  = 1,
#     text.size   = 1,
#     title.color = "white"
#   ) +
#   

tm_shape(mpas, bbox = bbox_sf) +
  tm_polygons(
    fill       = "aquamarine1",
    fill_alpha = 0.3,
    col        = "aquamarine1",
    lwd        = 1.5,
    col.legend = tm_legend_hide()) +
  tm_add_legend(
    type       = "polygons",
    fill       = "aquamarine1",
    fill_alpha = 0.3,
    col        = "aquamarine1",
    lwd        = 1.5,
    labels     = "Marine Protected Areas",  # text now sits to the right of the box
    title      = "",                         # no title above
    # 1st x position (0 = left, 1 = right) 
    # 2nd y (0 = bottom, 1 = top)
   position = c(0.035, 0.13),
    frame      = FALSE,
    bg.color   = NA,
    width      = 18,
    height     = 2,
    title.size  = 1,
    text.size   = 1,
    title.color = "white",
    text.color  = "white"
  ) +
  #-----------------------------------
# Study area section lines
#-----------------------------------
tm_shape(h_line) +
  tm_lines(col = "white", lwd = 1.5) +
  
  tm_shape(v_lines) +
  tm_lines(col = "white", lwd = 1.5) +
  
  #-----------------------------------
# Kernel density of dolphin observations
#-----------------------------------
tm_shape(kde_wgs) +
  tm_raster(
    col.scale  = tm_scale_continuous(values = heat_colors,
                                     limits = c(0, 1)),
    col_alpha  = 1,
    col.legend = tm_legend(
      title       = "Encounters (relative density)",
      orientation = "landscape",
      position    = tm_pos_in("left", "top"),
      frame       = FALSE,
      bg.color    = NA,
      width       = 11.5,
      height      = 2,
      title.size  = 1,
      text.size   = 1)) +
  
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
  
  #-----------------------------------
# MPA names
#-----------------------------------
tm_shape(mpa_names_sf) +
  tm_text(
    text     = "name",
    size     = 1,
    fontface = "plain",
    col      = "aquamarine1",
    options  = opt_tm_text(shadow = FALSE),
    ymod     = 0.3) +
  
  #-----------------------------------
# Graticule labels
#-----------------------------------
tm_graticules(
    x           = seq(22.8, 24.2, by = 0.2),   # longitude labels every 0.2 degrees
    y           = seq(-34.2, -33.9, by = 0.1), # latitude unchanged
    lines       = FALSE,
    labels.size = 0.8) + 

  
#-----------------------------------
# Scale bar — moved left to stay inside new east border
#-----------------------------------
tm_scalebar(breaks = c(0, 10, 20, 30, 40), position = c("right", "top"), text.size = 1)

#m

# ============================================================
# 8. Save
# ============================================================
tmap_save(m, "./figures/Figure1B.png",
          width  = 6000,
          height = 3000,
          units  = "px",
          dpi    = 600)

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
  lon  = c(31.2,  23.4,  22.153, 26.6,  18.2,  32.1),
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

#sa_map

tmap_save(sa_map, "./figures/Figure1A.png",
           width  = 4000,
           height = 4000,
           units  = "px",
           dpi    = 600)

# ============================================================
# Combine maps with magick
# ============================================================
library(magick)

mapA <- image_read("./figures/Figure1A.png")
mapB <- image_read("./figures/Figure1B.png")

mapA <- image_trim(mapA)
mapB <- image_trim(mapB)

dpi <- image_info(mapB)$density
dpi <- ifelse(is.na(dpi), 72, as.numeric(strsplit(dpi, "x")[[1]][1]))

margin_px <- round(1 * dpi / 2.54)
infoB     <- image_info(mapB)

mapA_small <- image_resize(mapA, paste0(round(infoB$width * 0.5)))

mapA_centered <- image_extent(
  mapA_small,
  geometry = paste0(infoB$width, "x", image_info(mapA_small)$height),
  gravity  = "center"
)

mapA_centered <- image_background(mapA_centered, "white", flatten = TRUE)

label_size <- round(dpi * 1.25)

mapA_labeled <- image_annotate(
  mapA_centered,
  text     = "A",
  gravity  = "northwest",
  location = paste0("+", margin_px/3, "+", margin_px/3),
  size     = label_size,
  font     = "Arial",
  color    = "black",
  weight   = 400
)

mapB_labeled <- image_annotate(
  mapB,
  text     = "B",
  gravity  = "northwest",
  location = paste0("+", margin_px/3, "+", margin_px/3),
  size     = label_size,
  font     = "Arial",
  color    = "black",
  weight   = 400
)

gap     <- image_blank(infoB$width, margin_px, color = "white")
stacked <- image_append(c(mapA_labeled, gap, mapB_labeled), stack = TRUE)
final   <- image_border(stacked, color = "white",
                        geometry = paste0(margin_px, "x", margin_px))

#print(final)
image_write(final, "./figures/Figure1.png")


# ============================================================
# Interactive map
# ============================================================
tmap_mode("view")  # switch to interactive mode

m_interactive <-
  tm_shape(bathy_raster) +
  tm_raster(
    col.scale  = tm_scale_continuous(values = depth_colors, limits = c(-100, 0)),
    col_alpha  = 0.7,
    col.legend = tm_legend(title = "Depth (m)")) +
  
  tm_shape(mpas) +
  tm_polygons(
    fill       = "aquamarine1",
    fill_alpha = 0.3,
    col        = "aquamarine1",
    lwd        = 1.5) +
  
  tm_shape(kde_wgs) +
  tm_raster(
    col.scale  = tm_scale_continuous(values = heat_colors, limits = c(0, 1)),
    col_alpha  = 0.8,
    col.legend = tm_legend(title = "Encounters (relative density)")) +
  
  tm_shape(places_sf) +
  tm_text(text = "name", size = 1, col = "black", ymod = 0.3) +
  
  tm_shape(mpa_names_sf) +
  tm_text(text = "name", size = 1, col = "aquamarine1", ymod = 0.3) +
  
  tm_basemap("CartoDB.Positron")  # interactive basemap

# Save as HTML
tmap_save(m_interactive, "./supplement/Figure1_map.html")

# Switch back to static for rest of script
tmap_mode("plot")
