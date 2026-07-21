
# Calculate coastline distances and proportion of area covered by MPA

# ============================================================
# 0. Packages
# ============================================================
library(sf)
library(dplyr)
library(tmap)
library(maptiles) 
library(terra)

# ============================================================
# 1. Get coastline as a single string
# ============================================================
coast <- st_read("./gis/MPA/ph_coast_li_sa.shp")

plot(coast)

table(coast$TYPE)

mainland <- subset(coast, TYPE == "Mainland")
st_geometry_type(mainland)
plot(mainland)

parts <- st_geometry(mainland)[[1]]
plot(parts)
class(parts)

# Order by mean x-coordinate
ord <- order(sapply(parts, function(x) mean(x[,1])))
parts <- parts[ord]

coords <- parts[[1]]

for(i in 2:length(parts)) {
  
  this <- parts[[i]]
  
  # If this segment runs east->west, reverse it
  if(this[1,1] > this[nrow(this),1])
    this <- this[nrow(this):1,]
  
  coords <- rbind(coords, this)
}

coastline <- st_sfc(st_linestring(coords), crs = st_crs(mainland))

plot(coastline)
class(coastline)

# set projection
st_crs(coastline) <- 4326

# ============================================================
# 2. Set MPA BOUNDARY COORDINATES (Longitude, Latitude)
# ============================================================

goukamma_west  <- c(lon = 22.835426, lat = -34.046392)
goukamma_east <- c(lon = 22.978978, lat = -34.078576)

robberg_west <- c(lon = 23.371578, lat = -34.101775)
robberg_east <- c(lon = 23.371600, lat = -34.081944)

tsitsikamma_west <- c(lon = 23.576156, lat = -33.983314)
tsitsikamma_east <- c(lon = 24.194845, lat = -34.059735)

# ============================================================
# 3. Load MPA areas
# ============================================================
mpas <- st_read("./gis/MPA/MPAs_adjustedHWM.shp")

mpa_names <- data.frame(
  name = c("Goukamma", "Robberg", "Tsitsikamma"),
  lon  = c(22.91, 23.5, 24.00),
  lat  = c(-34.14, -34.14, -34.14)  # moved up from -34.14 to sit inside new ymin of -34.20
)

mpa_names_sf <- st_as_sf(mpa_names, coords = c("lon", "lat"), crs = 4326)


# ============================================================
# 4. GET COAST POINTS
# ============================================================

boundaries <- tibble(
  label = c(
    "Goukamma West",
    "Goukamma East",
    "Robberg West",
    "Robberg East",
    "Tsitsikamma West",
    "Tsitsikamma East"
  ),
  lon = c(
    goukamma_west ["lon"],
    goukamma_east["lon"],
  
    robberg_west["lon"],
    robberg_east["lon"],
    tsitsikamma_west["lon"],
    tsitsikamma_east["lon"]
  ),
  lat = c(
    goukamma_west ["lat"],
    goukamma_east["lat"],
 
    robberg_west["lat"],
    robberg_east["lat"],
    tsitsikamma_west["lat"],
    tsitsikamma_east["lat"]
  )
)

boundaries 

get_coast_point <- function(lon, lat, coastline){
  
  coast_xy <- st_coordinates(coastline)
  
  i <- which.min(
    (coast_xy[,1] - lon)^2 +
      (coast_xy[,2] - lat)^2
  )
  
  st_sfc(
    st_point(coast_xy[i, 1:2]),
    crs = st_crs(coastline)
  )
}

points <- Map(
  get_coast_point,
  lon = boundaries$lon,
  lat = boundaries$lat,
  MoreArgs = list(coastline = coastline)
)

names(points) <- boundaries$label

# ============================================================
# 5. STRAIGHT-LINE DISTANCES
# ============================================================

segments <- tibble(
  section = c(
    "Goukamma MPA",
    "Goukamma-Robberg",
    "Robberg MPA",
    "Robberg-Tsitsikamma",
    "Tsitsikamma MPA"
  ),
  from = c(
    "Goukamma West",
    "Goukamma East",
    "Robberg West",
    "Robberg East",
    "Tsitsikamma West"
  ),
  to = c(
    "Goukamma East",
    "Robberg West",
    "Robberg East",
    "Tsitsikamma West",
    "Tsitsikamma East"))

segments$straight_line_km <- NA_real_

for(i in seq_len(nrow(segments))){
  
  xy1 <- st_coordinates(points[[segments$from[i]]])
  xy2 <- st_coordinates(points[[segments$to[i]]])
  
  segments$straight_line_km[i] <-
    geosphere::distGeo(xy1, xy2)/1000
}

segments

# ============================================================
# 6. ALONG-COAST DISTANCES
# ============================================================

coast_proj <- st_transform(coastline, 32734)

# DO SMOOTHING OF THE COASTLINE 

coast_smooth <- st_simplify(
  coast_proj,
  dTolerance = 300,
  preserveTopology = FALSE)

coast_xy <- st_coordinates(coast_smooth)

coast_dist <- function(coords, i1, i2){
  
  idx <- seq(min(i1,i2), max(i1,i2))
  
  d <- sqrt(diff(coords[idx,1])^2 +
              diff(coords[idx,2])^2)
  
  sum(d)
}

segments$coast_km <- NA_real_

for(i in seq_len(nrow(segments))){
  
  p1 <- st_coordinates(st_transform(points[[segments$from[i]]],32734))
  p2 <- st_coordinates(st_transform(points[[segments$to[i]]],32734))
  
  i1 <- which.min((coast_xy[,1]-p1[1,1])^2 +
                    (coast_xy[,2]-p1[1,2])^2)
  
  i2 <- which.min((coast_xy[,1]-p2[1,1])^2 +
                    (coast_xy[,2]-p2[1,2])^2)
  
  segments$coast_km[i] <-
    coast_dist(coast_xy,i1,i2)/1000
}

# ============================================================
# 7. SUMMARY TABLE
# ============================================================

distance_summary = segments

distance_summary <- bind_rows(
  distance_summary,
  tibble(
    section = "Total",
    straight_line_km = sum(distance_summary$straight_line_km),
    coast_km = sum(distance_summary$coast_km)  )
)

print(distance_summary)

# ============================================================
# SUMMARY 1: Three management sections
# ============================================================

distance_summary_sections <- tibble(
  section = c(
    "Goukamma",
    "Robberg",
    "Tsitsikamma",
    "Total"
  ),
  straight_line_km = c(
    sum(distance_summary$straight_line_km[1:2]),
    sum(distance_summary$straight_line_km[3:4]),
    distance_summary$straight_line_km[5],
    sum(
      sum(distance_summary$straight_line_km[1:2]),
      sum(distance_summary$straight_line_km[3:4]),
      distance_summary$straight_line_km[5]
    )
  ),
  coast_km = c(
    sum(distance_summary$coast_km[1:2]),
    sum(distance_summary$coast_km[3:4]),
    distance_summary$coast_km[5],
    sum(sum(distance_summary$coast_km[1:2]),
        sum(distance_summary$coast_km[3:4]),
        distance_summary$coast_km[5])
  )
) 

print(distance_summary_sections)


# ============================================================
# SUMMARY 2: MPA vs non-MPA
# ============================================================

distance_summary_mpa <- tibble(
  section = c(
    "MPA",
    "Non-MPA",
    "Total"
  ),
  straight_line_km = c(
    sum(distance_summary$straight_line_km[c(1,3,5)]),
    sum(distance_summary$straight_line_km[c(2,4)]),
    sum(  sum(distance_summary$straight_line_km[c(1,3,5)]),
          sum(distance_summary$straight_line_km[c(2,4)]))
  ),
  coast_km = c(
    sum(distance_summary$coast_km[c(1,3,5)]),
    sum(distance_summary$coast_km[c(2,4)]),
    sum(sum(distance_summary$coast_km[c(1,3,5)]),
        sum(distance_summary$coast_km[c(2,4)]))
  )
)

print(distance_summary_mpa)


# Proportion of coastline within MPAs
mpa_coast_km <- sum(distance_summary$coast_km[c(1, 3, 5)])
total_coast_km <- sum(distance_summary$coast_km[c(1:5)])

prop_mpa <- mpa_coast_km / total_coast_km
perc_mpa <- 100 * prop_mpa

cat("MPA coastline:", round(mpa_coast_km, 1), "km\n")
cat("Total coastline:", round(total_coast_km, 1), "km\n")
cat("Proportion protected by MPA:", round(prop_mpa, 3), "\n")
cat("Percentage protected by MPA:", round(perc_mpa, 1), "%\n")


# ============================================================
# 8. GET COAST SEGMENT GEOMETRIES FOR PLOTTING
# ============================================================

# Boundary points in projected coordinates
points_utm <- lapply(points, st_transform, crs = 32734)

# Create one LINESTRING for every section in the distance table
segment_geoms <- vector("list", nrow(segments))

point_indices <- integer(length(points))
names(point_indices) <- names(points)

for(i in seq_along(points)){
  
  xy <- st_coordinates(st_transform(points[[i]], 32734))
  
  point_indices[i] <- which.min(
    (coast_xy[,1] - xy[1,1])^2 +
      (coast_xy[,2] - xy[1,2])^2
  )
}


segment_geoms <- vector("list", nrow(segments))

for(i in seq_len(nrow(segments))){
  
  idx1 <- point_indices[segments$from[i]]
  idx2 <- point_indices[segments$to[i]]
  
  idx <- seq(min(idx1, idx2), max(idx1, idx2))
  
  if(length(idx) >= 2){
    segment_geoms[[i]] <- st_linestring(coast_xy[idx,1:2, drop=FALSE])
  } else {
    segment_geoms[[i]] <- st_geometrycollection()
  }
}

segments_sf <- st_sf(
  section = segments$section,
  geometry = st_sfc(
    segment_geoms,
    crs = st_crs(coast_proj)
  )
)


# Boundary points for plotting
points_sf <- st_sf(
  label = boundaries$label,
  geometry = st_sfc(
    lapply(points_utm, st_geometry) |> lapply(`[[`,1),
    crs = st_crs(coast_proj)))


# ============================================================
# 9. PLOT WITH TMAP
# ============================================================

xmin_map <- 22.635
xmax_map <- 24.308
ymin_map <- -34.205
ymax_map <- -33.825

bbox_sf <- st_bbox(
  c(
    xmin = xmin_map,
    ymin = ymin_map,
    xmax = xmax_map,
    ymax = ymax_map
  ),
  crs = st_crs(4326)
) |>
  st_as_sfc()

basemap <- get_tiles(
  bbox_sf,
  provider = "CartoDB.Positron",
  zoom = 11,
  crop = TRUE,
  cachedir = "gis/tiles"
)


#-----------------------------------
# Study area section lines
#-----------------------------------
# DoN't use fancy colors for leaflet
section_colors <- c(
  "Goukamma MPA"        = "#E69F00",
  "Goukamma-Robberg"    =  "blue",
  "Robberg MPA"         = "#61D04F" ,
  "Robberg-Tsitsikamma" = "#F0E442",
  "Tsitsikamma MPA"     =  "#0072B2")

# ===================
# 9. PLOT WITH TMAP
# ===================
tmap_mode("plot") 


coast_check_map <-
  
  tm_shape(basemap) +
  tm_rgb() +
  
#-----------------------------------
# MPA polygons
#-----------------------------------
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
    position = c(0.035, 0.23),
    frame      = FALSE,
    bg.color   = NA,
    width      = 18,
    height     = 2,
    title.size  = 1,
    text.size   = 1,
    title.color = "black",
    text.color  = "black"
  ) +
  
  tm_shape(coast_proj) +
  tm_lines(col = "grey40", lwd = 0.8) +
  
  
  tm_shape(segments_sf) +
  tm_lines(
    col = "section",
    col.scale = tm_scale_categorical(values = section_colors),
    lwd = 5,
    col.legend = tm_legend(title = "Section")
  ) +
  
  tm_shape(points_sf) +
  tm_dots(size = 0.7, fill = "magenta", col = "red") 


coast_check_map

# ============================================================
# 9. Save
# ============================================================
tmap_save(coast_check_map, "./supplement/Extra_Coastal distances.png",
          width  = 6000,
          height = 3000,
          units  = "px",
          dpi    = 600)


# =====================
# 9. PLOT WITH LEAFLET
# =====================

library(leaflet)
library(htmlwidgets)

points_leaflet   <- st_transform(points_sf, 4326)

segments_leaflet <-
  st_transform(segments_sf, 4326) |>
  dplyr::filter(!st_is_empty(geometry))

# Don't use fancy colors like red2 for leaflet - they don't plot
segments_leaflet$colour <-
  c("#E69F00",
     "blue",
    "#61D04F",
    "#F0E442",
    "#0072B2"
  )[match(
    segments_leaflet$section,
    c(
      "Goukamma MPA",
      "Goukamma-Robberg",
      "Robberg MPA",
      "Robberg-Tsitsikamma",
      "Tsitsikamma MPA"))]

leaflet() |>
  
  addProviderTiles(providers$CartoDB.Positron) |>
  
  addPolylines(
    data = coastline,
    color = "grey40",
    weight = 2
  ) |>
  
  addPolygons(
    data = mpas,
    fillColor = "aquamarine",
    fillOpacity = 0.3,
    color = "aquamarine",
    weight = 2
  ) |>
  
  addPolylines(
    data = segments_leaflet,
    color = ~colour,
    weight = 8
  )|>
  
  addCircleMarkers(
    data = points_leaflet,
    radius = 5,
    fillColor = "magenta",
    color = "red",
    fillOpacity = 1,
    stroke = TRUE
  ) |>
  
  onRender("
    function(el, x) {

      this.on('click', function(e) {

        L.popup()
          .setLatLng(e.latlng)
          .setContent(
            '<b>Longitude:</b> ' + e.latlng.lng.toFixed(6) +
            '<br><b>Latitude:</b> ' + e.latlng.lat.toFixed(6)
          )
          .openOn(this);

      });

    }
  ")


#----------------------
#Encounters
#----------------------

#--------------------------------------------------
# Length of each section
#--------------------------------------------------

seg_lengths <- as.numeric(st_length(segments_sf))

cum_lengths <- c(0, cumsum(seg_lengths))

section_names <- segments_sf$section

# Which sections are MPAs?
is_mpa <- grepl("MPA", section_names)

total_length <- sum(seg_lengths)

#--------------------------------------------------
# Monte Carlo simulation
#--------------------------------------------------
set.seed(123)

n_obs <- 98

n_sim <- 200

sim_hits <- numeric(n_sim)

for(i in seq_len(n_sim)){
  
  i = 1
  
  # Random distances along the coastline
  d <- runif(n_obs, 0, total_length)
  
  # Which section does each point fall into?
  sec <- findInterval(d, cum_lengths,
                      rightmost.closed = TRUE)
  
  sim_hits[i] <- sum(is_mpa[sec])
  
}

sim_hits

#--------------------------------------------------
# Observed value
#--------------------------------------------------

# Replace with your observed number of dolphin encounters
# inside MPAs

obs_hits <- 63      # <-- YOUR VALUE HERE

#--------------------------------------------------
# P-value
#--------------------------------------------------

p_value <- mean(sim_hits >= obs_hits)

cat("Observed:", obs_hits, "\n")
cat("Expected mean:", mean(sim_hits), "\n")
cat("95% interval:",
    quantile(sim_hits, c(0.025,0.975)), "\n")
cat("P =", p_value, "\n")

