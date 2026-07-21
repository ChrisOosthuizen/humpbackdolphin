# The code below assumes:
#   
# segments_sf contains the coastline LINESTRINGs divided into 5 sections:
# This object was created in the "2026 Extra Coastline and MPA distance" script
#> segments_sf
#Simple feature collection with 5 features and 1 field
#Geometry type: LINESTRING
#Dimension:     XY
#Bounding box:  xmin: 669672.9 ymin: 6222953 xmax: 794891.4 ymax: 6237082
#Projected CRS: WGS 84 / UTM zone 34S
#section                       geometry
#1 Goukamma MPA LINESTRING (669672.9 623102...
#2 Goukamma-Robberg LINESTRING (682496.1 622726...
#3 Robberg MPA LINESTRING (718676.6 622409...
#4 Robberg-Tsitsikamma LINESTRING (718807.1 622655...
#5 Tsitsikamma MPA LINESTRING (738269.4 623655...
                                                                                                                           

# section contains the names exactly as shown.
# n_obs = 98 dolphin encounters.
# We know the Search effort per section:
# Knysna = 231 h
# Plettenberg Bay = 190 h
# Tsitsikamma = 132 h

#--------------------------------------------------
# Length of each section
#--------------------------------------------------
# Length of each of the 5 sections, in meter
seg_lengths <- as.numeric(st_length(segments_sf))
seg_lengths 

# cumulative length from the west to the east, in meter
cum_lengths <- c(0, cumsum(seg_lengths))
cum_lengths

section_names <- segments_sf$section
section_names

# Which sections are MPAs?
is_mpa <- grepl("MPA", section_names)
is_mpa

total_length <- sum(seg_lengths)
total_length    # in m
total_length/1000   # in km

# ----------------------------------------------------------
# Merge the five sections into one coastline
# ----------------------------------------------------------
study_coast <- st_line_merge(st_union(segments_sf))

# Check
plot(st_geometry(study_coast), lwd = 3)
plot(st_geometry(segments_sf), lwd = 3, col = 2:6, add = TRUE)

# Region lengths
knysna_length <- sum(seg_lengths[1:2])
plett_length  <- sum(seg_lengths[3:4])
tsi_length    <- seg_lengths[5]

# Cumulative distances along the whole coastline
region_start <- c(
  Knysna = 0,
  Plettenberg = knysna_length,
  Tsitsikamma = knysna_length + plett_length)

region_start

region_length <- c(
  Knysna = knysna_length,
  Plettenberg = plett_length,
  Tsitsikamma = tsi_length)

region_length

# Survey effort hours
effort <- c(
  Knysna = 231,
  Plettenberg = 190,
  Tsitsikamma = 132
)

region_prob <- effort / sum(effort)
region_prob 


#--------------------------------------------------
# Monte Carlo simulation - generate points along the coast. This would be similar to the GLM analysis
#--------------------------------------------------
# set.seed(1234)
# 
# n_obs <- 98    # real nr of dolphin encoutners in data
# n_sim <- 10000  # MCMC samples
# 
# sim_hits <- numeric(n_sim)
# 
# example_plot_number = 10
# 
# # Store the first 10 simulated point sets
# sim_points <- vector("list", example_plot_number)
# 
# for(i in seq_len(n_sim)){
#   
#   # Choose survey region according to search effort
#   region <- sample(
#     names(region_prob),
#     size = n_obs,
#     replace = TRUE,
#     prob = region_prob
#   )
#   
#   # Generate distances along the coastline
#   d <- numeric(n_obs)
#   
#   for(j in seq_len(n_obs)){
#     
#     d[j] <-
#       region_start[region[j]] +
#       runif(1, 0, region_length[region[j]])
#     
#   }
#   
# 
#   # Which section does each point fall into?
#   sec <- findInterval(d, cum_lengths,
#                       rightmost.closed = TRUE)
#   
#   sim_hits[i] <- sum(is_mpa[sec])
#   
#   # Save first 10 simulations
#   if(i <= example_plot_number){
#     
#     sim_points[[i]] <- st_line_sample(
#       study_coast,
#       sample = d / total_length
#     ) |>
#       st_cast("POINT") |>
#       st_sf() |>
#       mutate(
#         section = section_names[sec],
#         MPA = ifelse(is_mpa[sec], "MPA", "Non-MPA")
#       )
#   }
# }
# 
# 
# sim_hits
# 
# 
# #-----------
# # Map this
# #-----------
# 
# library(tmap)
# 
# tmap_mode("plot")
# 
# maps <- lapply(1:example_plot_number, function(i){
#   
#   tm_shape(study_coast) +
#     tm_lines(col = "grey60") +
#     
#     tm_shape(sim_points[[i]]) +
#     tm_dots(
#       col = "MPA",
#       palette = c("red", "blue"),
#       size = 0.7
#     ) +
#     
#     tm_layout(
#       title = paste("Simulation", i),
#       legend.show = FALSE
#     )
# })
# 
# tmap_arrange(maps, ncol = 2)


#--------------------------------------------------
# Observed encounters inside MPAs
#--------------------------------------------------
dat <- read.csv("./gis/sightings_data.csv")
head(dat)

encounters_sf  <- st_as_sf(dat, coords = c("lon", "lat"), crs = 4326)
encounters_sf <- st_transform(encounters_sf, 32734)

obs <- st_nearest_feature(encounters_sf, segments_sf)

obs_section <- segments_sf$section[obs]

observed_hits <- sum(grepl("MPA", obs_section))

observed_hits


# Add section and MPA status to encounters
encounters_sf$section <- obs_section

encounters_sf$MPA <- ifelse(
  grepl("MPA", obs_section),
  "MPA",
  "Non-MPA")

# Colours for coastline sections
section_colors <- c(
  "Goukamma MPA"         = "forestgreen",
  "Goukamma-Robberg"     = "orange",
  "Robberg MPA"          = "dodgerblue",
  "Robberg-Tsitsikamma"  = "red",
  "Tsitsikamma MPA"      = "purple")

tmap_mode("view")

tm_shape(segments_sf) +
  tm_lines(
    col = "section",
    palette = section_colors,
    lwd = 5,
    title = "Coast section"
  ) +
  tm_shape(encounters_sf) +
  tm_dots(
    col = "MPA",
    palette = c("MPA" = "cyan", "Non-MPA" = "yellow"),
    size = 0.68,
    title = "Encounter" )



#--------------------------------------------------
# Monte Carlo p-value
#--------------------------------------------------

#mean(sim_hits)
#sd(sim_hits)
#p_value <- (sum(sim_hits >= observed_hits) + 1) / (n_sim + 1)

# cat("Observed MPA encounters :", observed_hits, "\n")
# cat("Expected under null     :", round(mean(sim_hits),2), "\n")
# cat("SD of simulations       :", round(sd(sim_hits),2), "\n")
# cat("Monte Carlo p-value     :", p_value, "\n")


# hist(sim_hits,
#      breaks = 30,
#      col = "grey80",
#      border = "white",
#      xlab = "Number of encounters inside MPAs",
#      main = "Monte Carlo randomization")
# 
# abline(v = observed_hits,
#        col = "red",
#        lwd = 3)
# 
# legend("topright",
#        legend = paste("Observed =", observed_hits),
#        lwd = 3,
#        col = "red",
#        bty = "n")


# Calculate an effect size
# This is useful because the p-value alone doesn't tell you how large the effect is.
# 
# z <- (observed_hits - mean(sim_hits)) / sd(sim_hits)
# z
# 
# #Also report the percentile
# percentile <- ecdf(sim_hits)(observed_hits)
# percentile


#-------------------
# Binomial test
#-------------------
p_knysna <- seg_lengths[1] / sum(seg_lengths[1:2])
p_plett  <- seg_lengths[3] / sum(seg_lengths[3:4])

encounters_sf <- encounters_sf %>%
  mutate(region = case_when(
    section %in% c("Goukamma MPA", "Goukamma-Robberg") ~ "Knysna",
    section %in% c("Robberg MPA", "Robberg-Tsitsikamma") ~ "Plettenberg Bay",
    section == "Tsitsikamma MPA" ~ "Tsitsikamma",
    TRUE ~ NA_character_
  ))

# don't use area - for some reason it doesn't work (I think this is where surveys had the LAUNCH SITE but it is not the 
# coastal section!)

counts <- encounters_sf %>%
  st_drop_geometry() %>%
  count(region, MPA) %>%
  tidyr::pivot_wider(
    names_from = MPA,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total = MPA + `Non-MPA`)

counts

binom.test(
  x = counts$MPA[counts$region == "Knysna"],
  n = counts$total[counts$region == "Knysna"],
  p = p_knysna)

binom.test(
  x = counts$MPA[counts$region == "Plettenberg Bay"],
  n = counts$total[counts$region == "Plettenberg Bay"],
  p = p_plett)



#---------------------------------------------------------
# Survey data (from 'constructing tracks)
#---------------------------------------------------------

# Plot the maximum end points of surveys from notes:

survey_sf <- survey_sf |>
  st_set_crs(4326) |>
  st_transform(32734)


tm_shape(segments_sf) +
  tm_lines(
    col = "section",
    palette = section_colors,
    lwd = 5) +
  
  tm_shape(encounters_sf) +
  tm_dots(
    col = "MPA",
    palette = c("MPA" = "red", "Non-MPA" = "yellow"),
     size = 1) 
  # 
  # tm_shape(survey_sf) +
  # tm_dots(fill = "orange2",
  #   col = "red",
  #   size = 1)

# plot 

tmap_mode("plot")

coast_check_map + 
  tm_shape(encounters_sf) +
  tm_dots(
    col = "MPA",
    palette = c("MPA" = "red", "Non-MPA" = "yellow"),
    size = 0.58,
    title = "Encounter" )

# ============================================================
# 9. Save
# ============================================================
tmap_save(coast_check_map, "./supplement/Extra_Coastal distances.png",
          width  = 6000,
          height = 3000,
          units  = "px",
          dpi    = 600)


