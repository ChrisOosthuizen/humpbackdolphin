library(tidyverse)
library(sf)
library(mapview)

ef = read.csv("./gis/survey_effort.csv")

df = read.csv("./gis/sightings_data.csv")

head(df)

d = read.csv("./gis/all_tracks.csv")
dim(d)
head(d)

dd <- d %>%
  distinct(Date, Time, y_proj, x_proj, .keep_all = TRUE)

dim(dd)

unique(dd$Date)


# I don't have all the survey effort GIS data - only 64 days. 