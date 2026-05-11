
library(ggOceanMaps)
library(ggspatial) # for data plotting

# Basemap

basemap(limits = -60) # A synonym: basemap(60) 

# Rectangular maps are plotted by specifying the limits argument as a numeric vector of length 4 
# where the first element defines the 
#start longitude, the second element the 
#end longitude, the third element the 
#minimum latitude and the fourth element the 
#maximum latitude of the bounding box:
  
basemap(limits = c(12, 40, -30, -35), bathymetry = TRUE)

basemap(limits = c(20, 30, -33, -35), bathymetry = TRUE)

basemap(limits = c(20, 30, -33, -35), bathymetry = TRUE,
                                bathy.style = "rcb") # synonym to "raster_continuous_blues"

