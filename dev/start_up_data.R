library(sf)
library(data.table)
library(feather)
# area shapefile
uk_sf_area <- st_read('~/Dropbox/MappaR/Shapefiles/areas_MappaR.gpkg', quiet = TRUE)
uk_sf_area$area <- as.numeric(st_area(uk_sf_area))/1000000 # km^2
area_coords <- as.data.frame(st_coordinates(st_centroid(uk_sf_area$geom)))
uk_areas <- cbind(uk_sf_area, area_coords)
# sector shapefile
uk_sf_sector <- st_read('~/Dropbox/MappaR/Shapefiles/sectors_MappaR.gpkg', quiet = TRUE)
sector_coords <- as.data.frame(st_coordinates(st_centroid(uk_sf_sector$geom)))
uk_sectors <- cbind(uk_sf_sector, sector_coords)
# unit data.table
uk_units <- read_feather('~/Dropbox/MappaR/Shapefiles/uk_units.feather')
setDT(uk_units)
#uk_units <- uk_units[PostcodeArea=='E',]
cols_to_keep <- c('PostcodeUnit','X','Y')
uk_units <- uk_units[usertype==0, ..cols_to_keep]
setkey(uk_units, PostcodeUnit)
# postcode area lookups
postcode_area_name_mapping <- fread('~/Dropbox/MappaR/Shapefiles/postcode_area_name_mapping.csv')
# lgbm objectives
lgbm_objectives <- fread('~/Dropbox/lucidum support/lgbm_objectives.csv')
# glm objectives
glm_objectives <- fread('~/Dropbox/lucidum support/glm_objectives.csv')
# usethis
usethis::use_data(uk_areas,
                  uk_sectors,
                  uk_units,
                  postcode_area_name_mapping,
                  lgbm_objectives,
                  glm_objectives,
                  internal = TRUE,
                  overwrite = TRUE)
