# ---- script header ----
# script name: usgov_tidy_reg_bounds_data_script.R
# purpose of script: tidy up regional bounds data for project
# author: sheila saia
# date created: 20201102  
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:
 
s
# ---- to do ----
# to do list

# TODO remove urban bounds and urban spatial data?


# ---- 1. load libraries ----
library(tidyverse)
library(sf)
library(tidylog)
library(here)


# ---- 2. set paths and define projections ----
# spatial data input path
spatial_data_input_path <- here::here("data", "spatial", "region_state_bounds_raw")

# spatial data input path
spatial_data_output_path <- here::here("data", "spatial", "region_state_bounds_tidy")

# define epsg and proj for CONUS Albers projection (projecting to this)
conus_albers_epsg <- 5070
# conus_albers_proj <- "+init=EPSG:5070"

# define egsp for wgs84
# wgs84_epsg <- 4326


# ---- 3. import data ----
# state bounds
state_bounds_raw <- st_read(paste0(spatial_data_input_path, "/tl_2017_us_state.shp"))
# st_crs(state_bounds_raw) # is projected (epsg = 4269)

# urban bounds
# urban_bounds_raw <- st_read(paste0(spatial_data_input_path, "/NCDOT_Smoothed_Urban_Boundaries.shp"))
# st_crs(urban_bounds_raw) # is projected (epsg = 2264, NC State Plan Albers in ft)


# ---- 4. project data ----
# project state bounds
state_bounds_albers <- state_bounds_raw %>%
  st_transform(conus_albers_epsg) %>%
  dplyr::select(REGION:NAME)
# st_crs(state_bounds_albers) # check!
# names(state_bounds_albers)

# project urban bounds
# urban_bounds_albers <- urban_bounds_raw %>%
#   st_transform(conus_albers_epsg) %>%
#   dplyr::select(OBJECTID:POP_EST_YR, NAME)
# st_crs(urban_bounds_albers) # check!
# names(urban_bounds_albers)

# ---- 5. select NC from state bounds ----
# select NC
nc_bounds_albers <- state_bounds_albers %>%
  dplyr::filter(NAME == "North Carolina") %>%
  group_by(NAME) %>%
  summarize(geometry = sf::st_union(geometry)) %>%
  ungroup()


# ---- 6. make 10 km buffer around NC ----
# buffer
nc_bounds_buffer_albers <- nc_bounds_albers %>%
  st_buffer(dist = 10000) %>% # buffer distance is in m so 10 * 1000m = 10km
  st_convex_hull() # simple buffer


# ---- 7. dissolve urban bounds into one polygon ----
# urban_bounds_combo_albers <- urban_bounds_albers %>%
#   st_combine()


# ---- 8. export data ----
# export state bounds
st_write(state_bounds_albers, paste0(spatial_data_output_path, "/state_bounds_albers.shp"), delete_layer = TRUE)

# export nc state bounds
st_write(nc_bounds_albers, paste0(spatial_data_output_path, "/nc_bounds_albers.shp"), delete_layer = TRUE)

# export nc buffer
st_write(nc_bounds_buffer_albers, paste0(spatial_data_output_path, "/nc_bounds_10kmbuf_albers.shp"), delete_layer = TRUE)

