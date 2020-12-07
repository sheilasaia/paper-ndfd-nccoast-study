
# ---- script header ----
# script name: define_coast_bounds_script.R
# purpose of script: define bounds for coastal vs non-coastal analysis
# author: sheila saia
# date created: 20201112
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:
 

# ---- to do ----
# to do list

# TODO run analysis for county-based and watershed-based bounds
# TODO run analysis for gages with and without buffer
# 


# ---- 1. load libraries ----
library(tidyverse)
library(sf)
# library(tigris)


# ---- 2. set paths and define projections ----
# spatial data input path
spatial_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/"

# spatial data output path
spatial_data_output_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/"

# define epsg and proj for CONUS Albers projection (projecting to this)
conus_albers_epsg <- 5070
# conus_albers_proj <- "+init=EPSG:5070"

# define egsp for wgs84
# wgs84_epsg <- 4326


# ---- 3. import data ----
# county bounds
county_bounds_raw <- st_read(paste0(spatial_data_input_path, "county_bounds_raw/tl_2016_us_county/tl_2016_us_county.shp"))
# st_crs(county_bounds_raw) # is projected (epsg = 4269)

# nc bounds
nc_bounds_albers <- st_read(paste0(spatial_data_output_path, "region_state_bounds/nc_bounds_albers.shp")) %>%
  st_geometry()
# st_crs(nc_bounds_albers) # is projected (epsg = 5070)

# watershed bounds (HUC 8)
ws_bounds_raw <- st_read(paste0(spatial_data_input_path, "watershed_bounds_raw/WBD_National_GDB/resaved_gdb_to_shp_files/WBDHU8.shp"))
# st_crs(ws_bounds_raw) # is projected (epsg = 4269)

# shorline bounds
shoreline_noaa_raw <- st_read(paste0(spatial_data_input_path, "shoreline_raw/us_medium_shoreline.shp"))
# st_crs(ws_bounds_raw) # is projected (epsg = 4269)

# sga bounds
sga_bounds_dissolve_albers <- st_read(paste0(spatial_data_output_path, "sga_bounds/sga_bounds_dissolve_albers.shp"))
# st_crs(ws_bounds_raw) # is projected (epsg = 5070)


# ---- 4. project data ----
# project county bounds
county_bounds_albers <- county_bounds_raw %>%
  st_transform(crs = conus_albers_epsg) %>%
  dplyr::select(STATEFP:NAMELSAD)
# st_crs(county_bounds_albers)

# project watershed bounds
ws_bounds_albers <- ws_bounds_raw %>%
  st_transform(crs = conus_albers_epsg)
# st_crs(county_bounds_albers)

# project noaa shoreline dataset
shoreline_noaa_albers <- shoreline_noaa_raw %>%
  st_transform(crs = conus_albers_epsg)
# st_crs(shoreline_noaa_albers) # is projected (epsg = 5070)


# ---- 5. identify nc coastal counties ----
# get NC counties
nc_county_bounds_albers <- county_bounds_albers %>%
  dplyr::filter(STATEFP == 37)

# get NC coastal counties
nc_coast_county_bounds_albers <- nc_county_bounds_albers %>%
  dplyr::mutate(overlap = st_overlaps(., sga_bounds_dissolve_albers, sparse = FALSE)) %>%
  dplyr::filter(overlap == TRUE) %>%
  dplyr::select(-overlap) %>%
  st_union()


# ---- 6. identify nc coastal watersheds ----
nc_coast_ws_bounds_albers <- ws_bounds_albers %>%
  dplyr::mutate(overlap = st_overlaps(., sga_bounds_dissolve_albers, sparse = FALSE)) %>%
  dplyr::filter(overlap == TRUE) %>%
  dplyr::select(-overlap) %>%
  st_union()


# ---- 7. get 2015 TIGER national US coastline dataset ----
# coastlines
# shoreline_tiger2015_raw <- tigris::coastline(year = 2015)
# st_crs(coastline_data_raw) # crs = 4269

# convert to albers
# shoreline_tiger2015_albers <- shoreline_tiger2015_data_raw %>%
#  st_transform(crs = conus_albers_epsg)

# clip to NC
# nc_shoreline_tiger2015_albers <- shoreline_tiger2015_albers %>%
#   st_intersection(nc_bounds_albers)

# buffer by 5 km
# nc_shoreline_tiger2015_5kmbuf_albers <- nc_shoreline_tiger2015_albers %>%
#   st_buffer(dist = 5000) # in m, 5000 m = 5 km


# ---- 8. clip and buffer NOAA shoreline dataset ----
# clip to nc
nc_shoreline_noaa_albers <- shoreline_noaa_albers %>%
  st_intersection(nc_bounds_albers)

# buffer
nc_shoreline_noaa_5kmbuf_albers <- nc_shoreline_noaa_albers %>%
  st_buffer(dist = 5000) %>% # in m, 5000 m = 5 km
  st_union()


# ---- 9. export data ----
# export nc coast county bounds
st_write(nc_coast_county_bounds_albers, paste0(spatial_data_output_path, "coast_bounds/nc_coast_county_bounds_albers.shp"), delete_layer = TRUE)

# export nc coast watershed bounds
st_write(nc_coast_ws_bounds_albers, paste0(spatial_data_output_path, "coast_bounds/nc_coast_ws_bounds_albers.shp"), delete_layer = TRUE)

# export tiger 2015 coastline bounds
# st_write(shoreline_tiger2015_raw, paste0(spatial_data_input_path, "/sheila_generated/region_state_bounds/shoreline_tiger2015_raw.shp"), delete_layer = TRUE)

# export nc tiger 2015 coastline buffer bounds
# st_write(nc_shoreline_tiger2015_5kmbuf_albers, paste0(spatial_data_output_path, "coast_bounds/nc_shoreline_tiger2015_5kmbuf_albers.shp"), delete_layer = TRUE)

# export nc shoreline bounds with 5km buffer
st_write(nc_shoreline_noaa_5kmbuf_albers, paste0(spatial_data_output_path, "coast_bounds/nc_shoreline_noaa_5kmbuf_albers.shp"), delete_layer = TRUE)

