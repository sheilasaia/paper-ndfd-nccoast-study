
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

# sga bounds
sga_bounds_dissolve_albers <- st_read(paste0(spatial_data_output_path, "sga_bounds/sga_bounds_dissolve_albers.shp"))
# st_crs(ws_bounds_raw) # is projected (epsg = 5070)


# ---- 4. project data ----
# project county bounds
county_bounds_albers <- county_bounds_raw %>%
  st_transform(crs = conus_albers_epsg) %>%
  dplyr::select(STATEFP:NAMELSAD)
# st_crs(county_bounds_albers)

# projet watershed bounds
ws_bounds_albers <- ws_bounds_raw %>%
  st_transform(crs = conus_albers_epsg)
# st_crs(county_bounds_albers)


# ---- 5. identify nc coastal counties ----
# get NC counties
nc_county_bounds_albers <- county_bounds_albers %>%
  dplyr::filter(STATEFP == 37)

# get NC coastal counties
nc_coast_county_bounds_albers <- nc_county_bounds_albers %>%
  dplyr::mutate(overlap = st_overlaps(., sga_bounds_dissolve_albers, sparse = FALSE)) %>%
  dplyr::filter(overlap == TRUE) %>%
  dplyr::select(-overlap)


# ---- 6. identify nc coastal watersheds ----
nc_coast_ws_bounds_albers <- ws_bounds_albers %>%
  dplyr::mutate(overlap = st_overlaps(., sga_bounds_dissolve_albers, sparse = FALSE)) %>%
  dplyr::filter(overlap == TRUE) %>%
  dplyr::select(-overlap)


# ---- 7. export data ----
# export nc coast county bounds
st_write(nc_coast_county_bounds_albers, paste0(spatial_data_output_path, "coast_bounds/nc_coast_county_bounds_albers.shp"), delete_layer = TRUE)

# export nc coast watershed bounds
st_write(nc_coast_ws_bounds_albers, paste0(spatial_data_output_path, "coast_bounds/nc_coast_ws_bounds_albers.shp"), delete_layer = TRUE)

