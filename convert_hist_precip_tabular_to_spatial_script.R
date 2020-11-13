
# ---- script header ----
# script name: historic_precip_tabular_to_spatial_script.R
# purpose of script: takes all tabular historic records and converts them to spatial data
# author: sheila saia
# date created: 20201109
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:


# ---- to do ----
# to do list

# TODO going to need to compile all datasets here (not just cocorahs)
# 


# ---- 1. load libraries ----
library(tidyverse)
library(lubridate)
library(sf)


# ---- 2. define paths and projections ----

# historic precip tabular data path
tabular_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/hist_precip_data/"

# histric precip spatial data path
spatial_data_output_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/hist_precip_data/"

# define epsg for wgs 84
wgs84_epsg <- 4326

# define epsg and proj for CONUS Albers projection (projecting to this)
conus_albers_epsg <- 5070
# conus_albers_proj <- "+init=EPSG:5070"


# ---- 3. load in data ----
# cocorahs data and metadata
cocorahs_data <- read_csv(paste0(tabular_data_input_path, "cocorahs_data.csv"), col_names = TRUE)
cocorahs_metadata <- read_csv(paste0(tabular_data_input_path, "cocorahs_metadata.csv"), col_names = TRUE)


# ---- make tabular data spatial ----
# shorten column names for shp files (10 characters max)
# data
cocorahs_data_rename <- cocorahs_data %>%
  transmute(loc_id = location_id, 
            date_et = as.character(mdy_hm(str_trim(datetime_chr_et, side = "both"), tz = "America/New_York")), 
            precip_in)

# metadata
cocorahs_metadata_rename <- cocorahs_metadata %>%
  transmute(loc_id = location_id,
            loc_name = location_name,
            network = network_type,
            state = "NC",
            lat = latitude_degrees_north,
            long = longitude_degrees_east,
            elev_ft = as.numeric(elevation_ft_chr),
            start_date = as.character(mdy(start_date_chr, tz = "America/New_York")),
            end_date = as.character(mdy(end_date_chr, tz = "America/New_York")),
            obs_types = obtypes_available)

# convert to spatial data
cocorahs_spatial_metadata <- st_as_sf(cocorahs_metadata_rename, coords = c("long", "lat"), crs = wgs84_epsg, dim = "XY") %>%
  st_transform(crs = conus_albers_epsg)

# check
head(cocorahs_spatial_metadata)


# ---- export ----
st_write(cocorahs_spatial_metadata, paste0(spatial_data_output_path, "hist_precip_metadata_albers.shp"), delete_layer = TRUE)

write_csv(cocorahs_data_rename, paste0(tabular_data_input_path, "hist_precip_data_compiled.csv"))
