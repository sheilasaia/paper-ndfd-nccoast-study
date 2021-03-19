
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
library(tidylog)


# ---- 2. define paths and projections ----
# historic precip tabular data path
tabular_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/hist_precip_data/"
# tabular_data_input_path <- "/Users/sheila/Desktop/transfer/shellcast_analysis/data/tabular/sheila_generated/hist_precip_data/"

# histric precip spatial data path
spatial_data_output_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/hist_precip_data/"
# spatial_data_output_path <- "/Users/sheila/Desktop/transfer/shellcast_analysis/data/spatial/sheila_generated/hist_precip_data/"

# define epsg for wgs 84
wgs84_epsg <- 4326

# define epsg and proj for CONUS Albers projection (projecting to this)
conus_albers_epsg <- 5070
# conus_albers_proj <- "+init=EPSG:5070"


# ---- 3. load in data ----
# data
hist_precip_data <- read_csv(paste0(tabular_data_input_path, "hist_precip_data_compiled.csv"), col_names = TRUE, col_types = cols(.default = col_character()))

# metadata
hist_precip_metadata <- read_csv(paste0(tabular_data_input_path, "hist_precip_metadata_compiled.csv"), col_names = TRUE, col_types = cols(.default = col_character()))


# ---- check how complete ----
# check how complete each station record is
hist_precip_data_completeness <- hist_precip_data %>%
  group_by(loc_id) %>%
  summarize(count = n(),
            perc_compl = round((count/731) * 100, digits = 3)) %>%
  select(loc_id, perc_compl) # %>%
# filter(perc_complete >= 95)
# 1780 total stations and 779 are 95% or more complete

# join completeness with metadata
hist_precip_metadata_complete <- hist_precip_metadata %>%
  left_join(hist_precip_data_completeness, by = "loc_id")

# ---- make tabular data spatial ----
# convert to spatial data
hist_precip_spatial_metadata <- st_as_sf(hist_precip_metadata_complete, coords = c("long", "lat"), crs = wgs84_epsg, dim = "XY") %>%
  st_transform(crs = conus_albers_epsg)

# check
head(hist_precip_spatial_metadata)


# ---- export ----
st_write(hist_precip_spatial_metadata, paste0(spatial_data_output_path, "hist_precip_metadata_albers.shp"), delete_layer = TRUE)



# ---- old code ----
# cocorahs data and metadata
# cocorahs_data <- read_csv(paste0(tabular_data_input_path, "cocorahs_data.csv"), col_names = TRUE)
# cocorahs_metadata <- read_csv(paste0(tabular_data_input_path, "cocorahs_metadata.csv"), col_names = TRUE)

# shorten column names for shp files (10 characters max)
# data
# cocorahs_data_rename <- cocorahs_data %>%
#   transmute(loc_id = location_id, 
#             date_et = as.character(mdy_hm(str_trim(datetime_chr_et, side = "both"), tz = "America/New_York")), 
#             precip_in)

# metadata
# cocorahs_metadata_rename <- cocorahs_metadata %>%
#   transmute(loc_id = location_id,
#             loc_name = location_name,
#             network = network_type,
#             state = "NC",
#             lat = latitude_degrees_north,
#             long = longitude_degrees_east,
#             elev_ft = as.numeric(elevation_ft_chr),
#             start_date = as.character(mdy(start_date_chr, tz = "America/New_York")),
#             end_date = as.character(mdy(end_date_chr, tz = "America/New_York")),
#             obs_types = obtypes_available)

# convert to spatial data
# cocorahs_spatial_metadata <- st_as_sf(cocorahs_metadata_rename, coords = c("long", "lat"), crs = wgs84_epsg, dim = "XY") %>%
#   st_transform(crs = conus_albers_epsg)

# check
# head(cocorahs_spatial_metadata)

# export
# write_csv(cocorahs_data_rename, paste0(tabular_data_input_path, "hist_precip_data_compiled.csv"))

