# ---- script header ----
# script name: 04_obs_data_tabular_to_spatial_script.R
# purpose of script: takes all tabular historic records and converts them to spatial data
# author: sheila saia
# date created: 20201109
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:


# ---- to do ----
# to do list


# ---- 1. load libraries ----
library(tidyverse)
library(lubridate)
library(sf)
library(here)
# library(tidylog)


# ---- 2. define paths and projections ----
# tabular data input path
tabular_data_input_path <- here::here("data", "tabular", "obs_data_tidy")

# spatial data output path
spatial_data_output_path <- here::here("data", "spatial", "obs_data_tidy")

# define epsg for wgs 84
wgs84_epsg <- 4326

# define epsg and proj for CONUS Albers Equal Area projection (projecting to this)
conus_albers_epsg <- 5070
# conus_albers_proj <- "+init=EPSG:5070"


# ---- 3. load in data ----
# data
obs_precip_data <- read_csv(paste0(tabular_data_input_path, "/obs_data_compiled.csv"), col_names = TRUE, col_types = cols(.default = col_character()))

# metadata
obs_precip_metadata <- read_csv(paste0(tabular_data_input_path, "/obs_metadata_compiled.csv"), col_names = TRUE, col_types = cols(.default = col_character()))


# ---- check how complete ----
# number of days in study (i.e., 2 years --> 2015 and 2016 with 7 day buffer for 3-day rolling)
number_of_days <- length(unique(obs_precip_data$date))

# check how complete each station record is
obs_precip_data_completeness <- obs_precip_data %>%
  na.omit() %>% # if you don't add this NA cell will be counted in n()
  dplyr::group_by(loc_id) %>%
  dplyr::summarize(count = n(),
                   num_events = sum(precip_in != 0),
                   perc_rec = round((count/number_of_days) * 100, digits = 3), # number of missing days in 2015-2016 record
                   perc_evt = round((num_events/number_of_days) * 100, digits = 3)) %>% # number of non-zero events (precip_in > 0) in 2015-2016 period
  dplyr::select(loc_id, perc_rec, perc_evt)

# location sites that have no events
# find cmu's where there are no events (rainfall = 0 cm) for the full 2015-2016 period by valid period
# this is due to provisional data (see script 03_combine_obs_data_script.R for note on data QC scores)
# in some cases score was provisional but zeros were given for full 2015-2016 record

# join completeness with metadata
obs_precip_metadata_completeness <- obs_precip_metadata %>%
  dplyr::left_join(obs_precip_data_completeness, by = "loc_id") %>%
  dplyr::mutate(perc_rec = if_else(is.na(perc_rec), 0, perc_rec),
                perc_evt = if_else(is.na(perc_evt), 0, perc_evt))


# ---- make tabular data spatial ----
# convert to spatial data
obs_metadata_albers <- st_as_sf(obs_precip_metadata_completeness, coords = c("long", "lat"), crs = wgs84_epsg, dim = "XY") %>%
  st_transform(crs = conus_albers_epsg)

# check
head(obs_metadata_albers)


# ---- export ----
st_write(obs_metadata_albers, paste0(spatial_data_output_path, "/obs_metadata_albers.shp"), delete_layer = TRUE)

