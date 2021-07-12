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

# TODO going to need to compile all datasets here (not just cocorahs)


# ---- 1. load libraries ----
library(tidyverse)
library(lubridate)
library(sf)
library(tidylog)
library(here)


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
# check how complete each station record is
obs_precip_data_completeness <- obs_precip_data %>%
  na.omit() %>% # if you don't add this NA cell will be counted in n()
  group_by(loc_id) %>%
  summarize(count = n(),
            perc_compl = round((count/731) * 100, digits = 3)) %>%
  select(loc_id, perc_compl)

# join completeness with metadata
obs_precip_metadata_completeness <- obs_precip_metadata %>%
  left_join(obs_precip_data_completeness, by = "loc_id") %>%
  mutate(perc_compl = if_else(is.na(perc_compl), 0, perc_compl))


# ---- make tabular data spatial ----
# convert to spatial data
obs_precip_spatial_metadata <- st_as_sf(obs_precip_metadata_completeness, coords = c("long", "lat"), crs = wgs84_epsg, dim = "XY") %>%
  st_transform(crs = conus_albers_epsg)

# check
head(obs_precip_spatial_metadata)


# ---- export ----
st_write(obs_precip_spatial_metadata, paste0(spatial_data_output_path, "/obs_metadata_albers.shp"), delete_layer = TRUE)

