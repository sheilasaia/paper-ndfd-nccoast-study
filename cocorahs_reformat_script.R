# cocorahs data reformatting script

# ---- 1. load libraries and set paths ----
library(tidyverse)
library(sf)

# paths
tabular_data_raw_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/cocorahs_raw/"


# ---- 2. read in data ----
# cocorahs data
cocorahs_data_raw <- read_csv(paste0(tabular_data_raw_path, "cocorahs_data_raw.csv"))


# ----- 3. pull out unique sites ----
# make list of unique station info
stations_data_tabular <- cocorahs_data_raw %>%
  distinct(station_id, station_name, lat, long)

# convert list to shp file

