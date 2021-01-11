# cocorahs data reformatting script

# ---- 1. load libraries ----
library(tidyverse)


# ---- 2. define paths ----
# tabular data paths
tabular_data_raw_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/cocorahs_raw/"

# output path
tabular_data_output_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/hist_precip_data/"


# ---- 3. read in data ----
# cocorahs data
cocorahs_data_raw <- read_csv(paste0(tabular_data_raw_path, "cocorahs_data_raw.csv"))


# ---- 4. separate out metadata ----
cocorahs_metadata <- cocorahs_data_raw %>%
  # change column names so they are compatible with NC SCO API outputs
  select(location_id = station_id, location_name = station_name, latitude_degrees_north = lat, longitude_degrees_east = long) %>%
  distinct_all() %>%
  # add columns so this is compatible with NC SCO API outputs
  mutate(network_type = "CoCoRaHS",
         city = NA, 
         county = NA, 
         state = NA, 
         elevation_ft_chr = NA, 
         supporting_agency_for_location = "CoCoRaHS",
         start_date_chr = NA,
         end_date_chr = NA,
         obtypes_available = NA) %>%
  select(location_id, network_type, location_name,city:state, latitude_degrees_north, longitude_degrees_east, elevation_ft_chr:obtypes_available)


# ---- 5. separate out data ----
cocorahs_data <- cocorahs_data_raw %>%
  select(location_id = station_id, datetime_chr_et = datetime_est, precip_in) %>%
  # add columns so this is compatible with NC SCO API outputs
  mutate(var = "precip",
         value = precip_in,
         unit = "in",
         score = NA,
         nettype = NA,
         vartype = NA,
         obtime = NA,
         obtype = NA,
         obnum = NA,
         value_accum = NA) %>%
  select(location_id, datetime_chr_et, var:value_accum)
  


# ---- 6. export data ----
write_csv(x = cocorahs_data, path = paste0(tabular_data_output_path, "cocorahs_data.csv"))
write_csv(x = cocorahs_metadata, path = paste0(tabular_data_output_path, "cocorahs_metadata.csv"))

