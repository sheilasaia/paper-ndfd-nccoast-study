
# ---- script header ----
# script name: combine_hist_precip_obs_data.R
# purpose of script: combine all historic point observations of precip
# author: sheila saia
# date created: 20210111
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:
 

# ---- to do ----
# to do list


# ---- 1. load libraries ----
library(tidyverse)
library(lubridate)
library(tidylog)


# ---- 2. define paths ----
# cocorahs tabular raw data path
tabular_cocorahs_input_data_raw_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/hist_precip_data/"

# sci api tabular raw data path
tabular_sco_api_input_data_raw_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sco_api_raw/"

# output path
tabular_compiled_output_data_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/hist_precip_data/"


# ---- 3. load data ----
# cocorahs
cocorahs_data_raw <- read_csv(paste0(tabular_cocorahs_input_data_raw_path, "cocorahs_data.csv"), col_types = cols(.default = col_character()))
cocorahs_metadata_raw <- read_csv(paste0(tabular_cocorahs_input_data_raw_path, "cocorahs_metadata.csv"), col_types = cols(.default = col_character()))

# sco api data
# NOTE: All columns are in the character format!
# asos
asos_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "asos_data_raw.csv"), col_types = cols(.default = col_character()))
asos_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "asos_metadata_raw.csv"), col_types = cols(.default = col_character()))

# awos
awos_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "awos_data_raw.csv"), col_types = cols(.default = col_character()))
awos_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "awos_metadata_raw.csv"), col_types = cols(.default = col_character()))

# coop
coop_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "coop_data_raw.csv"), col_types = cols(.default = col_character()))
coop_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "coop_metadata_raw.csv"), col_types = cols(.default = col_character()))

# econet
econet_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "econet_data_raw.csv"), col_types = cols(.default = col_character()))
econet_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "econet_metadata_raw.csv"), col_types = cols(.default = col_character()))

# ncsu
ncsu_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "ncsu_data_raw.csv"), col_types = cols(.default = col_character()))
ncsu_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "ncsu_metadata_raw.csv"), col_types = cols(.default = col_character()))

# nos
nos_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "nos_data_raw.csv"), col_types = cols(.default = col_character()))
nos_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "nos_metadata_raw.csv"), col_types = cols(.default = col_character()))

# raws-mw
rawsmw_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "raws-mw_data_raw.csv"), col_types = cols(.default = col_character()))
rawsmw_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "raws-mw_metadata_raw.csv"), col_types = cols(.default = col_character()))

# threadex
threadex_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "threadex_data_raw.csv"), col_types = cols(.default = col_character()))
threadex_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "threadex_metadata_raw.csv"), col_types = cols(.default = col_character()))


# ---- 4. check data dimensions ----
# check data dimensions
dim(cocorahs_data_raw)[2]
dim(asos_data_raw)[2]
dim(awos_data_raw)[2]
dim(coop_data_raw)[2]
dim(econet_data_raw)[2]
dim(ncsu_data_raw)[2]
dim(nos_data_raw)[2]
dim(rawsmw_data_raw)[2]
dim(threadex_data_raw)[2]
# all are 12 columns! check.

# check metadata dimensions
dim(cocorahs_metadata_raw)[2]
dim(asos_metadata_raw)[2]
dim(awos_metadata_raw)[2]
dim(coop_metadata_raw)[2]
dim(econet_metadata_raw)[2]
dim(ncsu_metadata_raw)[2]
dim(nos_metadata_raw)[2]
dim(rawsmw_metadata_raw)[2]
dim(threadex_metadata_raw)[2]
# all are 14 columns! check.

# check column names order for data
names(cocorahs_data_raw)
names(asos_data_raw)
names(awos_data_raw)
names(coop_data_raw)
names(econet_data_raw)
names(ncsu_data_raw)
names(nos_data_raw)
names(rawsmw_data_raw)
names(threadex_data_raw)
# all are the same! check.

# check column names order for metadata
names(cocorahs_metadata_raw)
names(asos_metadata_raw)
names(awos_metadata_raw)
names(coop_metadata_raw)
names(econet_metadata_raw)
names(ncsu_metadata_raw)
names(nos_metadata_raw)
names(rawsmw_metadata_raw)
names(threadex_metadata_raw)
# all are the same! check.


# ---- 5. compile all data ----
# compile and tidy up data
hist_precip_data_compiled <- rbind(cocorahs_data_raw,
                                   asos_data_raw,
                                   awos_data_raw,
                                   coop_data_raw,
                                   econet_data_raw,
                                   ncsu_data_raw,
                                   nos_data_raw,
                                   rawsmw_data_raw,
                                   threadex_data_raw) %>%
  mutate(date_et = ymd(str_sub(datetime_et, start = 1, end = 10)),
         precip_in = as.numeric(value)) %>% #,
         #precip_month_acc_in = as.numeric(value_accum)) %>%
  select(loc_id = location_id, date_et, precip_in) %>% # , precip_month_acc_in) %>%
  arrange(loc_id, date_et) # %>%
  # na.omit()

#compile and tidy up metadata
hist_precip_metadata_compiled <- rbind(cocorahs_metadata_raw,
                                       asos_metadata_raw,
                                       awos_metadata_raw,
                                       coop_metadata_raw,
                                       econet_metadata_raw,
                                       ncsu_metadata_raw,
                                       nos_metadata_raw,
                                       rawsmw_metadata_raw,
                                       threadex_metadata_raw) %>%
  mutate(loc_id = location_id,
         long = as.numeric(longitude_degrees_east),
         lat = as.numeric(latitude_degrees_north),
         network = network_type,
         cosponsor = supporting_agency_for_location,
         elev_ft = as.numeric(elevation_feet),
         start_date = ymd(start_date),
         end_date = ymd(end_date),
         obs_types = obtypes_available) %>%
  select(loc_id, long, lat, network, city, county, state, elev_ft, start_date, end_date, cosponsor, obs_types) %>%
  arrange(network, city)


# ---- 6. export data ----
write_csv(x = hist_precip_data_compiled, path = paste0(tabular_compiled_output_data_path, "hist_precip_data_compiled.csv"))
write_csv(x = hist_precip_metadata_compiled, path = paste0(tabular_compiled_output_data_path, "hist_precip_metadata_compiled.csv"))


