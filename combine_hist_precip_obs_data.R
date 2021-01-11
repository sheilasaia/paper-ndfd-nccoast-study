
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


# ---- 2. define paths ----
# cocorahs tabular raw data path
tabular_cocorahs_input_data_raw_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/hist_precip_data/"

# sci api tabular raw data path
tabular_sco_api_input_data_raw_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sco_api_raw/"

# output path
tabular_compiled_output_data_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/hist_precip_data/"


# ---- 3. load data ----
# cocorahs
cocorahs_data_raw <- read_csv(paste0(tabular_cocorahs_input_data_raw_path, "cocorahs_data.csv"))
cocorahs_metadata_raw <- read_csv(paste0(tabular_cocorahs_input_data_raw_path, "cocorahs_metadata.csv"))

# sco api data
# asos
asos_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "asos_data_raw.csv"))
asos_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "asos_metadata_raw.csv"))

# awos
awos_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "awos_data_raw.csv"))
awos_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "awos_metadata_raw.csv"))

# coop
coop_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "coop_data_raw.csv"))
coop_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "coop_metadata_raw.csv"))

# econet
econet_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "econet_data_raw.csv"))
econet_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "econet_metadata_raw.csv"))

# nos
# nos_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "nos_data_raw.csv"))
# nos_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "nos_metadata_raw.csv"))

# raws-mw
rawsmw_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "raws-mw_data_raw.csv"))
rawsmw_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "raws-mw_metadata_raw.csv"))

# threadex
threadex_data_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "threadex_data_raw.csv"))
threadex_metadata_raw <- read_csv(paste0(tabular_sco_api_input_data_raw_path, "threadex_metadata_raw.csv"))


# ---- 4. check data dimensions ----
# check data dimensions
dim(cocorahs_data_raw)[2]
dim(asos_data_raw)[2]
dim(awos_data_raw)[2]
dim(coop_data_raw)[2]
dim(econet_data_raw)[2]
# dim(nos_data_raw)[2]
dim(rawsmw_data_raw)[2]
dim(threadex_data_raw)[2]

# some columns are missing, which ones?
names(cocorahs_data_raw)
names(asos_data_raw)
names(awos_data_raw)
# "obtime" is missing from asos, awos, coop, and econet
# i think this is b/c i downloaded these in sept 2020 and downloaded the others in jan 2021

# add obtime to asos, awos, coop, and econet data (for now b/c i don't want to redownload)
asos_data_raw_fix <- asos_data_raw %>%
  mutate(obtime = NA) %>%
  select(location_id:vartype, obtime, obtype:value_accum)
names(asos_data_raw_fix)
dim(asos_data_raw_fix)[2] # ok now

awos_data_raw_fix <- awos_data_raw %>%
  mutate(obtime = NA) %>%
  select(location_id:vartype, obtime, obtype:value_accum)
names(awos_data_raw_fix)
dim(awos_data_raw_fix)[2] # ok now

coop_data_raw_fix <- coop_data_raw %>%
  mutate(obtime = NA) %>%
  select(location_id:vartype, obtime, obtype:value_accum)
names(coop_data_raw_fix)
dim(coop_data_raw_fix)[2] # ok now

econet_data_raw_fix <- econet_data_raw %>%
  mutate(obtime = NA) %>%
  select(location_id:vartype, obtime, obtype:value_accum)
names(econet_data_raw_fix)
dim(econet_data_raw_fix)[2] # ok now

# check metadata

# ---- 5. compile all data ----

