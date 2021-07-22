# ---- script header ----
# script name: 09_tidy_obs_ndfd_data_script.R
# purpose of script: this script combines the observed and ndfd (forecasted) datasets into one
# author: sheila saia
# date created: 20210715
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:


# ---- to do ----
# to do list:
# TODO move functions into separate file
# TODO fix function so it has checks for units and data types (decimal, etc.)

# ---- load libraries ----
library(tidyverse)
library(tidylog)
library(here)
library(lubridate)
library(sf)


# ---- define paths ----
# tabular data input path
obs_tabular_data_input_path <- here::here("data", "tabular", "obs_data_tidy")

# path to observed spatial inputs
obs_spatial_data_input_path <- here::here("data", "spatial", "obs_data_tidy")

# path to observed spatial outputs
obs_tabular_data_output_path <- here::here("data", "tabular", "obs_data_tidy")

# path to ndfd tabular inputs
ndfd_tabular_data_intput_path <- here::here("data", "tabular", "ndfd_data_tidy")

# path to ndfd tabular outputs
ndfd_tabular_data_output_path <- here::here("data", "tabular", "ndfd_data_tidy")

# ---- load data ----
# observed data
obs_data <- read_csv(file = paste0(obs_tabular_data_input_path, "/obs_data_compiled.csv"),
                     col_types = list(col_character(), col_date(), col_number()))

# observed metadata
obs_metadata_shp <- st_read(paste0(obs_spatial_data_input_path, "/obs_metadata_albers_sel.shp"))

# ndfd (foretasted) data
ndfd_data <- read_csv(file = paste0(ndfd_tabular_data_intput_path, "/ndfd_data_sel.csv"))


# ---- load functions ----
# calculate the probability of closure (as a decimal)
calc_closure_perc <- function(rain_thresh_depth, qpf, pop_notdecimal) {
  # add notes about units needing to be the same for rain_thresh_depth and qpf
  # add checks for notdecimal/decimal
  cloure_perc <- round(pop_notdecimal * exp((-rain_thresh_depth/qpf)), 2) # percent closure as decimal percent
  return(cloure_perc)
}

# ---- wrangling observed data ----
# drop geometry and save only columns that are needed 
obs_metadata <- obs_metadata_shp %>%
  st_drop_geometry() %>%
  dplyr::select(loc_id, network, perc_compl, cmu_name:rain_lab)

# join metadata and observations
obs_data_metadata_join <- obs_data %>%
  dplyr::left_join(obs_metadata, by = "loc_id") %>%
  na.omit()

# check number of unique cmus
# length(unique(obs_data_metadata_join$cmu_name))
# 91 ok!

# take average by cmu and date
obs_avg_data <- obs_data_metadata_join %>%
  dplyr::select(loc_id, date, precip_in, cmu_name) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(date, cmu_name) %>%
  dplyr::summarize(obs_avg_in = round(mean(precip_in, na.rm = TRUE), 2), # calculate average precip
                   obs_avg_cm = round(obs_avg_in * 2.54, 2), # convert to SI units
                   obs_measurement_count = n()) %>% # keep track of the number of stations being summarized for each day
  dplyr::select(-obs_avg_in) # drop English units

# check number of unique cmus
# length(unique(obs_avg_data$cmu_name))
# 91 ok!

# cmu and rainfall threshold key
cmu_rain_thresh_key <- obs_metadata %>%
  dplyr::select(cmu_name, rain_in) %>%
  dplyr::mutate(rain_depth_thresh_cm = round(rain_in * 2.54, 2)) %>% # convert threshold to SI units
  dplyr::select(-rain_in) %>%
  dplyr::distinct()


# ---- wrangling ndfd data ----
# calculate the mean 
ndfd_avg_data <- ndfd_data %>%
  dplyr::filter((date >= as.Date("2015-01-03")) & (date <= as.Date("2016-12-31"))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(date, valid_period_hrs, cmu_name) %>%
  # use mean to summarize here for both loc- and cmu-based calcs when there are more than one station in a cmu
  # loc calcs can be different but cmu calcs will be the same, mean will take care of both
  dplyr::summarize(# loc_pop_perc = round(mean(loc_pop_perc, na.rm = TRUE), 2),
                   # loc_qpf_in = round(mean(loc_qpf_in, na.rm = TRUE), 2),
                   cmu_pop_perc = round(mean(cmu_pop_avg_perc, na.rm = TRUE), 2),
                   cmu_qpf_cm = round(mean(cmu_qpf_in, na.rm = TRUE) * 2.54, 2),
                   ndfd_measurement_count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(cmu_rain_thresh_key, by = "cmu_name") %>% # need to rejoin
  dplyr::mutate(# loc_closure_perc = calc_closure_perc(rain_thresh_in = rain_in, qpf_in = loc_qpf_in, pop_notdecimal = loc_pop_perc),
                cmu_closure_perc = calc_closure_perc(rain_thresh_depth = rain_depth_thresh_cm, qpf = cmu_qpf_cm, pop_notdecimal = cmu_pop_perc)) %>%
  dplyr::mutate(month_chr = fct_relevel(as.character(month(date, label = TRUE)), c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                month_num = as.numeric(month(date)),
                month_type = case_when(month_num <= 3 | month_num >= 10 ~ "cool",
                                       month_num > 4 | month_num < 10 ~ "warm"),
                month_type = fct_relevel(month_type, "warm", "cool"))


# ---- export data ----
write_csv(x = obs_avg_data, file = paste0(obs_tabular_data_output_path, "/obs_avg_data.csv"))
write_csv(x = ndfd_avg_data, file = paste0(ndfd_tabular_data_output_path, "/ndfd_avg_data.csv"))
