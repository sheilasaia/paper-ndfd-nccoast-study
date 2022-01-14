# ---- script header ----
# script name: 09_tidy_obs_ndfd_data_script.R
# purpose of script: this script tidies the obs and ndfd data sets, checks that they can be combined (but does not combine! see next script)
# author: sheila saia
# date created: 20210715
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:


# ---- to do ----
# to do list:
# TODO rerun ndfd data wrangling section after running non-event fix
# TODO move functions into separate file
# TODO fix function so it has checks for units and data types (decimal, etc.)


# ---- load libraries ----
library(tidyverse)
library(here)
library(lubridate)
library(sf)
# library(tidylog)


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
obs_metadata_albers_sel <- st_read(paste0(obs_spatial_data_input_path, "/obs_metadata_albers_sel.shp"))

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
obs_metadata <- obs_metadata_albers_sel %>%
  st_drop_geometry() %>%
  dplyr::select(loc_id, network, perc_rec, perc_evt, cmu_name:rain_lab)

# join metadata and observations
obs_data_metadata_join <- obs_data %>%
  dplyr::left_join(obs_metadata, by = "loc_id") %>%
  na.omit()

# check number of unique cmus
# length(unique(obs_data_metadata_join$cmu_name))
# 88 ok!

# look at perc_evt and perc_rec to make sure they are ok
# ok!

# make list of unique cmu's
cmu_info_unique <- obs_data_metadata_join %>%
  dplyr::select(loc_id, cmu_name) %>%
  dplyr::distinct()

# check unique cmu's
# length(unique(cmu_info_unique$cmu_name))
# 88 ok!

# take average by cmu and date
obs_avg_data <- obs_data_metadata_join %>%
  dplyr::select(loc_id, date, precip_in, cmu_name) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(date, cmu_name) %>%
  dplyr::summarize(obs_avg_in = round(mean(precip_in, na.rm = TRUE), 2), # calculate average precip
                   obs_avg_cm = round(obs_avg_in * 2.54, 2), # convert to SI units
                   obs_measurement_count = n()) %>% # keep track of the number of stations being summarized for each day
  dplyr::select(-obs_avg_in) %>% # drop English units
  dplyr::ungroup() %>%
  dplyr::filter((date >= as.Date("2015-01-01")) & (date <= as.Date("2016-12-31")))

# check number of unique cmus
# length(unique(obs_avg_data$cmu_name))
# 88 ok!

# cmu and rainfall threshold key
cmu_rain_thresh_key <- obs_metadata %>%
  dplyr::select(cmu_name, rain_in) %>%
  dplyr::mutate(rain_depth_thresh_cm = round(rain_in * 2.54, 2)) %>% # convert threshold to SI units
  dplyr::select(-rain_in) %>%
  dplyr::distinct()


# ---- wrangling ndfd data ----
# ndfd date key
ndfd_data_date_key <- ndfd_data %>%
  dplyr::select(ndfd_date, date, valid_period_hrs, cmu_name) %>%
  dplyr::distinct()

# check that each day has three observations (for 24, 48, and 72 hrs)
ndfd_data_date_check <- ndfd_data_date_key %>%
  dplyr::ungroup() %>%
  dplyr::group_by(date, cmu_name) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::filter(count == 3) %>%
  dplyr::select(date) %>%
  dplyr::distinct()

# make it a list
ndfd_data_date_check_list <- c(ndfd_data_date_check$date)
# these are the days that have all three valid periods available
# use this list to remove unwanted dates without enough data available

# calculate the mean 
ndfd_avg_data <- ndfd_data %>%
  dplyr::ungroup() %>%
  dplyr::filter(date %in% ndfd_data_date_check_list) %>%
  dplyr::filter((ndfd_date >= as.Date("2015-01-01")) & (ndfd_date <= as.Date("2016-12-31"))) %>%
  dplyr::group_by(date, valid_period_hrs, cmu_name) %>%
  # use mean to summarize here for both loc- and cmu-based calcs when there are more than one station in a cmu
  # averaging doesn't really do anything and is a means to ingnore the loc calcs
  # put another way - cmu_pop_avg_perc is repeated for each loc_id within the cmu and taking the average just gives the repeated value
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
                month_type = fct_relevel(month_type, "warm", "cool")) %>%
  dplyr::select(date, month_chr:month_type, valid_period_hrs, cmu_name, rain_depth_thresh_cm, cmu_pop_perc, cmu_qpf_cm, cmu_closure_perc, ndfd_measurement_count)

# check number of unique cmus
# length(unique(ndfd_avg_data$cmu_name))
# 88 ok!

# check number of unique days
# length(unique(ndfd_avg_data$date))
# 706 (706/731 = 96.58% complete)

# check again for all three valid period just to be sure 
ndfd_avg_data_date_check <- ndfd_avg_data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(date, cmu_name) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::filter(count != 3)
# zero long, so all have three valid periods ok!

# ndfd_measurement_count and obs_measurement_count are the same (do i need both?)


# ---- export data ----
write_csv(x = obs_avg_data, file = paste0(obs_tabular_data_output_path, "/obs_avg_data.csv"))
write_csv(x = obs_data_metadata_join, file = paste0(obs_tabular_data_output_path, "/obs_data_metadata_join.csv"))
write_csv(x = ndfd_avg_data, file = paste0(ndfd_tabular_data_output_path, "/ndfd_avg_data.csv"))
