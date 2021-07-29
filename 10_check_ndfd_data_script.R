# ---- script header ----
# script name: 10_check_ndfd_data_script.R
# purpose of script: check (including evaluating using model metrics) ndfd data against obs data
# author:
# date created:
# email:


# ---- notes ----
# notes:


# ---- to do ----
# to do list: 


# ---- load libraries ----
library(tidyverse)
library(tidylog)
library(here)
library(lubridate)


# ---- define paths ----
# tabular data input path
obs_tabular_data_input_path <- here::here("data", "tabular", "obs_data_tidy")

# path to ndfd tabular inputs
ndfd_tabular_data_intput_path <- here::here("data", "tabular", "ndfd_data_tidy")


# ---- 4. define functions ----
# calculate nse
# as defined in Moriasi et al. 2007
calculate_nse <- function(obs_data, frcst_data) {
  # obs_data is a list of observed data
  # frcst_data is a list of forecased (or simulated or modeled) data
  
  # calculate mean of observed
  obs_mean <- mean(obs_data, na.rm = TRUE)
  
  # calculate numerator
  nse_top <- sum((obs_data - frcst_data)^2, na.rm = TRUE)
  
  # calculate denominator
  nse_bot <- sum((obs_data - obs_mean)^2, na.rm = TRUE)
  
  # if top and bottom are not zero
  if ((nse_top != 0) & (nse_bot != 0)) {
    
    # calculate nse
    nse <- round(1 - (nse_top/nse_bot), 3)
  }
  
  # perfect fit (usually for zero rainfall)
  else if ((nse_top == 0) & (nse_bot == 0)) {
    nse <- 1
  }
  
  # if -Inf b/c denominator is zero
  else if ((nse_top != 0) & (nse_bot == 0)) {
    # calculate nse but will give -Inf
    nse <- round(1 - (nse_top/nse_bot), 3)
  }
  
  # can't determine nse
  else {
    nse <- NA
  }
  
  # return output
  return(nse)
  # ranges from -Inf to 1 with 1 being perfect fit, values between 0 and 1 are acceptable
  # -Inf when nse_top = postive number and nse_bot = 0
  # Nan when nse_top and nse_bot are both = 0
}

# calculate the kappa coefficient
calculate_kappa <- function() {
  
  
}


# ---- load data ----
# observed daily data (averaged by cmu)
obs_avg_data <- read_csv(file = paste0(obs_tabular_data_input_path, "/obs_avg_data.csv"), col_names = TRUE)

# ndfd daily data (averaged by cmu)
ndfd_avg_data <- read_csv(file = paste0(ndfd_tabular_data_intput_path, "/ndfd_avg_data.csv"), col_names = TRUE)


# ---- join obs and ndfd data ----
# join data and filter for data in 2015-01-04 to 2016-12-31 (start on Jan 4 because of 72 hour window)
obs_ndfd_join_data <- ndfd_avg_data %>%
  dplyr::left_join(obs_avg_data, by = c("date", "cmu_name")) %>%
  dplyr::filter(date >= ymd("2015-01-04")) %>% # remove days where we don't have all valid periods
  dplyr::filter(is.na(obs_measurement_count) == FALSE) %>% # remove rows where there are no observations
  dplyr::select(date, valid_period_hrs, month_chr, month_num, month_type, 
                cmu_name, rain_depth_thresh_cm, cmu_pop_perc, cmu_qpf_cm, cmu_closure_perc, ndfd_measurement_count,
                obs_avg_cm, obs_measurement_count) # reorganize

# check unique cmus
# length(unique(obs_ndfd_join_data$cmu_name))
# 91 ok!

# check for na's
# sum(is.na(obs_ndfd_join_data$cmu_qpf_cm))
# sum(is.na(obs_ndfd_join_data$obs_avg_cm))
# 0 means no na's

# check that each cmu for each day has 3 observations (for 24, 48, and 72 hr valid periods)
valid_period_check <- obs_ndfd_join_data %>%
  ungroup() %>%
  group_by(date, cmu_name) %>%
  summarize(check_count = n())

# some do not have observations for all three valid periods
# so need to remove them
date_cmu_to_remove <- valid_period_check %>%
  dplyr::filter(check_count < 3) %>%
  dplyr::select(-check_count)

# number of days being removed
# length(unique(date_cmu_to_remove$date))
# 22

# final dataset
obs_ndfd_tidy_data <- obs_ndfd_join_data %>%
  anti_join(date_cmu_to_remove, by = c("date", "cmu_name"))

# check again
valid_period_recheck <- obs_ndfd_tidy_data %>%
  ungroup() %>%
  group_by(date, cmu_name) %>%
  summarize(check_count = n())

# unique(valid_period_recheck$check_count)
# only 3 is the answer ok!

# check unique cmus
# length(unique(obs_ndfd_tidy_data$cmu_name))
# 91 ok!

  
# ---- 

# all data facet by valid period
ggplot(data = obs_ndfd_tidy_data) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm)) +
  facet_wrap(~ as.factor(valid_period_hrs))

# 24 hr valid period
ggplot(data = obs_ndfd_tidy_data %>% filter(valid_period_hrs == 24)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm)) +
  facet_wrap(~ month_num)

# 48 hr valid period
ggplot(data = obs_ndfd_tidy_data %>% filter(valid_period_hrs == 48)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm)) +
  facet_wrap(~ month_num)

# 72 hr valid period
ggplot(data = obs_ndfd_tidy_data %>% filter(valid_period_hrs == 72)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm)) +
  facet_wrap(~ month_num)

# calculate seasonal nse by cmu
# calculate 
