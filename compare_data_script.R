
# ---- script header ----
# script name: compare_data_script.R
# purpose of script: compare point and grid data
# author: sheila
# date created: 20200917
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:
 

# ---- to do ----
# to do list


# ---- 1. load libraries and set paths ----
library(tidyverse)
library(sf)
library(raster)


# historic tabular data path
hist_precip_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/hist_precip_data/"


# historic precip data
hist_precip_data <- read_csv(paste0(hist_precip_data_input_path, "hist_precip_data_compiled.csv"))


# ---- 5. join metadata and data ----
# join precip data to select coastal data
hist_precip_coast_data <- hist_precip_metadata_coast_albers %>%
  st_drop_geometry() %>%
  dplyr::left_join(hist_precip_data, by = "loc_id") %>%
  dplyr::select(loc_id, cmb_class, date_et, precip_in) %>%
  dplyr::mutate(year = year(date_et),
                month = str_pad(as.character(month(date_et)), width = 2, side = "left", pad = "0"),
                day = str_pad(as.character(day(date_et)), width = 2, side = "left", pad = "0"),
                month_year = paste0(month, "_", year),
                date_ymd = paste0(year, "-", month, "-", day))

# daily sum per station
daily_sum_hist_precip_coast_data <- hist_precip_coast_data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(loc_id, date_ymd, cmb_class) %>%
  dplyr::summarise(precip_in = sum(precip_in, na.rm = TRUE))

# monthly avg summaries
monthly_avg_hist_precip_coast_data <- hist_precip_coast_data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(loc_id, month, cmb_class) %>%
  dplyr::summarise(monthly_avg_precip_in = mean(precip_in, na.rm = TRUE))

# monthly total summaries
monthly_sum_hist_precip_coast_data <- hist_precip_coast_data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(loc_id, month, cmb_class) %>%
  dplyr::summarise(monthly_sum_precip_in = sum(precip_in, na.rm = TRUE))



# ---- 6. plot joined rainfall data ----

# plot monthly average per station
ggplot(data = monthly_avg_hist_precip_coast_data) +
  geom_boxplot(aes(x = month, y = monthly_avg_precip_in, color = cmb_class), alpha = 0.5) +
  xlab("Month") +
  ylab("Monthly Average Precipitation from 2015 and 2016 (in)") + 
  theme_bw() +
  theme(text = element_text(size = 14))

# plot monthly sum per station
ggplot(data = monthly_sum_hist_precip_coast_data) +
  geom_boxplot(aes(x = month, y = monthly_sum_precip_in, color = cmb_class)) +
  xlab("Month") +
  ylab("Monthly Total Precipitation from 2015 and 2016 (in)") + 
  theme_bw() +
  theme(text = element_text(size = 14))



# ---- 8. compare results -----
obs_data <- daily_sum_hist_precip_coast_data %>%
  dplyr::select(loc_id, cmb_class, date_ymd, precip_in) %>%
  dplyr::filter(date_ymd == "2015-09-01") %>%
  dplyr::ungroup() %>%
  dplyr::select(-date_ymd)

frcst_data <- ndfd_pts_calcs_data %>%
  dplyr::mutate(date_ymd = as.character(datetime_uct)) %>%
  dplyr::select(loc_id, date_ymd, valid_period_hrs, precip_frcst_in)

compare_data <- left_join(obs_data, frcst_data, by = "loc_id") %>%
  dplyr::select(loc_id, date_ymd, valid_period_hrs, cmb_class, precip_in, precip_frcst_in) %>%
  dplyr::mutate(precip_frcst_in_check = precip_frcst_in * (1000) * (1/100) * (2.54))

ggplot(data = compare_data) +
  geom_point(aes(x = precip_frcst_in, y = precip_in, color = cmb_class), alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0) +
  #xlim(0, 3) +
  #ylim(0, 3) + 
  facet_wrap(~cmb_class) +
  xlab("Forecasted QPF (converted units) x POP12") +
  ylab("Observed Precip (in)") + 
  theme_bw() +
  theme(text = element_text(size = 14))

# with adjusted units
ggplot(data = compare_data) +
  geom_point(aes(x = precip_frcst_in_check, y = precip_in, color = cmb_class), alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0) +
  xlim(0, 8) +
  ylim(0, 8) + 
  facet_wrap(~cmb_class) +
  xlab("Forecasted QPF (original units) x POP12") +
  ylab("Observed Precip (in)") + 
  theme_bw() +
  theme(text = element_text(size = 14))

