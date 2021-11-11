# ---- script header ----
# script name: 11_analysis_script.R
# purpose of script: combines ndfd and obs data and do paper analysis
# author:
# date created:
# email:


# ---- notes ----
# notes:

# paletes: http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html
# colors in R: https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf


# ---- to do ----
# to do list: 
# TODO roc analysis plots (see analysis_roc_script.R for code)
# TODO don't really need obs_metadata_compiled.csv here --> can just use obs_data_compiled.csv


# ---- load libraries ----
library(tidyverse)
library(here)
library(lubridate)
library(sf)
library(RColorBrewer)
library(forcats)
library(cutpointr)
library(broom)
# library(tidylog)


# ---- define paths ----
# obs tabular data input path
obs_tabular_data_input_path <- here::here("data", "tabular", "obs_data_tidy")

# obs spatial data input path
obs_spatial_data_input_path <- here::here("data", "spatial", "obs_data_tidy")

# path to ndfd tabular inputs
ndfd_tabular_data_input_path <- here::here("data", "tabular", "ndfd_data_tidy")

# path to normals data input path
normals_tabular_data_input_path <- here::here("data", "tabular", "normals_data_tidy")
# 142.91 cm is annual normal value for these sites

# path to roc tabular outputs
roc_tabular_data_output_path <- here::here("data", "tabular", "roc_data")

# path to nc state bounds spatial data
nc_spatial_data_input_path <- here::here("data", "spatial", "region_state_bounds_tidy")

# path to ncdmf spatial data
ncdmf_spatial_data_input_path <- here::here("data", "spatial", "ncdmf_data_tidy")

# figure path
figure_output_path <- here::here("figures")


# ---- define functions ----
# calculate the probability of closure (as a decimal)
calc_closure_perc <- function(rain_thresh, qpf, pop_notdecimal) {
  # rain_thresh and qpf have to be in the same units!
  closure_perc <- round(pop_notdecimal * exp((-rain_thresh/qpf)), 2) # percent closure as decimal percent
  return(closure_perc)
}

# calculate rmse
calculate_rmse <- function(obs_data, frcst_data) {
  num_obs <- length(obs_data)
  rmse_inner <- sum((frcst_data - obs_data)^2) / num_obs
  rmse <- round(sqrt(rmse_inner), 3)
  
  return(rmse)
}

#calculate r squared
calculate_r2 <- function(obs_data, frcst_data) {
  obs_frcst_lm <- lm(obs_data ~ frcst_data)
  obs_frcst_lm_glance <- broom::glance(obs_frcst_lm)
  r2 <- round(as.numeric(obs_frcst_lm_glance$r.squared), 3)
  return(r2)
}

#calculate p value
calculate_pval <- function(obs_data, frcst_data) {
  obs_frcst_lm <- lm(obs_data ~ frcst_data)
  obs_frcst_lm_glance <- broom::glance(obs_frcst_lm)
  # obs_frcst_lm_tidy <- broom::tidy(obs_frcst_lm)
  pval <- round(as.numeric(obs_frcst_lm_glance$p.value), 3)
  # pval <- round(as.numeric(obs_frcst_lm_tidy$p.value[1]), 3)
  return(pval)
}

# calculate slope
calculate_slope <- function(obs_data, frcst_data) {
  obs_frcst_lm <- lm(obs_data ~ frcst_data)
  obs_frcst_lm_tidy <- broom::tidy(obs_frcst_lm)
  slope <- round(obs_frcst_lm_tidy$estimate[2], 3)
  return(slope)
}

# calculate intercept
calculate_intercept <- function(obs_data, frcst_data) {
  obs_frcst_lm <- lm(obs_data ~ frcst_data)
  obs_frcst_lm_tidy <- broom::tidy(obs_frcst_lm)
  intercept <- round(obs_frcst_lm_tidy$estimate[2], 3)
  return(intercept)
}

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
# calculate_kappa <- function() {
# }


# ---- load data ----
# observed daily data (averaged by cmu)
obs_avg_data <- read_csv(file = paste0(obs_tabular_data_input_path, "/obs_avg_data.csv"), col_names = TRUE)

# obs daily (all) and metadata joined
obs_data_metadata_join <- read_csv(file = paste0(obs_tabular_data_input_path, "/obs_data_metadata_join.csv"), col_names = TRUE)

# normals data
normals_month_precip_summary <- read_csv(file = paste0(normals_tabular_data_input_path, "/normals_month_precip_summary.csv"), col_names = TRUE)

# obs daily metadata shapefile
obs_metadata_shp <- st_read(paste0(obs_spatial_data_input_path, "/obs_metadata_albers_sel.shp"))

# ndfd daily data (averaged by cmu)
ndfd_avg_data <- read_csv(file = paste0(ndfd_tabular_data_input_path, "/ndfd_avg_data.csv"), col_names = TRUE)

# nc state bounds
nc_bounds_shp <- st_read(paste0(nc_spatial_data_input_path, "/nc_bounds_albers.shp"))

# cmu bounds
cmu_bounds_shp <- st_read(paste0(ncdmf_spatial_data_input_path, "/cmu_bounds_albers.shp"))


# ---- join obs and ndfd data ----
# join data and filter for data in 2015-01-04 to 2016-12-31 (start on Jan 4 because of 72 hour window)
obs_ndfd_data <- ndfd_avg_data %>%
  dplyr::left_join(obs_avg_data, by = c("date", "cmu_name")) %>%
  dplyr::filter(is.na(obs_measurement_count) == FALSE) %>% # remove rows where there are no observations
  dplyr::mutate(year = year(date),
                precip_binary = if_else(obs_avg_cm >= rain_depth_thresh_cm, 1, 0))

# check unique cmus
# length(unique(obs_ndfd_data$cmu_name))
# 88 ok!

# check for na's
# sum(is.na(obs_ndfd_data$cmu_qpf_cm))
# sum(is.na(obs_ndfd_data$obs_avg_cm))
# 0 means no na's ok!

# check that each cmu for each day has 3 observations (for 24, 48, and 72 hr valid periods)
valid_period_check <- obs_ndfd_data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(date, cmu_name) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::filter(count != 3)
# zero long, so all have three valid periods ok!

# check that cmu's have at least some events
# this was an issue, see script 04_convert_obs_data_to_spatial_script.R and
# script 08_ndfd_cmu_calcs_script.R for more info
event_check <- obs_ndfd_data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(cmu_name, valid_period_hrs) %>%
  dplyr::summarize(num_events = sum(obs_avg_cm == 0))

# check unique cmu's
# length(unique(event_check$cmu_name))
# 88 ok!


# ---- confusion matrix analysis ----
# compare data and define event/non-event and correct/not correct
compare_events_data <- obs_ndfd_data %>%
  dplyr::select(date, year, month_num, month_type, cmu_name, valid_period_hrs, rain_depth_thresh_cm, cmu_qpf_cm, cmu_closure_perc, obs_avg_cm, precip_binary) %>%
  dplyr::mutate(event_type = case_when(obs_avg_cm > 0 & cmu_qpf_cm > 0 ~ "correct_event",
                                       obs_avg_cm == 0 & cmu_qpf_cm == 0 ~ "correct_no-event",
                                       obs_avg_cm > 0 & cmu_qpf_cm == 0 ~ "incorrect_event",
                                       obs_avg_cm == 0 & cmu_qpf_cm > 0 ~ "incorrect_no-event",
                                       obs_avg_cm >= 0 & is.na(cmu_qpf_cm) == TRUE ~ "no_forecast"))

# find the number of days that don't have a forecast
length(unique(compare_events_data$date[compare_events_data$event_type == "no_forecast"]))
# 0 since took all these out beforehand

# max number of days per month key
month_seq = rep(seq(1,12,1), 2)
year_seq = c(rep(2015, 12), rep(2016, 12))
max_days_per_month_key <- data.frame(month = month_seq, year = year_seq) %>%
  dplyr::mutate(year_month = paste0(as.character(year), "-", as.character(month)),
                max_num_days = as.numeric(days_in_month(ym(year_month)))) %>%
  dplyr::select(-year, -month)

# count number of observations per month for each station
station_monthly_obs_count <- compare_events_data %>%
  dplyr::filter(event_type != "no_forecast") %>% # there are none but leaving this in for now
  dplyr::select(date, cmu_name, month_num, valid_period_hrs) %>%
  dplyr::mutate(year_month = paste0(as.character(year(date)), "-", as.character(month_num))) %>% # recreate this
  dplyr::ungroup() %>%
  dplyr::group_by(cmu_name, year_month, valid_period_hrs) %>%
  dplyr::summarize(num_days_available = n()) %>%
  # dplyr::left_join(max_days_per_month_key, by = "year_month") %>%
  # dplyr::mutate(percent_complete = num_days_available/max_num_days)
  dplyr::select(cmu_name, year_month, valid_period_hrs, num_days_available)

# count number of stations in different event statuses per month
station_event_type_monthly_summary <- compare_events_data %>%
  dplyr::filter(event_type != "no_forecast") %>% # there are none but leaving this in for now
  dplyr::mutate(year_month = paste0(as.character(year(date)), "-", as.character(month_num))) %>% # recreate this
  dplyr::ungroup() %>%
  dplyr::group_by(cmu_name, year_month, valid_period_hrs, event_type) %>%
  dplyr::summarize(num_days = n()) %>%
  dplyr::left_join(station_monthly_obs_count, by = c("cmu_name", "year_month", "valid_period_hrs")) %>%
  dplyr::mutate(perc_month = round((num_days/num_days_available) * 100, 2),
                year = as.numeric(str_sub(string = year_month, start = 1, end = 4)),
                month = as.numeric(str_sub(string = year_month, start = 6, end = -1)))


# ---- roc analysis ----
# make an empty dataframe
roc_calcs_data <- NULL

# make list of unique cmu's
cmu_info_unique <- compare_events_data %>%
  dplyr::select(cmu_name) %>%
  dplyr::distinct()

# number of cmus
num_cmus <- length(cmu_info_unique$cmu_name)

# number of valid periods
num_valid_periods <- length(unique(compare_events_data$valid_period_hrs))

# valid period values
valid_period_list <- unique(compare_events_data$valid_period_hrs)

# run number (counter for row id)
run_num = 0

# number of bootstrap runs
num_boot_runs = 500

# record start time
start_time <- now()

# loop
for (i in 1:num_cmus) { # i = cmu_name
  # pick cmu
  temp_cmu <- cmu_info_unique$cmu_name[i]
  
  # save cmu info
  temp_cmu_info_unique <- cmu_info_unique %>%
    filter(cmu_name == temp_cmu)
  
  for (j in 1:num_valid_periods) { # j = valid_period_hr
    # pick valid period
    temp_valid_period <- valid_period_list[j]
    
    # filter data
    temp_roc_data <- compare_events_data %>%
      dplyr::filter(cmu_name == temp_cmu & valid_period_hrs == temp_valid_period)
    
    # bootstrapped cutpoint results without season subgroup
    # accuracy metric
    set.seed(100)
    temp_result_no_sub_acc_raw <- cutpointr::cutpointr(data = temp_roc_data, x = cmu_closure_perc, class = precip_binary, 
                                                       direction = ">=", pos_class = 1, neg_class = 0, 
                                                       boot_runs = num_boot_runs, boot_stratify = TRUE,
                                                       method = maximize_metric, metric = accuracy, silent = TRUE)
    temp_result_no_sub_acc_summary <- summary(temp_result_no_sub_acc_raw) %>%
      dplyr::select(n_obs:n_neg)
    temp_result_no_sub_acc <- temp_result_no_sub_acc_raw %>%
      dplyr::mutate(subgroup = "none") %>%
      dplyr::select(subgroup, direction:boot) %>%
      dplyr::mutate(metric = "accuracy",
                    metric_value = accuracy,
                    cmu_name = temp_cmu,
                    valid_period_hrs = temp_valid_period,
                    n_obs = temp_result_no_sub_acc_summary$n_obs,
                    n_pos = temp_result_no_sub_acc_summary$n_pos,
                    n_neg = temp_result_no_sub_acc_summary$n_neg) %>%
      dplyr::select(- accuracy)
    
    # cohen's kappa metric
    set.seed(100)
    temp_result_no_sub_cohens_raw <- cutpointr::cutpointr(data = temp_roc_data, x = cmu_closure_perc, class = precip_binary, 
                                                          direction = ">=", pos_class = 1, neg_class = 0, 
                                                          boot_runs = num_boot_runs, boot_stratify = TRUE,
                                                          method = maximize_metric, metric = cohens_kappa, silent = TRUE)
    temp_result_no_sub_cohens_summary <- summary(temp_result_no_sub_cohens_raw) %>%
      dplyr::select(n_obs:n_neg)
    temp_result_no_sub_cohens <- temp_result_no_sub_cohens_raw %>%
      dplyr::mutate(subgroup = "none") %>%
      dplyr::select(subgroup, direction:boot) %>%
      dplyr::mutate(metric = "cohens_kappa",
                    metric_value = cohens_kappa,
                    cmu_name = temp_cmu,
                    valid_period_hrs = temp_valid_period,
                    n_obs = temp_result_no_sub_cohens_summary$n_obs,
                    n_pos = temp_result_no_sub_cohens_summary$n_pos,
                    n_neg = temp_result_no_sub_cohens_summary$n_neg) %>%
      dplyr::select(- cohens_kappa)
    
    # bootstrapped cutpoint results with season subgroup
    # accuracy metric
    set.seed(100)
    temp_result_sub_acc_raw <- cutpointr::cutpointr(data = temp_roc_data, x = cmu_closure_perc, class = precip_binary, subgroup = month_type,
                                                direction = ">=", pos_class = 1, neg_class = 0,
                                                boot_runs = num_boot_runs, boot_stratify = TRUE,
                                                method = maximize_metric, metric = accuracy, silent = TRUE)
    temp_result_sub_acc_summary <- summary(temp_result_sub_acc_raw) %>%
      dplyr::select(n_obs:n_neg)
    temp_result_sub_acc <- temp_result_sub_acc_raw %>%
      dplyr::select(- grouping) %>%
      dplyr::mutate(metric = "accuracy",
                    metric_value = accuracy,
                    cmu_name = temp_cmu,
                    valid_period_hrs = temp_valid_period,
                    n_obs = temp_result_sub_acc_summary$n_obs,
                    n_pos = temp_result_sub_acc_summary$n_pos,
                    n_neg = temp_result_sub_acc_summary$n_neg) %>%
      dplyr::select(- accuracy)
  
    # cohen's kappa metric
    set.seed(100)
    temp_result_sub_cohens_raw <- cutpointr::cutpointr(data = temp_roc_data, x = cmu_closure_perc, class = precip_binary, subgroup = month_type,
                                                       direction = ">=", pos_class = 1, neg_class = 0,
                                                       boot_runs = num_boot_runs, boot_stratify = TRUE,
                                                       method = maximize_metric, metric = cohens_kappa, silent = TRUE)
    temp_result_sub_cohens_summary <- summary(temp_result_sub_cohens_raw) %>%
      dplyr::select(n_obs:n_neg)
    temp_result_sub_cohens <- temp_result_sub_cohens_raw %>%
      dplyr::select(- grouping) %>%
      dplyr::mutate(metric = "cohens_kappa",
                    metric_value = cohens_kappa,
                    cmu_name = temp_cmu,
                    valid_period_hrs = temp_valid_period,
                    n_obs = temp_result_sub_cohens_summary$n_obs,
                    n_pos = temp_result_sub_cohens_summary$n_pos,
                    n_neg = temp_result_sub_cohens_summary$n_neg) %>%
      dplyr::select(- cohens_kappa)
    
    # bind all cuptpoint analyses and add in metadata
    # temp_roc_calcs_data <- bind_rows(temp_result_no_sub_acc, temp_result_no_sub_cohens)
    
    # bind all cuptpoint analyses and add in metadata
    temp_roc_calcs_data <- bind_rows(temp_result_no_sub_acc, temp_result_no_sub_cohens,
                                     temp_result_sub_acc, temp_result_sub_cohens)
    
    # advance counter to assign run number
    run_num <- run_num + 1
    
    # final dataset with run number id joined
    temp_roc_calcs_data_to_join <- temp_roc_calcs_data %>%
      dplyr::mutate(run_num_id = rep(run_num, dim(temp_roc_calcs_data)[1])) %>%
      dplyr::select(run_num_id, subgroup:n_neg)
    
    # append data
    roc_calcs_data <- bind_rows(roc_calcs_data, temp_roc_calcs_data_to_join)
    
    # print message
    print(paste0("appended cmu ", temp_cmu, " ", temp_valid_period, " hr valid period roc results"))
  }
}

# print time now
stop_time <- now()

# time to run loop
stop_time - start_time
# ~1 min (on MacBook Air) x 88 cmu's for 3 valid periods = 88 min / 60 = 1.5 hrs
# actual: ? hours

# export
saveRDS(roc_calcs_data, file = paste0(roc_tabular_data_output_path, "/roc_calcs_data.rds"))

# to read in use...
# roc_calcs_data <- readRDS(paste0(data_path, "tabular/sheila_generated/roc_analysis/roc_calcs_data.rds"))
# DO NOT TRY TO VIEW THIS FILE! RSTUDIO WILL CRASH! :(

# save a copy of the file without the nested tibbles (data, boot, and roc_curve columns)
roc_calcs_data_small <- roc_calcs_data %>%
  dplyr::select(run_num_id:predictor, metric:n_neg)

# change inf to 100
roc_calcs_data_small_fix <- roc_calcs_data_small %>%
  dplyr::mutate(optimal_cutpoint_fix = if_else(optimal_cutpoint == Inf, 100, optimal_cutpoint),
                cutpoint_type = if_else(optimal_cutpoint == Inf | n_pos == 0, "infinite", "definite")) %>%
  dplyr::left_join(cmu_info_unique, by = "cmu_name")

# export
write_csv(roc_calcs_data_small_fix, paste0(roc_tabular_data_output_path, "/roc_calcs_data_small_fix.csv"))


# ---- ndfd vs obs analysis ----
eval_results_by_valid_period <- compare_events_data %>%
  ungroup() %>%
  group_by(valid_period_hrs) %>%
  summarize(r2 = calculate_r2(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            pval = calculate_pval(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            rmse = calculate_rmse(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            slope = calculate_slope(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            intercept = calculate_intercept(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            num_obs = n())

eval_results_by_month_valid_period <- compare_events_data %>%
  ungroup() %>%
  group_by(valid_period_hrs, month_num) %>%
  summarize(r2 = calculate_r2(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            pval = calculate_pval(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            rmse = calculate_rmse(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            slope = calculate_slope(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            intercept = calculate_intercept(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            num_obs = n())

eval_results_by_cmu_valid_period <- compare_events_data %>%
  ungroup() %>%
  group_by(valid_period_hrs, cmu_name) %>%
  summarize(r2 = calculate_r2(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            pval = calculate_pval(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            rmse = calculate_rmse(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            slope = calculate_slope(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            intercept = calculate_intercept(obs_data = obs_avg_cm, frcst_data = cmu_qpf_cm),
            num_obs = n())


# ---- obs plots ----
# make list of unique cmu's
cmu_info_unique <- compare_events_data %>%
  dplyr::select(cmu_name) %>%
  dplyr::distinct()

# get cmu bounding box
cmu_bbox <- cmu_bounds_shp %>%
  st_buffer(dist = 10000) %>% # buffer distance is in m so 10 * 1000m = 10km
  st_bbox()

# get cmu bounding box coords in wgs84
# crs_wgs94 <- 4326
# cmu_bbox_wgs84 <- cmu_bounds_shp %>%
#   st_buffer(dist = 10000) %>% # buffer distance is in m so 10 * 1000m = 10km
#   st_transform(crs = crs_wgs94) %>%
#   st_bbox()
# cmu_bbox_wgs84
# xmin      ymin      xmax      ymax 
# -78.60738  33.76421 -75.35710  36.06080 

# crop nc bounds data to cmu bounds
nc_bounds_shp_cropped <- nc_bounds_shp %>%
  st_crop(xmin = as.numeric(cmu_bbox[1]), 
          xmax = as.numeric(cmu_bbox[3]),
          ymin = as.numeric(cmu_bbox[2]), 
          ymax = as.numeric(cmu_bbox[4]))

# set colors for 2015 (green) and 2016 (yellow)
my_year_colors = c("#66c2a5", "#ffd92f")

# month key
obs_month_key <- compare_events_data %>%
  dplyr::select(date) %>%
  dplyr::mutate(month_num = as.numeric(month(date)),
                month_chr = fct_relevel(as.character(month(date, label = TRUE)), 
                                        c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                month_type = case_when(month_num <= 3 | month_num >= 10 ~ "cool",
                                       month_num > 4 | month_num < 10 ~ "warm"),
                month_type = fct_relevel(month_type, "warm", "cool")) %>%
  dplyr::distinct()

# calculate monthly sum by station
obs_monthly_summary <- obs_data_metadata_join %>%
  dplyr::select(loc_id, date, precip_in, cmu_name) %>% # strip off metadata
  dplyr::distinct(loc_id, date, .keep_all = TRUE) %>%
  dplyr::filter((date >= as.Date("2015-01-01")) & (date <= as.Date("2016-12-31"))) %>%
  dplyr::mutate(month_num = as.numeric(month(date)),
                year_num = as.numeric(year(date))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(loc_id, month_num, year_num) %>%
  dplyr::summarize(precip_monthly_cm = sum(precip_in) * 2.54)

# plot observed monthly total precip at each station
pdf(paste0(figure_output_path, "/obs_precip_vs_month.pdf"), width = 12, height = 10)
ggplot() +
  geom_boxplot(data = obs_monthly_summary, 
               mapping = aes(x = as.factor(month_num), y = precip_monthly_cm, fill = as.factor(year_num))) +
  geom_point(data = obs_monthly_summary, 
             mapping = aes(x = as.factor(month_num), y = precip_monthly_cm, fill = as.factor(year_num)), size = 2, shape = 21, position = position_jitterdodge(), alpha = 0.50) +
  geom_point(data = normals_month_precip_summary,
             mapping = aes(x = as.factor(month_num), y = normals_monthly_area_mean_precip_cm), shape = 17, size = 3, fill = "black") +
  scale_fill_manual(values = my_year_colors) +
  ylim(0, 50) +
  labs(x = "Month", y = "Monthly Observed Precipitation (cm)", fill = "Year") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# map of NC and cmu bounds
pdf(paste0(figure_output_path, "/nc_context_map.pdf"), width = 12, height = 10)
ggplot() +
  geom_sf(data = nc_bounds_shp, fill = "grey80") +
  geom_sf(data = cmu_bounds_shp, fill = "white") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16))
dev.off()

# map of stations by network
pdf(paste0(figure_output_path, "/map_station_networks.pdf"), width = 12, height = 10)
ggplot() +
  geom_sf(data = nc_bounds_shp_cropped, fill = "grey80") +
  geom_sf(data = cmu_bounds_shp, fill = "white") +
  geom_sf(data = obs_metadata_shp, 
          aes(fill = network), size = 4, shape = 21, alpha = 0.75) +
  labs(x = "", y = "", fill = "Network") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16))
dev.off()

# map of stations by completeness
my_complete_colors <- colorRampPalette(brewer.pal(n = 5, name = "BuPu"))
my_complete_colors_length <- length(obs_metadata_shp$perc_rec)
my_complete_colors_min <- floor(min(obs_metadata_shp$perc_rec))
my_complete_colors_max <- 100
pdf(paste0(figure_output_path, "/map_station_completness.pdf"), width = 12, height = 10)
ggplot() +
  geom_sf(data = nc_bounds_shp_cropped, fill = "grey80") +
  geom_sf(data = cmu_bounds_shp, fill = "white") +
  geom_sf(data = obs_metadata_shp, 
          aes(fill = perc_rec), size = 4, shape = 21, alpha = 0.75) +
  scale_fill_gradientn(colors = my_complete_colors(my_complete_colors_length), 
                       limits = c(my_complete_colors_min, my_complete_colors_max)) +
  labs(x = "", y = "", fill = "Percent Complete (%)") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16))
dev.off()

# calculate rainfall threshold depths in cm
cmu_bounds_shp_cm <- cmu_bounds_shp %>%
  dplyr::mutate(rain_cm = rain_in * 2.54)

# map of cmu rainfall thresholds
my_cmu_colors <- brewer.pal(n = length(unique(cmu_bounds_shp_cm$rain_cm)), name = "BuPu")
pdf(paste0(figure_output_path, "/map_cmu_rainfall_thresholds.pdf"), width = 12, height = 10)
ggplot() +
  geom_sf(data = nc_bounds_shp_cropped, fill = "grey80") +
  geom_sf(data = cmu_bounds_shp_cm, aes(fill = as.factor(rain_cm)), color = "black", alpha = 0.75) +
  scale_fill_manual(values = my_cmu_colors) +
  # coord_sf(datum = st_crs(5070)) +
  labs(x = "", y = "", fill = "Rainfall Threshold Depths (cm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16))
dev.off()

# zoomed map of cmu rainfall thresholds
pdf(paste0(figure_output_path, "/map_cmu_rainfall_thresholds_zoom.pdf"), width = 12, height = 10)
ggplot() +
  geom_sf(data = nc_bounds_shp_cropped, fill = "grey80") +
  geom_sf(data = cmu_bounds_shp_cm, aes(fill = as.factor(rain_cm)), color = "black", alpha = 0.75) +
  coord_sf(xlim = c(1600000, 1770000), ylim = c(1350000, 1500000), expand = FALSE) +
  scale_fill_manual(values = my_cmu_colors) +
  labs(x = "", y = "", fill = "Rainfall Threshold Depths (cm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16))
dev.off()

# map of conditionally approved and approved cmus
# need to bind cmu data with sga_bounds_class_albers.shp to get ga_classes associated with cmu's


# wrangle data to join unique cmu and obs counts to cmu spatial data
cmu_obs_count_data <- obs_ndfd_data %>%
  dplyr::select(cmu_name, obs_measurement_count) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(cmu_name) %>%
  dplyr::summarize(min = min(obs_measurement_count),
                   mean = round(mean(obs_measurement_count), 2),
                   max = max(obs_measurement_count),
                   range = (range(obs_measurement_count)[2] - range(obs_measurement_count)[1])) %>%
  tidyr::pivot_longer(cols = min:range, names_to = "summary_calc") %>%
  dplyr::mutate(summary_calc = factor(summary_calc, levels = c("min", "mean", "max", "range")))
  # for each cmu, different days may have different observations counts so take min and max

# plot distributions of these observation counts for each cmu
pdf(paste0(figure_output_path, "/cmu_num_obs_density.pdf"), width = 10, height = 10)
ggplot(data = cmu_obs_count_data) +
  geom_density(aes(x = value), fill = "grey80") +
  facet_wrap(~ summary_calc) +
  xlim(0, 15) +
  labs(x = "Number of Obervations per CMU", y = "Density") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# add spatial data
cmu_obs_count_shp <- cmu_obs_count_data %>%
  left_join(cmu_bounds_shp_cm, by = "cmu_name")

# map of number of stations per cmu
pdf(paste0(figure_output_path, "/map_cmu_num_obs.pdf"), width = 12, height = 10)
ggplot() +
  geom_sf(data = nc_bounds_shp_cropped, fill = "grey80") +
  geom_sf(data = cmu_obs_count_shp %>% dplyr::filter(summary_calc == "mean"), 
          aes(fill = value), 
          color = "black", 
          alpha = 0.75) +
  #scale_fill_manual(values = my_cmu_colors) +
  labs(fill = "Average Number of Stations") +
  theme_classic()
dev.off()

# plot number of events per cmu for the period of study
obs_ndfd_data_sel


# ---- roc analysis plots ----
# set colors for 2015 (green) and 2016 (yellow)
my_year_colors = c("#66c2a5", "#ffd92f")

# plot percent of available monthly data in different event categories
pdf(paste0(figure_output_path, "/percent_month_vs_event_by_period.pdf"), width = 15, height = 7)
ggplot() +
  geom_boxplot(data = station_event_type_monthly_summary, 
               aes(x = event_type, y = perc_month, fill = as.factor(year)),
               outlier.color = "black") +
  geom_abline(slope = 0, intercept = 50, lty = 2) +
  facet_wrap(~ valid_period_hrs) +
  labs(x = "Event Occurence Type", y = "Percent of Each Month", fill = "Year") + 
  scale_fill_manual(values = my_year_colors) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# color scale for valid periods
my_validhrs_colors <- c("#66c2a5", "#fc8d62", "#8da0cb")

# plot percent of available monthly data in different event categories (all valid periods, no years)
pdf(paste0(figure_output_path, "/percent_month_vs_month_by_event_by_validprdhrs_no_years.pdf"), width = 15, height = 7)
ggplot() +
  geom_boxplot(data = station_event_type_monthly_summary,
               aes(x = as.factor(month), y = perc_month, fill = as.factor(valid_period_hrs)),
               outlier.color = "black") +
  geom_abline(slope = 0, intercept = 50, lty = 2) +
  facet_wrap(~ event_type) +
  labs(x = "Event Occurence Type", y = "Percent of Each Month", fill = "Valid Period Hours") +
  scale_fill_manual(values = my_validhrs_colors) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# plot percent of available monthly data in different event categories (24 hrs)
pdf(paste0(figure_output_path, "/percent_month_vs_month_by_event_24hr.pdf"), width = 15, height = 7)
ggplot() +
  geom_boxplot(data = station_event_type_monthly_summary %>% filter(valid_period_hrs == 24),
               aes(x = as.factor(month), y = perc_month, fill = as.factor(year)),
               outlier.color = "black") +
  geom_abline(slope = 0, intercept = 50, lty = 2) +
  facet_wrap(~ event_type) +
  labs(x = "Event Occurence Type", y = "Percent of Each Month", fill = "Year") +
  scale_fill_manual(values = my_year_colors) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# plot percent of available monthly data in different event categories (48 hrs)
pdf(paste0(figure_output_path, "/percent_month_vs_month_by_event_48hr.pdf"), width = 15, height = 7)
ggplot() +
  geom_boxplot(data = station_event_type_monthly_summary %>% filter(valid_period_hrs == 48),
               aes(x = as.factor(month), y = perc_month, fill = as.factor(year)),
               outlier.color = "black") +
  geom_abline(slope = 0, intercept = 50, lty = 2) +
  facet_wrap(~ event_type) +
  labs(x = "Event Occurence Type", y = "Percent of Each Month", fill = "Year") +
  scale_fill_manual(values = my_year_colors) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# plot percent of available monthly data in different event categories (72 hrs)
pdf(paste0(figure_output_path, "/percent_month_vs_month_by_event_72hr.pdf"), width = 15, height = 7)
ggplot() +
  geom_boxplot(data = station_event_type_monthly_summary %>% filter(valid_period_hrs == 72),
               aes(x = as.factor(month), y = perc_month, fill = as.factor(year)),
               outlier.color = "black") +
  geom_abline(slope = 0, intercept = 50, lty = 2) +
  facet_wrap(~ event_type) +
  labs(x = "Event Occurence Type", y = "Percent of Each Month", fill = "Year") +
  scale_fill_manual(values = my_year_colors) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# cutpoint by valid period (based on cohens kappa)
# minus infinate results
test3 <- roc_calcs_data_small_fix %>%
  dplyr::filter(cutpoint_type == "definite" & metric == "cohens_kappa") %>%
  dplyr::mutate(subgroup = fct_relevel(subgroup, 
                                       c("none", "warm", "cool")))
pdf(paste0(figure_output_path, "/roc_cutpoint_by_valid.pdf"), width = 12, height = 10)
ggplot() +
  geom_boxplot(data = test3,
               aes(x = subgroup, y = optimal_cutpoint_fix, fill = as.factor(valid_period_hrs)),
               outlier.color = "black") +
  ylim(0, 100) +
  labs(x = "ROC Group", y = "Closure Cutpoint (based on Cohen's Kappa)", fill = "Valid Period Hours") +
  scale_fill_manual(values = my_validhrs_colors) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# accuracy by valid period
# minus infinate results
test1 <- roc_calcs_data_small_fix %>%
  dplyr::filter(cutpoint_type == "definite" & metric == "accuracy") %>%
  dplyr::mutate(subgroup = fct_relevel(subgroup, 
                                       c("none", "warm", "cool")))
pdf(paste0(figure_output_path, "/roc_accuracy_by_valid.pdf"), width = 12, height = 10)
ggplot() +
  geom_boxplot(data = test1,
               aes(x = subgroup, y = metric_value, fill = as.factor(valid_period_hrs)),
               outlier.color = "black") +
  labs(x = "ROC Group", y = "Accuracy", fill = "Valid Period Hours") +
  scale_fill_manual(values = my_validhrs_colors) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# cohens kappa by valid period
# minus infinate results
test2 <- roc_calcs_data_small_fix %>%
  dplyr::filter(cutpoint_type == "definite" & metric == "cohens_kappa") %>%
  dplyr::mutate(subgroup = fct_relevel(subgroup, 
                                       c("none", "warm", "cool")))
pdf(paste0(figure_output_path, "/roc_cohens_by_valid.pdf"), width = 12, height = 10)
ggplot() +
  geom_boxplot(data = test2,
               aes(x = subgroup, y = metric_value, fill = as.factor(valid_period_hrs)),
               outlier.color = "black") +
  labs(x = "ROC Group", y = "Cohen's Kappa", fill = "Valid Period Hours") +
  scale_fill_manual(values = my_validhrs_colors) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# sensitivity by valid period (based on cohens kappa)
pdf(paste0(figure_output_path, "/roc_sens_by_valid.pdf"), width = 12, height = 10)
ggplot() +
  geom_boxplot(data = test3,
               aes(x = subgroup, y = sensitivity, fill = as.factor(valid_period_hrs)),
               outlier.color = "black") +
  ylim(0, 1) +
  labs(x = "ROC Group", y = "Sensitivity aka TPR\n(based on Cohen's Kappa)", fill = "Valid Period Hours") +
  scale_fill_manual(values = my_validhrs_colors) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# specificity by valid period (based on cohens kappa)
pdf(paste0(figure_output_path, "/roc_specif_by_valid.pdf"), width = 12, height = 10)
ggplot() +
  geom_boxplot(data = test3,
               aes(x = subgroup, y = (1-specificity), fill = as.factor(valid_period_hrs)),
               outlier.color = "black") +
  ylim(0, 0.1) +
  labs(x = "ROC Group", y = "(1 - Specificity) aka FPR\n(based on Cohen's Kappa)", fill = "Valid Period Hours") +
  scale_fill_manual(values = my_validhrs_colors) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# TRP vs FRP by valid period (based on cohens kappa)
pdf(paste0(figure_output_path, "/roc_tprvsfpr_by_valid.pdf"), width = 15, height = 5)
ggplot() +
  geom_point(data = test3,
               aes(x = (1-specificity), y = sensitivity, fill = as.factor(valid_period_hrs)), 
                   shape = 21, alpha = 0.50, size = 3) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlim(0, 1) +
  ylim(0, 1) +
  facet_wrap(~ as.factor(valid_period_hrs)) +
  labs(x = "False Positive Rate (1-Specificity)", y = "True Positive Rate (Sensitivity)", fill = "Valid Period Hours") +
  scale_fill_manual(values = my_validhrs_colors) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()



# ---- ndfd vs obs analysis plots ----
# color scale for valid periods
my_validhrs_colors <- c("#66c2a5", "#fc8d62", "#8da0cb")

# format annotations for results vs valid period
my_validhrs_eval_text <- eval_results_by_valid_period %>%
  dplyr::mutate(label = paste0("R-squared = ", r2, "\nSlope = ", slope, "\nRMSE = ", rmse)) %>%
  dplyr::select(valid_period_hrs, label)

# all data facet by valid period
pdf(paste0(figure_output_path, "/obs_vs_ndfd_by_valid_period.pdf"), width = 15, height = 5)
ggplot(data = compare_events_data) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm, fill = as.factor(valid_period_hrs)), shape = 21, alpha = 0.50, size = 3) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_text(data = my_validhrs_eval_text, mapping = aes(x = 20, y = 27, label = label), hjust = 1.0, vjust = 1.0) +
  xlim(0, 27) +
  ylim(0, 27) +
  facet_wrap(~ as.factor(valid_period_hrs)) +
  labs(x = "Forecasted (cm)", y = "Observed (cm)", fill = "Valid Period Hours") +
  theme_classic() +
  scale_fill_manual(values = my_validhrs_colors) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# format annotations for 24 hrs by month
my_24hr_validhrs_month_eval_text <- eval_results_by_month_valid_period %>%
  dplyr::filter(valid_period_hrs == 24) %>%
  dplyr::mutate(label = paste0("R-squared = ", r2, "\nSlope = ", slope, "\nRMSE = ", rmse)) %>%
  dplyr::select(valid_period_hrs, month_num, label)

# 24 hr valid period by month
pdf(paste0(figure_output_path, "/obs_vs_ndfd_24hr_by_month.pdf"), width = 12, height = 10)
ggplot(data = compare_events_data %>% filter(valid_period_hrs == 24)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[1], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_text(data = my_24hr_validhrs_month_eval_text, mapping = aes(x = 20, y = 27, label = label), hjust = 1.0, vjust = 1.0) +
  xlim(0, 27) +
  ylim(0, 27) +
  facet_wrap(~ month_num) +
  labs(x = "Forecasted (cm)", y = "Observed (cm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# 24 hr valid period month + event type
pdf(paste0(figure_output_path, "/obs_vs_ndfd_24hr_by_month_event.pdf"), width = 12, height = 10)
ggplot(data = compare_events_data %>% filter(valid_period_hrs == 24)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[1], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlim(0, 27) +
  ylim(0, 27) +
  facet_wrap(~ event_type + month_num, nrow = 4, ncol = 12) +
  labs(x = "Forecasted (cm)", y = "Observed (cm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        text = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# 24 hr valid period month + season
pdf(paste0(figure_output_path, "/obs_vs_ndfd_24hr_by_month_season.pdf"), width = 12, height = 10)
ggplot(data = compare_events_data %>% filter(valid_period_hrs == 24)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[1], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlim(0, 27) +
  ylim(0, 27) +
  facet_wrap(~ month_type + month_num, nrow = 2, ncol = 6) +
  labs(x = "Forecasted (cm)", y = "Observed (cm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        text = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# format annotations for 48 hrs by month
my_48hr_validhrs_month_eval_text <- eval_results_by_month_valid_period %>%
  dplyr::filter(valid_period_hrs == 48) %>%
  dplyr::mutate(label = paste0("R-squared = ", r2, "\nSlope = ", slope, "\nRMSE = ", rmse)) %>%
  dplyr::select(valid_period_hrs, month_num, label)

# 48 hr valid period by month
pdf(paste0(figure_output_path, "/obs_vs_ndfd_48hr_by_month.pdf"), width = 12, height = 10)
ggplot(data = compare_events_data %>% filter(valid_period_hrs == 48)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[2], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_text(data = my_48hr_validhrs_month_eval_text, mapping = aes(x = 20, y = 27, label = label), hjust = 1.0, vjust = 1.0) +
  xlim(0, 27) +
  ylim(0, 27) +
  facet_wrap(~ month_num) +
  labs(x = "Forecasted (cm)", y = "Observed (cm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# 48 hr valid period by month + event type
pdf(paste0(figure_output_path, "/obs_vs_ndfd_48hr_by_event.pdf"), width = 12, height = 10)
ggplot(data = compare_events_data %>% filter(valid_period_hrs == 48)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[2], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlim(0, 27) +
  ylim(0, 27) +
  facet_wrap(~ event_type + month_num, nrow = 4, ncol = 12) +
  labs(x = "Forecasted (cm)", y = "Observed (cm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        text = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# 48 hr valid period by month + season type
pdf(paste0(figure_output_path, "/obs_vs_ndfd_48hr_by_season.pdf"), width = 12, height = 10)
ggplot(data = compare_events_data %>% filter(valid_period_hrs == 48)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[2], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlim(0, 27) +
  ylim(0, 27) +
  facet_wrap(~ month_type + month_num, nrow = 2, ncol = 6) +
  labs(x = "Forecasted (cm)", y = "Observed (cm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        text = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# format annotations for 48 hrs by month
my_72hr_validhrs_month_eval_text <- eval_results_by_month_valid_period %>%
  dplyr::filter(valid_period_hrs == 72) %>%
  dplyr::mutate(label = paste0("R-squared = ", r2, "\nSlope = ", slope, "\nRMSE = ", rmse)) %>%
  dplyr::select(valid_period_hrs, month_num, label)

# 72 hr valid period by month
pdf(paste0(figure_output_path, "/obs_vs_ndfd_72hr_by_month.pdf"), width = 12, height = 10)
ggplot(data = compare_events_data %>% filter(valid_period_hrs == 72)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[3], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_text(data = my_72hr_validhrs_month_eval_text, mapping = aes(x = 20, y = 27, label = label), hjust = 1.0, vjust = 1.0) +
  xlim(0, 27) +
  ylim(0, 27) +
  facet_wrap(~ month_num) +
  labs(x = "Forecasted (cm)", y = "Observed (cm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# 72 hr valid period by month + event type
pdf(paste0(figure_output_path, "/obs_vs_ndfd_72hr_by_month_event.pdf"), width = 12, height = 10)
ggplot(data = compare_events_data %>% filter(valid_period_hrs == 72)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[3], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlim(0, 27) +
  ylim(0, 27) +
  facet_wrap(~ event_type + month_num, nrow = 4, ncol = 12) +
  labs(x = "Forecasted (cm)", y = "Observed (cm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        text = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# 72 hr valid period by month + season type
pdf(paste0(figure_output_path, "/obs_vs_ndfd_72hr_by_month_season.pdf"), width = 12, height = 10)
ggplot(data = compare_events_data %>% filter(valid_period_hrs == 72)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[3], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlim(0, 27) +
  ylim(0, 27) +
  facet_wrap(~ month_type + month_num, nrow = 2, ncol = 6) +
  labs(x = "Forecasted (cm)", y = "Observed (cm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        text = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# calculate seasonal nse by cmu
# calculate 




