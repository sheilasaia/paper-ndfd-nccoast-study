# !diagnostics off
# ---- script header ----
# script name: roc_analysis_script.R
# purpose of script: reliever operating characteristic (roc) curve analysis for ndfd performance
# author: sheila saia
# date created: 20210204
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:

# general info on ROC and AUC
# https://rviews.rstudio.com/2019/01/17/roc-curves/
# AUC is the probability that a randomly drawn interval with a signal present will 
# produce a higher X value than a signal interval containing noise alone
# threshold value is optimal value that maximizes optimization 

# Youden's J statistic
# cutoff threshold is max Youden J's stat
# best to use Youden's over closest topleft according to https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-12-77
# value of 0 means test us useless because it gives the same proportion of positive results for cases and controls
# value of 1 means test has no fp or fn therefore is perfect
# https://en.wikipedia.org/wiki/Youden%27s_J_statistic

# for pROC package
# https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
# https://cran.r-project.org/web/packages/pROC/pROC.pdf
# https://rdrr.io/cran/pROC/man/coords.html (see for glossary table)
# https://www.rdocumentation.org/packages/pROC/versions/1.17.0.1
# examples: https://rpubs.com/Wangzf/pROC

# cutpointr package
# https://cran.r-project.org/web/packages/cutpointr/vignettes/cutpointr.html
# https://cran.r-project.org/web/packages/cutpointr/cutpointr.pdf
# preprint: https://arxiv.org/pdf/2002.09209.pdf

# roc analysis
# https://acutecaretesting.org/en/articles/roc-curves-what-are-they-and-how-are-they-used
# https://www.youtube.com/watch?v=4jRBRDbJemM
# Youden and Closest Top Left https://academic.oup.com/aje/article/163/7/670/77813
# https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/NCSS/One_ROC_Curve_and_Cutoff_Analysis.pdf

# auc
# use this to compare tests, 1 indicates perfect fit and below 0.5 indicates a test failure
# 0.9 - 1 (very good), 0.8 - 0.9 (good), 0.8 - 0.7 (fair), 0.7 - 0.6 (poor), 0 - 0.6 (fail)

# predictor (cmu qpf in inches) --> not obs precip in inches
# predictor (cmu pc percentages) *** do this!
# outcome (binary whether obs precip was over rainfall threshold, 0 is not closed and 1 is closed) --> not whether cmu qpf was over rainfall threshold
# effective closure probability = cutpoint for the cmu pc percentage as predictor analysis


# ---- to do ----
# to do list

# TODO change code to work new cmu results that have avg and max values (e.g., joining df's and temp_cp calcs)
# TODO remove diagnosis off at top

# ---- 1. load libraries ----
library(tidyverse)
library(sf)
library(here)
library(lubridate)
library(forcats)
library(cutpointr)
# library(tidylog)
# library(pROC)


# ---- 2. define paths ----
# data path (for now --> use here package later)
data_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/"

# path to ndfd tabular inputs
ndfd_sco_tabular_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/ndfd_sco_hist_raw/"

# path to ndfd spatial inputs
ndfd_sco_spatial_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/ndfd_sco_hist/"


# ---- 3. load data ----
# ndfd selected data
ndfd_data_sel <- read_csv(paste0(data_path, "tabular/sheila_generated/ndfd_sco_hist/ndfd_data_sel_full.csv"), col_names = TRUE)

# precip metadata (that overlaps and is 90% complete)
hist_precip_metadata_albers_sel <- read_sf(paste0(data_path, "spatial/sheila_generated/hist_precip_data/hist_precip_metadata_albers_sel_full.shp"))

# import historic precip data (tabular)
hist_precip_data <- read_csv(file = paste0(data_path, "tabular/sheila_generated/hist_precip_data/hist_precip_data_compiled.csv"), col_names = TRUE,
                             col_types = list(col_character(), col_date(), col_number()))


# ---- 4. functions ----
# calculate the probability of closure (as a decimal)
calc_closure_perc <- function(rain_thresh_in, qpf_in, pop_notdecimal) {
  cloure_perc <- round(pop_notdecimal * exp((-rain_thresh_in/qpf_in)), 2) # percent closure as decimal percent
  return(cloure_perc)
}


# ---- 5. wrangling historic precip data ----
# drop geometry from metadata
hist_precip_metadata_sel_tabular <- hist_precip_metadata_albers_sel %>%
  st_drop_geometry() %>%
  dplyr::select(loc_id, network, perc_compl, cmu_name:rain_lab)
  
# join metadata
hist_precip_data_join <- hist_precip_data %>%
  dplyr::left_join(hist_precip_metadata_sel_tabular, by = "loc_id") %>%
  na.omit()

# check number of unique cmus
# length(unique(hist_precip_data_join$cmu_name))
# 91 ok!
  
# take average by cmu and date
hist_precip_data_avg <- hist_precip_data_join %>%
  dplyr::select(loc_id, date, precip_in, cmu_name) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(date, cmu_name) %>%
  dplyr::summarize(precip_avg_in = round(mean(precip_in, na.rm = TRUE), 2), # calculate average precip
                   station_count_day = n()) # keep track of the number of stations being summarized for each day

# check number of unique cmus
# length(unique(hist_precip_data_avg$cmu_name))
# 91 ok!

# cmu and rainfall threshold key
cmu_rain_thresh_key <- hist_precip_metadata_sel_tabular %>%
  dplyr::select(cmu_name, rain_in) %>%
  dplyr::distinct()

# join rainfall threshold to hist_precip_data_avg
hist_precip_data_avg_join <- hist_precip_data_avg %>%
  dplyr::left_join(cmu_rain_thresh_key, by = "cmu_name") %>%
  dplyr::select(-rain_in)


# ---- 6. wrangling ndfd data ----
ndfd_data_avg <- ndfd_data_sel %>%
  dplyr::ungroup() %>%
  dplyr::group_by(date, valid_period_hrs, cmu_name) %>%
  # use mean to summarize here for both loc- and cmu-based calcs when there are more than one station in a cmu
  # loc calcs can be different but cmu calcs will be the same, mean will take care of both
  dplyr::summarize(loc_pop_perc = round(mean(loc_pop_perc, na.rm = TRUE), 2),
                   loc_qpf_in = round(mean(loc_qpf_in, na.rm = TRUE), 2),
                   cmu_pop_perc = round(mean(cmu_pop_avg_perc, na.rm = TRUE), 2), # decided not to use cmu_pop_max_perc
                   cmu_qpf_in = round(mean(cmu_qpf_in, na.rm = TRUE), 2)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(cmu_rain_thresh_key, by = "cmu_name") %>% # need to rejoin
  dplyr::mutate(loc_closure_perc = calc_closure_perc(rain_thresh_in = rain_in, qpf_in = loc_qpf_in, pop_notdecimal = loc_pop_perc),
                cmu_closure_perc = calc_closure_perc(rain_thresh_in = rain_in, qpf_in = cmu_qpf_in, pop_notdecimal = cmu_pop_perc)) %>%
  dplyr::mutate(month_chr = fct_relevel(as.character(month(date, label = TRUE)), c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                month_num = as.numeric(month(date)),
                month_type = case_when(month_num <= 3 | month_num >= 10 ~ "cool",
                                       month_num > 4 | month_num < 10 ~ "warm"),
                month_type = fct_relevel(month_type, "warm", "cool"))


# ---- 7. check how similar loc and cmu approaches are ----
# plot loc qpf vs cmu qpf
ggplot(data = ndfd_data_avg) +
  geom_point(aes(x = loc_qpf_in, y = cmu_qpf_in, color = month_chr), alpha = 0.75, size = 3) +
  labs(x = "QPF value of observation gridcell (in)", y = "Weighted area avg QPF of CMU assoc. w/ observation (in)", color = "Month") +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  facet_wrap(~ month_chr) +
  theme_classic()
# very similar so use cmu approach since this is what ShellCast uses

# plot loc pop vs cmu avg pop
ggplot(data = ndfd_data_avg) +
  geom_point(aes(x = loc_pop_perc, y = cmu_pop_perc, color = month_chr), alpha = 0.75, size = 3) +
  labs(x = "POP value of observation gridcell (%)", y = "Weighted area avg POP of CMU assoc. w/ observation (%)", color = "Month") +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  facet_wrap(~ month_chr) +
  theme_classic()
# some scatter all around the 1:1 line

# plot loc pc vs cmu pc (with area-weighted avg cmu values)
ggplot(data = ndfd_data_avg) +
  geom_point(aes(x = loc_closure_perc, y = cmu_closure_perc, color = month_chr), alpha = 0.75, size = 3) +
  labs(x = "location closure (%)", y = "cmu weighted area avg closure (%)", color = "Month") +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  facet_wrap(~ valid_period_hrs + month_chr, nrow = 3, ncol = 12) +
  theme_classic()

# all of these are tight along the line so moving forward with using cmu-based calcs
# cmu-based calcs are more relevant to the ShellCast algorithm


# ---- 8. join data for roc analysis ----
# bring observations and forecasts together for roc analysis
roc_data <- ndfd_data_avg %>%
  dplyr::left_join(hist_precip_data_avg_join, by = c("date", "cmu_name")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(precip_binary = if_else(precip_avg_in >= rain_in, 1, 0)) %>%
  na.omit() %>%
  dplyr::select(date, valid_period_hrs, cmu_name, rain_in, station_count_day, month_chr:month_type, loc_closure_perc, cmu_closure_perc, precip_avg_in, precip_binary)

# check number of unique cmu
# length(unique(roc_data$cmu_name))
# 91 ok!

# first check whole period
roc_data_study_event_remove_list <- roc_data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(cmu_name, valid_period_hrs) %>%
  dplyr::summarize(pos_event_sum = sum(precip_binary, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(pos_event_sum == 0) %>%
  dplyr::select(cmu_name) %>%
  dplyr::distinct()
# these have at least 1 or more events in the whole study period

# next check seasonal
roc_data_season_event_remove_list <- roc_data %>%
  dplyr::anti_join(roc_data_study_event_remove_list, by = "cmu_name") %>%
  dplyr::ungroup() %>%
  dplyr::group_by(month_type, cmu_name, valid_period_hrs) %>%
  dplyr::summarize(pos_event_sum = sum(precip_binary, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(pos_event_sum == 0) %>%
  dplyr::select(cmu_name) %>%
  dplyr::distinct()

# bind rows of cmu's without enough data
roc_data_cmus_to_remove_list <- dplyr::bind_rows(roc_data_study_event_remove_list, roc_data_season_event_remove_list)

# final roc data for analysis
roc_data_final <- roc_data %>%
  dplyr::anti_join(roc_data_cmus_to_remove_list, by = "cmu_name")

# check number of unique cmu
# length(unique(roc_data_final$cmu_name))
# 74


# ---- 9. plot number of stations per cmu ----
# station summary counts by cmu
station_cmu_summary <- roc_data %>%
  dplyr::ungroup() %>%
  dplyr::select(cmu_name, station_count_day) %>%
  dplyr::distinct() %>%
  dplyr::group_by(cmu_name) %>%
  dplyr::summarize(station_count_study_sum = sum(station_count_day, na.rm = TRUE),
                   station_count_study_avg = mean(station_count_day, na.rm = TRUE))

# plot 
ggplot(station_cmu_summary) +
  geom_density(aes(x = station_count_study_avg, fill = station_count_study_avg)) +
  labs(x = "Average Number of Stations per CMU", y = "Density") +
  theme_classic()
# some stations are only availble on some days so this is why i'm using average here

# plot 
ggplot(station_cmu_summary) +
  geom_density(aes(x = station_count_study_sum, fill = station_count_study_sum)) +
  labs(x = "Sum Number of Stations per CMU", y = "Density") +
  theme_classic()
# some stations are only availble on some days so this is why i'm using average here


# ---- 10. plot number of cmu's for each rainfall threshold ----
# cmu summary counts
cmu_summary <- roc_data %>%
  dplyr::select(cmu_name, rain_in) %>%
  dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(rain_in) %>%
  dplyr::summarize(cmu_count = n())

# plot
ggplot(cmu_summary) +
  geom_col(aes(x = as.factor(rain_in), y = cmu_count)) +
  geom_text(aes(x = as.factor(rain_in), y = cmu_count + 1, label = cmu_count)) +
  labs(x = "Temporary Rainfall Threshold (in)", y = "Number of CMUs") +
  theme_classic()


# ---- 11. roc analysis for each cmu and valid period (no season subgroup) ----
# make an empty dataframe
roc_no_sub_calcs_data <- NULL

# unique cmu values with their rainfall depths
cmu_info_unique <- roc_data_final %>%
  dplyr::ungroup() %>%
  dplyr::select(cmu_name, rain_in, station_count_day) %>%
  dplyr::group_by(cmu_name, rain_in) %>%
  dplyr::summarize(station_count_day_min = min(station_count_day, na.rm = TRUE),
                   station_count_day_max = max(station_count_day, na.rm = TRUE),
                   station_count_day_avg = mean(station_count_day, na.rm = TRUE))

# number of cmus
num_cmus <- length(cmu_info_unique$cmu_name)

# number of valid periods
num_valid_periods <- length(unique(roc_data$valid_period_hrs))

# valid period values
valid_period_list <- unique(roc_data$valid_period_hrs)

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
    temp_roc_data <- roc_data_final %>%
      dplyr::filter(cmu_name == temp_cmu & valid_period_hrs == temp_valid_period)
    
    # bootstrapped cutpoint results without season subgroup
    # youden's j metric
    set.seed(100)
    temp_result_no_sub_youden_raw <- cutpointr::cutpointr(data = temp_roc_data, x = cmu_closure_perc, class = precip_binary, 
                                                          direction = ">=", pos_class = 1, neg_class = 0, 
                                                          boot_runs = num_boot_runs, boot_stratify = TRUE,
                                                          method = maximize_metric, metric = youden, silent = TRUE) 
    temp_result_no_sub_youden_summary <- summary(temp_result_no_sub_youden_raw) %>%
      dplyr::select(n_obs:n_neg)
    temp_result_no_sub_youden <- temp_result_no_sub_youden_raw %>%
      dplyr::mutate(subgroup = "none") %>%
      dplyr::select(subgroup, direction:boot) %>%
      dplyr::mutate(metric = "youden",
                    metric_value = youden,
                    cmu_name = temp_cmu,
                    valid_period_hrs = temp_valid_period,
                    n_obs = temp_result_no_sub_youden_summary$n_obs,
                    n_pos = temp_result_no_sub_youden_summary$n_pos,
                    n_neg = temp_result_no_sub_youden_summary$n_neg) %>%
      dplyr::select(- youden)
    # Does boot_strategy (1) exactly equal or (2) more reflective of distribution? - I think it's #2 here.
    
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
    # youden's j metric
    # set.seed(100)
    # temp_result_sub_youden_raw <- cutpointr::cutpointr(data = temp_roc_data, x = cmu_closure_perc, class = precip_binary, subgroup = month_type,
    #                                                direction = ">=", pos_class = 1, neg_class = 0, 
    #                                                boot_runs = num_boot_runs, boot_stratify = TRUE,
    #                                                method = maximize_metric, metric = youden, silent = TRUE)
    # temp_result_sub_youden_summary <- summary(temp_result_sub_youden_raw) %>%
    #   dplyr::select(n_obs:n_neg)
    # temp_result_sub_youden <- temp_result_sub_youden_raw %>%
    #   dplyr::select(- grouping) %>%
    #   dplyr::mutate(metric = "youden",
    #                 metric_value = youden,
    #                 cmu_name = temp_cmu,
    #                 valid_period_hrs = temp_valid_period,
    #                 n_obs = temp_result_sub_youden_summary$n_obs,
    #                 n_pos = temp_result_sub_youden_summary$n_pos,
    #                 n_neg = temp_result_sub_youden_summary$n_neg) %>%
    #   dplyr::select(- youden)
    
    # accuracy metric
    # set.seed(100)
    # temp_result_sub_acc_raw <- cutpointr::cutpointr(data = temp_roc_data, x = cmu_closure_perc, class = precip_binary, subgroup = month_type,
    #                                             direction = ">=", pos_class = 1, neg_class = 0,
    #                                             boot_runs = num_boot_runs, boot_stratify = TRUE,
    #                                             method = maximize_metric, metric = accuracy, silent = TRUE)
    # temp_result_sub_acc_summary <- summary(temp_result_sub_acc_raw) %>%
    #   dplyr::select(n_obs:n_neg)
    # temp_result_sub_acc <- temp_result_sub_acc_raw %>%
    #   dplyr::select(- grouping) %>%
    #   dplyr::mutate(metric = "accuracy",
    #                 metric_value = accuracy,
    #                 cmu_name = temp_cmu,
    #                 valid_period_hrs = temp_valid_period,
    #                 n_obs = temp_result_sub_acc_summary$n_obs,
    #                 n_pos = temp_result_sub_acc_summary$n_pos,
    #                 n_neg = temp_result_sub_acc_summary$n_neg) %>%
    #   dplyr::select(- accuracy)
    
    # cohen's kappa metric
    # set.seed(100)
    # temp_result_sub_cohens_raw <- cutpointr::cutpointr(data = temp_roc_data, x = cmu_closure_perc, class = precip_binary, subgroup = month_type,
    #                                                    direction = ">=", pos_class = 1, neg_class = 0, 
    #                                                    boot_runs = num_boot_runs, boot_stratify = TRUE,
    #                                                    method = maximize_metric, metric = cohens_kappa, silent = TRUE)
    # temp_result_sub_cohens_summary <- summary(temp_result_sub_cohens_raw) %>%
    #   dplyr::select(n_obs:n_neg)
    # temp_result_sub_cohens <- temp_result_sub_cohens_raw %>%
    #   dplyr::select(- grouping) %>%
    #   dplyr::mutate(metric = "cohens_kappa",
    #                 metric_value = cohens_kappa,
    #                 cmu_name = temp_cmu,
    #                 valid_period_hrs = temp_valid_period,
    #                 n_obs = temp_result_sub_cohens_summary$n_obs,
    #                 n_pos = temp_result_sub_cohens_summary$n_pos,
    #                 n_neg = temp_result_sub_cohens_summary$n_neg) %>%
    #   dplyr::select(- cohens_kappa)
    
    # bind all cuptpoint analyses and add in metadata
    temp_roc_calcs_data <- bind_rows(temp_result_no_sub_youden, temp_result_no_sub_acc, temp_result_no_sub_cohens)
    
    # bind all cuptpoint analyses and add in metadata
    # temp_roc_calcs_data <- bind_rows(temp_result_no_sub_youden, temp_result_no_sub_acc, temp_result_no_sub_cohens,
    #                                  temp_result_sub_youden, temp_result_sub_acc, temp_result_sub_cohens)
    
    # advance counter to assign run number
    run_num <- run_num + 1
    
    # final dataset with run number id joined
    temp_roc_calcs_data_to_join <- temp_roc_calcs_data %>%
      dplyr::mutate(run_num_id = rep(run_num, dim(temp_roc_calcs_data)[1])) %>%
      dplyr::select(run_num_id, subgroup:n_neg)
    
    # append data
    roc_no_sub_calcs_data <- bind_rows(roc_no_sub_calcs_data, temp_roc_calcs_data_to_join)
    
    # print message
    print(paste0("appended cmu ", temp_cmu, " ", temp_valid_period, " hr valid period roc results"))
  }
}

# print time now
stop_time <- now()

# time to run loop
stop_time - start_time
# ~2 min x 91 cmu's x 3 valid periods = 546 min / 60 = 9.1 hrs
# actual: ? hours

# export
saveRDS(roc_no_sub_calcs_data, file = paste0(data_path, "tabular/sheila_generated/roc_analysis/roc_no_sub_calcs_data.rds"))

# to read in use...
# roc_calcs_data <- readRDS(paste0(data_path, "tabular/sheila_generated/roc_analysis/roc_calcs_data.rds"))
# DO NOT TRY TO VIEW THIS FILE! RSTUDIO WILL CRASH! :(

# save a copy of the file without the nested tibbles (data, boot, and roc_curve columns)
roc_no_sub_calcs_data_small <- roc_no_sub_calcs_data %>%
  dplyr::select(run_num_id:predictor, metric:n_neg)

# export
# write_csv(roc_calcs_data_small, paste0(data_path, "tabular/sheila_generated/roc_analysis/roc_calcs_data_small.rds"))
write_csv(roc_no_sub_calcs_data_small, paste0(data_path, "tabular/sheila_generated/roc_analysis/roc_no_sub_calcs_data_small.csv"))


# ---- 12. roc analysis for each cmu and valid period (season subgroup) ----
# make an empty dataframe
roc_sub_calcs_data <- NULL

# unique cmu values with their rainfall depths
cmu_info_unique <- roc_data_final %>%
  dplyr::ungroup() %>%
  dplyr::select(cmu_name, rain_in, station_count_day) %>%
  dplyr::group_by(cmu_name, rain_in) %>%
  dplyr::summarize(station_count_day_min = min(station_count_day, na.rm = TRUE),
                   station_count_day_max = max(station_count_day, na.rm = TRUE),
                   station_count_day_avg = mean(station_count_day, na.rm = TRUE))

# number of cmus
num_cmus <- length(cmu_info_unique$cmu_name)
  
# number of valid periods
num_valid_periods <- length(unique(roc_data$valid_period_hrs))

# valid period values
valid_period_list <- unique(roc_data$valid_period_hrs)

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
    temp_roc_data <- roc_data_final %>%
      dplyr::filter(cmu_name == temp_cmu & valid_period_hrs == temp_valid_period)
    
    # bootstrapped cutpoint results with season subgroup
    # youden's j metric
    set.seed(100)
    temp_result_sub_youden_raw <- cutpointr::cutpointr(data = temp_roc_data, x = cmu_closure_perc, class = precip_binary, subgroup = month_type,
                                                   direction = ">=", pos_class = 1, neg_class = 0,
                                                   boot_runs = num_boot_runs, boot_stratify = TRUE,
                                                   method = maximize_metric, metric = youden, silent = TRUE)
    temp_result_sub_youden_summary <- summary(temp_result_sub_youden_raw) %>%
      dplyr::select(n_obs:n_neg)
    temp_result_sub_youden <- temp_result_sub_youden_raw %>%
      dplyr::select(- grouping) %>%
      dplyr::mutate(metric = "youden",
                    metric_value = youden,
                    cmu_name = temp_cmu,
                    valid_period_hrs = temp_valid_period,
                    n_obs = temp_result_sub_youden_summary$n_obs,
                    n_pos = temp_result_sub_youden_summary$n_pos,
                    n_neg = temp_result_sub_youden_summary$n_neg) %>%
      dplyr::select(- youden)
    
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
    temp_roc_calcs_data <- bind_rows(temp_result_sub_youden, temp_result_sub_acc, temp_result_sub_cohens)
    
    # advance counter to assign run number
    run_num <- run_num + 1
    
    # final dataset with run number id joined
    temp_roc_calcs_data_to_join <- temp_roc_calcs_data %>%
      dplyr::mutate(run_num_id = rep(run_num, dim(temp_roc_calcs_data)[1])) %>%
      dplyr::select(run_num_id, subgroup:n_neg)
    
    # append data
    roc_sub_calcs_data <- bind_rows(roc_sub_calcs_data, temp_roc_calcs_data_to_join)
    
    # print message
    print(paste0("appended cmu ", temp_cmu, " ", temp_valid_period, " hr valid period roc results"))
  }
}

# print time now
stop_time <- now()

# time to run loop
stop_time - start_time
# ~2 min x 91 cmu's x 3 valid periods = 546 min / 60 = 9.1 hrs
# actual: ? hours

# export
saveRDS(roc_sub_calcs_data, file = paste0(data_path, "tabular/sheila_generated/roc_analysis/roc_sub_calcs_data.rds"))

# save a copy of the file without the nested tibbles (data, boot, and roc_curve columns)
roc_sub_calcs_data_small <- roc_sub_calcs_data %>%
  dplyr::select(run_num_id:predictor, metric:n_neg)

# export
write_csv(roc_sub_calcs_data_small, paste0(data_path, "tabular/sheila_generated/roc_analysis/roc_sub_calcs_data_small.csv"))


# ---- plots ----
# change inf to 100
roc_no_sub_calcs_data_small_fix <- roc_no_sub_calcs_data_small %>%
  dplyr::mutate(optimal_cutpoint_fix = if_else(optimal_cutpoint == Inf, 100, optimal_cutpoint),
                cutpoint_type = if_else(optimal_cutpoint == Inf, "infinite", "definite")) %>%
  dplyr::left_join(cmu_rain_thresh_key, by = "cmu_name")

# cutpoint vs valid period (for definite cases)
ggplot(data = roc_no_sub_calcs_data_small_fix %>% filter(cutpoint_type == "definite")) +
  geom_boxplot(aes(x = as.factor(valid_period_hrs), y = optimal_cutpoint_fix, fill = metric)) +
  geom_jitter(aes(x = as.factor(valid_period_hrs), y = optimal_cutpoint_fix), alpha = 0.25, width = 0.1) +
  facet_wrap(~ metric) +
  labs(x = "NDFD Valid Period (hours)", y = "Percent Chance of Closure Cutpoint (%)", fill = "Metric") +
  theme_classic()

# cutpoint vs valid period (for infinite cases)
# summarize number of cutpoint cases
roc_no_sub_calcs_data_inf_summary_by_period <- roc_no_sub_calcs_data_small_fix %>% 
  dplyr::filter(cutpoint_type == "infinite") %>%
  dplyr::ungroup() %>%
  dplyr::group_by(valid_period_hrs, metric) %>%
  dplyr::summarize(case_count = n())

ggplot(data = roc_no_sub_calcs_data_inf_summary_by_period) +
  geom_col(aes(x = as.factor(valid_period_hrs), y = case_count, fill = metric)) +
  labs(x = "NDFD Valid Period (hours)", y = "Number of Infinite Cutpoint Cases", fill = "Metric") +
  facet_wrap(~ metric) +
  theme_classic()


# cutpoint vs rainfall threshold (for definite cases)
ggplot(data = roc_no_sub_calcs_data_small_fix %>% filter(cutpoint_type == "definite")) +
  geom_boxplot(aes(x = as.factor(rain_in), y = optimal_cutpoint_fix, fill = metric)) +
  geom_jitter(aes(x = as.factor(rain_in), y = optimal_cutpoint_fix), alpha = 0.25, width = 0.1) +
  facet_wrap(~ metric) +
  labs(x = "Rainfall Threshold (in)", y = "Percent Chance of Closure Cutpoint (%)", fill = "Metric") +
  theme_classic()

# cutpoint vs rainfall threshold (for infinite cases)
# summarize number of cutpoint cases
roc_no_sub_calcs_data_inf_summary_by_rain <- roc_no_sub_calcs_data_small_fix %>% 
  filter(cutpoint_type == "infinite") %>%
  ungroup() %>%
  group_by(rain_in, metric) %>%
  summarize(case_count = n())

ggplot(data = roc_no_sub_calcs_data_inf_summary_by_rain) +
  geom_col(aes(x = as.factor(rain_in), y = case_count, fill = metric)) +
  labs(x = "Rainfall Threshold (in)", y = "Number of Infinite Cutpoint Cases", fill = "Metric") +
  facet_wrap(~ metric) +
  theme_classic()


# metric vs accuracy by valid period (cohen's kappa only)
ggplot(data = roc_no_sub_calcs_data_small_fix %>% filter(cutpoint_type == "definite" & metric == "cohens_kappa")) +
  geom_point(aes(x = metric_value, y = acc, color = as.factor(valid_period_hrs))) +
  # facet_wrap(~ metric) +
  labs(x = "Cohen's Kappa", y = "Accuracy", color = "Valid Period Hours") +
  theme_classic()
# what does this look like by season?

# metric vs accuracy by rainfall threshold (cohen's kappa only)
ggplot(data = roc_no_sub_calcs_data_small_fix %>% filter(cutpoint_type == "definite" & metric == "cohens_kappa")) +
  geom_point(aes(x = metric_value, y = acc, color = as.factor(rain_in))) +
  # facet_wrap(~ metric) +
  labs(x = "Cohen's Kappa", y = "Accuracy", color = "Rainfall Threshold (in)") +
  theme_classic()
# what does this look like by season?


# change inf to 100
roc_no_sub_calcs_data_small_fix <- roc_no_sub_calcs_data_small %>%
  dplyr::mutate(optimal_cutpoint_fix = if_else(optimal_cutpoint == Inf, 100, optimal_cutpoint),
                cutpoint_type = if_else(optimal_cutpoint == Inf, "infinite", "definite")) %>%
  dplyr::left_join(cmu_rain_thresh_key, by = "cmu_name")

# cutpoint vs valid period (for definite cases)
ggplot(data = roc_no_sub_calcs_data_small_fix %>% filter(cutpoint_type == "definite")) +
  geom_boxplot(aes(x = as.factor(valid_period_hrs), y = optimal_cutpoint_fix, fill = metric)) +
  geom_jitter(aes(x = as.factor(valid_period_hrs), y = optimal_cutpoint_fix), alpha = 0.25, width = 0.1) +
  facet_wrap(~ metric) +
  labs(x = "NDFD Valid Period (hours)", y = "Percent Chance of Closure Cutpoint (%)", fill = "Metric") +
  theme_classic()


# ---- 13. look at some of the results ----
# density plot for lowest Youden stat (U144, 48 hrs)
low_performance_roc_data <- roc_calcs_data %>%
  dplyr::filter(cmu_name == "U144" & valid_period_hrs == 48) %>%
  dplyr::select(data_list) %>%
  unnest(data_list)

# save cutpoint
low_performance_cutpoint <- roc_calcs_data %>%
  dplyr::filter(cmu_name == "U144" & valid_period_hrs == 48) %>%
  dplyr::select(cutpoint_cutpointr)

# plot low performance result
ggplot(data = low_performance_roc_data) +
  geom_density(aes(x = cmu_qpf_in, fill = as.factor(precip_binary))) +
  geom_vline(xintercept = low_performance_cutpoint$cutpoint_cutpointr) +
  facet_wrap(~as.factor(precip_binary), scales = "free_y") +
  theme_classic()

# roc curve data
low_performance_roc_curve <- roc_calcs_data %>%
  dplyr::filter(cmu_name == "U144" & valid_period_hrs == 72) %>%
  dplyr::select(roc_curve_list) %>%
  unnest(roc_curve_list)

# plot roc curve data
ggplot(data = low_performance_roc_curve) +
  geom_line(aes(x = fpr, y = tpr)) +
  geom_point(aes(x = fpr, y = tpr)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  labs(x = "False Positive Rate (1-Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_classic()


# density plot for highest Youden stat (U144, 24 hrs)
high_performance_roc_data <- roc_calcs_data %>%
  dplyr::filter(cmu_name == "U144" & valid_period_hrs == 24) %>%
  dplyr::select(data_list) %>%
  unnest(data_list)

# save cutpoint
high_performance_cutpoint <- roc_calcs_data %>%
  dplyr::filter(cmu_name == "U144" & valid_period_hrs == 24) %>%
  dplyr::select(cutpoint_cutpointr)

# plot high performance result
ggplot(data = high_performance_roc_data) +
  geom_density(aes(x = cmu_qpf_in, fill = as.factor(precip_binary))) +
  geom_vline(xintercept = high_performance_cutpoint$cutpoint_cutpointr) +
  facet_wrap(~as.factor(precip_binary), scales = "free_y") +
  theme_classic()

# roc curve data
high_performance_roc_curve <- roc_calcs_data %>%
  dplyr::filter(cmu_name == "U144" & valid_period_hrs == 24) %>%
  dplyr::select(roc_curve_list) %>%
  unnest(roc_curve_list)

# plot roc curve data
ggplot(data = high_performance_roc_curve) +
  geom_line(aes(x = fpr, y = tpr)) +
  geom_point(aes(x = fpr, y = tpr)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  labs(x = "False Positive Rate (1-Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_classic()



# ---- 14. export data ----

# ---- test bootstrap ----

# U022 has 8 stations (1 in)
# U133 has 1 station (1 in)

# try U022 first
roc_data_sel <- roc_data %>%
  dplyr::filter(cmu_name == "U022" & valid_period_hrs == 24) %>%
  na.omit()

# no bootstrap
result_nb <- cutpointr::cutpointr(data = roc_data_sel, x = cmu_avg_closure_perc, class = precip_binary, 
                                  direction = ">=", pos_class = 1, neg_class = 0, 
                                  method = maximize_metric, metric = youden)
summary(result_nb)
plot_cutpointr(result_nb, xvar = cutpoint, yvar = tp, conf_lvl = 0.95) + geom_point()
plot_metric(result_nb)

# bootstrap
set.seed(100)
result_b <- cutpointr::cutpointr(data = roc_data_sel, x = cmu_avg_closure_perc, class = precip_binary, 
                                 direction = ">=", pos_class = 1, neg_class = 0, 
                                 boot_runs = 100, 
                                 method = maximize_metric, metric = youden)
summary(result_b)
# _b = in-bag results
# _oob = out of bag results
plot_cutpointr(result_b, xvar = cutpoint, yvar = tp, conf_lvl = 0.9) + geom_point()
plot_metric(result_b, conf_lvl = 0.95) # doesn't work see "accessing data, roc_curve, and boot"
# whatever you put as the metric in the cutpointr function this is what you can plot here

# result_nb and result_b are very similar

# bootstrap with month
# cool month vs warm month? (cool = Oct - Mar, warm = Apr - Sep)
# use maximize_boot_metric when you have a subgroup (i.e., month)
# use summary_func = <function> when you use maximize_boot_metric
set.seed(100)
result_b_season <- cutpointr::cutpointr(data = roc_data_sel, x = cmu_avg_closure_perc, class = precip_binary, subgroup = month_type,
                                        direction = ">=", pos_class = 1, neg_class = 0, 
                                        boot_runs = 100,
                                        method = maximize_metric, metric = youden) %>%
  dplyr::mutate(subgroup = forcats::fct_relevel(subgroup, "warm", "cool"))
summary(result_b_season)
# _b = in-bag results
# _oob = out of bag results
plot_cutpointr(result_b_season, xvar = cutpoint, yvar = tp, conf_lvl = 0.95) + geom_point()
plot_metric(result_b_season, conf_lvl = 0.95)
plot_roc(result_b_season)
plot_sensitivity_specificity(result_b_season)
plot_precision_recall(result_b_season)
plot_metric_boot(result_b_season)


# ---- test bootstrapping proof of concept ----
# get one cmu with the most stations for each rainfall threshold
roc_data_sel_key <- roc_data %>%
  dplyr::select(loc_id, cmu_name, rain_in) %>%
  dplyr::distinct() %>%
  dplyr::group_by(cmu_name, rain_in) %>%
  dplyr::summarize(station_count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(rain_in) %>%
  dplyr::filter(station_count == max(station_count)) %>%
  dplyr::filter(cmu_name != "U024" & cmu_name != "U025") %>% # delete replicates for 3 in for now
  dplyr::ungroup() %>%
  dplyr::select(cmu_name) # just keep name for joining

# select data for proof of concept
roc_data_sel <- roc_data %>%
  dplyr::right_join(roc_data_sel_key, by = "cmu_name")
# unique(roc_data_sel$cmu_name)




# ---- testing and old code ----

calc_prob_closure <- function(pop, thresh, qpf) {
  prob_closure <- (pop/100) * exp(-(thresh/qpf))
  return(prob_closure)
}

my_pop <- seq(0, 100, 1)
my_thresh <- 3
my_qpf <- seq(0.1, 8, 0.1)

output_1 <- calc_prob_closure(pop = 100, thresh = my_thresh, qpf = my_qpf)
df <- data.frame(qpf = my_qpf, output = output_1)
plot(x = my_qpf, y = output_1)

# turn closure_perc to 0 (control: no closure) or 1 (case: closure) based on CMU threshold
# ndfd_data_binary <- ndfd_data_sel_join %>%
#   dplyr::mutate(loc_closure_perc_binary = if_else(loc_qpf_in >= rain_in, 1, 0),
#                 cmu_closure_perc_binary = if_else(cmu_qpf_in >= rain_in, 1, 0))

# make an empty dataframe
# roc_calcs_by_month_data <- tibble(cmu_name = as.character(),
#                                   rain_in = as.numeric(),
#                                   month_num = as.numeric(),
#                                   num_obs_stations = as.numeric(),
#                                   valid_period_hrs = as.numeric(),
#                                   auc = as.numeric(),
#                                   youden_j = as.numeric(),
#                                   accuracy = as.numeric(),
#                                   # cutpoint_j = as.numeric(),
#                                   cutpoint_cutpointr = as.numeric(),
#                                   err_decimal_perc = as.numeric(),
#                                   cutpoint_tpr_sens = as.numeric(),
#                                   cutpoint_tnr_spec = as.numeric(),
#                                   num_controls = as.numeric(),
#                                   num_cases = as.numeric(),
#                                   num_total_obs = as.numeric(),
#                                   roc_curve_list = list())
# 
# # unique cmu values with their rainfall depths
# cmu_info_unique <- roc_data %>%
#   dplyr::group_by(cmu_name, rain_in) %>%
#   dplyr::summarize(num_obs_stations = length(unique(loc_id))) # %>% na.omit()
# 
# # number of cmus
# num_cmus <- length(cmu_info_unique$cmu_name)
# 
# # number of valid periods
# num_valid_periods <- length(unique(roc_data$valid_period_hrs))
# 
# # valid period values
# valid_period_list <- unique(roc_data$valid_period_hrs)
# 
# # loop
# for (i in 1:num_cmus) { # i = cmu
#   # record cmu
#   temp_cmu <- cmu_info_unique$cmu_name[i]
#   
#   # record rainfall depth
#   temp_cmu_rain_in <- cmu_info_unique$rain_in[i]
#   
#   # record number of stations for comparison
#   temp_num_obs_stations = cmu_info_unique$num_obs_stations[i]
#   
#   for (j in 1:num_valid_periods) { # j = valid_period_hr
#     # pick valid period
#     temp_valid_period <- valid_period_list[j]
#     
#     # filter data
#     temp_data <- roc_data %>%
#       dplyr::filter((cmu_name == temp_cmu) & (valid_period_hrs == temp_valid_period))
#     
#     # roc object
#     # temp_roc <- pROC::roc(temp_data$cmu_qpf_binary, temp_data$precip_in, na.rm = TRUE)
#     # names(temp_roc)
#     # plot(temp_roc, print.thres = "best", print.thres.best.method = "youden")
#     # plot(temp_roc, print.thres = "best", print.thres.best.method = "closest.topleft")
#     # these plots are both the same....???
#     
#     # cutpointr object
#     # temp_cp <- cutpointr::cutpointr(data = temp_data, x = precip_in, class = cmu_qpf_binary, 
#     #                                 direction = ">=", pos_class = 1, neg_class = 0, 
#     #                                 method = maximize_metric, metric = youden, na.rm = TRUE)
#     
#     # use method = oc_youden_kernal here and add in month_num column?
#     temp_cp <- cutpointr::cutpointr(data = temp_data, x = precip_in, class = cmu_qpf_binary, subgroup = month_num,
#                                     direction = ">=", pos_class = 1, neg_class = 0,
#                                     method = oc_youden_kernel, na.rm = TRUE)
#     # this isn't working....
#     
#     # summary(temp_cp)
#     # plot(temp_cp)
#     
#     # auc
#     # temp_auc <- as.numeric(temp_roc$auc)
#     temp_auc <- temp_cp$AUC
#     
#     # youden j stat
#     temp_youden_j <- temp_cp$youden
#     
#     # accuracy (= fraction correctly classified)
#     temp_accuracy = temp_cp$acc
#     
#     # cutpoint ( = threshold that maximizes tpr and minimizes fpr)
#     # temp_cutpoint_j <- as.numeric(pROC::coords(temp_roc, "best", ret = "threshold", best.method = "youden"))
#     temp_cutpoint_cutpointr <- temp_cp$optimal_cutpoint
#     
#     # percent error between depth and cutpoint
#     temp_err_decimal_perc <- abs(temp_cmu_rain_in - temp_cutpoint_cutpointr) / temp_cmu_rain_in
#     
#     # cutpoint tpr
#     # temp_cutpoint_tpr_sens <- as.numeric(pROC::coords(temp_roc, "best", ret = "sensitivity", best.method = "youden"))
#     temp_cutpoint_tpr_sens <- temp_cp$sensitivity
#     
#     # cutpoint tnr
#     # temp_cutpoint_tnr_spec <- as.numeric(pROC::coords(temp_roc, "best", ret = "specificity", best.method = "youden"))
#     temp_cutpoint_tnr_spec <- temp_cp$specificity
#     
#     # number of controls and cases
#     # temp_num_controls <- length(temp_roc$controls)
#     # temp_num_cases <- length(temp_roc$cases)
#     temp_cp_summary <- summary(temp_cp)
#     temp_num_controls <- temp_cp_summary$n_neg
#     temp_num_cases <- temp_cp_summary$n_pos
#     temp_num_obs <- temp_cp_summary$n_obs
#     
#     # roc curve output list
#     temp_roc_curve_list <- temp_cp$roc_curve
#     
#     # fill in data frame
#     temp_roc_calcs_by_month_data <- tibble(cmu_name = temp_cmu,
#                                            rain_in = temp_cmu_rain_in,
#                                            month_num = temp_month_num,
#                                            num_obs_stations = temp_num_obs_stations,
#                                            valid_period_hrs = temp_valid_period,
#                                            auc = temp_auc,
#                                            youden_j = temp_youden_j,
#                                            accuracy = temp_accuracy,
#                                            # cutpoint_j = temp_cutpoint_j,
#                                            cutpoint_cutpointr = temp_cutpoint_cutpointr,
#                                            err_decimal_perc = temp_err_decimal_perc,
#                                            cutpoint_tpr_sens = temp_cutpoint_tpr_sens,
#                                            cutpoint_tnr_spec = temp_cutpoint_tnr_spec,
#                                            num_controls = temp_num_controls,
#                                            num_cases = temp_num_cases,
#                                            num_total_obs = temp_num_obs,
#                                            roc_curve_list = temp_roc_curve_list)
#     
#     # bind rows
#     roc_calcs_by_month_data <-  dplyr::bind_rows(roc_calcs_by_month_data, temp_roc_calcs_by_month_data)
#   }
# }


                                                                                                                                                                    