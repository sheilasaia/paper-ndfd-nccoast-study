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
# value of 0 means test us useless because it gives the same proportion of positive results for cases and contorls
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

# roc analysis
# https://acutecaretesting.org/en/articles/roc-curves-what-are-they-and-how-are-they-used
# https://www.youtube.com/watch?v=4jRBRDbJemM
# Youden and Closest Top Left https://academic.oup.com/aje/article/163/7/670/77813


# auc
# use this to compare tests, 1 indicates perfect fit and below 0.5 indicates a test failure
# 0.9 - 1 (very good), 0.8 - 0.9 (good), 0.8 - 0.7 (fair), 0.7 - 0.6 (poor), 0 - 0.6 (fail)

# ---- to do ----
# to do list


# ---- 1. load libraries ----
library(tidyverse)
library(sf)
library(here)
library(tidylog)
library(lubridate)
library(cutpointr)
library(pROC)


# ---- 2. define paths ----
# data path (for now --> use here package later)
data_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/"

# path to ndfd tabular inputs
ndfd_sco_tabular_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/ndfd_sco_hist_raw/"

# path to ndfd spatial inputs
ndfd_sco_spatial_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/ndfd_sco_hist/"


# ---- 3. load data ----
# ndfd selected data
ndfd_data_sel <- read_csv(paste0(data_path, "tabular/sheila_generated/ndfd_sco_hist/ndfd_data_sel.csv"), col_names = TRUE)

# precip metadata (that overlaps and is 90% complete)
hist_precip_metadata_albers_sel <- st_read(paste0(data_path, "spatial/sheila_generated/hist_precip_data/hist_precip_metadata_albers_sel.shp"))

# import historic precip data (tabular)
hist_precip_data <- read_csv(file = paste0(data_path, "tabular/sheila_generated/hist_precip_data/hist_precip_data_compiled.csv"), col_names = TRUE,
                             col_types = list(col_character(), col_date(), col_number()))


# ---- 4. roc curve analysis ----
# select metadata columns to join
loc_metadata_to_join <- hist_precip_metadata_albers_sel %>%
  st_drop_geometry() %>%
  dplyr::select(loc_id, cmu_name, rain_in)

# join to ndfd data
ndfd_data_sel_join <- ndfd_data_sel %>%
  # filter(loc_id %in% c(as.character(unique(hist_precip_metadata_sel$loc_id)))) %>% # don't need this once rerun ndfd_data_sel
  dplyr::left_join(loc_metadata_to_join, by = c("loc_id", "cmu_name"))

# turn qpf to 0 (control: no closure) or 1 (case: closure) based on CMU treshhold
ndfd_data_binary <- ndfd_data_sel_join %>%
  dplyr::mutate(loc_qpf_binary = if_else(loc_qpf_in >= rain_in, 1, 0),
                cmu_qpf_binary = if_else(cmu_qpf_in >= rain_in, 1, 0))

# select observations to join
loc_data_to_join <- hist_precip_data %>%
  dplyr::mutate(loc_id = as.character(loc_id)) %>%
  dplyr::select(loc_id, date, precip_in)

# join observations
roc_data <- ndfd_data_binary %>%
  dplyr::left_join(loc_data_to_join, by = c("loc_id", "date")) %>%
  dplyr::mutate(precip_binary = if_else(precip_in >= rain_in, 1, 0),
                month_num = as.numeric(month(date)))


# ---- 5. loop through calcs to make an roc analysis table ----
# make an empty dataframe
roc_calcs_data <- tibble(cmu_name = as.character(),
                         rain_in = as.numeric(),
                         num_obs_stations = as.numeric(),
                         valid_period_hrs = as.numeric(),
                         auc = as.numeric(),
                         youden_j = as.numeric(),
                         accuracy = as.numeric(),
                         # cutpoint_j = as.numeric(),
                         cutpoint_cutpointr = as.numeric(),
                         err_decimal_perc = as.numeric(),
                         cutpoint_tpr_sens = as.numeric(),
                         cutpoint_tnr_spec = as.numeric(),
                         num_controls = as.numeric(),
                         num_cases = as.numeric(),
                         num_total_obs = as.numeric(),
                         roc_curve_list = list())

# unique cmu values with their rainfall depths
cmu_info_unique <- roc_data %>%
  dplyr::group_by(cmu_name, rain_in) %>%
  dplyr::summarize(num_obs_stations = length(unique(loc_id))) # %>% na.omit()

# number of cmus
num_cmus <- length(cmu_info_unique$cmu_name)
  
# number of valid periods
num_valid_periods <- length(unique(roc_data$valid_period_hrs))

# valid period values
valid_period_list <- unique(roc_data$valid_period_hrs)

# loop
for (i in 1:num_cmus) { # i = cmu_name
  # pick cmu
  temp_cmu <- cmu_info_unique$cmu_name[i]
  
  # record rainfall depth
  temp_cmu_rain_in <- cmu_info_unique$rain_in[i]
  
  # number of stations for comparison
  temp_num_obs_stations = cmu_info_unique$num_obs_stations[i]
  
  for (j in 1:num_valid_periods) { # j = valid_period_hr
    # pick valid period
    temp_valid_period <- valid_period_list[j]
    
    # filter data
    temp_data <- roc_data %>%
      dplyr::filter(cmu_name == temp_cmu & valid_period_hrs == temp_valid_period)
    
    # roc object
    # temp_roc <- pROC::roc(temp_data$cmu_qpf_binary, temp_data$precip_in, na.rm = TRUE)
    # names(temp_roc)
    # plot(temp_roc, print.thres = "best", print.thres.best.method = "youden")
    # plot(temp_roc, print.thres = "best", print.thres.best.method = "closest.topleft")
    # these plots are both the same....???
    
    # cutpointr object
    temp_cp <- cutpointr::cutpointr(data = temp_data, x = precip_in, class = cmu_qpf_binary, 
                                    direction = ">=", pos_class = 1, neg_class = 0, 
                                    method = maximize_metric, metric = youden, na.rm = TRUE)
    # summary(temp_cp)
    # plot(temp_cp)
    
    # auc
    # temp_auc <- as.numeric(temp_roc$auc)
    temp_auc <- temp_cp$AUC
    
    # youden j stat
    temp_youden_j <- temp_cp$youden
    
    # accuracy (= fraction correctly classified)
    temp_accuracy = temp_cp$acc
    
    # cutpoint ( = threshold that maximizes tpr and minimizes fpr)
    # temp_cutpoint_j <- as.numeric(pROC::coords(temp_roc, "best", ret = "threshold", best.method = "youden"))
    temp_cutpoint_cutpointr <- temp_cp$optimal_cutpoint
    
    # percent error between depth and cutpoint
    temp_err_decimal_perc <- abs(temp_cmu_rain_in - temp_cutpoint_cutpointr) / temp_cmu_rain_in
      
    # cutpoint tpr
    # temp_cutpoint_tpr_sens <- as.numeric(pROC::coords(temp_roc, "best", ret = "sensitivity", best.method = "youden"))
    temp_cutpoint_tpr_sens <- temp_cp$sensitivity
    
    # cutpoint tnr
    # temp_cutpoint_tnr_spec <- as.numeric(pROC::coords(temp_roc, "best", ret = "specificity", best.method = "youden"))
    temp_cutpoint_tnr_spec <- temp_cp$specificity
    
    # number of controls and cases
    # temp_num_controls <- length(temp_roc$controls)
    # temp_num_cases <- length(temp_roc$cases)
    temp_cp_summary <- summary(temp_cp)
    temp_num_controls <- temp_cp_summary$n_neg
    temp_num_cases <- temp_cp_summary$n_pos
    temp_num_obs <- temp_cp_summary$n_obs
    
    # roc curve output list
    temp_roc_curve_list <- temp_cp$roc_curve
    
    # fill in data frame
    temp_roc_calcs_data <- tibble(cmu_name = temp_cmu,
                                  rain_in = temp_cmu_rain_in,
                                  num_obs_stations = temp_num_obs_stations,
                                  valid_period_hrs = temp_valid_period,
                                  auc = temp_auc,
                                  youden_j = temp_youden_j,
                                  accuracy = temp_accuracy,
                                  # cutpoint_j = temp_cutpoint_j,
                                  cutpoint_cutpointr = temp_cutpoint_cutpointr,
                                  err_decimal_perc = temp_err_decimal_perc,
                                  cutpoint_tpr_sens = temp_cutpoint_tpr_sens,
                                  cutpoint_tnr_spec = temp_cutpoint_tnr_spec,
                                  num_controls = temp_num_controls,
                                  num_cases = temp_num_cases,
                                  num_total_obs = temp_num_obs,
                                  roc_curve_list = temp_roc_curve_list)
    
    # bind rows
    roc_calcs_data <-  dplyr::bind_rows(roc_calcs_data, temp_roc_calcs_data)
  }
}


# ---- 5. loop through calcs to make an roc analysis table by month ----
# make an empty dataframe
roc_calcs_by_month_data <- tibble(cmu_name = as.character(),
                                  rain_in = as.numeric(),
                                  month_num = as.numeric(),
                                  num_obs_stations = as.numeric(),
                                  valid_period_hrs = as.numeric(),
                                  auc = as.numeric(),
                                  youden_j = as.numeric(),
                                  accuracy = as.numeric(),
                                  # cutpoint_j = as.numeric(),
                                  cutpoint_cutpointr = as.numeric(),
                                  err_decimal_perc = as.numeric(),
                                  cutpoint_tpr_sens = as.numeric(),
                                  cutpoint_tnr_spec = as.numeric(),
                                  num_controls = as.numeric(),
                                  num_cases = as.numeric(),
                                  num_total_obs = as.numeric(),
                                  roc_curve_list = list())

# unique cmu values with their rainfall depths
cmu_info_unique <- roc_data %>%
  dplyr::group_by(cmu_name, rain_in) %>%
  dplyr::summarize(num_obs_stations = length(unique(loc_id))) # %>% na.omit()

# number of cmus
num_cmus <- length(cmu_info_unique$cmu_name)

# number of valid periods
num_valid_periods <- length(unique(roc_data$valid_period_hrs))

# valid period values
valid_period_list <- unique(roc_data$valid_period_hrs)

# loop
for (i in 1:num_cmus) { # i = cmu
  # record cmu
  temp_cmu <- cmu_info_unique$cmu_name[i]
  
  # record rainfall depth
  temp_cmu_rain_in <- cmu_info_unique$rain_in[i]
  
  # record number of stations for comparison
  temp_num_obs_stations = cmu_info_unique$num_obs_stations[i]
  
  for (j in 1:num_valid_periods) { # j = valid_period_hr
    # pick valid period
    temp_valid_period <- valid_period_list[j]
    
    # filter data
    temp_data <- roc_data %>%
      dplyr::filter((cmu_name == temp_cmu) & (valid_period_hrs == temp_valid_period))
    
    # roc object
    # temp_roc <- pROC::roc(temp_data$cmu_qpf_binary, temp_data$precip_in, na.rm = TRUE)
    # names(temp_roc)
    # plot(temp_roc, print.thres = "best", print.thres.best.method = "youden")
    # plot(temp_roc, print.thres = "best", print.thres.best.method = "closest.topleft")
    # these plots are both the same....???
    
    # cutpointr object
    # temp_cp <- cutpointr::cutpointr(data = temp_data, x = precip_in, class = cmu_qpf_binary, 
    #                                 direction = ">=", pos_class = 1, neg_class = 0, 
    #                                 method = maximize_metric, metric = youden, na.rm = TRUE)
    
    # use method = oc_youden_kernal here and add in month_num column?
    temp_cp <- cutpointr::cutpointr(data = temp_data, x = precip_in, class = cmu_qpf_binary, subgroup = month_num,
                                    direction = ">=", pos_class = 1, neg_class = 0,
                                    method = oc_youden_kernel, na.rm = TRUE)
    # this isn't working....
    
    # summary(temp_cp)
    # plot(temp_cp)
    
    # auc
    # temp_auc <- as.numeric(temp_roc$auc)
    temp_auc <- temp_cp$AUC
    
    # youden j stat
    temp_youden_j <- temp_cp$youden
    
    # accuracy (= fraction correctly classified)
    temp_accuracy = temp_cp$acc
    
    # cutpoint ( = threshold that maximizes tpr and minimizes fpr)
    # temp_cutpoint_j <- as.numeric(pROC::coords(temp_roc, "best", ret = "threshold", best.method = "youden"))
    temp_cutpoint_cutpointr <- temp_cp$optimal_cutpoint
    
    # percent error between depth and cutpoint
    temp_err_decimal_perc <- abs(temp_cmu_rain_in - temp_cutpoint_cutpointr) / temp_cmu_rain_in
    
    # cutpoint tpr
    # temp_cutpoint_tpr_sens <- as.numeric(pROC::coords(temp_roc, "best", ret = "sensitivity", best.method = "youden"))
    temp_cutpoint_tpr_sens <- temp_cp$sensitivity
    
    # cutpoint tnr
    # temp_cutpoint_tnr_spec <- as.numeric(pROC::coords(temp_roc, "best", ret = "specificity", best.method = "youden"))
    temp_cutpoint_tnr_spec <- temp_cp$specificity
    
    # number of controls and cases
    # temp_num_controls <- length(temp_roc$controls)
    # temp_num_cases <- length(temp_roc$cases)
    temp_cp_summary <- summary(temp_cp)
    temp_num_controls <- temp_cp_summary$n_neg
    temp_num_cases <- temp_cp_summary$n_pos
    temp_num_obs <- temp_cp_summary$n_obs
    
    # roc curve output list
    temp_roc_curve_list <- temp_cp$roc_curve
    
    # fill in data frame
    temp_roc_calcs_by_month_data <- tibble(cmu_name = temp_cmu,
                                           rain_in = temp_cmu_rain_in,
                                           month_num = temp_month_num,
                                           num_obs_stations = temp_num_obs_stations,
                                           valid_period_hrs = temp_valid_period,
                                           auc = temp_auc,
                                           youden_j = temp_youden_j,
                                           accuracy = temp_accuracy,
                                           # cutpoint_j = temp_cutpoint_j,
                                           cutpoint_cutpointr = temp_cutpoint_cutpointr,
                                           err_decimal_perc = temp_err_decimal_perc,
                                           cutpoint_tpr_sens = temp_cutpoint_tpr_sens,
                                           cutpoint_tnr_spec = temp_cutpoint_tnr_spec,
                                           num_controls = temp_num_controls,
                                           num_cases = temp_num_cases,
                                           num_total_obs = temp_num_obs,
                                           roc_curve_list = temp_roc_curve_list)
    
    # bind rows
    roc_calcs_by_month_data <-  dplyr::bind_rows(roc_calcs_by_month_data, temp_roc_calcs_by_month_data)
  }
}


# ---- 6. export data ----




