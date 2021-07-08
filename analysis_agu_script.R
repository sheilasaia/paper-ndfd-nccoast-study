# ---- script header ----
# script name: compare_data_script.R
# purpose of script: compare observed historic precip data to forecasted ndfd
# author: sheila
# date created: 20201119
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:
 

# ---- to do ----
# to do list

# TODO fix hist_precip_metadata_county_based_coast_albers so loc_id is not factor (need to go back to convert_hist_precip_tabular_to_spatial.R)
# TODO take out stations that don't have complete datasets?
# TODO add 30-year normals (are 2015 and 2016 above/below average?)
# TODO check hist_precip_metadata_county_based_coast_albers data (see issue #?)


# ---- 1. load libraries and set paths ----
library(tidyverse)
library(lubridate)
library(sf)
library(caret)
library(broom)
library(here)
library(tidylog)


# ---- 2. define paths and projections ----
# spatial data path
hist_precip_spatial_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/hist_precip_data/"

# historic tabular data path
hist_precip_tabular_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/hist_precip_data/"

# path to ndfd tabular data
ndfd_tabular_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/ndfd_sco_hist/"

# path to figure
figure_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/results/agu_2020/figures/"


# ---- 3. load in data ----
# historic precip spatial metadata (county based)
hist_precip_metadata_county_based_coast_albers <- st_read(paste0(hist_precip_spatial_data_input_path, "county_based/hist_precip_metadata_coast_albers.shp"))

# historic precip spatial metadata (watershed based)


# historic precip spatial metadata (shoreline based)


# historic precip tabular data
hist_precip_data <- read_csv(paste0(hist_precip_tabular_data_input_path, "hist_precip_data_compiled.csv"))

# ndfd data (county based)
ndfd_calcs_county_based_data <- read_csv(paste0(ndfd_tabular_data_input_path, "ndfd_calcs_county_based_data.csv"))

# ndfd data (ws based)


# ndfd data (shoreline based)


# define epsg and proj for CONUS Albers projection (projecting to this)
conus_albers_epsg <- 5070
conus_albers_proj <- "+init=EPSG:5070"


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

# calculate slope
# calculate_lm_coeffs <- function(obs_data, frcst_data) {
#   # obs_data is a list of observed data
#   # frcst_data is a list of forecased (or simulated or modeled) data
#   
#   # fit linear model with obs vs frcst
#   temp_lm <- lm(obs_data ~ frcst_data)
#   
#   # save intercept
#   intercept <- round(as.numeric(temp_lm$coefficients[1]), 3)
#   
#   # save slope
#   slope <- round(as.numeric(temp_lm$coefficients[2]), 3)
#   
#   # return outputs
#   return(c(intercept, slope))
# }

# calculate event status
# calculate_event_status <- function(obs_data, frcst_data) {
#   # obs_data is a list of observed data
#   # frcst_data is a list of forecased (or simulated or modeled) data
#   
# }


# ---- 5. county based: join hist precip metadata and data ----
# get number of unique sites
# length(unique(hist_precip_metadata_county_based_coast_albers$loc_id[hist_precip_metadata_county_based_coast_albers$cmb_class == "urban_coast"]))
# length(unique(hist_precip_metadata_county_based_coast_albers$loc_id[hist_precip_metadata_county_based_coast_albers$cmb_class == "non-urban_coast"]))
# 162+190

# join precip data to coastal data
hist_precip_coast_data <- hist_precip_metadata_county_based_coast_albers %>%
  st_drop_geometry() %>%
  dplyr::left_join(hist_precip_data, by = "loc_id") %>%
  dplyr::select(loc_id, cmb_class, date_et, precip_in) %>%
  dplyr::mutate(year = year(date_et),
                month = str_pad(as.character(month(date_et)), width = 2, side = "left", pad = "0"),
                day = str_pad(as.character(day(date_et)), width = 2, side = "left", pad = "0"),
                month_year = paste0(month, "_", year),
                date_ymd = paste0(year, "-", month, "-", day)) %>%
  dplyr::filter(year == 2015)

# daily sum per station
daily_hist_precip_coast_data <- hist_precip_coast_data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(loc_id, date_ymd, cmb_class) %>%
  dplyr::summarize(precip_in = sum(precip_in, na.rm = TRUE))

# monthly avg summaries
# monthly_avg_hist_precip_coast_data <- hist_precip_coast_data %>%
#   ungroup() %>%
#   group_by(loc_id, month, cmb_class) %>%
#   summarize(monthly_avg_precip_in = mean(precip_in, na.rm = TRUE))

# monthly total summaries
monthly_sum_hist_precip_coast_data <- hist_precip_coast_data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(loc_id, month, cmb_class) %>%
  dplyr::summarize(monthly_sum_precip_in = sum(precip_in, na.rm = TRUE))


# ---- 6. county based: compare results -----
# clean up observed data (historic precip)
obs_data <- daily_hist_precip_coast_data %>%
  dplyr::select(loc_id, cmb_class, date_ymd, precip_in)

# clean up foretasted data (ndfd)
frcst_data <- ndfd_calcs_county_based_data %>%
  dplyr::mutate(date_ymd = as.character(datetime_uct),
                precip_frcst_in = qpf_in) %>%
  dplyr::select(loc_id, date_ymd, valid_period_hrs, precip_frcst_in)

# combine observed and foretasted data
compare_daily_data <- left_join(obs_data, frcst_data, by = c("loc_id", "date_ymd")) %>%
  dplyr::select(loc_id, date_ymd, valid_period_hrs, cmb_class, precip_in, precip_frcst_in) %>%
  dplyr::mutate(event_type = case_when(precip_in > 0 & precip_frcst_in > 0 ~ "correct_event",
                                       precip_in == 0 & precip_frcst_in == 0 ~ "correct_no-event",
                                       precip_in > 0 & precip_frcst_in == 0 ~ "incorrect_event",
                                       precip_in == 0 & precip_frcst_in > 0 ~ "incorrect_no-event",
                                       precip_in >= 0 & is.na(precip_frcst_in) == TRUE ~ "no_forecast"),
                month = month(date_ymd))

# find the number of days that don't have a forecast
length(unique(compare_daily_data$date_ymd[compare_daily_data$event_type == "no_forecast"]))
# it's 4 for 2015

# max number of days per month key
month_seq = seq(1,12,1)
max_days_per_month_key <- data.frame(month = month_seq,
                                     max_num_days = days_in_month(month(month_seq)))

# count number of observations per month for each station
station_monthly_obs_count <- compare_daily_data %>%
  dplyr::filter(event_type != "no_forecast") %>% # filtuer out no_forecast days for now (= 4 days for 2015)
  dplyr::ungroup() %>%
  dplyr::select(loc_id, date_ymd) %>%
  dplyr::distinct(loc_id, date_ymd) %>%
  dplyr::mutate(month = month(date_ymd)) %>% # recreate this
  dplyr::group_by(loc_id, month) %>%
  dplyr::summarize(num_days_available = n()) %>%
  dplyr::left_join(max_days_per_month_key, by = "month") %>%
  dplyr::mutate(percent_complete = num_days_available/max_num_days)

# count number of stations in different event statuses per month
station_event_type_monthly_summary <- compare_daily_data %>%
  dplyr::filter(event_type != "no_forecast") %>% # filtuer out no_forecast days for now (= 4 days for 2015)
  dplyr::ungroup() %>%
  dplyr::group_by(loc_id, month, cmb_class, event_type, valid_period_hrs) %>%
  dplyr::summarize(num_days = n()) %>% # count(name = "num_days") %>%
  dplyr::left_join(station_monthly_obs_count, by = c("loc_id", "month")) %>%
  dplyr::mutate(perc_month = round((num_days/num_days_available) * 100, 3))

# calculate the mean perc_month for different event statuses (summary)
station_event_type_monthly_summary_mean <- station_event_type_monthly_summary %>%
  dplyr::ungroup() %>%
  dplyr::group_by(cmb_class, valid_period_hrs, event_type) %>%
  dplyr::summarize(mean_perc_month = mean(perc_month, na.rm = TRUE))

# calculate the mean perc_month by month for different event statuses for 24 hr (detail)
station_event_type_month_detail_mean_24hr <- station_event_type_monthly_summary %>%
  dplyr::filter(valid_period_hrs == 24) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(cmb_class, month, event_type) %>%
  dplyr::summarize(mean_perc_month = mean(perc_month, na.rm = TRUE))

# calculate NSE for correct_events by cmb_class and by month for 24 hr
compare_daily_nse_summary <- compare_daily_data %>%
  dplyr::filter(event_type == "correct_event" & valid_period_hrs == 24) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(cmb_class, month) %>%
  dplyr::summarize(nse = calculate_nse(obs_data = precip_in, frcst_data = precip_frcst_in),
                   num_observations = n(),
                   num_stations = length(unique(loc_id)))

# ---- 7. calculate confusion matrix stats ---- 
# combine observed and foretasted data (confusion matrix)
compare_daily_data_conf <- left_join(obs_data, frcst_data, by = c("loc_id", "date_ymd")) %>%
  dplyr::select(loc_id, date_ymd, valid_period_hrs, cmb_class, precip_in, precip_frcst_in) %>%
  dplyr::mutate(obs_occ = if_else(precip_in > 0, "event", "non-event"),
                frcst_occ = if_else(precip_frcst_in > 0, "event", "non-event"),
                month = month(date_ymd)) %>%
  dplyr::select(-precip_in, -precip_frcst_in)

# function
calculate_confusion_matrix <- function(frcst_data, obs_data) {
  data = table(frcst_data, obs_data)
  confusion_matrix_output <- caret::confusionMatrix(data = data, positive = "event")
  
  # tidy output
  tidy_output <- broom::tidy(confusion_matrix_output)
  
  # calculate accuracy
  acc_calc = (confusion_matrix_output$table[1] + confusion_matrix_output$table[4])/sum(confusion_matrix_output$table)
  # true positives + true negatives / n
  
  # get sensitivity (true positives) and specificity (true negatives)
  tp_calc = tidy_output$estimate[tidy_output$term == "sensitivity"]
  tn_calc = tidy_output$estimate[tidy_output$term == "specificity"]
  
  # create output data
  all_calcs <- list(set_names(c(acc_calc, tp_calc, tn_calc), c("accuracy", "tp_rate", "tn_rate")))
  
  # return result
  return(all_calcs)
}

# take out months that are less than 90% complete
confusion_complete_month_key <- compare_daily_data_conf %>%
  na.omit() %>% # remove days with NA values
  dplyr::ungroup() %>%
  dplyr::group_by(loc_id, month, valid_period_hrs) %>% 
  dplyr::count() %>%
  dplyr::left_join(max_days_per_month_key, by = "month") %>%
  dplyr::mutate(perc_complete = n/max_num_days) %>%
  dplyr::filter(perc_complete >= 0.90) %>%
  dplyr::mutate(keep_key = paste0(loc_id, "_", month, "_", valid_period_hrs)) %>%
  dplyr::ungroup() %>%
  dplyr::select(keep_key)

# cmb_class key
cmb_class_key <- compare_daily_data_conf %>%
  dplyr::ungroup() %>%
  dplyr::select(loc_id, cmb_class) %>%
  dplyr::distinct()
  
# confusion calcs by site for each valid period and month
confusion_month_calc_data <- compare_daily_data_conf %>%
  na.omit() %>% # remove days with NA values
  dplyr::mutate(keep_key = paste0(loc_id, "_", month, "_", valid_period_hrs)) %>%
  dplyr::right_join(confusion_complete_month_key, by = "keep_key") %>%
  dplyr::ungroup() %>%
  dplyr::group_by(loc_id, month, valid_period_hrs) %>%
  # dplyr::count() # to check that it's the same length as confusion_month_tally
  dplyr::summarize(conf_mat_calc = calculate_confusion_matrix(frcst_data = frcst_occ, obs_data = obs_occ)) %>%
  tidyr::unnest_wider(conf_mat_calc) %>%
  dplyr::left_join(cmb_class_key, by = "loc_id")

# pivot longer
confusion_month_calc_data_long <- confusion_month_calc_data %>%
  dplyr::select(loc_id, cmb_class, month:tn_rate) %>%
  tidyr::pivot_longer(cols = accuracy:tn_rate, names_to = "event_occur_metric", values_to = "value")

# max number of days per month key
month_seq = seq(1,12,1)
max_days_per_month_key <- data.frame(month = month_seq,
                                     max_num_days = days_in_month(month(month_seq)))


# ---- 8. poster plots ----
# set colors for non-urban (green) and urban (yellow)
my_cmb_class_colors = c("#66c2a5", "#ffd92f")

# plot observed monthly total precip at each station
pdf(paste0(figure_path, "obs_precip_vs_month.pdf"), width = 12, height = 10)
ggplot(data = monthly_sum_hist_precip_coast_data) +
  geom_boxplot(aes(x = as.factor(as.numeric(month)), y = monthly_sum_precip_in, fill = cmb_class)) +
  xlab("Month") +
  ylab("2015 Monthly Observed Precipitation (in)") + 
  scale_fill_manual(values = my_cmb_class_colors) +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
        #legend.position = "none")
dev.off()

# plot percent of available monthly data in different event categories
pdf(paste0(figure_path, "percent_month_vs_event_by_period.pdf"), width = 15, height = 7)
ggplot() +
  geom_boxplot(data = station_event_type_monthly_summary, 
               aes(x = event_type, y = perc_month, fill = cmb_class),
               outlier.color = "black") +
  geom_point(data = station_event_type_monthly_summary_mean,
             aes(x = event_type, y = mean_perc_month, fill = cmb_class), 
             pch = 23, size = 3, color = "black", position = position_dodge(width = 0.75, preserve = "total")) +
  geom_abline(slope = 0, intercept = 50, lty = 2) +
  facet_wrap(~ valid_period_hrs) +
  xlab("Event Occurence Type") +
  ylab("Percent of Each Month") + 
  scale_fill_manual(values = my_cmb_class_colors) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# plot percent of available monthly data by month for 24 hrs only (since that's typically the best)
pdf(paste0(figure_path, "percent_month_vs_month_by_event_24hr.pdf"), width = 12, height = 10)
ggplot() +
  geom_boxplot(data = station_event_type_monthly_summary %>% filter(valid_period_hrs == 24),
               aes(x = as.factor(month), y = perc_month, fill = cmb_class)) +
  geom_point(data = station_event_type_month_detail_mean_24hr,
             aes(x = as.factor(month), y = mean_perc_month, fill = cmb_class), 
             pch = 23, size = 3, color = "black", position = position_dodge(width = 0.75, preserve = "total")) +
  geom_abline(slope = 0, intercept = 50, lty = 2) +
  facet_wrap(~ event_type) +
  xlab("Month") +
  ylab("Percent of Each Month") + 
  scale_fill_manual(values = my_cmb_class_colors) +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
        #legend.position = "none")
dev.off()

# plot obs vs forecast for different event statuses for correct_event status in 24 hrs only
pdf(paste0(figure_path, "obs_vs_frcst_by_month_24hr.pdf"), width = 12, height = 10)
ggplot(data = compare_daily_data %>% filter(event_type == "correct_event" & valid_period_hrs == 24)) +
  geom_point(aes(x = precip_frcst_in, y = precip_in, fill = cmb_class), pch = 21, size = 2, color = "black") +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  facet_wrap(~ month, ncol = 3, nrow = 4) +
  xlim(0, 10) +
  ylim(0, 10) +
  xlab("24hr Forecasted Daily Precipitation (in)") +
  ylab("Observated Daily Precipitation (in)") + 
  scale_fill_manual(values = my_cmb_class_colors) +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
        #legend.position = "none")
dev.off()

# plot event occur metric value vs valid time for each cmb_class
pdf(paste0(figure_path, "event_occur_metric_vs_period.pdf"), width = 15, height = 7)
ggplot() +
  geom_boxplot(data = confusion_month_calc_data_long, 
               aes(x = as.factor(valid_period_hrs), y = value, fill = cmb_class),
               outlier.color = "black") +
  facet_wrap(~ event_occur_metric) +
  xlab("Forecast Validation Period (hrs)") +
  ylab("Monthly Metric") + 
  scale_fill_manual(values = my_cmb_class_colors) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        #axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# plot event occur metric value vs month for 24 hrs only (since that's typically the best)
pdf(paste0(figure_path, "event_occur_metric_vs_month_24hr.pdf"), width = 8, height = 12)
ggplot() +
  geom_boxplot(data = confusion_month_calc_data_long %>% filter(valid_period_hrs == 24),
               aes(x = as.factor(month), y = value, fill = cmb_class)) +
  facet_wrap(~ event_occur_metric, nrow = 3, ncol = 1) +
  xlab("Month") +
  ylab("Metric") + 
  scale_fill_manual(values = my_cmb_class_colors) +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())#,
#legend.position = "none")
dev.off()

# ---- extra county based: plot results ----
ggplot(data = event_status_monthly_summary) +
  geom_point(aes(x = event_status, y = avg_percent, color = cmb_class)) +
  geom_pointrange(aes(x = event_status, y = avg_percent, color = cmb_class, ymin = lower_percent, ymax = upper_percent), position = "dodge") +
  facet_wrap(~valid_period_hrs)

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


# ---- extra county based: plot joined rainfall data ----

# plot monthly average per station
ggplot(data = monthly_avg_hist_precip_coast_data) +
  geom_boxplot(aes(x = month, y = monthly_avg_precip_in, color = cmb_class), alpha = 0.5) +
  xlab("Month") +
  ylab("Monthly Average Precipitation from 2015 and 2016 (in)") + 
  theme_bw() +
  theme(text = element_text(size = 14))


# ---- extra draft calcs ----
# calculate the number of stations per day
station_count_summary <- compare_daily_data %>%
  select(loc_id, date_ymd, cmb_class, valid_period_hrs) %>%
  na.omit() %>% # for now!
  ungroup() %>%
  group_by(date_ymd, cmb_class) %>%
  summarize(total_num_stations = n()/3) # divide by three because there are three validation periods

# calculate event status on daily basis
event_status_daily_summary <- compare_daily_data %>%
  na.omit() %>% # for now!
  ungroup() %>%
  group_by(date_ymd, valid_period_hrs, cmb_class, event_status) %>%
  count(name = "num_stations") %>%
  left_join(station_count_summary, by = c("date_ymd", "cmb_class")) %>%
  mutate(percent = round(num_stations/total_num_stations, 3))

# calculate event status on montly basis
event_status_monthly_summary <- event_status_daily_summary %>%
  mutate(month = month(date_ymd)) %>%
  ungroup() %>%
  group_by(month, valid_period_hrs, cmb_class, event_status) %>%
  summarize(avg_percent = mean(percent),
            std_percent = sd(percent)) %>%
  mutate(upper_percent = avg_percent + std_percent,
         lower_percent = avg_percent - std_percent)


# if you add percent vals for one date_ymd + one valid_per_hrs + all cmb_class values you will get 1

# calculate nse for each day and each ndfd valid period
nse_daily_data <- compare_daily_data %>%
  na.omit() %>% # for now!
  ungroup() %>%
  group_by(date_ymd, valid_period_hrs) %>%
  summarize(event_status = ,
            nse = calculate_nse(obs_data = precip_in, frcst_data = precip_frcst_in),
            intercept = calculate_lm_coeffs(obs_data = precip_in, frcst_data = precip_frcst_in)[1],
            slope = calculate_lm_coeffs(obs_data = precip_in, frcst_data = precip_frcst_in)[2])

test_data <- compare_daily_data %>% filter(date_ymd == "2015-01-04") %>% filter(valid_period_hrs == 72)
obs_data <- test_data$precip_in
frcst_data <- test_data$precip_frcst_in
test_nse <- calculate_nse(obs_data = test_data$precip_in, 
                          frcst_data = test_data$precip_frcst_in)
test_nse


# ---- draft plots ----
# plot obs vs forecast for different event statuses for correct_event status in 48 hrs only
# ggplot(data = compare_daily_data %>% filter(event_status == "correct_event" & valid_period_hrs == 48)) +
#   geom_point(aes(x = precip_frcst_in, y = precip_in, fill = cmb_class), pch = 21, size = 3, color = "black") +
#   geom_abline(slope = 1, intercept = 0, lty = 2) +
#   facet_wrap(~ month, ncol = 6, nrow = 2) +
#   xlim(0, 10) +
#   ylim(0, 10) +
#   xlab("48hr Forecasted Daily Precipitation (in)") +
#   ylab("Observated Daily Precipitation (in)") + 
#   scale_fill_manual(values = my_cmb_class_colors) +
#   theme_bw() +
#   theme(text = element_text(size = 14))

# plot obs vs forecast for different event statuses for correct_event status in 72 hrs only
# ggplot(data = compare_daily_data %>% filter(event_status == "correct_event" & valid_period_hrs == 48)) +
#   geom_point(aes(x = precip_frcst_in, y = precip_in, fill = cmb_class), pch = 21, size = 3, color = "black") +
#   geom_abline(slope = 1, intercept = 0, lty = 2) +
#   facet_wrap(~ month, ncol = 6, nrow = 2) +
#   xlim(0, 10) +
#   ylim(0, 10) +
#   xlab("72hr Forecasted Daily Precipitation (in)") +
#   ylab("Observated Daily Precipitation (in)") + 
#   scale_fill_manual(values = my_cmb_class_colors) +
#   theme_bw() +
#   theme(text = element_text(size = 14))


