# ---- script header ----
# script name: 10_analysis_script.R
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


# ---- load libraries ----
library(tidyverse)
library(here)
library(lubridate)
library(sf)
library(RColorBrewer)
# library(tidylog)


# ---- define paths ----
# obs tabular data input path
obs_tabular_data_input_path <- here::here("data", "tabular", "obs_data_tidy")

# obs spatial data input path
obs_spatial_data_input_path <- here::here("data", "spatial", "obs_data_tidy")

# path to ndfd tabular inputs
ndfd_tabular_data_intput_path <- here::here("data", "tabular", "ndfd_data_tidy")

# path to nc state bounds spatial data
nc_spatial_data_input_path <- here::here("data", "spatial", "region_state_bounds_tidy")

# path to ncdmf spatial data
ncdmf_spatial_data_input_path <- here::here("data", "spatial", "ncdmf_data_tidy")

# figure path
figure_output_path <- here::here("figures")


# ---- define functions ----
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

# obs daily (all) and metadata joined
obs_data_metadata_join <- read_csv(file = paste0(obs_tabular_data_input_path, "/obs_data_metadata_join.csv"), col_names = TRUE)

# obs daily metadata shapefile
obs_metadata_shp <- st_read(paste0(obs_spatial_data_input_path, "/obs_metadata_albers_sel.shp"))

# ndfd daily data (averaged by cmu)
ndfd_avg_data <- read_csv(file = paste0(ndfd_tabular_data_intput_path, "/ndfd_avg_data.csv"), col_names = TRUE)

# nc state bounds
nc_bounds_shp <- st_read(paste0(nc_spatial_data_input_path, "/nc_bounds_albers.shp"))

# cmu bounds
cmu_bounds_shp <- st_read(paste0(ncdmf_spatial_data_input_path, "/cmu_bounds_albers.shp"))


# ---- join obs and ndfd data ----
# join data and filter for data in 2015-01-04 to 2016-12-31 (start on Jan 4 because of 72 hour window)
obs_ndfd_data <- ndfd_avg_data %>%
  dplyr::left_join(obs_avg_data, by = c("date", "cmu_name")) %>%
  dplyr::filter(is.na(obs_measurement_count) == FALSE) %>% # remove rows where there are no observations
  dplyr::mutate(year = year(date))

# check unique cmus
# length(unique(obs_ndfd_data$cmu_name))
# 102 ok!

# check for na's
# sum(is.na(obs_ndfd_data$cmu_qpf_cm))
# sum(is.na(obs_ndfd_data$obs_avg_cm))
# 0 means no na's

# check that each cmu for each day has 3 observations (for 24, 48, and 72 hr valid periods)
valid_period_check <- obs_ndfd_data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(date, cmu_name) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::filter(count != 3)
# zero long, so all have three valid periods ok!


# ---- confusion matrix analysis ----
# compare data and define event/non-event and correct/not correct
compare_events_data <- obs_ndfd_data %>%
  dplyr::select(date, month_num, month_type, cmu_name, valid_period_hrs, rain_depth_thresh_cm, cmu_qpf_cm, obs_avg_cm) %>%
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


# ---- obs plots ----
# set colors for 2015 (green) and 2016 (yellow)
my_year_colors = c("#66c2a5", "#ffd92f")

# month key
obs_month_key <- obs_data_metadata_join %>%
  dplyr::select(date) %>%
  dplyr::filter((date >= as.Date("2015-01-01")) & (date <= as.Date("2016-12-31"))) %>%
  dplyr::mutate(month_num = as.numeric(month(date)),
                month_chr = fct_relevel(as.character(month(date, label = TRUE)), 
                                        c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                month_type = case_when(month_num <= 3 | month_num >= 10 ~ "cool",
                                       month_num > 4 | month_num < 10 ~ "warm"),
                month_type = fct_relevel(month_type, "warm", "cool")) %>%
  dplyr::distinct()

# calculate monthly sum by station
obs_monthly_summary <- obs_data_metadata_join %>%
  dplyr::filter((date >= as.Date("2015-01-01")) & (date <= as.Date("2016-12-31"))) %>%
  dplyr::mutate(month_num = as.numeric(month(date)),
                year_num = as.numeric(year(date))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(loc_id, month_num, year_num) %>%
  dplyr::summarize(precip_monthly_cm = sum(precip_in))

# plot observed monthly total precip at each station
pdf(paste0(figure_output_path, "/obs_precip_vs_month.pdf"), width = 12, height = 10)
ggplot(data = obs_monthly_summary, 
       aes(x = as.factor(month_num), y = precip_monthly_cm, fill = as.factor(year_num))) +
  geom_boxplot() +
  geom_point(aes(fill = as.factor(year_num)), size = 2, shape = 21, position = position_jitterdodge(), alpha = 0.50) +
  scale_fill_manual(values = my_year_colors) +
  labs(x = "Month", y = "Monthly Observed Precipitation (cm)", fill = "Year") +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

# map of stations by network
pdf(paste0(figure_output_path, "/map_station_networks.pdf"), width = 12, height = 10)
ggplot() +
  # geom_sf(data = nc_bounds_shp, fill = NA) +
  geom_sf(data = cmu_bounds_shp, fill = NA) +
  geom_sf(data = obs_metadata_shp, 
          aes(fill = network), size = 4, shape = 21, alpha = 0.75) +
  labs(x = "", y = "", fill = "Network") +
  theme_classic()
dev.off()

# map of stations by completeness
my_complete_colors <- colorRampPalette(brewer.pal(n = 5, name = "BuPu"))
my_complete_colors_length <- length(obs_metadata_shp$perc_compl)
my_complete_colors_min <- floor(min(obs_metadata_shp$perc_compl))
my_complete_colors_max <- 100
pdf(paste0(figure_output_path, "/map_station_completness.pdf"), width = 12, height = 10)
ggplot() +
  # geom_sf(data = nc_bounds_shp, fill = NA) +
  geom_sf(data = cmu_bounds_shp, fill = NA) +
  geom_sf(data = obs_metadata_shp, 
          aes(fill = perc_compl), size = 4, shape = 21, alpha = 0.75) +
  scale_fill_gradientn(colors = my_complete_colors(my_complete_colors_length), 
                       limits = c(my_complete_colors_min, my_complete_colors_max)) +
  labs(x = "", y = "", fill = "Percent Complete (%)") +
  theme_classic()
dev.off()

# calculate rainfall threshold depths in cm
cmu_bounds_shp_cm <- cmu_bounds_shp %>%
  dplyr::mutate(rain_cm = rain_in * 2.54)



# map of cmu rainfall thresholds
my_cmu_colors <- brewer.pal(n = length(unique(cmu_bounds_shp_cm$rain_cm)), name = "BuPu")
pdf(paste0(figure_output_path, "/map_cmu_rainfall_thresholds.pdf"), width = 12, height = 10)
ggplot() +
  # geom_sf(data = nc_bounds_shp, fill = NA) +
  geom_sf(data = cmu_bounds_shp_cm, aes(fill = as.factor(rain_cm)), color = "black", alpha = 0.75) +
  scale_fill_manual(values = my_cmu_colors) +
  labs(x = "", y = "", fill = "Rainfall Threshold Depths (cm)") +
  theme_classic()
dev.off()


# ---- analysis plots ----
# color scale for valid periods
my_validhrs_colors <- c("#66c2a5", "#fc8d62", "#8da0cb")

# all data facet by valid period
pdf(paste0(figure_output_path, "/obs_vs_ndfd_by_valid_period.pdf"), width = 15, height = 5)
ggplot(data = obs_ndfd_data) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm, fill = as.factor(valid_period_hrs)), shape = 21, alpha = 0.50, size = 3) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
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

# 24 hr valid period
pdf(paste0(figure_output_path, "/obs_vs_ndfd_24hr_by_month.pdf"), width = 12, height = 10)
ggplot(data = obs_ndfd_data %>% filter(valid_period_hrs == 24)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[1], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
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

# 48 hr valid period
pdf(paste0(figure_output_path, "/obs_vs_ndfd_48hr_by_month.pdf"), width = 12, height = 10)
ggplot(data = obs_ndfd_data %>% filter(valid_period_hrs == 48)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[2], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
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

# 72 hr valid period
pdf(paste0(figure_output_path, "/obs_vs_ndfd_72hr_by_month.pdf"), width = 12, height = 10)
ggplot(data = obs_ndfd_data %>% filter(valid_period_hrs == 72)) +
  geom_point(aes(x = cmu_qpf_cm, y = obs_avg_cm), shape = 21, size = 3, fill = my_validhrs_colors[3], alpha = 0.50) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
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

# calculate seasonal nse by cmu
# calculate 


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

# plot percent of available monthly data in different event categories
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

