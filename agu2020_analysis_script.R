
# ---- script header ----
# script name: lab_test_analysis_script.R
# purpose of script:
# author:
# date created:
# email:


# ---- notes ----
# notes:


# ---- to do ----
# to do list

# TODO run analysis for all days and all valid times
# TODO fix uct and et times in comparison part


# ----

# ---- 1. load libraries ----
library(tidyverse)
library(lubridate)
library(raster)
library(sf)


# ---- 2. define paths and projections ----

# historic tabular data path
hist_precip_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/hist_precip_data/"

# spatial data path
spatial_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/"

# results/figures
# figures_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/results/figures/"

# define epsg and proj4 for N. America Albers projection (projecting to this)
# na_albers_proj4 <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
# na_albers_epsg <- 102008

# define epsg and proj for CONUS Albers projection (projecting to this)
conus_albers_epsg <- 5070
conus_albers_proj <- "+init=EPSG:5070"


# ---- 3. load in data ----
# historic precip netadata (locations)
hist_precip_metadata_albers <- st_read(paste0(spatial_data_input_path, "hist_precip_data/hist_precip_metadata_albers.shp"))

# historic precip data
hist_precip_data <- read_csv(paste0(hist_precip_data_input_path, "hist_precip_data_compiled.csv"))

# urban boundaries
urban_bounds_combo_albers <- st_read(paste0(spatial_data_input_path, "region_state_bounds/urban_bounds_combo_albers.shp"))

# cmu boundaries
cmu_bounds_albers <- st_read(paste0(spatial_data_input_path, "cmu_bounds/cmu_bounds_10kmbuf_albers.shp"))

# all have crs = 5070, checks!


# ---- clip historic precip locations urban/non-urban and coast/non-coast ----
# urban or non-urban
urban_logic_list <- st_intersects(hist_precip_metadata_albers, urban_bounds_combo_albers, sparse = FALSE)

# coast or non-coast
coast_logic_list <- st_intersects(hist_precip_metadata_albers, cmu_bounds_albers, sparse = FALSE)


# add to spatial data
hist_precip_metadata_logic_albers <- hist_precip_metadata_albers %>%
  dplyr::mutate(urb_logic = urban_logic_list, # false for non-urban, true for urban
                cst_logic = coast_logic_list, # false for non-coast, true for coast
         urb_class = dplyr::if_else(urb_logic == TRUE, "urban", "non-urban"), 
         cst_class = dplyr::if_else(cst_logic == TRUE, "coast", "non-coast"),
         cmb_class = paste0(urb_class, "_", cst_class)) %>%
  dplyr::select(-urb_logic, -cst_logic)

# counts
length(hist_precip_metadata_logic_albers$urb_class[hist_precip_metadata_logic_albers$urb_class == "urban"]) # 545
length(hist_precip_metadata_logic_albers$urb_class[hist_precip_metadata_logic_albers$urb_class == "non-urban"]) # 534
# 545 + 534
length(hist_precip_metadata_logic_albers$cst_class[hist_precip_metadata_logic_albers$cst_class == "coast"]) # 178
length(hist_precip_metadata_logic_albers$cst_class[hist_precip_metadata_logic_albers$cst_class == "non-coast"]) # 901
# 178 + 901

# pull out just coastal data for now
hist_precip_metadata_coast_albers <- hist_precip_metadata_logic_albers %>%
  dplyr::filter(cmb_class == "urban_coast" | cmb_class == "non-urban_coast")

# make a buffer
hist_precip_metadata_coast_5kmbuf_albers <- hist_precip_metadata_coast_albers %>%
  st_buffer(dist = 5000) %>% # in m, so 1000m x 5 = 5km
  dplyr::select(loc_id, cmb_class)

# counts
length(hist_precip_metadata_coast_albers$cmb_class[hist_precip_metadata_coast_albers$cmb_class == "urban_coast"]) # 106
length(hist_precip_metadata_coast_albers$cmb_class[hist_precip_metadata_coast_albers$cmb_class == "non-urban_coast"]) # 72
# 106 + 72


# ---- join metadata and data to plot rainfall data ----
# join precip data to select coastal data
hist_precip_coast_data <- hist_precip_metadata_coast_albers %>%
  st_drop_geometry() %>%
  dplyr::left_join(hist_precip_data, by = "loc_id") %>%
  dplyr::select(loc_id, cmb_class, date_et, precip_in) %>%
  dplyr:: mutate(year = year(date_et),
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

# plot monthly average per station
ggplot(data = monthly_avg_hist_precip_coast_data) +
  geom_boxplot(aes(x = month, y = monthly_avg_precip_in, color = cmb_class), alpha = 0.5) +
  xlab("Month") +
  ylab("Monthly Average Precipitation from 2015 and 2016 (in)") + 
  theme_bw() +
  theme(text = element_text(size = 14))

# monthly total summaries
monthly_sum_hist_precip_coast_data <- hist_precip_coast_data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(loc_id, month, cmb_class) %>%
  dplyr::summarise(monthly_sum_precip_in = sum(precip_in, na.rm = TRUE))

# plot monthly sum per station
ggplot(data = monthly_sum_hist_precip_coast_data) +
  geom_boxplot(aes(x = month, y = monthly_sum_precip_in, color = cmb_class)) +
  xlab("Month") +
  ylab("Monthly Total Precipitation from 2015 and 2016 (in)") + 
  theme_bw() +
  theme(text = element_text(size = 14))


# ---- read in ndfd data ----

# path to ndfd tabular inputs
ndfd_tabular_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/ndfd_sco_hist_raw/"

# data available
data_available <- read_csv(paste0(ndfd_tabular_data_input_path, "data_available.csv"), col_names = TRUE)

# read in pop12 data and convert to 5070
pop12_data <- raster::raster(paste0(spatial_data_input_path, "ndfd_sco_hist/pop12_24hr_nc_albers_2015090100.tif"))
pop12_data_albers <- raster::projectRaster(pop12_data, crs = conus_albers_proj)
writeRaster(pop12_data_albers, paste0(spatial_data_input_path, "ndfd_sco_hist_5070/pop12_24hr_nc_albers_2015090100.tif"), overwrite = TRUE)

# read in qpf data and convert to 5070
qpf_data <- raster::raster(paste0(spatial_data_input_path, "ndfd_sco_hist/qpf_24hr_nc_albers_2015090100.tif"))
qpf_data_albers <- raster::projectRaster(qpf_data, crs = conus_albers_proj)
writeRaster(qpf_data_albers, paste0(spatial_data_input_path, "ndfd_sco_hist_5070/qpf_24hr_nc_albers_2015090100.tif"), overwrite = TRUE)

# go through and for each data point calculate area weighted avg qpf and pop12
ndfd_pts_calcs_data <- data.frame(row_num = as.numeric(),
                                  loc_id = as.character(),
                                  datetime_uct = as.character(),
                                  valid_period_hrs = as.numeric(),
                                  pop12_perc = as.numeric(),
                                  qpf_in = as.numeric(),
                                  precip_frcst_in = as.numeric())

# valid period list
valid_period_list <- c(24)

# define date
ndfd_date_uct <- lubridate::ymd_hm("2015-09-01 00:00")

# rasters lists
pop12_pts_raster_list <- c(pop12_data_albers)
qpf_pts_raster_list <- c(qpf_data_albers)

# number of pts's
num_pts <- length(hist_precip_metadata_coast_5kmbuf_albers$loc_id)

# row dimentions
num_pts_row <- length(valid_period_list)*num_pts

# set row number and start iterator
pts_row_num_list <- seq(1:num_pts_row)
pts_row_num <- pts_row_num_list[1]

# record start time
start_time <- now()

# for loop
# i denotes valid period (3 values), j denotes number of points (178 for cocorahs coastal sites)
for (i in 1:length(valid_period_list)) {
  for (j in 1:num_pts) {
    # valid period
    temp_valid_period <- valid_period_list[i]
    
    # save raster
    temp_pop12_raster <- pop12_pts_raster_list[i][[1]]
    temp_qpf_raster <- qpf_pts_raster_list[i][[1]]
    
    # save raster resolution
    temp_pop12_raster_res <- raster::res(temp_pop12_raster)
    temp_qpf_raster_res <- raster::res(temp_qpf_raster)
    
    # save pts name
    temp_pts_name <- as.character(hist_precip_metadata_coast_5kmbuf_albers$loc_id[j])
    
    # get pts bounds vector
    temp_pts_bounds <- hist_precip_metadata_coast_5kmbuf_albers %>%
      dplyr::filter(loc_id == temp_pts_name)
    
    # pts bounds area
    temp_pts_area <- as.numeric(st_area(temp_pts_bounds)) # in m^2
    
    # get value and weight of each gridcell that overlaps the pts
    temp_pop12_pts_raster_perc_cover_df <- data.frame(raster::extract(temp_pop12_raster, temp_pts_bounds, weights = TRUE)[[1]])
    temp_qpf_pts_raster_perc_cover_df <- data.frame(raster::extract(temp_qpf_raster, temp_pts_bounds, weights = TRUE)[[1]])
    
    # calculate area weighted avg value for the pts
    temp_pop12_area_weighted_df <- temp_pop12_pts_raster_perc_cover_df %>%
      dplyr::mutate(area_weighted_avg = value * weight)
    temp_qpf_area_weighted_df <- temp_qpf_pts_raster_perc_cover_df %>%
      dplyr::mutate(area_weighted_avg = value * weight)
    
    # sum weighted values to get result
    temp_pts_pop12_result <- round((sum(temp_pop12_area_weighted_df$area_weighted_avg)/100), 2)
    temp_pts_qpf_result <- round(sum(temp_qpf_area_weighted_df$area_weighted_avg), 2)
    temp_pts_pop12_qpf_result <- round(temp_pts_pop12_result * temp_pts_qpf_result, 2)

    # save data
    temp_ndfd_pts_calcs_data <- data.frame(row_num = pts_row_num,
                                           loc_id = temp_pts_name,
                                           datetime_uct = ndfd_date_uct,
                                           valid_period_hrs = temp_valid_period,
                                           pop12_perc = temp_pts_pop12_result,
                                           qpf_in = temp_pts_qpf_result,
                                           precip_frcst_in = temp_pts_pop12_qpf_result)

    # bind results
    ndfd_pts_calcs_data <-  rbind(ndfd_pts_calcs_data, temp_ndfd_pts_calcs_data)
    
    # next row
    print(paste0("finished row: ", pts_row_num))
    pts_row_num <- pts_row_num + 1
  }
}

# print time now
stop_time <- now()

# time to run loop
stop_time - start_time
# Time difference of ~3 to 4 min

# print date
print(paste0(ndfd_date_uct, " spatial averaging complete"))


# ---- compare results -----
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


# ---- export all data ----

st_write(hist_precip_metadata_logic_albers, paste0(spatial_data_input_path, "hist_precip_data/hist_precip_metadata_logic_albers.shp"), delete_layer = TRUE)
st_write(hist_precip_metadata_coast_albers, paste0(spatial_data_input_path, "hist_precip_data/hist_precip_metadata_coast_albers.shp"), delete_layer = TRUE)
st_write(hist_precip_metadata_coast_5kmbuf_albers, paste0(spatial_data_input_path, "hist_precip_data/hist_precip_metadata_coast_5kmbuf_albers.shp"), delete_layer = TRUE)




