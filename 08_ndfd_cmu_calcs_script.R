# ---- script header ----
# script name: 08_ndfd_cmu_calcs_script.R
# purpose of script: get and wrangle data required for analysis
# author: sheila saia
# date created: 20210408
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:


# ---- to do ----
# to do list

# TODO need to add in cmu, lease, and other tidy data scripts before this one
# TODO remove mention of roc here for now

# ---- 1. load libraries ----
library(tidyverse)
library(sf)
library(raster)
library(here)
library(lubridate)
# library(tidylog)


# ---- 2. define paths ----
# path to ndfd tabular inputs
ndfd_tabular_data_input_path <- here::here("data", "tabular", "ndfd_data_raw")

# path to ndfd tabular outputs
ndfd_tabular_data_output_path <- here::here("data", "tabular", "ndfd_data_tidy")

# path to ndfd spatial inputs
ndfd_spatial_data_input_path <- here::here("data", "spatial", "ndfd_data_tidy")

# path to ncdmf tabular inputs
ncdmf_tabular_data_input_path <- here::here("data", "tabular", "ncdmf_data_tidy")

# path to ncdfm spatial inputs
ncdmf_spatial_data_input_path <- here::here("data", "spatial", "ncdmf_data_tidy")

# path to ncdfm spatial inputs
ncdmf_spatial_data_output_path <- here::here("data", "spatial", "ncdmf_data_tidy")

# path to observed spatial inputs
obs_spatial_data_input_path <- here::here("data", "spatial", "obs_data_tidy")

# path to observed spatial outputs
obs_spatial_data_output_path <- here::here("data", "spatial", "obs_data_tidy")


# ---- 3. load data ----
# import cmu bounds (spatial)
cmu_bounds_albers <- st_read(paste0(ncdmf_spatial_data_input_path, "/cmu_bounds_albers.shp"))

# import lease centroids (spatial)
lease_centroids_albers <- st_read(paste0(ncdmf_spatial_data_input_path, "/lease_centroids_albers.shp"))

# import historic precip metadata (spatial)
obs_metadata_albers <- st_read(paste0(obs_spatial_data_input_path, "/obs_metadata_albers.shp"))

# cmu information (tabular)
cmu_sga_key <- read_csv(file = paste0(ncdmf_tabular_data_input_path, "/cmu_sga_key.csv"), col_names = TRUE)

# sga information (tabular)
sga_key <- read_csv(file = paste0(ncdmf_tabular_data_input_path, "/sga_key.csv"), col_names = TRUE)

# rainfall thresholds (tabular)
rainfall_thresh_data_raw <- read_csv(file = paste0(ncdmf_tabular_data_input_path, "/rainfall_thresholds_raw_tidy.csv"), col_names = TRUE)

# ndfd data available
data_available <- read_csv(paste0(ndfd_tabular_data_input_path, "/data_available.csv"), col_names = TRUE)


# ---- 4. find cmu's with the most leases ----
# tidy rainfall threshold data 
rainfall_thresh_data <- rainfall_thresh_data_raw %>%
  dplyr::select(HA_CLASS, cmu_name) %>%
  dplyr::left_join(cmu_sga_key, by = "HA_CLASS") %>%
  dplyr::left_join(sga_key, by = "grow_area") %>%
  dplyr::select(-googlemaps_description, -sga_desc_short)

# summarize number of leases in each cmu
lease_cmu_count <- lease_centroids_albers %>%
  st_drop_geometry() %>%
  dplyr::group_by(cmu_name) %>%
  dplyr::summarize(lease_count = n()) %>%
  dplyr::left_join(rainfall_thresh_data, by = "cmu_name")

# join lease counts to cmu data (for plotting)
cmu_bounds_lease_count_join <- cmu_bounds_albers %>%
  dplyr::left_join(lease_cmu_count, by = "cmu_name")

# tabular data
cmu_bounds_lease_count_join_tabular <- cmu_bounds_lease_count_join %>%
  st_drop_geometry() %>%
  dplyr::select(-sga_county) %>%
  dplyr::distinct()

# plot by lease count
# pdf(file = here::here("figures", "cmu_lease_counts.pdf"), width = 10, height = 10)
# ggplot(data = cmu_bounds_lease_count_join) +
#   geom_sf(aes(fill = lease_count)) + 
#   scale_fill_gradient(low = "white", high = "blue", na.value = "grey80") +
#   theme_classic()
# dev.off()

# plot rainfall threshold depth by number of leases
# ggplot(data = cmu_bounds_lease_count_join) +
#   geom_point(aes(x = rain_in, y = lease_count)) +
#   theme_classic()

# leases to pick
# cmu with 10 leases at 1 in (U092)
# cmu with 30 leases at 1.5 in (U096)
# cmu with 23 leases at 3 in (U144)
# cmu with 34 leases at 1.5 in (U130) - this one has very few observations so decided not to choose it


# ---- 5. select cmu's for analysis ----
# cmu list
# cmu_sel_list <- c("U092", "U096", "U144") # to start
cmu_sel_list <- unique(cmu_bounds_albers$cmu_name) # select all (i.e., 149)

# select cmus for analysis
cmu_bounds_sel <- cmu_bounds_albers %>%
  filter(cmu_name %in% cmu_sel_list)

# buffer by 5km
cmu_bounds_sel_5kmbuf <- cmu_bounds_sel %>%
  st_buffer(dist = 5000) # dist = 5000 means 5km

# plot selected cmu's with buffers
# pdf(file = here::here("figures", "cmu_selection_with_buffer_fullanalaysis.pdf"), width = 10, height = 10)
# ggplot() +
#   geom_sf(data = cmu_bounds_sel_5kmbuf, color = "blue", alpha = 0.75) +
#   geom_sf(data = cmu_bounds_lease_count_join) + 
#   geom_sf(data = cmu_bounds_sel, fill = "blue") +
#   theme_classic()
# dev.off()


# ---- 6. find precip data that overlaps ----
obs_metadata_albers_sel <- obs_metadata_albers %>%
  st_intersection(cmu_bounds_sel_5kmbuf) %>%
  dplyr::filter(perc_compl >= 90) # filter out ones that are 90% complete or more
# without the buffer there are no rain gages within the cmus

# plot cmus and observations
# pdf(file = here::here("figures", "cmu_selection_with_obs_fullanalysis.pdf"), width = 10, height = 10)
# ggplot() +
#   geom_sf(data = cmu_bounds_sel_5kmbuf, color = "blue", alpha = 0.75) +
#   geom_sf(data = cmu_bounds_sel, fill = "blue") +
#   geom_sf(data = obs_metadata_albers_sel, color = "black") +
#   theme_classic()
# dev.off()

# number of unique stations
length(unique(obs_metadata_albers_sel$loc_id))
# new is 54

# total number of stations
length(unique(obs_metadata_albers$loc_id))
# 1778
 
# number of unique cmus
length(unique(obs_metadata_albers_sel$cmu_name))
# 102

# total number of cmus
length(cmu_sel_list)
# 149

# so we have 102/149 (68%) cmu's represented based on available observed rainfall data

# what cmu's are not represented in this analysis?
# these missing cmu's do not have enough observation data available for analysis
`%notin%` <- Negate(`%in%`)
cmu_missing <- cmu_bounds_albers %>%
  st_drop_geometry() %>%
  dplyr::filter(cmu_name %notin% unique(obs_metadata_albers_sel$cmu_name)) %>%
  dplyr::left_join(rainfall_thresh_data, by = "cmu_name") %>%
  dplyr::distinct()
# note some cmu's are in multiple sga so the length of cmu_missing is > 47
# can find unique cmu's by looking at HA_CLASS
  
# check for uniqueness and total
length(unique(cmu_missing$HA_CLASS))
# 47 + 102 = 149


# ---- 7. loop through ndfd data to get forecast result ----
# files available
file_list <- list.files(path = paste0(ndfd_spatial_data_input_path, "/"))

# valid period list
valid_period_list <- c(24, 48, 72)

# number of observations
# num_obs_stations <- dim(obs_metadata_albers_sel)[1]
obs_stations_list <- unique(obs_metadata_albers_sel$loc_id)
num_obs_stations <- length(obs_stations_list)

# make empty data frame
ndfd_data_sel <- data.frame(loc_id = as.character(),
                            cmu_name = as.character(),
                            date = as.character(),
                            valid_period_hrs = as.numeric(),
                            loc_pop_perc = as.numeric(),
                            loc_qpf_in = as.numeric(),
                            cmu_pop_avg_perc = as.numeric(),
                            cmu_pop_max_perc = as.numeric(),
                            cmu_qpf_in = as.numeric())
# loc_qpf_in is based on the ndfd grid cell that the gage is in
# cmu_qpf_in is based on the area weighted avg footprint of the cmu associated with the gage (shellcast)

# record start time
start_time <- now()

# for loop (i = day with data, j = valid period, k = observation)
for (i in 1:dim(data_available)[1]) {
  # check if data is available
  status <- data_available$status[i]
  
  if (status == "available") {
    # select date of data to pull
    temp_date <- ymd(data_available$datetime_uct_str[i])
    
    # print date status
    print(paste0("started ", temp_date, " analysis"))
    
    # convert date to string
    temp_date_str <- paste0(str_remove_all(strftime(temp_date, format = "%Y-%m-%d"), "-"), "00")
    
    # get files with that date
    temp_file_pattern <- paste0("*", temp_date_str, ".tif")
    temp_files <- file_list[grep(pattern = temp_file_pattern, x = file_list)]
    
    # save ndfd file names
    temp_pop_file_name <- sort(temp_files[grep(pattern = "pop12", x = temp_files)]) # probability of precipitation (pop)
    temp_qpf_file_name <- sort(temp_files[grep(pattern = "qpf", x = temp_files)]) # quantitative precipitation forecast (qpf)
    
    # step through each period
    for (j in 1:length(valid_period_list)) {
      # valid period
      temp_valid_period <- valid_period_list[j]
      
      # correct date for forecast comparisons later on
      # 24 hr is date as is (today)
      # 48 hr is date + 1 day (tomorrow)
      # 72 hr is date + 2 days (in two days)
      if (j == 1) {
        temp_date_fix <- temp_date
      } else if (j == 2) {
        temp_date_fix <- temp_date + days(1)
      } else {
        temp_date_fix <- temp_date + days(2)
      }
      
      # print when starts valid period
      print(paste0("started ", temp_valid_period, " hr valid period"))
      
      # find valid period files
      temp_prd_pop_file_name <- temp_pop_file_name[grep(pattern = paste0(as.character(temp_valid_period), "hr"), 
                                                        x = temp_pop_file_name)]
      temp_prd_qpf_file_name <- temp_qpf_file_name[grep(pattern = paste0(as.character(temp_valid_period), "hr"), 
                                                        x = temp_qpf_file_name)]
      
      # read in ndfd raster data
      temp_pop_raster <- raster::raster(paste0(ndfd_spatial_data_input_path, "/", temp_prd_pop_file_name))
      temp_qpf_raster <- raster::raster(paste0(ndfd_spatial_data_input_path, "/", temp_prd_qpf_file_name))
      
      # save raster resolution
      temp_pop_raster_res <- raster::res(temp_pop_raster)
      temp_qpf_raster_res <- raster::res(temp_qpf_raster)
      
      # --- location-based calcs ---
      # get location
      temp_loc_coord <- obs_metadata_albers_sel %>%
        dplyr::select(geometry)
      
      # get value of grid cell that overlaps with each unique location
      temp_loc_pop_list <- raster::extract(temp_pop_raster, temp_loc_coord, weights = FALSE)
      temp_loc_qpf_list <- round(raster::extract(temp_qpf_raster, temp_loc_coord, weights = FALSE), 4)
      
      # location-based calcs df
      temp_loc_result_df <- data.frame(loc_id = obs_metadata_albers_sel$loc_id,
                                       temp_loc_pop_result = temp_loc_pop_list,
                                       temp_loc_qpf_result = temp_loc_qpf_list) %>%
        dplyr::distinct() # there are redundant stations in obs_metadata_albers_sel
      
      
      # --- cmu-based calcs (like shellcast) ---
      # get unique cmu list
      unique_cmu_list <- unique(obs_metadata_albers_sel$cmu_name)
      num_unique_cmu <- length(unique(obs_metadata_albers_sel$cmu_name))
      
      # cmu-based calcs df
      # start with one that's empty
      temp_cmu_result_df <- data.frame(cmu_name = as.character(),
                                       temp_cmu_pop_avg_result = as.numeric(), # area weighted avg value of pop
                                       temp_cmu_pop_max_result = as.numeric(), # max value of pop
                                       temp_cmu_qpf_result = as.numeric()) # area weight avg value of qpf
      
      # run for each unique cmu and append to temp_cmu_result_df
      # do this because there are redundant cmus in obs_metadata_albers_sel
      for (k in 1:num_unique_cmu) {
        # save cmu name
        temp_cmu_name <- as.character(unique_cmu_list[k])
        
        # get cmu bounds vector
        temp_cmu_bounds <- cmu_bounds_sel %>%
          dplyr::filter(cmu_name == temp_cmu_name) %>%
          dplyr::select(geometry)
        
        # cmu bounds area
        temp_cmu_area <- as.numeric(st_area(temp_cmu_bounds)) # in m^2
        
        # get value and weight of all gridcells that overlap the cmu
        temp_pop_cmu_raster_perc_cover_df <- data.frame(raster::extract(temp_pop_raster, temp_cmu_bounds, weights = TRUE)[[1]])
        temp_qpf_cmu_raster_perc_cover_df <- data.frame(raster::extract(temp_qpf_raster, temp_cmu_bounds, weights = TRUE)[[1]])
        
        # calculate max value for pop
        temp_pop_area_weighted_df <- temp_pop_cmu_raster_perc_cover_df %>%
          dplyr::mutate(area_weighted_avg = value * weight,
                        max_val = max(value))
        
        # calculate area weighted avg value for qpf
        temp_qpf_area_weighted_df <- temp_qpf_cmu_raster_perc_cover_df %>%
          dplyr::mutate(area_weighted_avg = value * weight)
        
        # save data
        # keep max value to get cmu pop reslut
        # sum weighted values to get cmu qpf result
        temp_cmu_result_df_part <- data.frame(cmu_name = temp_cmu_name,
                                              temp_cmu_pop_avg_result = round(sum(temp_pop_area_weighted_df$area_weighted_avg), 4),
                                              temp_cmu_pop_max_result = max(temp_pop_area_weighted_df$max_val),
                                              temp_cmu_qpf_result = round(sum(temp_qpf_area_weighted_df$area_weighted_avg), 4)) 
        
        # bind results
        temp_cmu_result_df <-  bind_rows(temp_cmu_result_df, temp_cmu_result_df_part)
        
        # print when finished with point
        # print(paste0("finished cmu: ", k, " of ", num_unique_cmu))
      }
      
      # join location- and cmu-based results together
      temp_loc_cmu_results_df_join <- obs_metadata_albers_sel %>%
        st_drop_geometry() %>%
        dplyr::select(loc_id, cmu_name) %>%
        dplyr::left_join(temp_loc_result_df, by = "loc_id") %>%
        dplyr::left_join(temp_cmu_result_df, by = "cmu_name")
      
      # save data
      temp_ndfd_data_sel <- data.frame(loc_id = temp_loc_cmu_results_df_join$loc_id,
                                       cmu_name = temp_loc_cmu_results_df_join$cmu_name,
                                       date = temp_date_fix,
                                       valid_period_hrs = temp_valid_period,
                                       loc_pop_perc = temp_loc_cmu_results_df_join$temp_loc_pop_result,
                                       loc_qpf_in = temp_loc_cmu_results_df_join$temp_loc_qpf_result,
                                       cmu_pop_avg_perc = temp_loc_cmu_results_df_join$temp_cmu_pop_avg_result,
                                       cmu_pop_max_perc = temp_loc_cmu_results_df_join$temp_cmu_pop_max_result,
                                       cmu_qpf_in = temp_loc_cmu_results_df_join$temp_cmu_qpf_result)
      
      # bind results
      ndfd_data_sel <-  bind_rows(ndfd_data_sel, temp_ndfd_data_sel)
      
      # print when done with period
      print(paste0(temp_valid_period, " hr valid period completled for ", temp_date))
    }
    
    # print when finished with day
    print(paste0("finished ", temp_date, " analysis"))
  }
  
  # move to next data entry
  else {
    next
  }
}

# print time now
stop_time <- now()

# time to run loop
stop_time - start_time
# ~3 min for 1 day x 731 days = 2193 min / 60 = 36 hrs
# 20.39836 hours


# ---- 8. export data ----
# export spatial data
st_write(cmu_bounds_sel, paste0(ncdmf_spatial_data_output_path, "/cmu_bounds_sel.shp"), delete_layer = TRUE)
st_write(cmu_bounds_sel_5kmbuf, paste0(ncdmf_spatial_data_output_path, "/cmu_bounds_sel_5kmbuf_sel.shp"), delete_layer = TRUE)

# export precip metadata that overlaps and is 90% complete
st_write(obs_metadata_albers_sel, paste0(obs_spatial_data_output_path, "/obs_metadata_albers_sel.shp"), delete_layer = TRUE)

# export ndfd data
write_csv(ndfd_data_sel, paste0(ndfd_tabular_data_output_path, "/ndfd_data_sel.csv"))

