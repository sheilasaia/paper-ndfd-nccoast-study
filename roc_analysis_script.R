
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

# for pROC package
# https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
# https://cran.r-project.org/web/packages/pROC/pROC.pdf


# ---- to do ----
# to do list

# TODO move roc ndfd calcs to separate script! do just import ndfd_data_sel.csv here
# reloop through roc ndfd calcs b/c fewer leases now due to issue with counting NAs in previous script

# ---- 1. load libraries ----
library(tidyverse)
library(sf)
library(here)
library(tidylog)
library(lubridate)
library(pROC)


# ---- 2. define paths ----
# data path (for now --> use here package later)
data_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/"

# path to ndfd tabular inputs
ndfd_sco_tabular_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/ndfd_sco_hist_raw/"

# path to ndfd spatial inputs
ndfd_sco_spatial_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/ndfd_sco_hist/"


# ---- 3. load data ----
# import cmu bounds (spatial)
cmu_bounds_albers <- st_read(paste0(data_path, "spatial/sheila_generated/cmu_bounds/cmu_bounds_albers.shp"))

# import lease centroids (spatial)
lease_centroids_albers <- st_read(paste0(data_path, "spatial/sheila_generated/lease_bounds/lease_centroids_albers.shp"))

# import historic precip metadata (spatial)
hist_precip_metadata_albers <- st_read(paste0(data_path, "spatial/sheila_generated/hist_precip_data/hist_precip_metadata_albers.shp"))

# import historic precip data (tabular)
hist_precip_data <- read_csv(file = paste0(data_path, "tabular/sheila_generated/hist_precip_data/hist_precip_data_compiled.csv"), col_names = TRUE,
                             col_types = list(col_character(), col_date(), col_number()))

# cmu information (tabular)
cmu_sga_key <- read_csv(file = paste0(data_path, "tabular/sheila_generated/ncdmf_rainfall_thresholds/cmu_sga_key.csv"), col_names = TRUE)

# sga information (tabular)
sga_key <- read_csv(file = paste0(data_path, "tabular/sheila_generated/ncdmf_rainfall_thresholds/sga_key.csv"), col_names = TRUE)

# rainfall thresholds (tabular)
rainfall_thresh_data_raw <- read_csv(file = paste0(data_path, "tabular/sheila_generated/ncdmf_rainfall_thresholds/rainfall_thresholds_raw_tidy.csv"), col_names = TRUE)

# ndfd data available
data_available <- read_csv(paste0(ndfd_sco_tabular_data_input_path, "data_available.csv"), col_names = TRUE)

# ndfd selected data
ndfd_data_sel <- read_csv(paste0(data_path, "tabular/sheila_generated/ndfd_sco_hist/ndfd_data_sel.csv"), col_names = TRUE)


# ---- 4. find cmu's with the most leases ----
# tidy rainfall threshold data 
rainfall_thresh_data <- rainfall_thresh_data_raw %>%
  select(HA_CLASS, cmu_name) %>%
  left_join(cmu_sga_key, by = "HA_CLASS") %>%
  left_join(sga_key, by = "grow_area") %>%
  select(-googlemaps_description, -sga_desc_short)

# summarize number of leases in each cmu
lease_cmu_count <- lease_centroids_albers %>%
  st_drop_geometry() %>%
  group_by(cmu_name) %>%
  summarize(lease_count = n()) %>%
  left_join(rainfall_thresh_data, by = "cmu_name")

# join lease counts to cmu data (for plotting)
cmu_bounds_lease_count_join <- cmu_bounds_albers %>%
  left_join(lease_cmu_count, by = "cmu_name")

# tabular data
cmu_bounds_lease_count_join_tabular <- cmu_bounds_lease_count_join %>%
  st_drop_geometry()

# plot by lease count
pdf(file = here::here("figures", "cmu_lease_counts.pdf"), width = 10, height = 10)
ggplot(data = cmu_bounds_lease_count_join) +
  geom_sf(aes(fill = lease_count)) + 
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey80") +
  theme_classic()
dev.off()

# plot rainfall threshold depth by number of leases
ggplot(data = cmu_bounds_lease_count_join) +
  geom_point(aes(x = rain_in, y = lease_count)) +
  theme_classic()

# leases to pick
# cmu with 10 leases at 1 in (U092)
# cmu with 30 leases at 1.5 in (U096)
# cmu with 34 leases at 1.5 in (U130) - this one has very few observations so decided not to choose it
# cmu with 23 leases at 3 in (U144)


# ---- 5. select cmu's for analysis ----
# cmu list
cmu_sel_list <- c("U092", "U096", "U144")

# select cmus
cmu_bounds_roc_sel <- cmu_bounds_albers %>%
  filter(cmu_name %in% cmu_sel_list)

# buffer by 5km
cmu_bounds_5kmbuf_roc <- cmu_bounds_roc_sel %>%
  st_buffer(dist = 5000) # dist = 5000 means 5km

# plot selected cmu's with buffers
pdf(file = here::here("figures", "cmu_selection_with_buffer.pdf"), width = 10, height = 10)
ggplot() +
  geom_sf(data = cmu_bounds_5kmbuf_roc, color = "blue", alpha = 0.75) +
  geom_sf(data = cmu_bounds_lease_count_join) + 
  geom_sf(data = cmu_bounds_roc_sel, fill = "blue") +
  theme_classic()
dev.off()


# ---- 6. find precip data that overlaps ----
hist_precip_metadata_sel <- hist_precip_metadata_albers %>%
  st_intersection(cmu_bounds_5kmbuf_roc) %>%
  filter(perc_compl >= 90) # filter out ones that are 90% complete or more
# without the buffer there are no raingages within the cmus

# plot cmus and observations
pdf(file = here::here("figures", "cmu_selection_with_obs.pdf"), width = 10, height = 10)
ggplot() +
  geom_sf(data = cmu_bounds_5kmbuf_roc, color = "blue", alpha = 0.75) +
  geom_sf(data = cmu_bounds_roc_sel, fill = "blue") +
  geom_sf(data = hist_precip_metadata_sel, color = "black") +
  theme_classic()
dev.off()


# ---- 7. loop through ndfd data to get forecast result ----
# files available
file_list <- list.files(path = ndfd_sco_spatial_data_input_path)

# valid period list
valid_period_list <- c(24, 48, 72)

# number of observations
num_obs <- dim(hist_precip_metadata_sel)[1]

# make empty data frame
ndfd_data_sel <- data.frame(loc_id = as.character(),
                            cmu_name = as.character(),
                            date = as.character(),
                            valid_period_hrs = as.numeric(),
                            loc_qpf_in = as.numeric(),
                            cmu_qpf_in = as.numeric())
# loc_qpf_in is based on the ndfd grid cell that the gage is in
# cmu_qpf_in is based on the area weighted avg footprint of the cmu associated with the gage (shellcast)

# record start time
start_time <- now()

# for loop (i = day with data, j = valid period, k = observation)
for (i in 288:dim(data_available)[1]) {
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
    temp_qpf_file_name <- sort(temp_files[grep(pattern = "qpf", x = temp_files)])
    
    # step through each period
    for (j in 1:length(valid_period_list)) {
      # valid period
      temp_valid_period <- valid_period_list[j]
      
      # find valid period files
      temp_prd_qpf_file_name <- temp_qpf_file_name[grep(pattern = paste0(as.character(temp_valid_period), "hr"), x = temp_qpf_file_name)]
      
      # read in ndfd raster data
      temp_qpf_raster <- raster::raster(paste0(ndfd_sco_spatial_data_input_path, temp_prd_qpf_file_name))
      
      # save raster resolution
      temp_qpf_raster_res <- raster::res(temp_qpf_raster)
      
      # step through each obs (with unique loc_id and cmu_name)
      for (k in 1:num_obs) {
        # save location name
        temp_loc_id <- as.character(hist_precip_metadata_sel$loc_id[k])
        
        # save cmu name
        temp_cmu_name <- as.character(hist_precip_metadata_sel$cmu_name[k])
        
        # location-based calcs
        # get location location
        temp_loc_coord <- hist_precip_metadata_sel %>%
          dplyr::filter(loc_id == temp_loc_id) %>%
          dplyr::select(geometry)
        
        # get value of gridcell that overlaps with location
        temp_loc_qpf_result <- round(raster::extract(temp_qpf_raster, temp_loc_coord, weights = FALSE)[[1]], 4)
        
        # cmu-based calcs (like shellcast)
        # get cmu bounds vector
        temp_cmu_bounds <- cmu_bounds_roc_sel %>%
          dplyr::filter(cmu_name == temp_cmu_name) %>%
          dplyr::select(geometry)
        
        # cmu bounds area
        temp_cmu_area <- as.numeric(st_area(temp_cmu_bounds)) # in m^2
        
        # get value and weight of all gridcells that overlap the cmu
        temp_qpf_cmu_raster_perc_cover_df <- data.frame(raster::extract(temp_qpf_raster, temp_cmu_bounds, weights = TRUE)[[1]])
        
        # calculate area weighted avg value for the pts
        temp_qpf_area_weighted_df <- temp_qpf_cmu_raster_perc_cover_df %>%
          dplyr::mutate(area_weighted_avg = value * weight)
        
        # sum weighted values to get cmu result
        temp_cmu_qpf_result <- round(sum(temp_qpf_area_weighted_df$area_weighted_avg), 4)
        
        # save data
        temp_ndfd_data_sel <- data.frame(loc_id = temp_loc_id,
                                         cmu_name = temp_cmu_name,
                                         date = temp_date,
                                         valid_period_hrs = temp_valid_period,
                                         loc_qpf_in = temp_loc_qpf_result,
                                         cmu_qpf_in = temp_cmu_qpf_result)
        
        # bind results
        ndfd_data_sel <-  rbind(ndfd_data_sel, temp_ndfd_data_sel)
        
        # print when finished with point
        # print(paste0("finished row: ", k))
      }
      
      # print when done with period
      print(paste0(temp_valid_period, " hr period appended for ", temp_date))
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
# Time difference of X min per day
# X min x 731 = X min / 60 = X hours


# ---- 7. roc curve analysis ----
# plot loc qpf vs cmu qpf (do ROC for both?)
ggplot(data = ndfd_data_sel) +
  geom_point(aes(x = loc_qpf_in, y = cmu_qpf_in, color = month(date, label = TRUE)), alpha = 0.75, size = 3) +
  labs(x = "QPF value of observation gridcell (in)", y = "Weighted area avg QPF of CMU assoc. w/ observation (in)", color = "Month") +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  theme_classic()
# very similar 

# select metadata columns to join
loc_metadata_to_join <- hist_precip_metadata_sel %>%
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
  dplyr::left_join(loc_data_to_join, by = c("loc_id", "date"))

# 1in 24hr cmu-based
roc_data_1in <- roc_data %>%
  dplyr::filter(rain_in == 1.0)
roc_1in_24hr_cmu <- roc(roc_data_1in$cmu_qpf_binary, roc_data_1in$precip_in)
roc_1in_24hr_cmu$auc # 0.74
plot(roc_1in_24hr_cmu, print.thres="best", print.thres.best.method="youden")
plot(roc_1in_24hr_cmu, print.thres="best", print.thres.best.method="closest.topleft")
# coords(roc_1in_24hr_cmu, "best", ret="threshold", best.method="closest.topleft")  # this gives the threshold value 

# 1.5in 24hr cmu-based
roc_data_1.5in <- roc_data %>%
  dplyr::filter(rain_in == 1.5)
roc_data_1.5in <- roc(roc_data_1.5in$cmu_qpf_binary, roc_data_1.5in$precip_in)
roc_data_1.5in$auc # 0.75
plot(roc_data_1.5in, print.thres="best", print.thres.best.method="youden")
plot(roc_data_1.5in, print.thres="best", print.thres.best.method="closest.topleft")
# coords(roc_data_1.5in, "best", ret="threshold", best.method="closest.topleft")  # this gives the threshold value 

# 3in 24hr cmu-based
roc_data_3in <- roc_data %>%
  dplyr::filter(rain_in == 3)
roc_data_3in <- roc(roc_data_3in$cmu_qpf_binary, roc_data_3in$precip_in)
roc_data_3in$auc # 0.90
plot(roc_data_3in, print.thres="best", print.thres.best.method="youden")
plot(roc_data_3in, print.thres="best", print.thres.best.method="closest.topleft")
# coords(roc_data_1.5in, "best", ret="threshold", best.method="closest.topleft")  # this gives the threshold value 



# ---- 8. export data ----
# export spatial data
st_write(cmu_bounds_roc_sel, paste0(data_path, "spatial/sheila_generated/cmu_bounds/cmu_bounds_roc_sel.shp"), delete_layer = TRUE)
st_write(cmu_bounds_5kmbuf_roc, paste0(data_path, "spatial/sheila_generated/cmu_bounds/cmu_bounds_5kmbuf_roc.shp"), delete_layer = TRUE)

# export ndfd data
write_csv(ndfd_data_sel, paste0(data_path, "tabular/sheila_generated/ndfd_sco_hist/ndfd_data_sel.csv"))

# data <- st_read("/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/sga_bounds/sga_bounds_class_albers.shp")
# length(data$grow_area) # 617 total
# unique(data$ga_class)
# length(data$grow_area[data$ga_class == "cond_approved"]) # 84
# 84/617 * 100
# # 13.6
# length(data$grow_area[data$ga_class == "approved"])/length(data$grow_area) * 100
# # 10.7
# length(data$grow_area[data$ga_class == "restricted"])/length(data$grow_area) * 100
# # 22
# length(data$grow_area[data$ga_class == "prohibited"])/length(data$grow_area) * 100
# # 54
# 54 + 22 + 11 + 13



