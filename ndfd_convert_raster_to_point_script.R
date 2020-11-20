# ---- script header ----
# script name: ndfd_convert_raster_to_point_script.R
# purpose of script: this script takes ndfd rasters (.tif) and historic precip gage locations
# and calculates ndfd variable (pop12 and qpf) for these locations based on various methods
# author: sheila saia
# date created: 20201109
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:


# ---- to do ----
# to do list

# TODO run analysis for all days and all valid times
# TODO run analysis for sites outside of NC but still in ws bounds?
# TODO run analysis for ws and noaa shoreline bounds too
# TODO run without 5 km buffer on obs
# TODO fix uct and et times in comparison part

# ---- 1. load libraries ----
library(tidyverse)
library(lubridate)
library(raster)
library(sf)


# ---- 2. define paths and projections ----

# spatial data path
spatial_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/"

# path to ndfd tabular inputs
ndfd_sco_tabular_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/ndfd_sco_hist_raw/"

# path to ndfd spatial inputs
ndfd_sco_spatial_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/ndfd_sco_hist/"

# path to ndfd tabular outputs
ndfd_sco_tabular_data_output_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sheila_generated/ndfd_sco_hist/"

# results/figures
# figures_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/results/figures/"

# define epsg and proj for CONUS Albers projection (projecting to this)
conus_albers_epsg <- 5070
conus_albers_proj <- "+init=EPSG:5070"


# ---- 3. load in data ----
# historic precip netadata (locations)
hist_precip_metadata_albers <- st_read(paste0(spatial_data_input_path, "hist_precip_data/hist_precip_metadata_albers.shp"))

# ndfd data available
data_available <- read_csv(paste0(ndfd_sco_tabular_data_input_path, "data_available.csv"), col_names = TRUE)

# urban boundaries
urban_bounds_combo_albers <- st_read(paste0(spatial_data_input_path, "region_state_bounds/urban_bounds_combo_albers.shp"))

# coast bounds (county based)
nc_coast_county_bounds_albers <- st_read(paste0(spatial_data_input_path, "coast_bounds/nc_coast_county_bounds_albers.shp"))

# coast bounds (ws based)

# coast bounds (shoreline based)

# all have crs = 5070, checks!


# ---- 4. county based: classify historic precip as urban/non-urban and coast/non-coast ----
# coast/non-coast determining dataset
boundary_dataset <- nc_coast_county_bounds_albers

# urban or non-urban
urban_logic_list <- st_intersects(hist_precip_metadata_albers, urban_bounds_combo_albers, sparse = FALSE)

# coast or non-coast
coast_logic_list <- st_intersects(hist_precip_metadata_albers, boundary_dataset, sparse = FALSE)

# add to spatial data
hist_precip_metadata_logic_albers <- hist_precip_metadata_albers %>%
  dplyr::mutate(urb_logic = urban_logic_list, # false for non-urban, true for urban
                cst_logic = coast_logic_list, # false for non-coast, true for coast
         urb_class = dplyr::if_else(urb_logic == TRUE, "urban", "non-urban"), 
         cst_class = dplyr::if_else(cst_logic == TRUE, "coast", "non-coast"),
         cmb_class = paste0(urb_class, "_", cst_class)) %>%
  dplyr::select(-urb_logic, -cst_logic)

# counts
# length(hist_precip_metadata_logic_albers$urb_class[hist_precip_metadata_logic_albers$urb_class == "urban"]) # 545
# length(hist_precip_metadata_logic_albers$urb_class[hist_precip_metadata_logic_albers$urb_class == "non-urban"]) # 534
# 545 + 534
# length(hist_precip_metadata_logic_albers$cst_class[hist_precip_metadata_logic_albers$cst_class == "coast"]) # 352
# length(hist_precip_metadata_logic_albers$cst_class[hist_precip_metadata_logic_albers$cst_class == "non-coast"]) # 727
# 352 + 727

# pull out just coastal data for now
hist_precip_metadata_coast_albers <- hist_precip_metadata_logic_albers %>%
  dplyr::filter(cmb_class == "urban_coast" | cmb_class == "non-urban_coast")

# make a buffer
hist_precip_metadata_coast_5kmbuf_albers <- hist_precip_metadata_coast_albers %>%
  st_buffer(dist = 5000) %>% # in m, so 1000m x 5 = 5km
  dplyr::select(loc_id, cmb_class)

# counts
# length(hist_precip_metadata_coast_albers$cmb_class[hist_precip_metadata_coast_albers$cmb_class == "urban_coast"]) # 106
# length(hist_precip_metadata_coast_albers$cmb_class[hist_precip_metadata_coast_albers$cmb_class == "non-urban_coast"]) # 190
# 106 + 190


# ---- 5. county based: compute area weighted ndfd point values ----
# files available
file_list <- list.files(path = ndfd_sco_spatial_data_input_path)

# valid period list
valid_period_list <- c(24, 48, 72)

# number of pts's
num_pts <- length(hist_precip_metadata_coast_5kmbuf_albers$loc_id)

# boundary type
temp_bound_type <- "county"

# go through and for each data point calculate area weighted avg qpf and pop12
ndfd_pts_calcs_data <- data.frame(loc_id = as.character(),
                                  bounds_type = as.character(),
                                  datetime_uct = as.character(),
                                  valid_period_hrs = as.numeric(),
                                  pop12_perc = as.numeric(),
                                  qpf_in = as.numeric())

# record start time
start_time <- now()



# read in data that's available
for (i in 32:181) { #dim(data_available)[1]) {
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
    temp_pop12_file_name <- sort(temp_files[grep(pattern = "pop12", x = temp_files)]) # sort to make sure order is 24, 48, 72
    temp_qpf_file_name <- sort(temp_files[grep(pattern = "qpf", x = temp_files)])
    
    # step through each period
    for (j in 1:length(valid_period_list)) {
      # valid period
      temp_valid_period <- valid_period_list[j]
      
      # find valid period files
      temp_prd_pop12_file_name <- temp_pop12_file_name[grep(pattern = paste0(as.character(temp_valid_period), "hr"), x = temp_pop12_file_name)]
      temp_prd_qpf_file_name <- temp_qpf_file_name[grep(pattern = paste0(as.character(temp_valid_period), "hr"), x = temp_qpf_file_name)]
      
      # read in ndfd raster data
      temp_pop12_raster <- raster::raster(paste0(ndfd_sco_spatial_data_input_path, temp_prd_pop12_file_name))
      temp_qpf_raster <- raster::raster(paste0(ndfd_sco_spatial_data_input_path, temp_prd_qpf_file_name))
      
      # save raster resolution
      temp_pop12_raster_res <- raster::res(temp_pop12_raster)
      temp_qpf_raster_res <- raster::res(temp_qpf_raster)
      
      # step through each point
      for (k in 1:num_pts) {
        # save pts name
        temp_pts_name <- as.character(hist_precip_metadata_coast_5kmbuf_albers$loc_id[k])
        
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
        
        # save data
        temp_ndfd_pts_calcs_data <- data.frame(loc_id = temp_pts_name,
                                               bounds_type = temp_bound_type,
                                               datetime_uct = temp_date,
                                               valid_period_hrs = temp_valid_period,
                                               pop12_perc = temp_pts_pop12_result,
                                               qpf_in = temp_pts_qpf_result)
        
        # bind results
        ndfd_pts_calcs_data <-  rbind(ndfd_pts_calcs_data, temp_ndfd_pts_calcs_data)
        
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
# Time difference of 6 min per day
# 6 min x 730 = 4380 min / 60 = 73 hours

# rename for export
ndfd_calcs_county_based_data <- ndfd_pts_calcs_data


# ---- 6. export all data ----
# export county based spatial data
st_write(hist_precip_metadata_logic_albers, paste0(spatial_data_input_path, "hist_precip_data/county_based/hist_precip_metadata_logic_albers.shp"), delete_layer = TRUE)
st_write(hist_precip_metadata_coast_albers, paste0(spatial_data_input_path, "hist_precip_data/county_based/hist_precip_metadata_coast_albers.shp"), delete_layer = TRUE)
st_write(hist_precip_metadata_coast_5kmbuf_albers, paste0(spatial_data_input_path, "hist_precip_data/county_based/hist_precip_metadata_coast_5kmbuf_albers.shp"), delete_layer = TRUE)

# export county based ndfd calcs
write_csv(ndfd_calcs_county_based_data, paste0(ndfd_sco_tabular_data_output_path, "ndfd_calcs_county_based_data_to20150201to20150630.csv"))





# ---- old code ----
# valid period list
valid_period_list <- c(24)

# define date
ndfd_date_uct <- lubridate::ymd_hm("2015-01-02 00:00")

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
