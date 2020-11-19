# ---- script header ----
# script name: ndfd_convert_df_to_raster_script.R
# purpose of script: converts raw ndfd dataframes to raster data for downstream analysis
# author: sheila saia
# date created: 20200917
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:


# ---- to do ----
# to do list

# TODO (wishlist) use here package
# TODO (wishlist) use terra package for raster stuff
# TODO add versions of packages used, version of R used
# TODO reorganize data


# ---- 1. load packages as necessary ----
library(tidyverse)
library(raster)
library(sf)
library(lubridate)


# ---- 2. define base paths ----
# path to ndfd tabular inputs
ndfd_sco_tabular_data_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/ndfd_sco_hist_raw/"

# path to nc buffer spatial inputs
nc_buffer_spatial_input_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/region_state_bounds/"

# path to ndfd spatial outputs
ndfd_sco_spatial_data_output_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/ndfd_sco_hist/"


# ---- 3. define projections ----
# define proj4 string for ndfd data
ndfd_proj4 = "+proj=lcc +lat_1=25 +lat_2=25 +lat_0=25 +lon_0=-95 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"
# source: https://spatialreference.org/ref/sr-org/6825/

# define epsg and proj4 for N. America Albers projection (projecting to this)
# na_albers_proj4 <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
# na_albers_epsg <- 102008

# define epsg and proj for CONUS Albers projection (projecting to this)
conus_albers_proj <- "+init=EPSG:5070"
conus_albers_epsg <- 5070

# define wgs 84 projection
# wgs84_epsg <- 4326
# wgs84_proj4 <- "+proj=longlat +datum=WGS84 +no_defs"


# ---- 4. load data ----
# data available
data_available <- read_csv(paste0(ndfd_sco_tabular_data_input_path, "data_available.csv"), col_names = TRUE)

# nc buffer bounds
nc_buffer_albers <- st_read(paste0(nc_buffer_spatial_input_path, "nc_bounds_10kmbuf_albers.shp"))


# ---- 5. loop ----

# files available
file_list <- list.files(path = ndfd_sco_tabular_data_input_path)

# read in data that's available
for (i in 1:4) { #dim(data_available)[1]) {
  status <- data_available$status[i]
  
  if (status == "available") {
    # select date of data to pull
    temp_date <- ymd(data_available$datetime_uct_str[i])
    
    # convert date to string
    temp_date_str <- paste0(str_remove_all(strftime(temp_date, format = "%Y-%m-%d"), "-"), "00")
    
    # get files with that date
    temp_file_pattern <- paste0("*", temp_date_str, ".csv")
    temp_files <- file_list[grep(pattern = temp_file_pattern, x = file_list)]
    
    # save pop12 and qpf file names
    temp_pop12_file_name <- temp_files[grep(pattern = "pop12", x = temp_files)]
    temp_qpf_file_name <- temp_files[grep(pattern = "qpf", x = temp_files)]
    
    # load in tabular data
    temp_ndfd_pop12_data_raw <- read_csv(paste0(ndfd_sco_tabular_data_input_path, temp_pop12_file_name),
                                         col_types = list(col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(),
                                                          col_character(), col_character(), col_character(), col_character(), col_character()))
    temp_ndfd_qpf_data_raw <- read_csv(paste0(ndfd_sco_tabular_data_input_path, temp_qpf_file_name),
                                       col_types = list(col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(),
                                                        col_character(), col_character(), col_character(), col_character(), col_character()))
    
    # initial clean up
    temp_ndfd_pop12_data <- temp_ndfd_pop12_data_raw %>%
      dplyr::select(x_index, y_index, latitude_km, longitude_km, time_uct, time_nyc, pop12_value_perc, valid_period_hrs) %>%
      dplyr::mutate(latitude_m = latitude_km * 1000,
                    longitude_m = longitude_km * 1000)
    temp_ndfd_qpf_data <- temp_ndfd_qpf_data_raw %>%
      dplyr::select(x_index, y_index, latitude_km, longitude_km, time_uct, time_nyc, qpf_value_kgperm2, valid_period_hrs) %>%
      dplyr::mutate(latitude_m = latitude_km * 1000,
                    longitude_m = longitude_km * 1000,
                    qpf_value_in = qpf_value_kgperm2 * (1/1000) * (100) * (1/2.54)) # convert to m (density of water is 1000 kg/m3) then cm then inches
    
    # convert to spatial data
    temp_ndfd_pop12_albers <- st_as_sf(temp_ndfd_pop12_data,
                                       coords = c("longitude_m", "latitude_m"),
                                       crs = ndfd_proj4,
                                       dim = "XY") %>%
      st_transform(crs = conus_albers_epsg)
    temp_ndfd_qpf_albers <- st_as_sf(temp_ndfd_qpf_data,
                                     coords = c("longitude_m", "latitude_m"),
                                     crs = ndfd_proj4,
                                     dim = "XY") %>%
      st_transform(crs = conus_albers_epsg)
    
    # select 1-day
    temp_ndfd_pop12_albers_1day <- temp_ndfd_pop12_albers %>%
      dplyr::filter(valid_period_hrs == 24)
    temp_ndfd_qpf_albers_1day <- temp_ndfd_qpf_albers %>%
      dplyr::filter(valid_period_hrs == 24)
    
    # select 2-day
    temp_ndfd_pop12_albers_2day <- temp_ndfd_pop12_albers %>%
      dplyr::filter(valid_period_hrs == 48)
    temp_ndfd_qpf_albers_2day <- temp_ndfd_qpf_albers %>%
      dplyr::filter(valid_period_hrs == 48)
    
    # select 3-day
    temp_ndfd_pop12_albers_3day <- temp_ndfd_pop12_albers %>%
      dplyr::filter(valid_period_hrs == 72)
    temp_ndfd_qpf_albers_3day <- temp_ndfd_qpf_albers %>%
      dplyr::filter(valid_period_hrs == 72)
    
    # check pop12 projection
    # st_crs(temp_ndfd_pop12_albers)
    # pop12 periods available
    # unique(temp_ndfd_pop12_albers$valid_period_hrs)
    # st_crs(temp_ndfd_qpf_albers)
    # qpf periods available
    # unique(temp_ndfd_qpf_albers$valid_period_hrs)
    
    # make empty raster for 1-day, 2-day, and 3-day forecasts
    temp_ndfd_pop12_grid_1day <- raster(ncol = length(unique(temp_ndfd_pop12_albers_1day$longitude_km)),
                                   nrows = length(unique(temp_ndfd_pop12_albers_1day$latitude_km)),
                                   crs = conus_albers_proj,
                                   ext = extent(temp_ndfd_pop12_albers_1day))
    temp_ndfd_pop12_grid_2day <- raster(ncol = length(unique(temp_ndfd_pop12_albers_2day$longitude_km)),
                                   nrows = length(unique(temp_ndfd_pop12_albers_2day$latitude_km)),
                                   crs = conus_albers_proj,
                                   ext = extent(temp_ndfd_pop12_albers_2day))
    temp_ndfd_pop12_grid_3day <- raster(ncol = length(unique(temp_ndfd_pop12_albers_3day$longitude_km)),
                                   nrows = length(unique(temp_ndfd_pop12_albers_3day$latitude_km)),
                                   crs = conus_albers_proj,
                                   ext = extent(temp_ndfd_pop12_albers_3day))
    temp_ndfd_qpf_grid_1day <- raster(ncol = length(unique(temp_ndfd_qpf_albers_1day$longitude_km)),
                                 nrows = length(unique(temp_ndfd_qpf_albers_1day$latitude_km)),
                                 crs = conus_albers_proj,
                                 ext = extent(temp_ndfd_qpf_albers_1day))
    temp_ndfd_qpf_grid_2day <- raster(ncol = length(unique(temp_ndfd_qpf_albers_2day$longitude_km)),
                                 nrows = length(unique(temp_ndfd_qpf_albers_2day$latitude_km)),
                                 crs = conus_albers_proj,
                                 ext = extent(temp_ndfd_qpf_albers_2day))
    temp_ndfd_qpf_grid_3day <- raster(ncol = length(unique(temp_ndfd_qpf_albers_3day$longitude_km)),
                                 nrows = length(unique(temp_ndfd_qpf_albers_3day$latitude_km)),
                                 crs = conus_albers_proj,
                                 ext = extent(temp_ndfd_qpf_albers_3day))
    
    # rasterize for 1-day, 2-day, and 3-day forecasts
    temp_ndfd_pop12_raster_1day_albers <- raster::rasterize(temp_ndfd_pop12_albers_1day, temp_ndfd_pop12_grid_1day, field = temp_ndfd_pop12_albers_1day$pop12_value_perc, fun = mean)
    temp_ndfd_pop12_raster_2day_albers <- raster::rasterize(temp_ndfd_pop12_albers_2day, temp_ndfd_pop12_grid_2day, field = temp_ndfd_pop12_albers_2day$pop12_value_perc, fun = mean)
    temp_ndfd_pop12_raster_3day_albers <- raster::rasterize(temp_ndfd_pop12_albers_3day, temp_ndfd_pop12_grid_3day, field = temp_ndfd_pop12_albers_3day$pop12_value_perc, fun = mean)
    temp_ndfd_qpf_raster_1day_albers <- raster::rasterize(temp_ndfd_qpf_albers_1day, temp_ndfd_qpf_grid_1day, field = temp_ndfd_qpf_albers_1day$qpf_value_in, fun = mean)
    temp_ndfd_qpf_raster_2day_albers <- raster::rasterize(temp_ndfd_qpf_albers_2day, temp_ndfd_qpf_grid_2day, field = temp_ndfd_qpf_albers_2day$qpf_value_in, fun = mean)
    temp_ndfd_qpf_raster_3day_albers <- raster::rasterize(temp_ndfd_qpf_albers_3day, temp_ndfd_qpf_grid_3day, field = temp_ndfd_qpf_albers_3day$qpf_value_in, fun = mean)
    # crs(temp_ndfd_pop12_grid_1day_albers)
    # crs(temp_ndfd_qpf_grid_1day_albers)
    
    # plot to check
    # plot(temp_ndfd_pop12_raster_1day_albers)
    # plot(temp_ndfd_qpf_raster_1day_albers)
    # plot(temp_ndfd_pop12_raster_2day_albers)
    # plot(temp_ndfd_qpf_raster_2day_albers)
    # plot(temp_ndfd_pop12_raster_3day_albers)
    # plot(temp_ndfd_qpf_raster_3day_albers)
    
    # crop raster to nc bounds for 1-day, 2-day, and 3-day forecasts
    temp_ndfd_pop12_raster_1day_nc_albers <- raster::crop(temp_ndfd_pop12_raster_1day_albers, nc_buffer_albers)
    temp_ndfd_pop12_raster_2day_nc_albers <- raster::crop(temp_ndfd_pop12_raster_2day_albers, nc_buffer_albers)
    temp_ndfd_pop12_raster_3day_nc_albers <- raster::crop(temp_ndfd_pop12_raster_3day_albers, nc_buffer_albers)
    temp_ndfd_qpf_raster_1day_nc_albers <- raster::crop(temp_ndfd_qpf_raster_1day_albers, nc_buffer_albers)
    temp_ndfd_qpf_raster_2day_nc_albers <- raster::crop(temp_ndfd_qpf_raster_2day_albers, nc_buffer_albers)
    temp_ndfd_qpf_raster_3day_nc_albers <- raster::crop(temp_ndfd_qpf_raster_3day_albers, nc_buffer_albers)
    
    # plot to check
    # plot(nc_buffer_albers)
    # plot(temp_ndfd_pop12_raster_1day_nc_albers)
    # plot(temp_ndfd_qpf_raster_1day_nc_albers)
    # plot(temp_ndfd_pop12_raster_2day_nc_albers)
    # plot(temp_ndfd_qpf_raster_2day_nc_albers)
    # plot(temp_ndfd_pop12_raster_3day_nc_albers)
    # plot(temp_ndfd_qpf_raster_3day_nc_albers)
    
    # export rasters for 1-day, 2-day, and 3-day forecasts
    writeRaster(temp_ndfd_pop12_raster_1day_nc_albers, paste0(ndfd_sco_spatial_data_output_path, "pop12_24hr_nc_albers_", temp_date_str, ".tif"), overwrite = TRUE)
    writeRaster(temp_ndfd_pop12_raster_2day_nc_albers, paste0(ndfd_sco_spatial_data_output_path, "pop12_48hr_nc_albers_", temp_date_str, ".tif"), overwrite = TRUE)
    writeRaster(temp_ndfd_pop12_raster_3day_nc_albers, paste0(ndfd_sco_spatial_data_output_path, "pop12_72hr_nc_albers_", temp_date_str, ".tif"), overwrite = TRUE)
    writeRaster(temp_ndfd_qpf_raster_1day_nc_albers, paste0(ndfd_sco_spatial_data_output_path, "qpf_24hr_nc_albers_", temp_date_str, ".tif"), overwrite = TRUE)
    writeRaster(temp_ndfd_qpf_raster_2day_nc_albers, paste0(ndfd_sco_spatial_data_output_path, "qpf_48hr_nc_albers_", temp_date_str, ".tif"), overwrite = TRUE)
    writeRaster(temp_ndfd_qpf_raster_3day_nc_albers, paste0(ndfd_sco_spatial_data_output_path, "qpf_72hr_nc_albers_", temp_date_str, ".tif"), overwrite = TRUE)
    
    # print status
    print(paste0("finished converting df to raster for ", temp_date_str))
  }
  
  # move to next data entry
  else {
    next
  }
}



