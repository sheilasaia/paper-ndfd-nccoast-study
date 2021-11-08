# ---- script header ----
# script name: 11_sco_get_normals_script.R
# purpose of script: pulling data from normals
# author:
# date created:
# email:


# ---- notes ----
# notes:

# SCO TDS server link to normals data
# https://tds.climate.ncsu.edu/thredds/catalog/prism/normals_4km/catalog.html

# helpful links
# https://ropensci.org/blog/2019/11/05/tidync/
# https://cran.r-project.org/web/packages/tidync/tidync.pdf
# https://www.r-bloggers.com/2016/08/a-netcdf-4-in-r-cheatsheet/
# https://pjbartlein.github.io/REarthSysSci/netCDF.html
# https://pjbartlein.github.io/REarthSysSci/raster_intro.html
# http://132.72.155.230:3838/r/index.html

# # prism data tutorial
# https://rstudio-pubs-static.s3.amazonaws.com/260164_978471fad4f248428a52b8ff72b774be.html


# ---- to do ----
# to do list:


# ---- install packages ----
library(tidyverse)
library(raster)
library(ncdf4)
library(sf)
library(here)


# ---- load data ----

# obs daily metadata shapefile
obs_metadata_shp <- sf::st_read(here::here("data", "spatial", "obs_data_tidy", "obs_metadata_albers_sel.shp"))


# ---- get (prism) annual 30yr normals ----
# define url
normals_annual_url <- "https://tds.climate.ncsu.edu/thredds/dodsC/prism/normals_4km/PRISM_30yr_normal_4kmM2_annual.nc"

# open nc data from url (.nc file extension = netcdf4 file)
normals_annual_nc <- nc_open(normals_annual_url)
# normals_annual_nc

# get longitude and latitude
normals_lon <- ncvar_get(normals_annual_nc, "longitude")
normals_lat <- ncvar_get(normals_annual_nc, "latitude")

# define extent
normals_xmin <- min(normals_lon)
normals_xmax <- max(normals_lon)
normals_ymin <- min(normals_lat)
normals_ymax <- max(normals_lat)

# note to self for whole united states
# top left (normals_xmin, normals_ymax)
# top right (normals_xmax, normals_ymax)
# bottom left (normals_xmin, normals_ymin)
# bottom right (normals_xmin, normals_ymin)

# check dimensions of longitude and latitude
nlon <- dim(normals_lon)
nlat <- dim(normals_lat)
print(c(nlon,nlat))
# 1405 x 621

# annual precip data
normals_annual_precip_data <- ncvar_get(normals_annual_nc, "ppt")
# in units of mm
# filled with NAs

# make a raster
normals_annual_precip_raster <- raster::raster(x = t(normals_annual_precip_data),
                                               xmn = normals_xmin,
                                               xmx = normals_xmax,
                                               ymn = normals_ymin, 
                                               ymx = normals_ymax,
                                               crs = "+proj=longlat +datum=WGS84")

# plot to make sure it's oriented correctly
# not really sure why i had to transpose these data...but plot looks fine
plot(normals_annual_precip_raster)

# define epsg and proj for CONUS Albers Equal Area projection (projecting to this)
# conus_albers_epsg <- 5070
conus_albers_proj <- "+init=EPSG:5070"
# resolution of prism normals
normals_res <- 4*1000 # 4km by 4km in m = 4000

# convert to albers
normals_precip_raster_albers <- raster::projectRaster(normals_annual_precip_raster, 
                                                      crs = conus_albers_proj, 
                                                      res = normals_res)
# plot to check
plot(normals_precip_raster_albers)
# looks good!

# pull data for observations locations
normals_annual_precip_vals <- obs_metadata_shp %>%
  dplyr::select(loc_id) %>% 
  dplyr::mutate(normals_annual_precip_cm = round((raster::extract(normals_precip_raster_albers, obs_metadata_shp, weights = FALSE) / 10), 2))

# plot to check
ggplot(data = normals_annual_precip_vals) +
  geom_sf(aes(color = normal_annual_precip_cm))


# ---- get (prism) monthly 30yr normals ----
# need to repeat this for all the months
num_months <- 12

# initiate empty data frame
normals_month_precip_vals <- obs_metadata_shp %>%
  dplyr::select(loc_id)

# loop through months
for (i in 1:num_months) {
  # pad single digits
  if (i < 10) {
    i_chr <- str_pad(string = as.character(i), width = 2, side = "left", pad = "0")
  } else {
    i_chr <- as.character(i)
  }
  
  # define path based on iteration
  temp_url <- paste0("https://tds.climate.ncsu.edu/thredds/dodsC/prism/normals_4km/PRISM_30yr_normal_4kmM2_", i_chr, ".nc")
  
  # open nc data from url
  temp_normals_data_nc <- nc_open(temp_url)
  
  # get precip data
  temp_normals_precip_nc <- ncvar_get(temp_normals_data_nc, "ppt")
  # in mm, filled with NAs 
  
  # make a raster
  temp_normals_precip_raster <- raster::raster(x = t(temp_normals_precip_nc),
                                               xmn = normals_xmin,
                                               xmx = normals_xmax,
                                               ymn = normals_ymin, 
                                               ymx = normals_ymax,
                                               crs = "+proj=longlat +datum=WGS84")
  
  # convert to albers
  temp_normals_precip_raster_albers <- raster::projectRaster(temp_normals_precip_raster, 
                                                             crs = conus_albers_proj, 
                                                             res = normals_res)
  
  # extract data for each station
  temp_normals_precip_cm <- data.frame(round((raster::extract(temp_normals_precip_raster_albers, obs_metadata_shp, weights = FALSE) / 10), 2))
  
  # rename column name
  names(temp_normals_precip_cm) <- eval(paste0("normals_month_precip_cm_", i_chr))
  
  # bind column
  normals_month_precip_vals <- bind_cols(normals_month_precip_vals, temp_normals_precip_cm)
  
  # print status
  print(paste0("finished month ", i_chr))
}

# summarize all months
normals_month_precip_summary <- normals_month_precip_vals %>%
  sf::st_drop_geometry() %>%
  tidyr::pivot_longer(cols = normals_month_precip_cm_01:normals_month_precip_cm_12,
                      names_to = "month",
                      values_to = "normals_monthly_precip_cm") %>%
  dplyr::mutate(month_num = as.numeric(str_sub(string = month, start = -2, end = -1))) %>%
  dplyr::select(-month) %>%
  dplyr::group_by(month_num) %>%
  dplyr::summarize(normals_monthly_area_mean_precip_cm = mean(normals_monthly_precip_cm))

# check
sum(normals_month_precip_summary$normals_monthly_area_mean_precip_cm)
# 145 cm which is about equal to the annual value

# summarize for annual data
normals_annual_precip_summary <- normals_annual_precip_vals %>%
  dplyr::distinct() %>%
  sf::st_drop_geometry() %>%
  dplyr::ungroup() %>%
  dplyr::summarize(normals_annual_area_mean_precip_mm = mean(normals_annual_precip_cm))

# get value
normals_annual_precip_summary$normals_annual_area_mean_precip_mm
# 142.91 cm




