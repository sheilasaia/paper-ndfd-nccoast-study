# ---- script header ----
# script name: 11_sco_get_normals_script.R
# purpose of script: pulling data from normals
# author:
# date created:
# email:


# ---- notes ----
# notes:


# ---- to do ----
# to do list:


# ---- install packages ----
# install.packages("tidync")
# remotes::install_github("ropensci/tidync", dependencies = TRUE)
# remote install version of tidync was giving me errors when i tab autocompleted
library(tidync)
library(tidyverse)
library(raster)

# https://ropensci.org/blog/2019/11/05/tidync/
# https://cran.r-project.org/web/packages/tidync/tidync.pdf
# https://www.r-bloggers.com/2016/08/a-netcdf-4-in-r-cheatsheet/
# https://pjbartlein.github.io/REarthSysSci/netCDF.html
# lat long is center of grid (likely)

# prism data tutorial
# https://rstudio-pubs-static.s3.amazonaws.com/260164_978471fad4f248428a52b8ff72b774be.html

# SCO TDS server link to normals data
# https://tds.climate.ncsu.edu/thredds/catalog/prism/normals_4km/catalog.html

# get annual normals
normal_ppt_annual_ras <- brick(x = "https://tds.climate.ncsu.edu/thredds/dodsC/prism/normals_4km/PRISM_30yr_normal_4kmM2_annual.nc",
                               varname = "ppt",
                               crs = "+proj=longlat +datum=WGS84")
# normal_ppt_annual_ras

# my extent
# has to be xmin, xmax, ymin, ymax
# cmu bounds
# cmu_bbox_wgs84
# xmin      ymin      xmax      ymax 
# -78.60738  33.76421 -75.35710  36.06080 
cmu_bbox_wgs84_extent <- extent(-78.60738, -75.35710, 33.76421, 36.06080)
# x = long
# y = lat

# crop with extent
cmu_normal_ppt_annual_ras <- extract(normal_ppt_annual_ras, cmu_bbox_wgs84_extent)


# get annual normals
normals_annual_nc <- tidync(x = "https://tds.climate.ncsu.edu/thredds/dodsC/prism/normals_4km/PRISM_30yr_normal_4kmM2_annual.nc")
normals_annual_nc
# variables: tmax, tmin, tmean, ppt



# convert nc file to raster
# normals_annual_ras <- raster::brick(x = normals_annual_nc, varname = "ppt")

# find lat and long indexes for cmu bounds
grid_id_annual_long <- "D1"
long_annual_data <- normals_annual_nc %>%
  activate(grid_id_annual_long) %>%
  hyper_tibble() %>%
  filter(longitude < -75.3 & longitude > -78.7)
range(long_annual_data$x)
# -78.7 is x = 1112
# -75.3 is x = 1192

grid_id_annual_lat <- "D2"
lat_annual_data <- normals_annual_nc %>%
  activate(grid_id_annual_lat) %>%
  hyper_tibble() %>%
  filter(latitude < 36.1 & latitude > 33.7)
range(lat_annual_data$y)
# 36.1 is y = 332
# 33.7 is y = 389

# bind lat and long annual data
# lat_long_annual_data <- bind_cols(long_annual_data, lat_annual_data)

# define data grids and create tibble
grid_id_annual_normals <- "D1,D2,D0"
normals_annual_data <- normals_annual_nc %>%
  hyper_tibble() %>%
  left_join(long_annual_data, by = "x") %>%
  left_join(lat_annual_data, by = "y")
  
  #activate(grid_id_annual_normals) %>%
  #hyper_filter(x = (x >= (1112)) & (x <= (1192))) %>% # longitude
  #hyper_filter(y = (y >= (332)) & (y <= (389))) %>% # latitude




