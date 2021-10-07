# ---- script header ----
# script name: 11_sco_get_normals_script.R
# purpose of script:
# author:
# date created:
# email:


# ---- notes ----
# notes:


# ---- to do ----
# to do list:

# install.packages("tidync")
# remotes::install_github("ropensci/tidync", dependencies = TRUE)
# remote install version of tidync was giving me errors when i tab autocompleted
library(tidync)
library(tidyverse)

# https://ropensci.org/blog/2019/11/05/tidync/
# https://cran.r-project.org/web/packages/tidync/tidync.pdf
# https://www.r-bloggers.com/2016/08/a-netcdf-4-in-r-cheatsheet/
# https://pjbartlein.github.io/REarthSysSci/netCDF.html


# bounding box for cmu boundary with 10 km buffer (from script 10_analysis_script.R)
#  xmin      ymin      xmax      ymax 
# -78.60738  33.76421 -75.35710  36.06080
cmu_bbox_wgs84_vec <- c(-78.60738, 33.76421, -75.35710, 36.06080)

# test_url <- "https://tds.climate.ncsu.edu/thredds/fileServer/prism/normals_4km/PRISM_30yr_normal_4kmM2_annual.nc"
annual_normals_url <- "https://tds.climate.ncsu.edu/thredds/dodsC/prism/normals_4km/PRISM_30yr_normal_4kmM2_annual.nc"
# test_file <- system.file(test_url, package = "tidync")
annual_normals_nc <- tidync(x = annual_normals_url)
annual_normals_nc

# get annual normals
grid_id_annual_normals <- "D1,D2,D0"
annual_normals_data <- annual_normals_nc %>% 
  activate(grid_id_annual_normals) %>%
  hyper_tibble()

# get longitude all vals
grid_id_long_all <- "D1"
annual_normals_long_data <- annual_normals_nc %>% 
  activate(grid_id_long) %>%
  hyper_tibble()

# 

# get latitude
grid_id_lat <- "D2"
annual_normals_lat_data <- annual_normals_nc %>% 
  activate(grid_id_lat) %>%
  hyper_tibble()

# join lat and long

