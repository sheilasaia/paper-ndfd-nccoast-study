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
library(tidyverse)
library(raster)
library(ncdf4)
library(sf)

# https://ropensci.org/blog/2019/11/05/tidync/
# https://cran.r-project.org/web/packages/tidync/tidync.pdf
# https://www.r-bloggers.com/2016/08/a-netcdf-4-in-r-cheatsheet/
# https://pjbartlein.github.io/REarthSysSci/netCDF.html
# https://pjbartlein.github.io/REarthSysSci/raster_intro.html
# http://132.72.155.230:3838/r/index.html
# lat long is center of grid (likely)

# prism data tutorial
# https://rstudio-pubs-static.s3.amazonaws.com/260164_978471fad4f248428a52b8ff72b774be.html

# SCO TDS server link to normals data
# https://tds.climate.ncsu.edu/thredds/catalog/prism/normals_4km/catalog.html

# define url
normals_annual_url <- "https://tds.climate.ncsu.edu/thredds/dodsC/prism/normals_4km/PRISM_30yr_normal_4kmM2_annual.nc"

# open nc data from url
normals_annual_data <- nc_open(normals_annual_url)
normals_annual_data

# get longitude and latitude
normals_lon <- ncvar_get(normals_annual_data, "longitude")
normals_lat <- ncvar_get(normals_annual_data, "latitude")

# define extent
normals_xmin <- min(normals_lon)
normals_xmax <- max(normals_lon)
normals_ymin <- min(normals_lat)
normals_ymax <- max(normals_lat)
# top left (normals_xmin, normals_ymax)
# top right (normals_xmax, normals_ymax)
# bottom left (normals_xmin, normals_ymin)
# bottom right (normals_xmin, normals_ymin)

# get dimensions of longitude and latitude
nlon <- dim(normals_lon)
nlat <- dim(normals_lat)
print(c(nlon,nlat))
# 1405 x 621

# annual ppt data
normals_annual_ppt_data <- ncvar_get(normals_annual_data, "ppt")
# in mm, filled with NAs

# make a raster
normals_ppt_raster <- raster::raster(x = t(normals_annual_ppt_data),
                                     xmn = normals_xmin,
                                     xmx = normals_xmax,
                                     ymn = normals_ymin, 
                                     ymx = normals_ymax,
                                     crs = "+proj=longlat +datum=WGS84")
# not really sure why i had to transpose these data...but plot looks much better
plot(normals_ppt_raster)

# define epsg and proj for CONUS Albers Equal Area projection (projecting to this)
# conus_albers_epsg <- 5070
conus_albers_proj <- "+init=EPSG:5070"
# resolution
normals_res <- 4*1000 # 4km by 4km in m = 4000

# convert to albers
normals_ppt_raster_albers <- raster::projectRaster(normals_ppt_raster, 
                                                   crs = conus_albers_proj, 
                                                   res = normals_res)
# plot
plot(normals_ppt_raster_albers)
# looks ok!

# need to repeat this for all the months
num_months <- 12
normals_month_precip_vals <- data.frame(loc_id = obs_metadata_shp$loc_id)

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
  names(temp_normals_precip_cm) <- eval(paste0("normals_monthly_precip_mm_", i_chr))
  
  # bind column
  normals_month_precip_vals <- bind_cols(normals_month_precip_vals, temp_normals_precip_cm)
  
  # print status
  print(paste0("finished month ", i_chr))
  
}

# 



# obs daily metadata shapefile
obs_metadata_shp <- sf::st_read(here::here("data", "spatial", "obs_data_tidy", "obs_metadata_albers_sel.shp"))

# add normals
obs_metadata_shp_normals <- obs_metadata_shp %>%
  dplyr::mutate(normal_annual_precip_cm = round((raster::extract(normals_ppt_raster_albers, obs_metadata_shp, weights = FALSE) / 10), 2))


# plot to check
ggplot(data = obs_metadata_shp_normals) +
  geom_sf(aes(color = normal_annual_precip_cm))




