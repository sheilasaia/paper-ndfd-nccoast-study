# ---- script header ----
# script name: ncsco_api_retrieval_script
# purpose of script: retrieve data from the CLOUDS API
# author: sheila saia
# date created: 20200610
# email: ssaia@ncus.edu


# ---- notes ----
# notes:
# CLOUDS API is here: https://api.climate.ncsu.edu/
# CLOUDS API help: https://api.climate.ncsu.edu/help

# networks available with weater data: 
# see https://api.climate.ncsu.edu/networks for the full list
# Automated Surface Observation Station (ASOS) - operated by the National Weather Service (NWS) and Federal Aviation Association (FAA)
# Automated Weather Observing Station (AWOS) - operated by the Federal Aviation Association (FAA)
# Buoy Network (BUOY) - operated by the National Data Buoy Center (NDBC)
# Coastal Marine Automated Network (CMAN) - operated by the National Data Buoy Center (NDBC)
# Cooperative Network (COOP) - operated by the National Centers for Environmental Information (NCEI)
# Environment and Climate Observing Network (ECONET) - operated by NC State Climate Office (SCO)
# ECONet NCSU Campus Sites (NCSU) - operated by NC State Climate Office (SCO)
# National Ocean Service (NOS) - operated by the National Data Buoy Center (NDBC)
# Remote Automatic Weather Stations (RAWS-MW) - operated by the US Forest Service (USFS)
# Threaded Station Extremes (THREADEX) - operated by NOAA regional climate centers
# US Climate Reference Network (USCRN) - operated by National Centers for Environmental Information (NCEI)

# some example url-based api queries
# need to set up the url to access data
# get daily precip (1 day interval) hmtl outputs for COOP from month of Jan 2016
# url looks like...
# https://api.climate.ncsu.edu/data.php?var=precip1m&loc=type=COOP;state=NC&int=1 day&start=2016-01-01&end=2016-01-31&&hash=cf71ae662d7477da3c53da8a1b6d87e49406a28b

# get daily precip (1 day interval) xls outputs for COOP from month of Jan 2016
# url looks like...
# https://api.climate.ncsu.edu/data.php?var=precip1m&loc=type=COOP;state=NC&int=1 day&start=2016-01-01&end=2016-01-31&output=xls&hash=cf71ae662d7477da3c53da8a1b6d87e49406a28b

# can also save as csv but header isn't set up with commas so would have to find a way to clean this up
# variables type=data or type=meta don't seem to be working

# find info on available url parameter codes here: https://api.climate.ncsu.edu/parameters
# find info on api arguments here: https://api.climate.ncsu.edu/help

# data column descriptions
# location_id - originally called "location" in API output, renamed to match similar metadata column (as db key), location of data
# location_id_code - location of data (replicate of location_id?)
# datetime_et - originally called "datetime" in API output, datetime of observation (timezone?), default is US/Eastern
# var - requested variable
# value - value of variable, if no value (MV = Missing Value, NA = Not Applicable)
# unit - unit of value
# score - quality Control score of value (0 = Good, 1 = Likely Good, 2 = Likely Bad, 3 = Bad, -1 = Pending, NA = Not Available)
# nettype - network type code (M = Measured, C = Calculated, S = Station, L = Location)
# vartype - variable type code (A = Aggregate of Multiple Values, M = Metadata, I = Single Value, S = Subset of Multiple Values)
# obtime - observation time (timezone?), default is US/Eastern
# obtype - observation type Code (D = Daily, H = Hourly, O = Minute, S = Special)
# obnum - number of observations used for value
# value_accum - accumulated value of numeric variable (grouped by location and month)

# metadata column description
# location_id - originally called "location" in API output, renamed to match similar metadata column (as db key), location of data
# location_id_code - location of data (replicate of location_id?)
# network_type - network name
# location_name - location name/description
# city - city name where station is located
# county - county name where station is located
# state - state where station is located (e.g., NC)
# latitude_degrees_north - latitude in decimal degrees north
# longitude_degrees_east - longitude in decimal degrees east
# elevation_feet - elevation of the station in feet (above sea level)
# supporting_agency_for_location - agency name that supports the station
# start_date - first day included in station record
# end_date - last day included in station record


# ---- to do ----
# to do list:
# TODO regenerate api key (hash) code for api (not sure what this means now...?)
# TODO make tests in case data isn't available and nothing to append or too much to append
# TODO use here package for paths - hard coding them for now
# TODO check difference bewteen precip1m and precip variables


# ---- 1. load libraries and set paths ----
library(tidyverse)
library(RCurl)
library(lubridate)
library(httr)
library(here)
library(tidylog)


# ---- 2. set keys and paths ----
# set tabular data export path
tabular_data_output_path <- here::here("data", "tabular", "obs_precip_raw")

# get sco clouds api key (need to request this from the nc sco)
CLOUDS_API_KEY <- Sys.getenv("CLOUDS_API_KEY")

# ---- 3. source functions ----
source(here::here("functions", "read_clouds_metadata.R"))
source(here::here("functions", "get_clouds_data.R"))


# ---- 4. get data for multiple networks ----
# list all networks to pull
my_clouds_networks = c("ASOS", "AWOS", "COOP", "ECONET", "NCSU", "NOS", "RAWS-MW", "THREADEX", "USCRN")
# my_clouds_networks = c("ASOS", "AWOS", "BUOY", "CMAN", "CoCoRaHS", "COOP", "ECONET", "NCSU", "NOS", "RAWS-MW", "THREADEX", "USCRN")
# BUOY there's no data coming up
# CMAN there's no data coming up
# CoCoRaHS data get directly from their website

for (n in 1:length(my_clouds_networks)) {
  # pick network
  temp_clouds_network <- my_clouds_networks[n]
  
  # get data
  data_list <- get_clouds_data(api_key = CLOUDS_API_KEY, 
                               clouds_network = temp_clouds_network, 
                               clouds_var = "precip1m", # accumulated precipitation at 1 m above Earth's surface 
                               start_date = "20150101", 
                               end_date = "20161231")
  
  # define data
  data_raw <- data_list$data_raw
  # NOTE: All columns are in the character format!
  
  # define metadata and keep replicates
  metadata_raw <- data_list$metadata_raw
  # NOTE: All columns are in the character format!
  
  # only export if there's data
  if (dim(data_raw)[1] > 0) {
    # export
    write_csv(x = data_raw, file = paste0(tabular_data_output_path, "/", str_to_lower(temp_clouds_network), "_data_raw.csv"))
    write_csv(x = metadata_raw, file = paste0(tabular_data_output_path, "/", str_to_lower(temp_clouds_network), "_metadata_raw.csv"))
    
    # print status
    print(paste0("exported ", temp_clouds_network, " network data"))
  }

  # print message that there's no data
  else {
    # print status
    print(paste0("did not export ", temp_clouds_network, " network data"))
  }
}


# record start time
# start_time = now()

# record end time
# end_time = now()

# time to run loop
# end_time - start_time
