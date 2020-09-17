# ---- script header ----
# script name: ncsco_api_retrieval_script
# purpose of script: retrieve data from the nc sco api
# author: sheila saia
# date created: 20200610
# email: ssaia@ncus.edu


# ---- notes ----
# notes:
# nc sco api is here: https://climate.ncsu.edu/api/beta/
# nc sco api help: https://climate.ncsu.edu/api/beta/data?help

# networks available with weater data: 
# see https://climate.ncsu.edu/api/beta/networks for the full list
# Automated Surface Observation Station (ASOS) - operated by the National Weather Service (NWS) and Federal Aviation Association (FAA)
# Automated Weather Observing Station (AWOS) - operated by the Federal Aviation Association (FAA)
# Buoy Network (BUOY) - operated by the National Data Buoy Center (NDBC)
# Coastal Marine Automated Network (CMAN) - operated by the National Data Buoy Center (NDBC)
# Cooperative Network (COOP) - operated by the National Centers for Environmental Information (NCEI)
# Environment and Climate Observing Network (ECONET) - operated by NC State Climate Office (NCSCO)
# ECONet NCSU Campus Sites (NCSU) - operated by NC State Climate Office (NCSCO)
# National Ocean Service (NOS) - operated by the National Data Buoy Center (NDBC)
# Remote Automatic Weather Stations (RAWS-MW) - operated by the US Forest Service (USFS)
# Threaded Station Extremes (THREADEX) - operated by NOAA regional climate centers
# US Climate Reference Network (USCRN) - operated by National Centers for Environmental Information (NCEI)

# some example url-based api queries
# need to set up the url to access data
# get daily precip (1 day interval) hmtl outputs for COOP from month of Jan 2016
# url looks like...
# https://climate.ncsu.edu/api/beta/data.php?var=precip1m&loc=type=COOP;state=NC&int=1 day&start=2016-01-01&end=2016-01-31&&hash=cf71ae662d7477da3c53da8a1b6d87e49406a28b

# get daily precip (1 day interval) xls outputs for COOP from month of Jan 2016
# url looks like...
# https://climate.ncsu.edu/api/beta/data.php?var=precip1m&loc=type=COOP;state=NC&int=1 day&start=2016-01-01&end=2016-01-31&output=xls&hash=cf71ae662d7477da3c53da8a1b6d87e49406a28b

# can also save as csv but header isn't set up with commas so would have to find a way to clean this up
# variables type=data or type=meta don't seem to be working

# find info on available url parameter codes here: https://climate.ncsu.edu/api/beta/parameters
# find info on api arguments here: https://climate.ncsu.edu/api/beta/help

# data column descriptions
# location - location of data
# datetime - datetime of observation (timezone?), default is US/Eastern
# var - requested variable
# value - value of variable, if no value (MV = Missing Value, NA = Not Applicable)
# unit - unit of value
# score - quality Control score of value (0 = Good, 1 = Likely Good, 2 = Likely Bad, 3 = Bad, -1 = Pending, NA = Not Available)
# nettype - network type Code (M = Measured, C = Calculated, S = Station, L = Location)
# vartype - variable type code (A = Aggregate of Multiple Values, M = Metadata, I = Single Value, S = Subset of Multiple Values)
# obtype - observation type Code (D = Daily, H = Hourly, O = Minute, S = Special)
# obnum - number of observations used for value
# value_accum - accumulated value of numeric variable (grouped by location)

# metadata column description




# ---- to do ----

# TODO regenerate api key (hash) code for api
# TODO fill out metadata column description in notes
# TODO make tests in case data isn't available and nothing to append or too much to append
# TODO use here package for paths - hard coding them for now
# TODO check difference bewteen precip1m and precip variables


# ---- 1. load libraries and set paths ----
library(tidyverse)
library(RCurl)
library(lubridate)
library(httr)


# ---- 2. source functions ----
# set data path
data_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sco_api_raw/"

# set path to functions directory
functions_path <- "/Users/sheila/Documents/github/paper-ndfd-nccoast-study/functions/"

# source functions
source(paste0(functions_path, "read_ncsco_api_metadata.R"))
source(paste0(functions_path, "get_ncsco_api_data.R"))


# ---- 3. define api key and paths
# get nc sco api key (need to request this from the nc sco)
NCSOC_API_KEY <- Sys.getenv("NCSOC_API_KEY")

# data export path
data_export_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sco_api_raw/"


# ---- 4. get data for multiple networks ----
# list all networks to pull
my_ncsco_networks = c("ASOS", "AWOS", "BUOY", "CMAN", "COOP", "ECONET", "NCSU", "NOS", "RAWS-MW", "THREADEX", "USCRN")
# my_ncsco_networks = c("ASOS", "AWOS", "BUOY", "COOP", "ECONET", "NCSU", "NOS", "RAWS-MW", "THREADEX", "USCRN")
# CMAN there's no data coming up

for (n in 7:7){ #length(my_ncsco_networks)) {
  # pick network
  temp_ncsco_network <- my_ncsco_networks[n]
  
  # get data
  data_list <- get_ncsco_api_data(ncsco_network = temp_ncsco_network, 
                                  ncsco_var = "precip1m", # accumulated precipitation at 1 m above Earth's surface
                                  start_date = "20150101", 
                                  end_date = "20161231", 
                                  api_key = NCSOC_API_KEY)
  
  # define data
  data_raw <- data_list$data_raw
  
  # define metadata and keep replicates
  metadata_raw <- data_list$metadata_raw
  
  # only export if there's data
  if (dim(data_raw)[1] > 0) {
    # export
    write_csv(data_raw, paste0(data_path, str_to_lower(temp_ncsco_network), "_data_raw.csv"))
    write_csv(metadata_raw, paste0(data_path, str_to_lower(temp_ncsco_network), "_metadata_raw.csv"))
    
    # print status
    print(paste0("exported ", temp_ncsco_network, " network data"))
  }

  # print message that there's no data
  else {
    # print status
    print(paste0("did not export ", temp_ncsco_network, " network data"))
  }
}


# record start time
# start_time = now()

# record end time
# end_time = now()

# time to run loop
# end_time - start_time
