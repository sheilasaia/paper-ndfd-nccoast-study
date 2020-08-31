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
# Environment and Climate Observing Network (ECONet) - operated by NC State Climate Office (NCSCO)
# ECONet NCSU Campus Sites (NCSU) - operated by NC State Climate Office (NCSCO)
# National Ocean Service (NOS) - operated by the National Data Buoy Center (NDBC)
# Remote Automatic Weather Stations (RAWS) - operated by the US Forest Service (USFS)
# Threaded Station Extremes (ThreadEx) - operated by NOAA regional climate centers
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

# TODO fill out metadata column description in notes
# TODO use here package for paths - hard coding them for now


# ---- 1. load libraries and set paths----
library(tidyverse)
library(RCurl)
library(lubridate)


# ---- 2. source functions ----
# set path to functions directory
functions_path <- "/Users/sheila/Documents/github/paper-ndfd-nccoast-study/functions/"

# source functions
source(paste0(functions_path, "read_ncsco_api_metadata.R"))


# ---- 3. define api key and paths

# get nc sco api key (need to request this from the nc sco)
NCSOC_API_KEY <- Sys.getenv("NCSOC_API_KEY")

# data export path
data_export_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/sco_api_raw/"


# ---- 4. define dates of interest ----

# date list df with start and end date for first step (i.e., the first month)
date_step_list <- data.frame(start_date = ymd("2015-01-01"),
                             end_date = ymd("2015-01-31"))

# number of months
num_months <- 24 # for 2015 and 2016 (12 x 2 = 24)

# loop
for(i in 2:num_months) {
  # redefine start and end
  temp_start_date <- date_step_list$start_date[i-1] %m+% months(1)
  temp_end_date <- temp_start_date %m+% months(1) %m-% days(1)
  # %m+% manual: https://www.rdocumentation.org/packages/lubridate/versions/1.7.9/topics/%25m%2B%25
  
  # make df to add to final df
  temp_date_list <- data.frame(start_date = temp_start_date,
                               end_date = temp_end_date)
  
  # save temp df to final df
  date_step_list <- rbind(date_step_list, temp_date_list)
}


# ---- 4. grabbing COOP data from the nc sco api ----

# define dataset
ncsco_api_datasets_sel <- "COOP"

# create tempty df's for data and metadata

# iterate through date_step_list

coop_data_raw <- data.frame(location_id = numeric(),
                            datetime_chr_et = character(),
                            var = character(),
                            value = numeric(),
                            unit = character(),
                            score = numeric(),
                            nettype = character(),
                            vartype = character(),
                            obtype = character(),
                            obnum = numeric(),
                            value_accum = numeric())

coop_metadata_raw <- data.frame(location_id = numeric(),
                                network_type = character(),
                                location_name = character(),
                                city = character(),
                                county = character(),
                                state = character(),
                                latitude_degrees_north = numeric(),
                                longitude_degrees_east = numeric(),
                                elevation_feet = character(),
                                supporting_agency_for_location = character(),
                                start_date_chr = character(),
                                end_date_chr = character(),
                                obtypes_available = character())

# record start time
start_time = now()

for (i in 1:num_months) {
  # define date range
  temp_start_date_sel <- date_step_list$start_date[i]
  temp_end_date_sel <- date_step_list$end_date[i]
  
  # define url parts that stay the same
  base_url <- "https://climate.ncsu.edu/api/beta/data.php?"
  url_var <- "var=precip1m&" # precipitation at 1 m above Earth's surface
  url_loc <- paste0("loc=type=", ncsco_api_datasets_sel, "&")
  url_state <- "state=NC&"
  url_int <- "int=1 day&" # data interval (1 day = daily)
  url_start_date <- paste0("start=", temp_start_date_sel, "&")
  url_end_date <- paste0("end=", temp_end_date_sel, "&")
  url_output <- "output=csv&" # csv output
  url_key <- paste0("hash=", NCSOC_API_KEY)
  # additional api arguments are here: https://climate.ncsu.edu/api/beta/help
  
  # put all together to get query url
  query_url <- paste0(base_url, url_var, url_loc, url_state, url_int, url_start_date, url_end_date, url_output, url_key)
  # query_url <- "https://climate.ncsu.edu/api/beta/data.php?var=precip1m&loc=type=COOP;state=NC&type=meta&int=1 day&start=2016-01-01&end=2017-01-31&output=csv&hash=cf71ae662d7477da3c53da8a1b6d87e49406a28b"
  # query_url <- "https://climate.ncsu.edu/api/beta/data.php?var=precip1m&loc=type=COOP;state=NC&int=1 day&start=2016-01-01&end=2016-01-31&output=csv&hash=cf71ae662d7477da3c53da8a1b6d87e49406a28b"
  
  # replace spaces in query url with %20 otherwise will get api error
  query_url_fix <- URLencode(query_url) # need to replace " " with "%20"
  
  # check that url exists
  # url.exists(query_url_fix)
  # this is still true even when gives 400 error
  
  # grab data from url (without metadata)
  temp_data_raw <- read_csv(file = query_url_fix, comment = "##")  %>% # this grabs just the data, length(test_data) > 1 then there's data, datetime is ET
    mutate(datetime_chr_et = as.character(datetime)) %>%
    select(location_id = location, datetime_chr_et, var:value_accum) # make location id column the same as metadata, use date as character columns for now
  
  # use function
  temp_metadata_raw <- read_ncsco_api_metadata(query_url_fix) %>%
    mutate(start_date_chr = as.character(start_date),
           end_date_chr = as.character(end_date)) %>%
    select(location_id:supporting_agency_for_location, start_date_chr, end_date_chr, obtypes_available) 

  # append data
  coop_data_raw <- bind_rows(temp_data_raw, coop_data_raw)
  coop_metadata_raw <- bind_rows(temp_metadata_raw, coop_metadata_raw)
  
  # print entry status
  print(paste0("appended month ", i))
  
}

# record end time
end_time = now()

# time to run loop
end_time - start_time



