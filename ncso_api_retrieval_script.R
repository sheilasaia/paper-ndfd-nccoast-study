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

# ---- to do ----
# to do list


# ---- 1. load libraries and set paths----
library(tidyverse)
library(RCurl)


# ---- 2. 

# get nc sco api key
NCSOC_API_KEY <- Sys.getenv("NCSOC_API_KEY")

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


# get daily precip (1 day interval) outputs for COOP from month of Jan 2016
# html output
# https://climate.ncsu.edu/api/beta/data.php?var=precip1m&loc=type=COOP;state=NC&int=1 day&start=2016-01-01&end=2016-01-31&&hash=cf71ae662d7477da3c53da8a1b6d87e49406a28b
# this works!

# get daily precip (1 day interval) outputs for COOP from month of Jan 2016
# xls output
# https://climate.ncsu.edu/api/beta/data.php?var=precip1m&loc=type=COOP;state=NC&int=1 day&start=2016-01-01&end=2016-01-31&output=xls&hash=cf71ae662d7477da3c53da8a1b6d87e49406a28b

# can also save as csv but header isn't set up with commas so would have to find a way to clean this up
# say type=data or type=meta to get data or metadata separately

# metadata
# https://climate.ncsu.edu/api/beta/data.php?var=precip1m&loc=type=COOP;state=NC&type=meta&int=1 day&start=2016-01-01&end=2016-01-31&output=xls&hash=cf71ae662d7477da3c53da8a1b6d87e49406a28b

# define changing url parts
ncsco_api_datasets_sel <- "COOP"
start_date_sel <- "2016-01-01"
end_date_sel <- "2016-01-31"

# define url parts that stay the same
base_url <- "https://climate.ncsu.edu/api/beta/data.php?"
url_var <- "var=precip1m&" # precipitation at 1 m above Earth's surface
url_loc <- paste0("loc=type=", ncsco_api_datasets_sel, "&")
url_state <- "state=NC&"
url_int <- "int=1 day&" # data interval (1 day = daily)
url_start_date <- paste0("start=", start_date_sel, "&")
url_end_date <- paste0("end=", end_date_sel, "&")
url_output <- "output=csv&" # csv output
url_key <- paste0("hash=", NCSOC_API_KEY)

# put all together to get query url
query_url <- paste0(base_url, url_var, url_loc, url_state, url_int, url_start_date, url_end_date, url_output, url_key)
# query_url <- "https://climate.ncsu.edu/api/beta/data.php?var=precip1m&loc=type=COOP;state=NC&type=meta&int=1 day&start=2016-01-01&end=2017-01-31&output=csv&hash=cf71ae662d7477da3c53da8a1b6d87e49406a28b"
# query_url <- "https://climate.ncsu.edu/api/beta/data.php?var=precip1m&loc=type=COOP;state=NC&int=1 day&start=2016-01-01&end=2016-01-31&output=csv&hash=cf71ae662d7477da3c53da8a1b6d87e49406a28b"
query_url_fix <- URLencode(query_url) # need to replace " " with "%20"

# check that url exists
# url.exists(query_url_fix)
# this is still true even when gives 400 error

# grab data from url (without metadata)
test_data <- read_csv(file = query_url_fix, comment = "##") # this grabs just the data
# length(test_data) > 1 then there's actual data there

# save metadata
test_meta_raw <- read_lines(file = query_url_fix)
comment_lines <- grep("## ", test_meta_raw)
test_meta_raw_sel <- test_meta_raw[min(comment_lines):max(comment_lines)]
data_lines_start <- grep("## Location Metadata", test_meta_raw_sel)
test_meta_repl <- gsub("## ", "", test_meta_raw_sel[(data_lines_start + 1):length(test_meta_raw_sel)]) # select metadata and replace ##'s
test_meta_repl_header <- test_meta_repl[1] %>%
  gsub(" ", "_", .) %>%
  gsub("[", "", ., fixed = TRUE) %>%
  gsub("]", "", ., fixed = TRUE)
test_meta_tidy <- read_csv(c(test_meta_repl_header, test_meta_repl[2:length(test_meta_repl)]), col_names = TRUE)



# i know this link works but getting error
# Error in open.connection(con, "rb") : HTTP error 400.
# try this? https://stackoverflow.com/questions/60635821/error-in-open-connectioncon-rb-http-error-400-with-r-studio
# try this: https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/namespaces-in-r/


