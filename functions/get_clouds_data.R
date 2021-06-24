# ---- script header ----
# script name: get_clouds_data.R
# purpose of script: function that gets data from the CLOUDS API for a specified network, start date, and end date
# author: sheila saia
# date created: 20200908
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:
# CLOUDS API is here: https://api.climate.ncsu.edu/
# CLOUDS API help: https://api.climate.ncsu.edu/help

# output descriptions:

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
# TODO allow for multiple networks (i.e., a list) as an input to clouds_network
# TODO allow for different step sizes (i.e., month, day, hour) --> right now is just daily
# TODO allow for other parameter functionality (e.g., specific location, etc.)
# TODO add package dependencies


# ---- function ----
#' Grab data from the CLOUDS API for a specified network.
#'
#' @param clouds_network A string describing the valid CLOUDS network name (can only be one value e.g., "ECONET")
#' @param clouds_var A string describing the valid CLOUDS variable name (can only be one value e.g., "precip")
#' @param start_date A YYYY-MM-DD string representing the start date of the data request (e.g., "2020-01-01")
#' @param end_date A YYYY-MM-DD string representing the end date of the data request, must be later than start_date (e.g., "2020-09-08")
#' @param api_key A string describing the CLOUDS key
#' @return A list with two tibbles (data and metadata) for the specified network and date range. NOTE: All colunms are in the character format!
#' 
#' Requires read_sco_api_metadata.R function
get_clouds_data <- function(api_key, clouds_network, clouds_var, start_date, end_date) {
  # step by month
  # date list df with start and end date for first step (i.e., the first month)
  date_step_list <- data.frame(start_date = ymd(start_date),
  #                              end_date = ymd(start_date) %m+% months(1) %m-% days(1))
  # step by day
  # date_step_list <- data.frame(start_date = ymd(start_date),
                               end_date = ymd(start_date) %m+% days(1))
  
  # number of steps
  num_steps <- round(time_length(ymd(end_date) - ymd(start_date), unit = "month"))
  # num_steps <- round(time_length(ymd(end_date) - ymd(start_date), unit = "day"))
  
  # if more than one step then append steps to date_step_list
  if (num_steps > 1) {
    # loop
    for(i in 2:num_steps) {
      # redefine start and end
      temp_start_date <- date_step_list$start_date[(i - 1)] %m+% months(1)
      temp_end_date <- temp_start_date %m+% months(1) %m-% days(1)
      # temp_start_date <- date_step_list$start_date[(i - 1)] %m+% days(1)
      # temp_end_date <- temp_start_date %m+% days(1)
      # %m+% manual: https://www.rdocumentation.org/packages/lubridate/versions/1.7.9/topics/%25m%2B%25
      
      # make df to add to final df
      temp_date_list <- data.frame(start_date = temp_start_date,
                                   end_date = temp_end_date)
      
      # save temp df to final df
      date_step_list <- rbind(date_step_list, temp_date_list)
    }
  }
  
  # else no appending
  
  # create empty df's for data and metadata
  # data_raw <- NULL
  data_raw <- tibble(location_id = character(),
                     datetime_et = character(),
                     var = character(),
                     value = character(),
                     unit = character(),
                     score = character(),
                     nettype = character(),
                     vartype = character(),
                     obtime = character(),
                     obtype = character(),
                     obnum = character(),
                     value_accum = character())
  
  # metadata_raw <- NULL
  metadata_raw <- tibble(location_id = character(),
                         location_id_code = character(), # not really sure what this is...looks the same as location_id
                         network_type = character(),
                         location_name = character(),
                         city = character(),
                         county = character(),
                         state = character(),
                         latitude_degrees_north = character(),
                         longitude_degrees_east = character(),
                         elevation_feet = character(),
                         supporting_agency_for_location = character(),
                         start_date = character(),
                         end_date = character(),
                         obtypes_available = character())
  
  for (j in 1:num_steps) {
    # define date range
    temp_start_date_sel <- date_step_list$start_date[j]
    temp_end_date_sel <- date_step_list$end_date[j]
    
    # define url parts that stay the same
    base_url <- "https://api.climate.ncsu.edu/data.php?"
    url_loc <- paste0("loc=type=", clouds_network, "&")
    url_var <- paste0("var=", clouds_var, "&")
    url_state <- "state=NC&"
    url_start_date <- paste0("start=", temp_start_date_sel, "&")
    url_end_date <- paste0("end=", temp_end_date_sel, "&")
    url_int <- "int=1 day&" # data interval (1 day = daily)
    url_output <- "output=csv&" # csv output
    url_attrib <- "attr=location,datetime,var,value,unit,score,nettype,vartype,obtime,obtype,obnum,value_accum&" # have to be in this order
    url_key <- paste0("hash=", api_key)
    # additional api arguments are here: https://api.climate.ncsu.edu/help
    
    # put all together to get query url
    query_url <- paste0(base_url, url_loc, url_state, url_var, url_start_date, url_end_date, url_int, url_output, url_attrib, url_key)
    
    # replace spaces in query url with %20 otherwise will get api error
    query_url_fix <- URLencode(query_url) # need to replace " " with "%20"
    
    # print status
    print("query started")
    
    # query
    temp_data_query_raw <- httr::GET(query_url_fix)
    
    # print status
    print("query finished")
    
    # check status
    temp_data_raw_status <- temp_data_query_raw$status
    # 200 = good
    # 504 = too big of a request or server timed out
    # 400 = query doesn't exist
    
    # only tidy data if status code is anything but 200
    if (temp_data_raw_status == 200) {
      # save text contents
      temp_data_text_raw <- httr::content(temp_data_query_raw, "text")
      
      # check if there's data
      temp_data_check <- read_csv(temp_data_text_raw, comment = "##", col_types = cols(.default = col_character()))
      # NOTE: All columns are in the character format!
      
      # if there's data then run
      if (dim(temp_data_check)[1] > 0) {
        
        # grab data from url (without metadata)
        temp_data_raw <- temp_data_check  %>% # this grabs just the data, length(test_data) > 1 then there's data, datetime is ET
          mutate(datetime_et = datetime,
                 location_id = location) %>%
          select(location_id, datetime_et, var:value_accum) # make location id column the same as metadata, use date as character columns for now
        
        # use function
        temp_metadata_raw <- read_clouds_metadata(temp_data_text_raw)
        
        # need check to stop if data column names aren't the same
        temp_data_cols <- names(temp_data_raw)
        data_cols <- names(data_raw)
        data_cols_check <- identical(temp_data_cols, data_cols) # must be TRUE
        
        # need to check to stop if metadata columns names aren't the same
        temp_metadata_cols <- names(temp_metadata_raw)
        metadata_cols <- names(metadata_raw)
        metadata_cols_check <- identical(temp_metadata_cols, metadata_cols) # must be TRUE
        
        # append if columns are the same
        if ((data_cols_check == TRUE) & (metadata_cols_check == TRUE)) {
          # append data
          data_raw <- bind_rows(temp_data_raw, data_raw)
          metadata_raw <- bind_rows(temp_metadata_raw, metadata_raw)
          
          # print entry status
          print(paste0("appended step ", j, " of ", num_steps, " steps"))
        }
        
        # print issue if not the same
        else {
          print(paste0("cannot append step ", j, " of ", num_steps, " steps because data and/or metadata columns don't match expected"))
          
          # move to next iterator
          next
        }
      }
      
      else {
        # print entry status 
        print(paste0("no data for step ", j, " of ", num_steps, " steps"))
        
        # move to next iterator
        next
      }
    }
    
    # if there's no data or server times out
    else {
      # print entry status 
      print(paste0(temp_data_raw_status, " error: no data for step ", j, " of ", num_steps, " steps"))
      
      # move to next iterator
      next
    }
  }
  
  # final tidy of metadata
  metadata_raw_no_duplicates <- metadata_raw %>% distinct_all()
  
  # return results
  return(list(data_raw = data_raw, metadata_raw = metadata_raw_no_duplicates))
}