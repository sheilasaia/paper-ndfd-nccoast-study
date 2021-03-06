
# ---- script header ----
# script name: get_sco_api_data.R
# purpose of script: function that gets data from the NC SCO API for a specified network, start date, and end date
# author: sheila saia
# date created: 20200908
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:
 

# ---- to do ----
# to do list

# TODO allow for multiple networks (i.e., a list) as an input to ncsco_network
# TODO allow for different step sizes (i.e., month, day, hour)

# ----
#' Grab data from the NC SCO API for a specified network.
#'
#' @param ncsco_network A string describing the valid NC SCO API network name (can only be one value i.e., "ASOS")
#' @param start_date A YYYY-MM-DD string representing the start date of the data request (e.g., "2020-01-01")
#' @param end_date A YYYY-MM-DD string representing the end date of the data request, must be later than start_date (e.g., "2020-09-08")
#' @param api_key A string describing the NC SCO API key
#' @return A list with two dataframes (data and metadata) for the specified network and date range.
#' 
#' Requires read_sco_api_metadata.R function
get_ncsco_api_data <- function(ncsco_network, ncsco_var, start_date, end_date, api_key) {
  # step by month
  # date list df with start and end date for first step (i.e., the first month)
  date_step_list <- data.frame(start_date = ymd(start_date),
                               end_date = ymd(start_date) %m+% months(1) %m-% days(1))
  # step by day
  # date_step_list <- data.frame(start_date = ymd(start_date),
  #                              end_date = ymd(start_date) %m+% days(1))
  
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
  data_raw <- data.frame(location_id = character(),
                         datetime_chr_et = character(),
                         var = character(),
                         value = numeric(),
                         unit = character(),
                         score = numeric(),
                         nettype = character(),
                         vartype = character(),
                         obtime = character(),
                         obtype = character(),
                         obnum = numeric(),
                         value_accum = numeric())
  
  metadata_raw <- data.frame(location_id = character(),
                             network_type = character(),
                             location_name = character(),
                             city = character(),
                             county = character(),
                             state = character(),
                             latitude_degrees_north = numeric(),
                             longitude_degrees_east = numeric(),
                             elevation_feet_chr = character(),
                             supporting_agency_for_location = character(),
                             start_date_chr = character(),
                             end_date_chr = character(),
                             obtypes_available = character())
  
  for (j in 1:num_steps) {
    # define date range
    temp_start_date_sel <- date_step_list$start_date[j]
    temp_end_date_sel <- date_step_list$end_date[j]
    
    # define url parts that stay the same
    base_url <- "https://climate.ncsu.edu/api/beta/data.php?"
    url_var <- paste0("var=", ncsco_var, "&")
    url_loc <- paste0("loc=type=", ncsco_network, "&")
    url_state <- "state=NC&"
    url_int <- "int=1 day&" # data interval (1 day = daily)
    url_start_date <- paste0("start=", temp_start_date_sel, "&")
    url_end_date <- paste0("end=", temp_end_date_sel, "&")
    url_output <- "output=csv&" # csv output
    url_key <- paste0("hash=", api_key)
    # additional api arguments are here: https://climate.ncsu.edu/api/beta/help
    
    # put all together to get query url
    query_url <- paste0(base_url, url_var, url_loc, url_state, url_int, url_start_date, url_end_date, url_output, url_key)

    # replace spaces in query url with %20 otherwise will get api error
    query_url_fix <- URLencode(query_url) # need to replace " " with "%20"
    
    # print status
    print("query started")
    
    # query
    temp_data_raw <- httr::GET(query_url_fix)

    # print status
    print("query finished")
    
    # check status
    temp_data_raw_status <- temp_data_raw$status
    # 200 = good
    # 504 = too big of a request or server timed out
    # 400 = query doesn't exist
    
    # only tidy data if status code is anything but 200
    if (temp_data_raw_status == 200) {
      # save text contents
      temp_data_text_raw <- httr::content(temp_data_raw, "text")
      
      # check if there's data
      # temp_data_check <- read_csv(temp_data_text_raw, comment = "##", col_types = cols())
      temp_data_check <- read_csv(temp_data_text_raw, comment = "##", col_types = list(col_character(), col_character(), col_character(), col_number(), col_character(), col_number(), col_character(), col_character(), col_character(), col_character(), col_number(), col_number()))
            
      # if there's data then run
      if (dim(temp_data_check)[1] > 0) {
        
        # grab data from url (without metadata)
        temp_data_raw <- temp_data_check  %>% # this grabs just the data, length(test_data) > 1 then there's data, datetime is ET
          mutate(datetime_chr_et = as.character(datetime),
                 location_id = as.character(location)) %>%
          select(location_id, datetime_chr_et, var:value_accum) # make location id column the same as metadata, use date as character columns for now
        
        # use function
        temp_metadata_raw <- read_ncsco_api_metadata(temp_data_text_raw) %>%
          mutate(start_date_chr = as.character(start_date),
                 end_date_chr = as.character(end_date),
                 elevation_feet_chr = as.character(elevation_feet),
                 location_id = as.character(location_id)) %>%
          select(location_id:longitude_degrees_east, elevation_feet_chr, supporting_agency_for_location, start_date_chr, end_date_chr, obtypes_available) 
        
        # need check to stop if column names aren't the same
        temp_data_cols <- names(temp_data_raw)
        data_cols <- names(data_raw)
        cols_check <- identical(temp_data_cols, data_cols) # must be TRUE
        
        # append if columns are the same
        if (cols_check == TRUE) {
          # append data
          data_raw <- bind_rows(temp_data_raw, data_raw)
          metadata_raw <- bind_rows(temp_metadata_raw, metadata_raw)
          
          # print entry status
          print(paste0("appended step ", j, " of ", num_steps, " steps"))
          
        }
        
        # print issue if not the same
        else {
          print(paste0("cannot append step ", j, " of ", num_steps, "steps because columns don't match expected"))
        
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
  
  # return results
  return(list(data_raw = data_raw, metadata_raw = metadata_raw))
}

