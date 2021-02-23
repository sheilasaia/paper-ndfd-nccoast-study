# ---- script header ----
# script name: cocorahs_retrieval_script.R
# purpose of script: retrieves cocorahs data for specified date range
# author: sheila saia
# date created: 20200224
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:
 

# ---- to do ----
# to do list


# ---- 1. load libraries and set paths----
library(tidyverse)
library(xml2)
library(lubridate)
# devtools::install_github("r-lib/xml2")
# https://github.com/r-lib/xml2
# helpful blog: https://blog.rstudio.com/2015/04/21/xml2/
# more helpful blog: https://lecy.github.io/Open-Data-for-Nonprofit-Research/Quick_Guide_to_XML_in_R.html

# paths
tabular_data_export_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/tabular/cocorahs_raw/"


# ---- 2. loop through data ----
# for loop for multiple days
# define start and end days
# start_date <- ymd("2016-01-01")
# end_date <- ymd("2018-12-31")
start_date <- ymd("2015-01-01")
# end_date <- ymd("2015-01-07") # testing
end_date <- ymd("2016-12-31")
day_step <- duration(num = 1, units = "days")
num_days <- time_length(end_date - start_date, unit = "days")

# make empty data frame/tibble
cocorahs_data_raw <- tibble(date = character(),
                            time = character(),
                            station_id = character(),
                            station_name = character(),
                            lat = numeric(),
                            long = numeric(),
                            precip_in = numeric())
# map(cocorahs_data, class) # checks classes of columns

# loop
for (i in 0:num_days) {
  # set-up date and save values
  temp_date <- start_date + i*day_step
  temp_day <- day(temp_date)
  temp_month <- month(temp_date)
  temp_year <- year(temp_date)
  
  # define url address
  temp_address <- paste0("http://data.cocorahs.org/export/exportreports.aspx?ReportType=Daily&dtf=1&Format=XML&State=NC&ReportDateType=reportdate&Date=", temp_month, "/", temp_day, "/", temp_year, "&TimesInGMT=False")
  
  # download and read xml data from address
  temp_data_xml_chr <- xml2::download_xml(url = temp_address)
  temp_xml <- read_xml(temp_data_xml_chr)
  
  # save data to lists
  # xml_child(xml_child((test_xml))) # see all report entries for one station
  # want to keep ObservationDate, ObservationTime, StationNumber, StationName, Latitude, Longitude, TotalPrecipAmt
  obs_date_list <- xml_text(xml_find_all(xml_children(xml_children((temp_xml)))[1], "//ObservationDate"))
  obs_time_list <- xml_text(xml_find_all(xml_children(xml_children((temp_xml)))[1], "//ObservationTime"))
  station_numbers_list <- xml_text(xml_find_all(xml_children(xml_children((temp_xml)))[1], "//StationNumber"))
  station_name_list <- xml_text(xml_find_all(xml_children(xml_children((temp_xml)))[1], "//StationName"))
  lat_list <- xml_text(xml_find_all(xml_children(xml_children((temp_xml)))[1], "//Latitude"))
  long_list <- xml_text(xml_find_all(xml_children(xml_children((temp_xml)))[1], "//Longitude"))
  precip_list <- xml_text(xml_find_all(xml_children(xml_children((temp_xml)))[1], "//TotalPrecipAmt")) # in inches (T = trace, NA = NA)
  
  # make dataframe/tibble
  temp_data_raw <- tibble(date = obs_date_list,
                          time = as.character(obs_time_list),
                          station_id = station_numbers_list,
                          station_name = station_name_list,
                          lat = as.numeric(lat_list),
                          long = as.numeric(long_list),
                          precip_in = as.numeric(precip_list))
  # NA warning is ok here this happens from using as.numeric
  # map(temp_data_raw, class) # checks classes of columns
  
  # bind to tibble
  cocorahs_data_raw <- bind_rows(temp_data_raw, cocorahs_data_raw)
  
  # print date
  print(temp_date)
}

# final clean up
cocorahs_data_raw_fin <- cocorahs_data_raw %>%
  mutate(time_og = hm(str_sub(time, start = 1, end = 5)),
         time_lab = str_sub(time, start = 7, end = -1), # need to deal with "AM" and "PM"
         time_fix = case_when((time_lab == "AM") & (time_og < hours(12)) ~ time_og,
                              (time_lab == "AM") & (time_og >= hours(12)) ~ (time_og - hours(12)),
                              (time_lab == "PM") & (time_og < hours(12)) ~ (time_og + hours(12)),
                              (time_lab == "PM") & (time_og >= hours(12)) ~ time_og),
         time_fix_fin = sprintf('%02d:%02d:%02d', hour(time_fix), minute(time_fix), second(time_fix))) %>% # make "AM" and "PM" military time
  mutate(datetime_et = as.character(ymd_hms(paste0(date, " ", time_fix_fin), tz = "EST"))) %>%
  na.omit() %>% # delete NA entries
  select(date, time, datetime_et, station_id:precip_in) %>%
  arrange(datetime_et)


# ---- 3. export data ----
# export to csv
write_csv(x = cocorahs_data_raw_fin, path = paste0(tabular_data_export_path, "cocorahs_data_raw.csv"))



