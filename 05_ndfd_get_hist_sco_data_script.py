# -*- coding: utf-8 -*-
"""
# ---- script header ----
script name: 05_ndfd_get_hist_sco_data_script.py
purpose of script: This script grabs historic National Digital Forecast Dataset (NDFD) data from the NC State Climate Office (SCO) TDS server, reformats it, and stores it in a local directory.
author: sheila saia
email: ssaia@ncsu.edu
date created: 20200408

# ---- notes ----
notes:
ndfd catalog website: https://tds.climate.ncsu.edu/thredds/catalog/nws/ndfd/catalog.html

help:
pydap help: https://pydap.readthedocs.io/en/latest/developer_data_model.html
thredds help (with python code): https://oceanobservatories.org/thredds-quick-start/#python
to see the nc sco catalog website: https://tds.climate.ncsu.edu/thredds/catalog/nws/ndfd/catalog.html


"""

# %% to do list

# TODO is there a more efficient way to loop through the different hours of data? (in the tidy data function)


# %% load libraries

import sys
import pandas # for data mgmt
import numpy # for data mgmt
import datetime as dt # for datetime mgmt
from pydap.client import open_url # to convert bin file
import requests # to check if website exists
from csv import writer

# %% set paths

# set project path
project_path = "/Users/ssaia/Documents/github/paper-ndfd-nccoast-study/"

# function path
function_path = project_path + "functions/"

# tabular data output path
tabular_data_output_path = project_path + "data/tabular/ndfd_data_raw/"


# %% set anchor to project and load custom functions
# set anchor
sys.path.insert(0, project_path)

# import custom functions
from functions import aggregate_ndfd_var_data, append_list_as_row, convert_ndfd_datetime_str, get_ndfd_data, get_var_col_name, tidy_ndfd_data # see functions.py file


# %% test functions

# datetime
test_datetime_uct_str = "2015-01-02 00:00"
#test_datetime_uct_str = "2016-04-13 00:00" # this one doesn't exist (but need to check it works)
#test_datetime_uct_str = "2016-01-20 00:00" # this one doesn't have the desired times
#test_datetime_uct_str = "2016-06-05 00:00" # this one has reftime and time
#test_datetime_uct_str = "2017-05-23 00:00" # this one has some data (pop12) but not others (qpf)

# test function
test_ym_str, test_ymd_str, test_ymdh_str = convert_ndfd_datetime_str(datetime_str = test_datetime_uct_str)

test_ym_str
test_ymd_str
test_ymdh_str

# define serve path
sco_server_url = 'https://tds.climate.ncsu.edu/thredds/dodsC/nws/ndfd/'
# this is the server path for historic ndfd forecasts
# to see the catalog website: https://tds.climate.ncsu.edu/thredds/catalog/nws/ndfd/catalog.html

# get data
test_data = get_ndfd_data(base_server_url = sco_server_url, datetime_uct_str = test_datetime_uct_str)
#test_data, test_url_status, test_url = get_ndfd_data(base_server_url = sco_server_url, datetime_uct_str = test_datetime_uct_str)

# tidy qpf data
test_qpf_data_pd, test_qpf_datetime_ymdh_str = tidy_ndfd_data(ndfd_data = test_data, datetime_uct_str = test_datetime_uct_str, ndfd_var = "qpf")

test_qpf_data_pd
test_qpf_datetime_ymdh_str

# tidy pop12 data
test_pop12_data_pd, test_pop12_datetime_ymdh_str = tidy_ndfd_data(ndfd_data = test_data, datetime_uct_str = test_datetime_uct_str, ndfd_var = "pop12")

test_pop12_data_pd
test_pop12_datetime_ymdh_str

# test non-valid ndfd_var option
#tidy_ndfd_data(ndfd_data = test_data, datetime_uct_str = test_datetime_uct_str, ndfd_var = "qff")

# %% generate datetime dataset for looping

# define start datetime
start_datetime_str = "2015-01-01 00:00"

# create list with start datetime as first value
datetime_list = [start_datetime_str]

# define length of list (number of days for however many years)
num_years = 2
num_days_per_year = 365

# loop to fill in datetime_list
for i in range(1, (num_days_per_year * num_years + 1)):
    start_step = pandas.to_datetime(datetime_list[i-1], format = "%Y-%m-%d %H:%M").tz_localize(tz = "UCT")
    next_step = start_step + pandas.Timedelta('1 days')
    next_step_str = next_step.strftime("%Y-%m-%d %H:%M")
    datetime_list.append(next_step_str)

# convert datetime_list to a pandas dataframe
datetime_list_pd = pandas.DataFrame(datetime_list, columns = {'datetime_uct_str'})

# %% loop variables

# define serve path
sco_server_url = 'https://tds.climate.ncsu.edu/thredds/dodsC/nws/ndfd/'
# this is the server path for historic ndfd forecasts
# to see the catalog website: https://tds.climate.ncsu.edu/thredds/catalog/nws/ndfd/catalog.html

# empty pandas dataframes
pop12_data_pd = pandas.DataFrame(columns = ['index', 'y_index', 'x_index', 'pop12_value_perc', 'valid_period_hrs',
       'longitude_km', 'latitude_km', 'time', 'time_uct_long', 'time_uct',
       'time_nyc_long', 'time_nyc'])
qpf_data_pd = pandas.DataFrame(columns = ['index', 'y_index', 'x_index', 'qpf_value_kgperm2', 'valid_period_hrs',
       'longitude_km', 'latitude_km', 'time', 'time_uct_long', 'time_uct',
       'time_nyc_long', 'time_nyc'])

# keep track of available dates
data_available_pd = pandas.DataFrame(columns = ['datetime_uct_str', 'status'])

# %% loop!
for date in range(0, len(datetime_list_pd)):
    # grab datetime
    temp_datetime_uct_str = datetime_list_pd['datetime_uct_str'][date]

    # get data
    temp_data = get_ndfd_data(base_server_url = sco_server_url, datetime_uct_str = temp_datetime_uct_str)

    # only append data when it exists
    if (len(temp_data) > 0):
        # tidy qpf and pop12 data
        temp_qpf_data_pd, temp_qpf_datetime_ymdh_str = tidy_ndfd_data(ndfd_data = temp_data, datetime_uct_str = temp_datetime_uct_str, ndfd_var = "qpf")
        temp_pop12_data_pd, temp_pop12_datetime_ymdh_str = tidy_ndfd_data(ndfd_data = temp_data, datetime_uct_str = temp_datetime_uct_str, ndfd_var = "pop12")

        # check if desired times were available, only keep when we have both
        if ((len(temp_qpf_data_pd) > 0) and (len(temp_pop12_data_pd) > 0)):
            # append data to final dataframe
            # qpf_data_pd = qpf_data_pd.append(temp_qpf_data_pd, ignore_index = True)
            # pop12_data_pd = pop12_data_pd.append(temp_pop12_data_pd, ignore_index = True)

            # define export path
            temp_datetime_ymdh_str = convert_ndfd_datetime_str(temp_datetime_uct_str)[2]
            temp_qpf_data_path = tabular_data_output_path + "qpf_" + temp_datetime_ymdh_str +  ".csv" # tabular_data_output_path definited at top of script
            temp_pop12_data_path = tabular_data_output_path + "pop12_" + temp_datetime_ymdh_str + ".csv" # tabular_data_output_path definited at top of script

            # export results
            temp_qpf_data_pd.to_csv(temp_qpf_data_path, index = False)
            temp_pop12_data_pd.to_csv(temp_pop12_data_path, index = False)

            # keep track of available data
            temp_data_available_pd = pandas.DataFrame({'datetime_uct_str':[temp_datetime_uct_str], 'status':["available"]})
            data_available_pd = data_available_pd.append(temp_data_available_pd, ignore_index = True)

            # print status
            # print("appended " + temp_datetime_uct_str + " data")
            print("exported " + temp_datetime_uct_str + " data")

        else:
            # keep track of available data
            temp_data_available_pd = pandas.DataFrame({'datetime_uct_str':[temp_datetime_uct_str], 'status':["not_available"]})
            data_available_pd = data_available_pd.append(temp_data_available_pd, ignore_index = True)

            # print status
            print("did not append " + temp_datetime_uct_str + " data")

    else:
        # keep track of available data
        temp_data_available_pd = pandas.DataFrame({'datetime_uct_str':[temp_datetime_uct_str], 'status':["not_available"]})
        data_available_pd = data_available_pd.append(temp_data_available_pd, ignore_index = True)

        # print status
        print("did not append " + temp_datetime_uct_str + " data")

# %%
# define qpf and pop12 data export paths
# qpf_data_path = tabular_data_output_path + "qpf" + "_.csv" # tabular_data_output_path definited at top of script
# pop12_data_path = tabular_data_output_path + "pop12" + "_data.csv" # tabular_data_output_path definited at top of script

# export qpf and pop12 data
# qpf_data_pd.to_csv(qpf_data_path, index = False)
# pop12_data_pd.to_csv(pop12_data_path, index = False)

# export data availability
data_availability_path = tabular_data_output_path + "data_available.csv"
data_available_pd.to_csv(data_availability_path, index = False)

# export datetime dataframe
datetime_pd_path = tabular_data_output_path + "datetime_list_pd.csv"
datetime_list_pd.to_csv(datetime_pd_path, index = False)

# it works! :)
# can do about 1-2 days per min so for all 1095 days would take about 18.25 hours



# %% checking data availability for different times of the day

#test_datetime_uct_str = "2016-01-01 22:00"
#
#test_ym_str, test_ymd_str, test_ymdh_str = convert_ndfd_datetime_str(datetime_str = test_datetime_uct_str)
#
#sco_server_url = 'https://tds.climate.ncsu.edu/thredds/dodsC/nws/ndfd/'
#
#test_data = get_ndfd_data(base_server_url = sco_server_url, datetime_uct_str = test_datetime_uct_str)
#
#ndfd_data = test_data
#
#var_data = ndfd_data['Total_precipitation_surface_6_Hour_Accumulation']
#
#var_data_dims = var_data.dimensions
#
##var_data_dims
#
#var_data_time_dim = var_data_dims[0]
#
##var_data_time_dim
#
#var_time_np = numpy.array(var_data[var_data_time_dim][:])
#
#len(var_time_np)
#var_time_np

# at 00:00 have the following valid periods available for qpfarray([ 6., 12., 18., 24., 30., 36., 42., 48., 54., 60., 66., 72.])
# at 12:00 have the following valid periods available for qpf array([ 6., 12., 18., 24., 30., 36., 42., 48., 54., 60.])
# at 13:00 have the following valid periods available for qpf array([ 5., 11., 17., 23., 29., 35., 41., 47., 53., 59.])
# from 13:00 - 21:00 there's no extra data available that would be helpful to us
# at 22:00 have the following valid periods available for qpf array([ 2.,  8., 14., 20., 26., 32., 38., 44., 50., 56., 62., 68., 74.])
# but why pull it at 22:00 when can pull it at 00:00?
