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



install.packages("tidync")
library(tidync)
library(tidyverse)

# https://ropensci.org/blog/2019/11/05/tidync/
# https://cran.r-project.org/web/packages/tidync/tidync.pdf

test <- tidync(x = "https://tds.climate.ncsu.edu/thredds/catalog/prism/normals_4km/catalog.html?dataset=prism/normals_4km/PRISM_30yr_normal_4kmM2_annual.nc")
