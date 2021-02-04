
# ---- script header ----
# script name: roc_analysis_script.R
# purpose of script: reliever operating characteristic (roc) curve analysis for ndfd performance
# author: sheila saia
# date created: 20210204
# email: ssaia@ncsu.edu


# ---- notes ----
# notes:
 

# ---- to do ----
# to do list


# ---- 1. load libraries ----
library(tidyverse)
library(sf)
library(here)


# ---- 2. load data ----
# data path (for now --> use here package later)
data_path <- "/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/"

# import cmu bounds (spatial)
cmu_bounds_albers <- st_read(paste0(data_path, "spatial/sheila_generated/cmu_bounds/cmu_bounds_albers.shp"))

# import lease centroids (spatial)
lease_centroids_albers <- st_read(paste0(data_path, "spatial/sheila_generated/lease_bounds/lease_centroids_albers.shp"))

# cmu information (tabular)
cmu_sga_key <- read_csv(file = paste0(data_path, "tabular/sheila_generated/ncdmf_rainfall_thresholds/cmu_sga_key.csv"), col_names = TRUE)

# rainfall thresholds (tabular)
rainfall_thresh_data_raw <- read_csv(file = paste0(data_path, "tabular/sheila_generated/ncdmf_rainfall_thresholds/rainfall_thresholds_raw_tidy.csv"), col_names = TRUE)


# ---- 3. find cmu's with the most leases ----
# tidy rainfall threshold data 
rainfall_thresh_data <- rainfall_thresh_data_raw %>%
  select(HA_CLASS, cmu_name, rain_in = rainfall_threshold_in)

# summarize number of leases in each cmu
lease_cmu_count <- lease_centroids_albers %>%
  st_drop_geometry() %>%
  group_by(cmu_name) %>%
  summarize(lease_count = n()) %>%
  left_join(rainfall_thresh_data, by = "cmu_name")

# join lease counts to cmu data (for plotting)
cmu_bounds_lease_count_join <- cmu_bounds_albers %>%
  left_join(lease_cmu_count, by = "cmu_name")

# tabular data
cmu_bounds_lease_count_join_tabular <- cmu_bounds_lease_count_join %>%
  st_drop_geometry()

# plot by lease count
pdf(file = here::here("figures", "cmu_lease_counts.pdf"), width = 10, height = 10)
ggplot(data = cmu_bounds_lease_count_join) +
  geom_sf(aes(fill = lease_count)) + 
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey75") +
  theme_classic()
dev.off()

# plot rainfall threshold depth by number of leases
ggplot(data = cmu_bounds_lease_count_join) +
  geom_point(aes(x = rain_in, y = lease_count))

# leases to pick
# cmu with 10 leases at 1 in (U092)
# cmu with 34 leases at 1.5 in (U130)
# cmu with 23 leases at 3 in (U144)


# ---- 4. select cmu's for analysis ----
# cmu list
cmu_sel_list <- c("U092", "U130", "U144")

# select cmus
cmu_bounds_albers_sel <- cmu_bounds_albers %>%
  filter(cmu_name %in% cmu_sel_list)


# ---- 5. find precip data that overlaps ----



# ---- 6. loop through ndfd data to get forecast result ----



# ---- 7. roc curve analysis ----





# data <- st_read("/Users/sheila/Documents/bae_shellcast_project/shellcast_analysis/data/spatial/sheila_generated/sga_bounds/sga_bounds_class_albers.shp")
# length(data$grow_area) # 617 total
# unique(data$ga_class)
# length(data$grow_area[data$ga_class == "cond_approved"]) # 84
# 84/617 * 100
# # 13.6
# length(data$grow_area[data$ga_class == "approved"])/length(data$grow_area) * 100
# # 10.7
# length(data$grow_area[data$ga_class == "restricted"])/length(data$grow_area) * 100
# # 22
# length(data$grow_area[data$ga_class == "prohibited"])/length(data$grow_area) * 100
# # 54
# 54 + 22 + 11 + 13



