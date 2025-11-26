#Author: Noormah Rizwan
## Code for Baseline Irrigation Adoption Model

# ==========================================================
# Load Required Packages
# ==========================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, terra, tidyverse, tidyr, dplyr, readxl, data.table)


# ==========================================================
# Load Irrigation Data
# ==========================================================
irrigation <- read.csv("/Irrigation2020_2022MeanCorrected.csv")

# Load Cadastral Shapefile (Field Boundaries) 
shp=st_read("/Cadastral_parcel.shp")

# Merge irrigation with shp to get a sf object
irrigation_sf <- merge(shp, irrigation, by = "id", all.x = TRUE)
class(irrigation_sf)

# Clean Up Intermediate Objects
rm(irrigation, shp)


# Calculate area of each field (Field Area Variable)
irrigation_sf$field_area <- st_area(irrigation_sf)
# Field area is in m^2

# Load operational canals shapefile 
canals <- st_read("OKM shape/okm.shp")

# Match CRS with sopatila irrigation data
canals <- st_transform(canals, st_crs(irrigation_sf))

# Calculate distance from each field to the nearest canal
nearest_canal_index <- st_nearest_feature(irrigation_sf, canals)
##This distance is in meters 


# Extract names of the canals that are nearest to the fields
nearest_canal_names <- canals$vodotoka[nearest_canal_index]

# Calculate the distance to the nearest canal
distance_to_nearest_canal <- st_distance(irrigation_sf, canals[nearest_canal_index, ], by_element = TRUE)

# Add the nearest canal name and distance to your irrigation_sf data
irrigation_sf <- irrigation_sf %>%
  mutate(
    nearest_canal_name = nearest_canal_names,
    distance_to_nearest_canal = as.numeric(distance_to_nearest_canal)
  )

##Convert to km
irrigation_sf <- irrigation_sf %>%
  mutate(
    distance_to_nearest_canal = distance_to_nearest_canal/1000
  )

# Clean Up Intermediate Objects
rm(canals)

# Convert to datafram
as.data.frame(irrigation_sf)

# Create Irrigation Variable 
irrigation_sf <- irrigation_sf %>%
  mutate(irrigation_2020 = rowSums(across(c(irrsoy20, irrmai20, irrbeet20)), na.rm = TRUE))

irrigation_sf <- irrigation_sf %>%
  mutate(irrigation_2021 = rowSums(across(c(irrsoy21, irrmai21, irrbeet21)), na.rm = TRUE))


irrigation_sf <- irrigation_sf %>%
  mutate(irrigation_2022 = rowSums(across(c(irrsoy22, irrmai22, irrbeet22)), na.rm = TRUE))

# Classify a field as irrigated if rowSum >= 0.5  
irrigation_sf <- irrigation_sf %>%
  mutate(irrigated_2020 = ifelse(irrigation_2020 >= 0.5, 1, 0))
irrigation_sf <- irrigation_sf %>%
  mutate(irrigated_2021 = ifelse(irrigation_2021 >= 0.5, 1, 0))

irrigation_sf <- irrigation_sf %>%
  mutate(irrigated_2022 = ifelse(irrigation_2022 >= 0.5, 1, 0))

# Keep relevant variables 
irrigation_sf <- irrigation_sf %>% select(-parcelid, -X, - irrsoy20, -irrmai20, -irrbeet20, -irrsoy21, -irrmai21, -irrbeet21, -irrsoy22, -irrmai22, -irrbeet22)


# Bring in Soil Quality data 
soil_quality <- readRDS("/field_soil_quality.rds")
soil_quality <- soil_quality %>% select(id, awc, geometry)

# Merge with irrigation data on field id 
irrigation_soil <- merge(irrigation, soil_quality, by = "id")

# Keep selected variables
irrigation_soil <- irrigation_soil %>% select(-geometry.y)
# Clean Up Intermediate Objects
rm(irrigation, soil_quality)


# Bring in Aquifer type data
aquifer_level <- readRDS("/field_aquifer_type.rds")

# Merge the 2 datasets on field id
irrigation_soil_gw <- merge(irrigation_soil, aquifer_level, by = "id")

# Keep selected variables
irrigation_soil_gw <- irrigation_soil_gw %>% rename(geometry = geometry.x)
# Clean Up Intermediate Objects
rm(aquifer_level, irrigation_soil)


# Bring in Temperature and Soil Moisture 
temp <- readRDS("/Temperature/historic_temp.rds")
soil_moisture <- readRDS("/Soil Moisture/field_weather.rds")

# Keep Relevant Variables
temp <- st_drop_geometry(temp)
soil_moisture <- soil_moisture %>% select(id, MEAN, irrigation_precip,irrigation_soil_moisture, historical_precip ,historical_soil_moisture)

weather <- cbind(final_data, soil_moisture)

names(weather)[7] <- "id_2"
weather <- weather %>% select(-id_2)

weather <- weather %>% arrange(id, Year)

# Combine the 2 datasets
##Reshape Irrigation Dataset 
irrigation_soil_gw <- irrigation_soil_gw %>% select(-irrigation_2020, -irrigation_2021, -irrigation_2022, -area, -nearest_canal_name)

irrigation_soil_gw <- irrigation_soil_gw %>%
  pivot_longer(cols = starts_with("irrigated"), 
               names_to = "Year", 
               names_prefix = "irrigated_", 
               values_to = "irrigation") %>%
  mutate(Year = as.numeric(Year)) 


irrigation_soil_gw_weather <- full_join(irrigation_soil_gw, weather, by = c("id", "Year"))
irrigation_soil_gw_weather <- irrigation_soil_gw_weather %>% arrange(id, Year)
rm(irrigation_soil_gw, weather)


# Create lagged weather variables
library(plm)
irrigation_soil_gw_weather <- pdata.frame(irrigation_soil_gw_weather, index = c("id", "Year"))

irrigation_soil_gw_weather$mean_temperature_lag <- lag(irrigation_soil_gw_weather$mean_mean_temp, 1)
irrigation_soil_gw_weather$above_35_lag <- lag(irrigation_soil_gw_weather$days_above_35, 1)
irrigation_soil_gw_weather$soil_moisture_lag <- lag(irrigation_soil_gw_weather$irrigation_soil_moisture, 1)
irrigation_soil_gw_weather$precip_lag <- lag(irrigation_soil_gw_weather$irrigation_precip, 1)

# Drop non-lagged variables
irrigation_soil_gw_weather <- irrigation_soil_gw_weather %>% select(-mean_maxmindiff_temp, -mean_mean_temp, -days_above_35, -irrigation_soil_moisture, -irrigation_precip)


# Keep Relevant Years (2020-2022)
irrigation_soil_gw_weather <- irrigation_soil_gw_weather %>% filter(Year != 2019)

# Bring in Crop Data to drop fields with no irrigation data 
crop <- readRDS("/Baseline/crop.rds")
crop <- crop %>% select(Year, id, crop)

crop <- crop %>% filter(Year > 2019)

# Merge with crop data 
irrigation_soil_gw_weather$Year <- as.numeric(as.character(irrigation_soil_gw_weather$Year)) 
crop$Year <- as.numeric(crop$Year)


irrigation_soil_gw_weather_crop <- merge(irrigation_soil_gw_weather, crop, by = c("id", "Year"))
rm(irrigation_soil_gw_weather, crop)

# Drop fields that will never be irrigated because they never grow maize, soybean or sugarbeet in any year. 
allowed_crops <- c(0, 2, 3)  

# Identify fields that have grown maize, soybean, or sugar beet at least once between 2020-2022
valid_fields <- irrigation_soil_gw_weather_crop %>%
  filter(Year %in% c(2020, 2021, 2022)) %>%  
  group_by(id) %>%
  filter(any(crop %in% allowed_crops)) %>%  
  ungroup()

irrigation_soil_gw_weather_crop <- irrigation_soil_gw_weather_crop %>%
  filter(id %in% valid_fields$id)


# For irrigated fields, keep the first year they irrigate and drop subsequent years. For unirrigated, keep all years. 
irrigation_soil_gw_weather_long <- irrigation_soil_gw_weather_crop %>%
  mutate(Year = as.numeric(as.character(Year))) %>%  # Ensure 'Year' is numeric
  group_by(id) %>%
  mutate(
    first_irrigation_year = ifelse(any(irrigation == 1), min(Year[irrigation == 1]), NA)
  ) %>%
  ungroup() %>%
  # Keep rows up to and including the first irrigation year, or all rows for fields that never irrigated
  filter(
    is.na(first_irrigation_year) |  # Fields that never irrigated
      Year <= first_irrigation_year  # Fields up to the first irrigation year
  )

irrigation_soil_gw_weather_long <-  irrigation_soil_gw_weather_long %>% select(-geometry, -first_irrigation_year)
irrigation_soil_gw_weather_long <-  irrigation_soil_gw_weather_long %>% rename(irrigation_adoption = irrigation)


##Save Dataset 
setwd("/Creating Irrigation Adoption Model Cross_section/Baseline")
library(haven)
library(janitor)
irrigation_soil_gw_weather_long <- janitor::clean_names(irrigation_soil_gw_weather_long)
haven::write_dta(irrigation_soil_gw_weather_long, "irrigation_adoption_baseline.dta")

