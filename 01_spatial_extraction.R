#Author: Noormah Rizwan

# ==========================================================
# Load Required Packages
# ==========================================================
library(sf)
library(sp)
library(terra)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readxl)
library(data.table)
library(lubridate) 
library(rmapshaper)
library(raster)
library(exactextractr)


# ==========================================================
# Load Daily Temperature Data
# ==========================================================
temp_min <- read.csv("/tmin_e-obs.csv") 
temp_max <- read.csv("/tmax_e-obs.csv") 


# ==========================================================
# Clean Column Names for Temperature Data
# ==========================================================
# Remove prefixes like 't_' and suffixes like '.txt' from column names
colnames(temp_max) <- gsub("t_|\\.txt", "", colnames(temp_max))
colnames(temp_min) <- gsub("t_|\\.txt", "", colnames(temp_min))



# ==========================================================
# Combine Max and Min Temperature Data (Side-by-Side)
# ==========================================================
temp_min_selected <- temp_min[, colnames(temp_max)]

# Rename min temperature columns to avoid name conflicts
colnames(temp_min_selected)[-1] <- paste0(colnames(temp_min_selected)[-1], "_min")

# Combine temp_max and temp_min_selected side by side, matching by Date
combined_temp <- cbind(temp_max, temp_min_selected[-1])  # Exclude the second Date column

# View the result
head(combined_temp)

# Append '_max' to max temperature columns to distinguish them
colnames(combined_temp)[-1] <- ifelse(grepl("_min", colnames(combined_temp)[-1]), 
                                      colnames(combined_temp)[-1], 
                                      paste0(colnames(combined_temp)[-1], "_max"))



# ==========================================================
# Reshape from Wide to Long Format with Separate Min/Max
# ==========================================================
reshaped_temp <- combined_temp %>%
  pivot_longer(
    cols = -Date,  # Keep 'Date' as it is
    names_to = c("Longitude_Latitude", "Temperature_Type"),  # Separate Longitude_Latitude and Temperature_Type
    names_pattern = "(.*)_(max|min)"  # Use regex to split based on _max or _min
  ) %>%
  pivot_wider(
    names_from = Temperature_Type,  # Spread the max and min into separate columns
    values_from = value  # Take the values for max and min temperatures
  )

# Rename the variables
reshaped_temp <- reshaped_temp %>%
  rename(`Max Temperature` = max, `Min Temperature` = min)

# ==========================================================
# Clean Up Intermediate Objects
# ==========================================================
rm(combined_temp, temp_max, temp_min, temp_min_selected)


# Keep Data from 1998 onwards
reshaped_temp <- reshaped_temp  %>%
  filter(as.Date(Date) >= as.Date("1988-01-01"))


# Filter to keep Peak Irrigating Season Months only (June-August)
reshaped_temp <- reshaped_temp  %>%
  filter(month(as.Date(Date)) %in% c(6, 7, 8))


# Calculate Mean Daily Temperature 
reshaped_temp <- reshaped_temp %>%
  mutate(`Mean Temperature` = (`Max Temperature` + `Min Temperature`)/2)


# Convert 'Date' Column to Proper Date Format
reshaped_temp <- reshaped_temp %>%
  mutate(Date = as.Date(Date))

# ==========================================================
# Create Temperature Variables Used in Irrigation Adoption Model
# ==========================================================
#  30-year Historical Averages for Irrigating Season (June–August)
historical_averages <- reshaped_temp %>%
  # Filter for the years 1988-2018 and the months June, July, August
  filter(year(Date) >= 1988, year(Date) <= 2018) %>%
  group_by(Longitude_Latitude) %%
  summarise(
    historical_mean_temperature = mean(`Mean Temperature`, na.rm = TRUE),
    historical_max_temperature = mean(`Max Temperature`, na.rm = TRUE),
    .groups = 'drop' 
  )

# Extreme Heat Days - Total Days Above 35°C in Peak Season (June–August)
days_above <- reshaped_temp %>%
  mutate(
    Year = year(Date),
    Month = month(Date)
  ) %>%
  filter(Month %in% c(6, 7, 8)) %>%
  group_by(Longitude_Latitude, Year) %>%
  summarise(
    days_above_35 = sum(`Max Temperature` > 35, na.rm = TRUE),
    .groups = 'drop'
  )

# Compute Yearly Mean Temperatures & Differences
mean_temperatures <- reshaped_temp  %>%
  mutate(Year = year(Date)) %>%
  group_by(Longitude_Latitude, Year) %>%
  summarise(mean_max_temp = mean(`Max Temperature`, na.rm = TRUE),
            mean_maxmindiff_temp = mean(`Max-Min Temperature`, na.rm = TRUE),
            mean_mean_temp = mean(`Mean Temperature`, na.rm = TRUE)) %>%
  ungroup()

# ==========================================================
# Merge All Temperature Variables into Final Dataset
# ==========================================================
final_data <- mean_temperatures %>%
  left_join(days_above, by = c("Longitude_Latitude", "Year"))

final_data <- final_data %>%
  left_join(historical_averages, by = "Longitude_Latitude")

# Clean up temporary objects
rm(days_above, mean_temperatures, historical_averages, reshaped_temp)

# Filter to Keep Relevant Years
final_data  <- final_data %>% filter(Year > 2018)


# ==========================================================
# Convert Temperature Data to Spatial Feature (sf object)
# ==========================================================
final_data <- final_data %>%
  separate(Longitude_Latitude, into = c("Longitude", "Latitude"), sep = "_", convert = TRUE) %>%
  mutate(Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude))

final_data <- final_data %>%
  mutate(Longitude_adj = Longitude / 1000,  # Try scaling down
         Latitude_adj = Latitude / 1000)

final_data_sf <- st_as_sf(final_data, coords = c("Longitude_adj", "Latitude_adj"), crs = 4326)

# Check bounding boxes
print(st_bbox(final_data_sf))  

# ==========================================================
# Extract Field-Level Temperature Metrics from Gridded Data
# ==========================================================
# This script processes temperature raster data and extracts polygon-weighted
# temperature metrics (mean/max/min temperatures, heat days) for agricultural fields.
# ==========================================================

# ==========================================================
# Load Cadastral Shapefile (Field Boundaries)
# ==========================================================
shp=st_read("/Cadastral_parcel.shp")

# ----------------------------------------------------------
# Coordinate Cleanup and CRS Alignment
# ----------------------------------------------------------
shp <- st_transform(shp,  crs = 4326)
final_data_sf <- final_data_sf %>% dplyr::select(-Longitude, -Latitude)
final_data_sf <- st_transform(final_data_sf, crs = st_crs(shp))

# Add projected coordinates (x, y) for interpolation
final_data_df <- final_data_sf %>%
  mutate(x = st_coordinates(.)[,1],  # X-coordinates
         y = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

# ----------------------------------------------------------
# Create Raster Stacks for Each Year (Temp Metrics)
# ----------------------------------------------------------
years <- unique(final_data_df$Year)

raster_list <- list()

for (yr in years) {
  # Subset data for the current year
  df_year <- final_data_df %>% filter(Year == yr) %>% 
    dplyr::select(x, y, mean_maxmindiff_temp, mean_mean_temp, days_above_35, historical_mean_temperature)
  
  # Create a raster for this year (one layer per variable, resulting in a multi-layer SpatRaster)
  r_year <- rast(df_year, type = "xyz")
  crs(r_year) <- st_crs(shp)$wkt  # Assign the proper CRS
  
  raster_list[[as.character(yr)]] <- r_year
}

# ----------------------------------------------------------
# Extract Weighted Mean Values per Field Polygon
# ----------------------------------------------------------
field_vector <- vect(shp)
extraction_list <- list() 

# Loop over each year (each raster in the list)
for (year in names(raster_list)) {
  r_year <- raster_list[[year]]
  
  # Loop over each layer (temperature variable) in this year's raster
  for (i in 1:nlyr(r_year)) {
    layer_name <- names(r_year)[i]
    
    # Extract weighted average for each polygon using exact_extract()
    extracted <- exact_extract(r_year[[i]], shp,
                               fun = function(values, coverage_fraction) {
                                 valid <- !is.na(values)
                                 if (!any(valid)) return(NA)
                                 sum(values[valid] * coverage_fraction[valid]) / sum(coverage_fraction[valid])
                               }
    )
    
    df <- data.frame(
      id = shp$id,         # original field IDs from shp
      Year = as.integer(year),
      variable = layer_name,
      value = extracted
    )
    
    # Append to the list using a combined key (year and variable)
    extraction_list[[paste0(year, "_", layer_name)]] <- df
  }
}

# ----------------------------------------------------------
# Reshape and Export Extracted Panel Dataset
# ----------------------------------------------------------
final_extraction <- bind_rows(extraction_list)

final_extraction_wide <- final_extraction %>%
  pivot_wider(
    id_cols = c(id, Year),        
    names_from = variable,       
    values_from = value           
  )

write.csv(final_extraction_wide, "/final_extraction_long.csv", row.names = FALSE)

# ==========================================================
# Impute Missing Temperature Metrics Using IDW (Inverse Distance Weighting)
# ==========================================================
# This section imputes missing field-level temperature variables using Inverse Distance Weighting
# (IDW) based on spatial coordinates. Only ~4% of polygons lack weather data.
# ==========================================================
rm(shp)
# Reload shapefile
shp=st_read("/Cadastral_parcel.shp")
variables_to_impute <- c(
  "mean_maxmindiff_temp", "mean_mean_temp",
  "days_above_35", "historical_mean_temperature"
)

final_extraction_wide_sf <- left_join(shp, final_extraction_wide, by = "id")

# Split dataset into polygons with and without temperature data
withvalues <- final_extraction_wide_sf %>% 
  filter(!if_any(all_of(variables_to_impute), is.na))
withoutvalues <- final_extraction_wide_sf %>% 
  filter(if_any(all_of(variables_to_impute), is.na))

##Drop datasets
rm(final_extraction_wide)


# Extract Centroids and Coordinates
withvalues <- withvalues %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(X = st_coordinates(centroid)[,1],
         Y = st_coordinates(centroid)[,2])

withoutvalues <- withoutvalues %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(X = st_coordinates(centroid)[,1],
         Y = st_coordinates(centroid)[,2])


# Ensure CRS consistency
crs_withvalues <- st_crs(withvalues)
crs_withoutvalues <- st_crs(withoutvalues)

# Set Up Known and Unknown Data for Imputation
known_data <- withvalues %>%
  dplyr::select(id, Year, X, Y, all_of(variables_to_impute))

unknown_data <- withoutvalues %>%
  dplyr::select(id, Year, X, Y)

# Convert known data to sf object with point geometries
known_data_sf <- st_as_sf(known_data, coords = c("X", "Y"), crs = crs_withvalues)

# Convert unknown data to sf object with point geometries
unknown_data_sf <- st_as_sf(unknown_data, coords = c("X", "Y"), crs = crs_withvalues)

# Force centroid for consistent distance calculation
unknown_data_sf <- st_centroid(unknown_data_sf)
known_data_sf   <- st_centroid(known_data_sf)

# ----------------------------------------------------------
# Define IDW Function using k-NN from RANN
# ----------------------------------------------------------
library(RANN)

perform_idw_imputation_rann <- function(known_data_sf, unknown_data_sf, variable, idp = 2, k = 5) {
  coords_known <- st_coordinates(known_data_sf)
  coords_unknown <- st_coordinates(unknown_data_sf)
  nn_result <- nn2(data = coords_known, query = coords_unknown, k = k)
  indices <- nn_result$nn.idx      # Matrix of neighbor indices
  distances <- nn_result$nn.dists  # Matrix of distances
  

  known_values <- known_data_sf[[variable]]
  
  weights <- 1 / (distances^idp)
  weights[!is.finite(weights)] <- 1
  

  imputed_values <- sapply(1:nrow(indices), function(i) {
    nbr_indices <- indices[i, ]
    nbr_values <- known_values[nbr_indices]
    valid <- !is.na(nbr_values)
    if (!any(valid)) return(NA)
    w <- weights[i, ][valid]
    sum(nbr_values[valid] * w) / sum(w)
  })
  
  data.frame(id = unknown_data_sf$id,
             Year = unknown_data_sf$Year,
             value = imputed_values)
}


# ----------------------------------------------------------
# Run IDW Imputation for All Years and Variables
# ----------------------------------------------------------
imputed_data_list <- list()
years <- sort(unique(known_data_sf$Year))

for (var in variables_to_impute) {
  cat("Imputing variable:", var, "\n")
 imputed_values_list <- list()
  
  for (yr in years) {
    cat("Year:", yr, "\n")
    known_subset <- known_data_sf %>% filter(Year == yr, !is.na(.data[[var]]))
    unknown_subset <- unknown_data_sf %>% filter(Year == yr)
    
    if (nrow(unknown_subset) > 0 & nrow(known_subset) > 0) {
      imputed_df <- perform_idw_imputation_rann(known_data_sf = known_subset,
                                                unknown_data_sf = unknown_subset,
                                                variable = var,
                                                idp = 2,
                                                k = 5)
      imputed_values_list[[as.character(yr)]] <- imputed_df
    } else {
      imputed_values_list[[as.character(yr)]] <- data.frame(id = unknown_subset$id,
                                                            value = NA)
    }
  }
  imputed_values_var <- bind_rows(imputed_values_list)
  imputed_values_var[[var]] <- imputed_values_var$value
  imputed_values_var$value <- NULL
  
  imputed_data_list[[var]] <- imputed_values_var
}


# ----------------------------------------------------------
# Merge Imputed and Observed Data into Final Dataset
# ----------------------------------------------------------
withoutvalues <- withoutvalues %>% dplyr::select(-one_of(variables_to_impute))
for (var in variables_to_impute) {
  imputed_values_var <- imputed_data_list[[var]]
  withoutvalues <- left_join(withoutvalues, imputed_values_var[, c("id", "Year", var)], by = c("id", "Year"))
}

withoutvalues <- withoutvalues %>% dplyr::select(id, Year, mean_maxmindiff_temp, mean_mean_temp, days_above_35, historical_mean_temperature)
withvalues <- withvalues %>% dplyr::select(id, Year, mean_maxmindiff_temp, mean_mean_temp, days_above_35, historical_mean_temperature)

final_data <- bind_rows(withvalues, withoutvalues) %>% arrange(id, Year)


# Save the Dataset
saveRDS(final_data, "/historic_temp.rds")
