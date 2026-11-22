source("R_codes/cleaning_data.R")

library(dplyr)
library(stringr)
library(sf)
library(tigris)
library(janitor)

options(tigris_use_cache = TRUE)

# Combine all county-level datasets
dashboard_data <- dfw_county_adoption %>%
  left_join(
    dfw_ev_stations_counts,
    by = "County"
  ) %>%
  left_join(
    dfw_ev_ports_counts,
    by = "County"
  ) %>%
  left_join(
    dfw_counties_income,
    by = "County"
  )

# Download Texas county boundaries
tx_counties <- tigris::counties(
  state = "TX",
  year = 2025,
  cb = TRUE,
  class = "sf"
)

# Keep only the 16 DFW counties
dfw_boundaries <- tx_counties %>%
  filter(NAME %in% dfw_counties) %>%
  mutate(County = NAME)

# Check which data counties have no boundary
missing_boundaries <- dashboard_data %>%
  distinct(County) %>%
  anti_join(
    st_drop_geometry(dfw_boundaries) %>%
      distinct(County),
    by = "County"
  )

print(missing_boundaries)

# Join data to geographic boundaries and standardize column names
dfw_map <- dfw_boundaries %>%
  left_join(
    dashboard_data,
    by = "County"
  ) %>%
  clean_names()

# Check the result using the cleaned column names
print(
  dfw_map %>%
    st_drop_geometry() %>%
    select(
      county,
      adoption_rate,
      charging_stations,
      charging_ports,
      median_household_income
    )
)

# Check missing values
colSums(is.na(st_drop_geometry(dfw_map)))