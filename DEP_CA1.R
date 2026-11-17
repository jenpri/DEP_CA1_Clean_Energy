# ---------------------------------------------------------
# Data Exploration & Preparation - CA1
# Clean Electricity Transition Across Countries
# ---------------------------------------------------------

# Author: Jenny Matarrita
# Module: Data Exploration & Preparation
# Dataset: Our World in Data - Energy Dataset



# 1. Load libraries


library(tidyverse)
library(ggplot2)



# 2. Load dataset


energy <- read.csv(
  "owid_energy.csv",
  stringsAsFactors = FALSE
)



# 3. Initial inspection


dim(energy)

names(energy)

head(energy)

str(energy)

summary(energy)


# 4. Initial data quality checks


colSums(is.na(energy))

sum(is.na(energy))

range(energy$year, na.rm = TRUE)

length(unique(energy$country))

unique(energy$country)[1:30]

sum(is.na(energy$iso_code))

head(
  energy[, c("country", "year", "iso_code")],
  20
)

# Key dataset information

dim(energy)

range(energy$year, na.rm = TRUE)

length(unique(energy$country))

sort(colSums(is.na(energy)), decreasing = TRUE)[1:20]

# 5. Select relevant variables


energy_selected <- energy %>%
  select(
    country,
    year,
    iso_code,
    population,
    electricity_carbon_intensity_gco2eq_per_kwh,
    electricity_demand_per_capita_kwh,
    fossil_fuels_electricity_share_pct,
    hydro_electricity_share_pct,
    renewables_electricity_share_pct,
    solar_electricity_share_pct,
    wind_electricity_share_pct
  )

# 6. Filter recent years and valid country codes


energy_clean <- energy_selected %>%
  filter(
    year >= 2010,
    nchar(iso_code) == 3
  )
dim(energy_clean)

colSums(is.na(energy_clean))

# 7. Check complete cases


sum(complete.cases(energy_clean))

dim(na.omit(energy_clean))

energy_clean <- na.omit(energy_clean)

dim(energy_clean)

colSums(is.na(energy_clean))