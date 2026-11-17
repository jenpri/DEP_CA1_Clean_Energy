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

# 8. Dataset characterisation


summary(energy_clean)

sapply(energy_clean, class)

length(unique(energy_clean$country))

range(energy_clean$year)

# Check duplicated rows
sum(duplicated(energy_clean))

# 9. Outlier exploration


boxplot(
  energy_clean$renewables_electricity_share_pct,
  main = "Renewable Electricity Share",
  ylab = "Percentage"
)

boxplot(
  energy_clean$electricity_carbon_intensity_gco2eq_per_kwh,
  main = "Electricity Carbon Intensity",
  ylab = "gCO2eq/kWh"
)

boxplot(
  energy_clean$electricity_demand_per_capita_kwh,
  main = "Electricity Demand per Capita",
  ylab = "kWh"
)
# Count outliers using the IQR method
count_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  
  sum(x < lower | x > upper)
}

count_outliers(energy_clean$renewables_electricity_share_pct)
count_outliers(energy_clean$electricity_carbon_intensity_gco2eq_per_kwh)
count_outliers(energy_clean$electricity_demand_per_capita_kwh)

# 10. Investigate unusual observations


# Highest electricity carbon intensity values
energy_clean %>%
  arrange(desc(electricity_carbon_intensity_gco2eq_per_kwh)) %>%
  select(
    country,
    year,
    electricity_carbon_intensity_gco2eq_per_kwh
  ) %>%
  head(15)


# Highest electricity demand per capita values
energy_clean %>%
  arrange(desc(electricity_demand_per_capita_kwh)) %>%
  select(
    country,
    year,
    electricity_demand_per_capita_kwh
  ) %>%
  head(20)
# -------------------------
# 11. Exploratory Data Analysis
# Research Question 1


ggplot(
  energy_clean,
  aes(
    x = renewables_electricity_share_pct,
    y = electricity_carbon_intensity_gco2eq_per_kwh
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Renewable Electricity Share vs Carbon Intensity",
    x = "Renewable Electricity Share (%)",
    y = "Carbon Intensity (gCO2eq/kWh)"
  ) +
  theme_minimal()
cor(
  energy_clean$renewables_electricity_share_pct,
  energy_clean$electricity_carbon_intensity_gco2eq_per_kwh
)
# -------------------------
# Research Question 2


ggplot(
  energy_clean,
  aes(
    x = fossil_fuels_electricity_share_pct,
    y = electricity_carbon_intensity_gco2eq_per_kwh
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fossil Fuel Electricity Share vs Carbon Intensity",
    x = "Fossil Fuel Electricity Share (%)",
    y = "Carbon Intensity (gCO2eq/kWh)"
  ) +
  theme_minimal()
cor(
  energy_clean$fossil_fuels_electricity_share_pct,
  energy_clean$electricity_carbon_intensity_gco2eq_per_kwh
)
# -------------------------
# Research Question 3
# Distribution of renewable electricity


ggplot(
  energy_clean,
  aes(x = renewables_electricity_share_pct)
) +
  geom_histogram(
    bins = 30,
    colour = "black",
    fill = "lightblue"
  ) +
  labs(
    title = "Distribution of Renewable Electricity Share",
    x = "Renewable Electricity Share (%)",
    y = "Frequency"
  ) +
  theme_minimal()
summary(energy_clean$renewables_electricity_share_pct)

# -------------------------
# Research Question 4
# Compare renewable sources


renewable_sources <- energy_clean %>%
  select(
    hydro_electricity_share_pct,
    solar_electricity_share_pct,
    wind_electricity_share_pct
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "source",
    values_to = "share"
  )

ggplot(
  renewable_sources,
  aes(x = source, y = share)
) +
  geom_boxplot() +
  labs(
    title = "Distribution of Renewable Electricity Sources",
    x = "Renewable Source",
    y = "Electricity Share (%)"
  ) +
  theme_minimal()

renewable_sources %>%
  group_by(source) %>%
  summarise(
    mean_share = mean(share),
    median_share = median(share),
    max_share = max(share)
  )

# -------------------------
# Research Question 5
# Renewable electricity trend over time


renewable_trend <- energy_clean %>%
  group_by(year) %>%
  summarise(
    mean_renewable_share = mean(
      renewables_electricity_share_pct
    )
  )

ggplot(
  renewable_trend,
  aes(
    x = year,
    y = mean_renewable_share
  )
) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Average Renewable Electricity Share Over Time",
    x = "Year",
    y = "Average Renewable Electricity Share (%)"
  ) +
  theme_minimal()

renewable_trend


# 12. Correlation analysis

correlation_data <- energy_clean %>%
  select(
    electricity_carbon_intensity_gco2eq_per_kwh,
    electricity_demand_per_capita_kwh,
    fossil_fuels_electricity_share_pct,
    hydro_electricity_share_pct,
    renewables_electricity_share_pct,
    solar_electricity_share_pct,
    wind_electricity_share_pct
  )

correlation_matrix <- cor(correlation_data)

round(correlation_matrix, 2)


library(corrplot)

# Short labels for correlation plot
colnames(correlation_matrix) <- c(
  "Carbon",
  "Demand",
  "Fossil",
  "Hydro",
  "Renewables",
  "Solar",
  "Wind"
)

rownames(correlation_matrix) <- c(
  "Carbon",
  "Demand",
  "Fossil",
  "Hydro",
  "Renewables",
  "Solar",
  "Wind"
)

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.8
)