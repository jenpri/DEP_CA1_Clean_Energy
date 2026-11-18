# Clean Electricity Transition Across Countries

Data Exploration & Preparation CA1 project analysing renewable electricity, fossil-fuel dependence and carbon intensity across countries using R.

## Project Objective

The project explores how electricity systems differ between countries and investigates relationships between renewable electricity, fossil fuels and carbon intensity.

The analysis includes:

- Data inspection and cleaning
- Missing value handling
- Outlier detection using boxplots and the IQR method
- Exploratory Data Analysis (EDA)
- Correlation analysis
- One-hot encoding
- Principal Component Analysis (PCA)
- Poster-ready visualisations

## Dataset

Source: Our World in Data Energy Dataset

Original dataset:

- 19,494 rows
- 130 variables

Final cleaned dataset:

- 3,135 rows
- 11 variables
- Analysis period: 2010–2025
- 207 ISO-coded countries and territories

Dataset source:

https://ourworldindata.org/energy

## Main Findings

- Renewable electricity share showed a strong negative relationship with carbon intensity: **r = -0.87**
- Fossil-fuel electricity share showed a very strong positive relationship with carbon intensity: **r = 0.93**
- Renewable electricity increased over the analysed period
- Hydropower had the highest typical electricity share among hydro, solar and wind
- PCA reduced seven numerical energy indicators to two principal components explaining **71.7% of total variation**
- PC1 mainly represented the contrast between fossil-intensive and renewable electricity systems
- PC2 mainly reflected differences in renewable source mix

## Technologies Used

- R
- RStudio
- tidyverse
- ggplot2
- corrplot

## Project Files

- `DEP_CA1.R` – complete R analysis
- `owid_energy.csv` – original dataset
- Poster-ready PNG visualisations
- `README.md`

## Running the Project

1. Download or clone this repository.
2. Make sure `owid_energy.csv` is in the same project folder as `DEP_CA1.R`.
3. Open the project in RStudio.
4. Run `DEP_CA1.R`.

Required packages:

```r
library(tidyverse)
library(ggplot2)
library(corrplot)
