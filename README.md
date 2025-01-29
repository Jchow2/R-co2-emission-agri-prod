# Level of CO2 Emissions on Agricultural Production

## Abstract
The rise in carbon dioxide emissions has significantly impacted global temperatures, leading to disruptions in food availability and quality. Climate change has increased the frequency of extreme weather events and reduced water availability, affecting agricultural productivity. This project investigates the effect of using non-renewable energy options on agricultural production across 60 countries. By employing elastic regression models on a cross-sectional dataset provided by the World Development Indicator Database, we analyze the relationship between CO2 emissions, renewable energy usage, and agricultural emissions.

Our interactive Shiny app allows users to explore how different levels of oil use and agricultural emissions affect renewable energy predictions. The results reveal significant trends and relationships in the data, demonstrating that increased CO2 emissions reduce agricultural production in the short term. Despite limitations, such as not accounting for year-to-year temperature changes, our findings contribute to the empirical evidence that CO2 emissions have adverse effects on agricultural productivity.

## Table of Contents
- [Project Overview](#project-overview)
- [Dataset](#dataset)
- [Scripts](#scripts)
  - [eda_agrico2.R](#eda_agrico2r)
  - [elastic_regression_agrico2.R](#elastic_regression_agrico2r)
  - [shinyapp.R](#shinyappr)
- [Shiny App](#shiny-app)
- [Installation](#installation)
- [Usage](#usage)
- [Results](#results)
- [Contributing](#contributing)
- [License](#license)
- [Reference](#reference)

## Project Overview
This project aims to analyze the impact of renewable energy usage and oil-powered emissions on agricultural emissions. By employing elastic regression models, we aim to identify significant predictors and trends in the data.

## Dataset
The datasets used in this project include:
- `Energy use (kg of oil equivalent per capita).csv`
- `Renewable energy consumption (% of total final energy consumption).csv`
- `Emissions_Agricultural_Energy use.csv`

## Scripts
This repository contains the following R scripts:

1. `eda_agrico2.R`: This script performs Exploratory Data Analysis (EDA) on the agricultural emissions and energy use datasets.

2. `elastic_regression_agrico2.R`: This script fits elastic regression models to the data and generates predictions based on the relationship between energy use and agricultural emissions.

3. `shinyapp.R`: This script creates a Shiny app to visualize the results of the elastic regression models and allows users to interactively explore the data.

## Shiny App
The Shiny app provides an interactive interface to visualize the relationship between energy use and agricultural emissions. Users can adjust the input parameters to see how they affect the predictions.

### Key Features
- **Interactive Sliders**: Adjust the logarithmic values of oil use and agricultural emissions to see real-time changes in the predictions.
- **Checkbox**: Toggle the high renewable energy usage to observe its impact on the predicted renewable energy use.
- **Dynamic Plot**: The app generates a scatter plot with a regression line, color-coded to show the predicted renewable energy use across different levels of oil use.

### Interpreting Results
The Shiny app visualizes the predicted renewable energy use as a function of oil use. By adjusting the sliders and checkbox, users can explore how different levels of oil use and agricultural emissions impact renewable energy use predictions. The interactive plot helps identify significant trends and relationships in the data.

## Installation
To run the scripts and Shiny app, follow these steps:

1. Clone this repository:
   ```bash
   git clone https://github.com/Jchow2/Agricultural-Emissions-Analysis.git

2. Install the required R packages:
   ```bash
   install.packages(c("tidyverse", "glmnet", "shiny", "ggplot2"))

## Usage

3. Exploratory Data Analysis: Run the eda_agrico2.R script to perform exploratory data analysis on the datasets.
   ```bash
   source("eda_agrico2.R")

4. Elastic Regression: Run the elastic_regression_agrico2.R script to fit the elastic regression models and generate predictions.
   ```bash
   source("elastic_regression_agrico2.R")

6. Shiny App: Run the shinyapp.R script to launch the interactive Shiny app.
   ```bash
   shiny::runApp("shinyapp.R")

## Results
The results of the analysis are visualized in the Shiny app, which provides an interactive interface for exploring the relationship between energy use and agricultural emissions. Users can adjust input parameters to observe changes in predictions, helping identify significant trends and insights from the data.

## Contributing
Contributions are welcome! Please feel free to submit a Pull Request or open an issue to discuss any changes.

## License
This project is licensed under the MIT License. See the LICENSE file for more information.

## Reference
Database: World Development Indicators
- Kurukulasuriya, Pradeep & Rosenthal, Shane. "Climate Change and Agriculture: A Review of Impacts and Adaptations" The World Bank Environmental Department. June 2003.
- Mendelsohn, Robert. "CROSS-SECTIONAL ANALYSES OF CLIMATE CHANGE IMPACTS" World Bank Policy Research Working Paper 3350. June 2004.



