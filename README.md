# World Happiness Analysis Dashboard

An interactive R Shiny dashboard for exploring global happiness trends using the World Happiness Report dataset (2006–2023). This application visualizes happiness scores, investigates correlations with socio-economic factors, and enables users to interactively analyze trends by country and continent, providing valuable insights into global well-being patterns[1][2].

---

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Demo](#demo)
- [Installation](#installation)
- [Usage](#usage)
- [Data Sources](#data-sources)
- [App Structure](#app-structure)
- [License](#license)

## Overview

This dashboard leverages the World Happiness Report dataset to present interactive visualizations and analyses of happiness trends across countries and continents from 2006 to 2023. Users can explore how happiness has evolved globally, identify the happiest and least happy countries, analyze the impact of various factors (GDP, social support, life expectancy, etc.), and visualize the relationships between these variables[1][2].

## Features

- **Global Happiness Trends:** Track average happiness scores over time worldwide[2].
- **Country Rankings:** View the top 10 happiest and least happy countries based on average scores[2].
- **Continental Analysis:** Explore happiness trends by continent and compare with country-level data[2].
- **Significant Changes:** Identify countries with the largest increases or decreases in happiness over time[2].
- **Correlation Matrix:** Analyze the relationships between happiness and contributing factors such as GDP, social support, and more[1].
- **Category Trends:** Compare happiness trends between the happiest countries and others[2].
- **GDP vs. Happiness:** Visualize the relationship between GDP per capita and happiness scores[1].
- **Interactive World Map:** Map happiness and related attributes for any selected year and variable[2].
- **Customizable Visualizations:** Filter by year, continent, and data attribute for tailored exploration[2].

---

## Demo

To run the application locally, follow the instructions in the [Installation](#installation) and [Usage](#usage) sections below.

---

## Installation

### Prerequisites

- R (version 4.0 or higher recommended)
- R packages: `shiny`, `shinydashboard`, `ggplot2`, `dplyr`, `plotly`, `readr`, `tidyr`, `DT`, `iconv`

### Steps

1. **Clone this repository:**
   ```bash
   git clone https://github.com/ProfOzpin/World-Happiness-Visualisation.git
   cd world-happiness-dashboard
   ```
2. **Install required R packages:**
   ```r
   install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "plotly", "readr", "tidyr", "DT"))
   ```
3. **Download the dataset:**
   - Download the [World Happiness Report 2024 dataset from Kaggle](https://www.kaggle.com/datasets/jainaru/world-happiness-report-2024-yearly-updated?resource=download&select=World-happiness-report-updated_2024.csv)
   - Place the CSV file in the project directory.

## Usage

1. Open `app.R` (or the relevant R script) in RStudio or your preferred R environment.
2. Run the Shiny app:
   ```r
   shiny::runApp()
   ```
3. The dashboard will launch in your default web browser. Use the sidebar to navigate between pages and tabs for different analyses[3][4].

## Data Sources

- **World Happiness Report (2006–2023):** [Kaggle Dataset](https://www.kaggle.com/datasets/jainaru/world-happiness-report-2024-yearly-updated?resource=download&select=World-happiness-report-updated_2024.csv)[1]
- **Continents for Each Country:** [Kaggle Dataset](https://www.kaggle.com/datasets/hserdaraltan/countries-by-continent)[1]

## App Structure

- **Home:** Introduction, project credits, and data sources.
- **Plots:** Multiple interactive tabs:
  - Global Happiness Trends
  - Happiest & Least Happy Countries
  - Significant Happiness Changes
  - Happiness by Continent
  - Country and Continent Happiness Trends
  - Correlation Matrix
  - Happiness Trends by Category
  - GDP vs Happiness
  - World Map (selectable by year and attribute)

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.
