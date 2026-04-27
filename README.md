---
title: "USGS Earthquake Dashboard"
output: html_document
---

## USGS Earthquake Dashboard

This project uses earthquake data from the United States Geological Survey (USGS) to build an interactive dashboard and map. 

The goal is to explore global earthquake activity over the past decade and allow users to examine patterns in location, magnitude, and depth as well as evaluate risks local to their area.

The dashboard includes interactive filtering tools as well as multiple visualizations to help users better understand how earthquakes vary across time and geography.

---

## Features
The dashboard includes the following interactive components:

- City filtering tool to view earthquakes near a selected city  
- Year slider with animation to explore changes over time  
- Continent filter to isolate specific regions  

### Visualizations:
- Interactive map showing earthquake locations, colored by magnitude 
- Scatterplot of magnitude vs depth separated by low, high, and medium magnitudes 
- Histogram of earthquake depths
- Histogram of earthquake magnitudes  
- Table and bar chart showing the top 5 largest earthquakes based on user selection  
- Summary statistics based on user inputs  

---

## Data

All datasets used in this project are stored in the `data/` folder as CSV files.  
These datasets contain cleaned earthquake records sourced from the USGS and a datset of cities in the world. 
They are used directly in the Shiny application.

---

## Live Dashboard

You can access the deployed dashboard here to learn more about earthquakes:

https://m-liv.github.io/usgs-data-project/

---

## Running the App Locally

To run this application locally, follow these steps:

### 1. Clone the repository

```{bash}
git clone https://github.com/m-liv/usgs-data-project.git
```

### 2. Open the project in RStudio

Open the project folder in RStudio 

### 3. Install required packages 
```{r}
install.packages(c(
  "shiny",
  "dplyr",
  "ggplot2",
  "plotly",
  "sf",
  "maps",
  "scales",
  "stringr"
))
```
### 4. Run the application 

Open app.R and run:

```{r}
shiny::runApp()
```

