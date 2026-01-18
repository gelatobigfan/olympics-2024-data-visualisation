# Olympics 2024 Data Visualisation

Live Interactive Demo  
https://clarapan.shinyapps.io/paris-2024-olympics-iv/

An interactive data visualisation application built with R Shiny to explore athlete participation and medal distributions in the Paris 2024 Olympic Games.

---

## Author

Clara Pan  
Master of Information Technology
University of Melbourne  

---

## Overview

This project presents an interactive, map-based visual analytics interface that allows users to explore Olympic data from both a global overview and country-level perspectives.

The application is designed following core information visualisation principles, enabling users to:
- Understand global athlete distribution
- Filter data by gender
- Drill down into detailed participation and medal patterns for individual countries

This project has been deployed as a live web application and is presented as a portfolio piece demonstrating data visualisation design, interactive analytics, and end-to-end delivery.

---

## Key Features

- Global choropleth map showing athlete counts by country  
- Gender-based filtering (Male / Female / All)  
- Country-level drill-down via interactive map selection  
- Coordinated multiple visual views, including:
  - Event participation pie chart
  - Athlete age distribution histogram
  - Medal timeline (Gold / Silver / Bronze)
  - Event-level medal heatmap
- Details-on-demand interaction through modal-based exploration

---

## Design Rationale

The visualisation follows an “overview first, zoom and filter, then details on demand” approach:

- The world map provides an immediate global overview
- Interactive filtering supports exploratory analysis
- Linked charts reveal contextual details once a country is selected
- A minimalist UI reduces cognitive load and supports non-technical users

Design decisions and interaction logic are documented in more detail in the design summary.

### Design Documentation

design/design-summary.pdf

---

## Data Source

Paris 2024 Olympic Summer Games Dataset  
Source: Kaggle  
https://www.kaggle.com/datasets/piterfm/paris-2024-olympic-summer-games

Data files used:
- athletes.csv
- medals.csv

---

## Tech Stack

- Language: R  
- Framework: Shiny  
- Visualisation: ggplot2, plotly, leaflet  
- Data Processing: tidyverse, lubridate  
- Spatial Data: sf, spData  
- Deployment: shinyapps.io (via rsconnect)
