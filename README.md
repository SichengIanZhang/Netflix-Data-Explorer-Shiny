# Netflix Data Explorer (Shiny App)

## Overview
Netflix Data Explorer is an interactive Shiny dashboard for analyzing patterns in Netflix’s content catalog. The project combines the Netflix Titles dataset with TMDb metadata to explore trends in ratings, popularity, genres, languages, countries, and content types.

The app enables both high-level exploration and targeted discovery, allowing users to understand how content evolves over time and identify hidden gems within the catalog.

## Features

- Interactive Shiny dashboard with multiple analysis tabs
- Release trends by year and content type
- IMDb rating and popularity analysis
- "Discover" tab for identifying hidden gems and blockbusters
- Similar-title recommendation based on rating–popularity space
- Content rating distribution and trends over time
- Language and country-level insights
- Duration and season-length distributions
- Genre trends and composition analysis

## Repository Structure

```text
├── app.R
├── tmdb_api.R
├── scripts_tmdb.R
├── report.qmd
├── data/
│   ├── netflix_titles.csv
│   └── netflix_tmdb.csv
├── images/
├── .gitignore
├── project.Rproj
└── README.md
```

## Data Sources

- Netflix Titles Dataset  
  https://www.kaggle.com/datasets/shivamb/netflix-shows  

- TMDb (The Movie Database) API  
  https://www.themoviedb.org/settings/api  

## How to Run

1. Install required packages:

install.packages(c("shiny", "tidyverse", "lubridate", "plotly", "scales", "httr2"))

2. Run the app:

shiny::runApp("app.R")

## TMDb API (Optional)

The app runs using the cached dataset:

data/netflix_tmdb.csv

To rebuild the data, set a TMDB_API_KEY and run:

source("scripts_tmdb.R")

## Collaboration

This project was completed as a group project for STA 523 at Duke University.

Collaborators:
- Robbie Hao  
- Zixuan Lin
- Ruibo Song
- Xun Shao 

## My Contribution

- Contributed to model and analysis design across multiple perspectives  
- Worked on data preprocessing and feature engineering  
- Developed and refined interactive visualizations in the Shiny dashboard  
- Participated in evaluation and interpretation of key metrics such as ratings, popularity, and distribution patterns  
