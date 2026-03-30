library(tidyverse)
library(lubridate)

# Load raw Netflix data -------------------------------------------------------
netflix <- readr::read_csv("data/netflix_titles.csv") %>%
  mutate(
    release_year = as.integer(release_year),
    date_added   = mdy(date_added),
    year_added   = year(date_added)
  )

# Load TMDb helper functions --------------------------------------------------
source("tmdb_api.R")

# Query TMDb for all titles (n_max = nrow(netflix) or Inf)
tmdb_info <- enrich_netflix_with_tmdb(netflix, n_max = nrow(netflix))

glimpse(tmdb_info)
mean(!is.na(tmdb_info$imdb_rating))

# Write TMDb results to cached CSV --------------------------------------------
dir.create("data", showWarnings = FALSE)
readr::write_csv(tmdb_info, "data/netflix_tmdb.csv")