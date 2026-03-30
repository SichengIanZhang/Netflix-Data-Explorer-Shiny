# tmdb_api.R

library(httr2)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)

# Helper: use a fallback value when NULL --------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

# Query a single title (movie / TV) -------------------------------------------
tmdb_search_one <- function(show_id, title, year, nf_type,
                            api_key = Sys.getenv("TMDB_API_KEY")) {
  if (!nzchar(api_key)) {
    stop("TMDB_API_KEY is not set", call. = FALSE)
  }
  
  is_movie <- nf_type == "Movie"
  endpoint <- if (is_movie) "search/movie" else "search/tv"
  url_base <- paste0("https://api.themoviedb.org/3/", endpoint)
  
  req <- request(url_base) |>
    req_url_query(
      api_key       = api_key,
      query         = title,
      language      = "en-US",
      include_adult = "true"
    ) |>
    req_timeout(10)
  
  # If a year is available, add it as a filter --------------------------------
  if (!is.na(year)) {
    year_param <- if (is_movie) "year" else "first_air_date_year"
    req <- req |> req_url_query(!!year_param := year)
  }
  
  resp <- req_perform(req)
  dat  <- resp_body_json(resp, simplifyVector = TRUE)
  
  # If TMDb returns an error (e.g., invalid key), it usually includes a status_message
  if (!is.null(dat$status_message)) {
    warning("TMDb API message: ", dat$status_message)
  }
  
  # If results is empty, return NULL immediately ------------------------------
  if (is.null(dat$results)) {
    return(NULL)
  }
  
  # Convert results to a tibble and take the first row as the “best match” 
  results_df <- as_tibble(dat$results)
  if (nrow(results_df) == 0) {
    return(NULL)
  }
  
  best <- results_df[1, ]
  
  tibble(
    show_id                 = show_id,
    tmdb_id                 = best$id,
    tmdb_title              = best$title  %||% best$name,
    tmdb_original_title     = best$original_title %||% best$original_name,
    tmdb_media_type         = if (is_movie) "movie" else "tv",
    tmdb_release_date       = best$release_date %||% best$first_air_date,
    imdb_rating             = suppressWarnings(as.numeric(best$vote_average)),
    imdb_votes              = suppressWarnings(as.numeric(best$vote_count)),
    tmdb_popularity         = suppressWarnings(as.numeric(best$popularity)),
    tmdb_original_language  = best$original_language,
    tmdb_adult              = isTRUE(best$adult)
  )
}

# Bulk-enrich Netflix data, returning a table with only show_id and TMDb fields
enrich_netflix_with_tmdb <- function(netflix, n_max = Inf) {
  
  netflix_sub <- netflix %>%
    filter(!is.na(title)) %>%
    arrange(desc(release_year)) %>%
    { if (is.finite(n_max)) slice_head(., n = n_max) else . }
  
  safe_fetch <- purrr::possibly(
    .f = function(show_id, title, year, nf_type) {
      Sys.sleep(0.25)  # Rate limiting: 4 requests per second, up to 40 requests within 10 seconds
      tmdb_search_one(show_id, title, year, nf_type)
    },
    otherwise = NULL,
    quiet = TRUE
  )
  
  res_list <- purrr::pmap(
    list(
      show_id = netflix_sub$show_id,
      title   = netflix_sub$title,
      year    = netflix_sub$release_year,
      nf_type = netflix_sub$type
    ),
    safe_fetch
  )
  
  res_list %>%
    purrr::compact() %>%
    bind_rows()
}

# Smart loader for Netflix data enriched with TMDb ----------------------------
load_enriched_netflix_tmdb <- function(netflix,
                                       csv_path = "data/netflix_tmdb.csv",
                                       n_max = Inf) {
  # If a cached CSV exists, simply left_join it (no API calls)
  if (file.exists(csv_path)) {
    tmdb <- readr::read_csv(csv_path, show_col_types = FALSE)
    netflix_enriched <- netflix %>%
      left_join(tmdb, by = "show_id")
    return(netflix_enriched)
  }
  
  # If there is no CSV but an API key is available, call TMDb and create one
  api_key <- Sys.getenv("TMDB_API_KEY", "")
  if (nzchar(api_key)) {
    message("No cached TMDb CSV found. Fetching data from TMDb ...")
    tmdb <- enrich_netflix_with_tmdb(netflix, n_max = n_max)
    
    dir.create(dirname(csv_path), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(tmdb, csv_path)
    
    netflix_enriched <- netflix %>%
      left_join(tmdb, by = "show_id")
    return(netflix_enriched)
  }
  
  # Fallback: when there is neither a CSV nor an API key, return the original Netflix data
  warning("No TMDb data available (no CSV, no TMDB_API_KEY). Returning original netflix.")
  netflix
}
