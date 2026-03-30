# load libraries --------------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(scales)

# 1. Read raw Netflix data ----------------------------------------------------

netflix_raw = readr::read_csv("data/netflix_titles.csv")

# 2. Basic preprocessing ------------------------------------------------------

netflix = netflix_raw %>%
  mutate(
    release_year = as.integer(release_year),
    date_added   = mdy(date_added),
    year_added   = year(date_added),
    
    # Movie duration (minutes)
    duration_min = if_else(
      type == "Movie",
      readr::parse_number(duration),
      # more forgiving than as.numeric()
      NA_real_
    ),
    
    # Number of seasons for TV shows
    seasons = if_else(type == "TV Show", readr::parse_number(duration), NA_real_)
  )

# 3. Load TMDb metadata and join ----------------------------------------------

tmdb_info = readr::read_csv("data/netflix_tmdb.csv", show_col_types = FALSE)

netflix = netflix %>%
  left_join(
    tmdb_info %>%
      select(
        show_id,
        tmdb_id,
        tmdb_title,
        tmdb_original_title,
        tmdb_media_type,
        tmdb_release_date,
        imdb_rating,
        imdb_votes,
        tmdb_popularity,
        tmdb_original_language,
        tmdb_adult
      ),
    by = "show_id"
  )

# 4. Map language codes to human-readable labels ------------------------------

language_lookup = tibble(
  tmdb_original_language = c(
    "af",
    "ak",
    "ar",
    "as",
    "bn",
    "ca",
    "cn",
    "cs",
    "da",
    "de",
    "el",
    "en",
    "es",
    "eu",
    "fa",
    "fi",
    "fr",
    "gl",
    "gu",
    "ha",
    "he",
    "hi",
    "hr",
    "hu",
    "id",
    "is",
    "it",
    "ja",
    "ka",
    "km",
    "kn",
    "ko",
    "ks",
    "ku",
    "la",
    "lb",
    "lt",
    "ml",
    "mr",
    "ms",
    "nb",
    "ne",
    "nl",
    "no",
    "pa",
    "pl",
    "pt",
    "qu",
    "ro",
    "ru",
    "sn",
    "sr",
    "sv",
    "ta",
    "te",
    "th",
    "tl",
    "tr",
    "uk",
    "ur",
    "vi",
    "wo",
    "xh",
    "xx",
    "yi",
    "yo",
    "zh",
    "zu"
  ),
  language_name = c(
    "Afrikaans",
    "Akan",
    "Arabic",
    "Assamese",
    "Bengali",
    "Catalan",
    "Chinese",
    "Czech",
    "Danish",
    "German",
    "Greek",
    "English",
    "Spanish",
    "Basque",
    "Persian (Farsi)",
    "Finnish",
    "French",
    "Galician",
    "Gujarati",
    "Hausa",
    "Hebrew",
    "Hindi",
    "Croatian",
    "Hungarian",
    "Indonesian",
    "Icelandic",
    "Italian",
    "Japanese",
    "Georgian",
    "Khmer",
    "Kannada",
    "Korean",
    "Kashmiri",
    "Kurdish",
    "Latin",
    "Luxembourgish",
    "Lithuanian",
    "Malayalam",
    "Marathi",
    "Malay",
    "Norwegian Bokmål",
    "Nepali",
    "Dutch",
    "Norwegian",
    "Punjabi",
    "Polish",
    "Portuguese",
    "Quechua",
    "Romanian",
    "Russian",
    "Shona",
    "Serbian",
    "Swedish",
    "Tamil",
    "Telugu",
    "Thai",
    "Tagalog",
    "Turkish",
    "Ukrainian",
    "Urdu",
    "Vietnamese",
    "Wolof",
    "Xhosa",
    "Unknown / Other",
    "Yiddish",
    "Yoruba",
    "Chinese",
    "Zulu"
  )
)

netflix = netflix %>%
  mutate(tmdb_original_language = as.character(tmdb_original_language)) %>%
  left_join(language_lookup, by = "tmdb_original_language") %>%
  mutate(
    language_label = case_when(
      !is.na(language_name) ~ paste0(language_name, " (", tmdb_original_language, ")"),
      tmdb_original_language == "xx" ~ "Unknown / Other",
      TRUE                           ~ tmdb_original_language
    )
  )

# Choices for language inputs (labels are "English (en)" etc., values are codes)
language_choices = netflix %>%
  filter(!is.na(tmdb_original_language)) %>%
  distinct(tmdb_original_language, language_label) %>%
  arrange(language_label) %>%
  {
    setNames(.$tmdb_original_language, .$language_label)
  }

# 5. Scores data: compute votes/log-votes and composite scores ----------------

netflix_scores = netflix %>%
  mutate(
    imdb_votes = as.numeric(imdb_votes),
    log_votes  = if_else(!is.na(imdb_votes) & imdb_votes > 0, log10(imdb_votes), NA_real_)
  ) %>%
  filter(!is.na(imdb_rating), !is.na(log_votes)) %>%
  mutate(
    # global z-scores across the catalog
    z_rating   = as.numeric(scale(imdb_rating)),
    z_logvotes = as.numeric(scale(log_votes)),
    # composite scores
    blockbuster_score = z_rating + z_logvotes,
    # high rating + high popularity
    hidden_gem_score  = z_rating - z_logvotes   # high rating + relatively low popularity
  )

# 6. Content rating groupings --------------------------------------------------

netflix = netflix %>%
  mutate(
    rating_group = case_when(
      rating %in% c("TV-Y", "TV-Y7", "TV-Y7-FV", "TV-G", "G") ~ "Kids",
      rating %in% c("TV-PG", "PG", "PG-13", "TV-14")          ~ "Family / Teens",
      rating %in% c("TV-MA", "R", "NC-17")                   ~ "Mature",
      rating %in% c("NR", "UR")                              ~ "Unrated / Other",
      is.na(rating)                                          ~ NA_character_,
      TRUE                                                   ~ "Unrated / Other"
    )
  )

# 7. Long-format genre and country tables -------------------------------------

genres_long = netflix %>%
  separate_rows(listed_in, sep = ", ") %>%
  mutate(listed_in = str_trim(listed_in))

countries_long = netflix %>%
  filter(!is.na(country)) %>%
  separate_rows(country, sep = ", ") %>%
  mutate(country = str_trim(country))

year_min = min(netflix$release_year, na.rm = TRUE)
year_max = max(netflix$release_year, na.rm = TRUE)

# UI --------------------------------------------------------------------------

ui = tagList(
  tags$head(
    tags$link(rel  = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Bebas+Neue&display=swap"),
    tags$style(
      HTML(
        "
      /* Global body background: light grey + dark text */
      body {
        background-color: #f5f5f5;
        color: #222222;
      }

      /* Navbar background: black + Netflix red */
      .navbar-default {
        background-color: #000000;
        border-color: #000000;
      }

      .navbar-default .navbar-brand {
        font-family: 'Bebas Neue', sans-serif !important;
        font-size: 28px !important;
        color: #e50914 !important;
        letter-spacing: 2px;
      }

      .navbar-default .navbar-nav > li > a {
        color: #f5f5f5 !important;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:focus,
      .navbar-default .navbar-nav > .active > a:hover {
        background-color: #e50914 !important;
        color: #ffffff !important;
      }

      /* Sidebar panel (left controls) */
      .well {
        background-color: #ffffff;
        border-color: #dddddd;
        color: #222222;
      }

      /* Main content cards */
      .content-card {
        background-color: #ffffff;
        border-radius: 8px;
        padding: 20px 25px;
        border: 1px solid #dddddd;
        box-shadow: 0 0 8px rgba(0,0,0,0.08);
        margin-bottom: 25px;
      }

      h3 {
        color: #111111;
        font-weight: 600;
        margin-top: 0;
        margin-bottom: 15px;
      }

      /* Slider: use Netflix red */
      .irs--shiny .irs-bar {
        background-color: #e50914 !important;
        border-color: #e50914 !important;
      }
      .irs--shiny .irs-bar-edge {
        background-color: #e50914 !important;
        border-color: #e50914 !important;
      }
      .irs--shiny .irs-handle {
        background-color: #e50914 !important;
        border-color: #e50914 !important;
      }
      .irs--shiny .irs-from,
      .irs--shiny .irs-to,
      .irs--shiny .irs-single {
        background-color: #e50914 !important;
        border-color: #e50914 !important;
        color: white !important;
      }
      .irs--shiny .irs-from:before,
      .irs--shiny .irs-to:before,
      .irs--shiny .irs-single:before {
        border-top-color: #e50914 !important;
      }

      .checkbox input[type='checkbox']:checked + label::after {
        color: white !important;
      }
    "
      )
    )
  ),
  
  navbarPage(
    title = "Netflix Data Explorer",
    
    # 1. Overview tab ---------------------------------------------------------
    tabPanel("Overview", sidebarLayout(
      sidebarPanel(
        sliderInput(
          "year_range",
          "Release year:",
          min   = year_min,
          max   = year_max,
          value = c(2000, year_max),
          step  = 1,
          sep   = ""
        ),
        checkboxGroupInput(
          "type_filter",
          "Type:",
          choices  = sort(unique(netflix$type)),
          selected = unique(netflix$type)
        )
      ),
      mainPanel(
        div(
          class = "content-card",
          h3("Titles released by year and type"),
          p(
            "This view shows how many movies and TV shows were released in each year
             in the Netflix catalog. Use the controls on the left to restrict the
             range of release years and to focus on movies, TV shows, or both."
          ),
          plotlyOutput("titles_by_year", height = "400px"),
          br(),
          h3("Overall count: Movies vs TV Shows"),
          p(
            "This bar chart compares the total number of movies and TV shows under
             the currently selected filters."
          ),
          plotlyOutput("type_count", height = "300px")
        )
      )
    )),
    
    # 2. Audience scores tab --------------------------------------------------
    tabPanel("Audience scores", sidebarLayout(
      sidebarPanel(
        sliderInput(
          "score_year_range",
          "Release year:",
          min   = year_min,
          max   = year_max,
          value = c(2000, year_max),
          step  = 1,
          sep   = ""
        ),
        sliderInput(
          "imdb_range",
          "IMDb rating:",
          min   = 0,
          max   = 10,
          value = c(4, 9),
          step  = 0.1
        ),
        checkboxGroupInput(
          "score_type_filter",
          "Type:",
          choices  = sort(unique(netflix$type)),
          selected = unique(netflix$type)
        )
      ),
      mainPanel(
        div(
          class = "content-card",
          h3("Average IMDb rating by year"),
          p(
            "This plot shows how average IMDb scores evolve over time,
             under the filters on the left."
          ),
          plotlyOutput("imdb_by_year", height = "400px")
        ),
        br(),
        div(
          class = "content-card",
          h3("Rating vs popularity"),
          p(
            "Each point is a title. The x-axis is IMDb rating and the y-axis
             is the number of IMDb votes (on a log scale), giving a sense of
             both quality and popularity."
          ),
          plotlyOutput("rating_votes_scatter", height = "400px")
        )
      )
    )),
    
    # 3. Discover tab ---------------------------------------------------------
    tabPanel("Discover", sidebarLayout(
      sidebarPanel(
        sliderInput(
          "disc_year_range",
          "Release year:",
          min   = year_min,
          max   = year_max,
          value = c(2000, year_max),
          step  = 1,
          sep   = ""
        ),
        checkboxGroupInput(
          "disc_type_filter",
          "Include types:",
          choices  = sort(unique(netflix$type)),
          selected = unique(netflix$type)
        ),
        sliderInput(
          "disc_rating_range",
          "IMDb rating:",
          min   = 0,
          max   = 10,
          value = c(6.5, 10),
          step  = 0.1
        ),
        numericInput(
          "disc_min_votes",
          "Minimum IMDb votes:",
          value = 100,
          min   = 0,
          step  = 50
        ),
        selectizeInput(
          "disc_lang_filter",
          "Original languages (optional):",
          choices  = language_choices,
          # Labels show “English (en)”, values are language codes
          selected = NULL,
          multiple = TRUE,
          options  = list(placeholder = "Leave empty for all languages", plugins     = list("remove_button"))
        ),
        selectizeInput(
          "disc_genre_filter",
          "Genres (optional):",
          choices  = sort(unique(genres_long$listed_in)),
          selected = NULL,
          multiple = TRUE,
          options  = list(placeholder = "Leave empty for all genres", plugins     = list("remove_button"))
        )
      ),
      mainPanel(
        div(
          class = "content-card",
          h3("Quality vs popularity"),
          p(
            "Each point is a title with IMDb rating on the vertical axis and the log10 of IMDb votes on the horizontal axis.
               The dashed lines mark the median rating and median number of votes under the current filters. Colors indicate
               whether a title is a blockbuster, a hidden gem, or something in between."
          ),
          plotlyOutput("disc_scatter", height = "400px")
        ),
        br(),
        fluidRow(column(
          width = 6, div(
            class = "content-card",
            h4("Top hidden gems"),
            p(
              "High rating but relatively few votes, ranked by the hidden-gem score."
            ),
            div(
              style = "overflow-x: auto;",
              tableOutput("disc_table_gems")
            )
          )
        ), column(
          width = 6, div(
            class = "content-card",
            h4("Top blockbusters"),
            p("High rating and many votes, ranked by the blockbuster score."),
            div(
              style = "overflow-x: auto;",
              tableOutput("disc_table_blocks")
            )
          )
        ))
      )
    )),
    
    # 3b. Similar titles tab --------------------------------------------------
    tabPanel("Similar titles", sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "sim_base_title",
          "Pick a title:",
          choices  = sort(unique(netflix_scores$title)),
          selected = NULL,
          options  = list(
            placeholder = "Start typing a title name",
            maxOptions  = 50
          )
        ),
        sliderInput(
          "sim_k",
          "Number of similar titles to show:",
          min   = 3,
          max   = 20,
          value = 10,
          step  = 1
        )
      ),
      mainPanel(
        div(
          class = "content-card",
          h3("Titles with similar score & popularity"),
          p(
            "We measure similarity using IMDb rating and log10 of IMDb vote count. ",
            "Points that are close together have similar audience scores and popularity."
          ),
          plotlyOutput("sim_scatter", height = "400px")
        ),
        br(),
        div(
          class = "content-card",
          h4("Most similar titles"),
          p("These are the titles closest to the selected one in score–popularity space."),
          div(
            style = "overflow-x: auto;",
            tableOutput("sim_table")
          )
        )
      )
    )),
    
    # 4. Content ratings tab --------------------------------------------------
    tabPanel("Content ratings", sidebarLayout(
      sidebarPanel(
        sliderInput(
          "rating_year_range",
          "Release year:",
          min   = year_min,
          max   = year_max,
          value = c(2000, year_max),
          step  = 1,
          sep   = ""
        ),
        checkboxGroupInput(
          "rating_type_filter",
          "Include types:",
          choices  = sort(unique(netflix$type)),
          selected = unique(netflix$type)
        )
      ),
      mainPanel(
        div(
          class = "content-card",
          h3("Distribution of content ratings"),
          p(
            "This plot summarizes how Netflix titles are distributed across different
             content ratings (e.g., TV-MA, TV-14, PG) under the filters on the left."
          ),
          plotlyOutput("rating_dist", height = "400px")
        ),
        br(),
        # Second panel: Kids / Family / Mature share over time
        div(
          class = "content-card",
          h3("Content rating mix over time"),
          p(
            "This stacked area chart shows how the share of kids, family/teen, and mature
             titles has evolved over release years, under the filters on the left."
          ),
          plotlyOutput("rating_time", height = "400px")
        ),
        br(),
        div(
          class = "content-card",
          h4("What do these rating labels mean?"),
          tags$ul(
            tags$li(
              tags$b("TV-MA: "),
              "Mature Audience Only. Content intended for adults; may contain strong language, violence, or sexual content."
            ),
            tags$li(
              tags$b("TV-14: "),
              "Parents Strongly Cautioned. May be unsuitable for children under 14."
            ),
            tags$li(
              tags$b("TV-PG: "),
              "Parental Guidance Suggested. Some material may not be suitable for children."
            ),
            tags$li(
              tags$b("TV-Y7 / TV-Y7-FV: "),
              "Designed for children age 7 and above (TV-Y7-FV includes fantasy violence)."
            ),
            tags$li(tags$b("TV-Y: "), "Designed for very young children."),
            tags$li(tags$b("TV-G: "), "General audience; suitable for all ages."),
            tags$li(tags$b("G: "), "General audiences; all ages admitted."),
            tags$li(
              tags$b("PG: "),
              "Parental Guidance Suggested; some material may not be suitable for children."
            ),
            tags$li(
              tags$b("PG-13: "),
              "Parents Strongly Cautioned; some material may be inappropriate for children under 13."
            ),
            tags$li(
              tags$b("R: "),
              "Restricted; under 17 requires accompanying parent or adult guardian."
            ),
            tags$li(tags$b("NC-17: "), "No one 17 and under admitted."),
            tags$li(
              tags$b("NR / UR: "),
              "Not Rated / Unrated; the title does not have an official rating."
            )
          )
        )
      )
    )),
    
    # 5. Languages tab --------------------------------------------------------
    tabPanel("Languages", sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "lang_type_filter",
          "Include types:",
          choices  = sort(unique(netflix$type)),
          selected = unique(netflix$type)
        ),
        numericInput(
          "lang_top_n",
          "Number of top languages:",
          value = 10,
          min   = 5,
          max   = 20,
          step  = 1
        )
      ),
      mainPanel(
        div(
          class = "content-card",
          h3("Language summary"),
          textOutput("lang_summary_text")
        ),
        br(),
        div(
          class = "content-card",
          h3("Top original languages"),
          p(
            "Top original languages by title count. All other languages are lumped into 'Other'."
          ),
          plotlyOutput("lang_count_plot", height = "350px")
        ),
        br(),
        div(
          class = "content-card",
          h3("Average IMDb rating by language"),
          p(
            "Average IMDb rating within each original language. The dashed line shows the overall mean."
          ),
          plotlyOutput("lang_rating_plot", height = "350px")
        ),
        br(),
        div(
          class = "content-card",
          h3("TMDb popularity vs IMDb votes"),
          p(
            "Each point is a title. Higher on both axes = more widely rated and more popular on TMDb."
          ),
          plotOutput("popularity_votes_plot", height = "350px")
        )
      )
    )),
    
    # 6. Countries tab --------------------------------------------------------
    tabPanel("Countries", sidebarLayout(
      sidebarPanel(
        sliderInput(
          "country_year_range",
          "Release year:",
          min   = year_min,
          max   = year_max,
          value = c(2000, year_max),
          step  = 1,
          sep   = ""
        ),
        checkboxGroupInput(
          "country_type_filter",
          "Include types:",
          choices  = sort(unique(netflix$type)),
          selected = unique(netflix$type)
        ),
        # Optional: restrict to titles that fall into selected genres
        selectizeInput(
          "country_genre_filter",
          "Genres (optional):",
          choices  = sort(unique(genres_long$listed_in)),
          selected = NULL,
          multiple = TRUE,
          options  = list(placeholder = "Leave empty for all genres", plugins     = list("remove_button"))
        ),
        # Filter out countries with very few titles (for more stable summaries)
        numericInput(
          "country_min_titles",
          "Minimum number of titles per country:",
          value = 5,
          min   = 1,
          max   = 200,
          step  = 1
        ),
        selectInput(
          "country_metric",
          "Metric to show on the map:",
          choices = c(
            "Number of titles"        = "n_titles",
            "Average IMDb rating"     = "avg_rating",
            "Median hidden-gem score" = "gem_score"
          ),
          selected = "n_titles"
        )
      ),
      mainPanel(
        div(
          class = "content-card",
          h3("Where in the world are these titles from?"),
          p(
            "This choropleth map aggregates Netflix titles by production country under the current filters.
             You can switch between the number of titles, their average IMDb rating,
             or how 'hidden-gem-ish' they are. Countries with very few titles can be filtered out
             using the minimum-title control on the left."
          ),
          plotlyOutput("country_map", height = "450px")
        ),
        br(),
        div(
          class = "content-card",
          h3("Country ranking"),
          p(
            "Top countries under the current filters (after applying the minimum-title threshold)."
          ),
          tableOutput("country_table")
        )
      )
    )),
    
    # 7. Durations tab --------------------------------------------------------
    tabPanel("Durations", sidebarLayout(
      sidebarPanel(
        sliderInput(
          "max_movie_duration",
          "Maximum movie duration (minutes):",
          min   = 60,
          max   = 300,
          value = 200
        ),
        sliderInput(
          "max_seasons",
          "Maximum number of seasons:",
          min   = 1,
          max   = 15,
          value = 8
        )
      ), mainPanel(
        div(
          class = "content-card",
          h3("Distribution of movie durations"),
          p(
            "This histogram shows the distribution of movie runtimes (in minutes).
             You can use the slider on the left to exclude extremely long movies
             and focus on a more typical range."
          ),
          plotlyOutput("movie_duration_hist"),
          br(),
          h3("Distribution of TV show seasons"),
          p(
            "This histogram shows how many seasons TV shows typically have in the
             Netflix catalog. The maximum number of seasons included can be adjusted
             using the slider on the left."
          ),
          plotlyOutput("season_hist")
        )
      )
    )),
    
    # 8. Genres tab -----------------------------------------------------------
    tabPanel("Genres", sidebarLayout(
      sidebarPanel(
        sliderInput(
          "genre_year_range",
          "Release year:",
          min   = year_min,
          max   = year_max,
          value = c(2000, year_max),
          step  = 1,
          sep   = ""
        ),
        checkboxGroupInput(
          "genre_type_filter",
          "Include types:",
          choices  = sort(unique(netflix$type)),
          selected = unique(netflix$type)
        ),
        selectizeInput(
          "genre_focus",
          "Genres to highlight (optional):",
          choices  = sort(unique(genres_long$listed_in)),
          selected = NULL,
          # Default: if nothing is selected, we automatically use top genres
          multiple = TRUE,
          options  = list(placeholder = "Leave empty to auto-select top genres", plugins     = list("remove_button"))
        )
      ),
      mainPanel(
        div(
          class = "content-card",
          h3("Genre popularity over time"),
          p(
            "This plot shows how often selected genres appear in Netflix titles
             over time, under the filters on the left."
          ),
          plotlyOutput("genre_trend", height = "400px")
        ),
        br(),
        div(
          class = "content-card",
          h3("Genre mix by type"),
          p(
            "This plot compares how genres are distributed between movies and TV shows
             within the selected year range."
          ),
          plotlyOutput("genre_type_share", height = "400px")
        )
      )
    ))
  )
)

# ggplot theme ----------------------------------------------------------------

netflix_theme = function() {
  theme_minimal(base_size = 16) +
    theme(
      plot.background   = element_rect(fill = "#ffffff", color = NA),
      panel.background  = element_rect(fill = "#ffffff", color = NA),
      panel.grid.major  = element_line(color = "#dddddd"),
      panel.grid.minor  = element_line(color = "#eeeeee"),
      text              = element_text(color = "#222222"),
      axis.title        = element_text(color = "#222222", face = "bold"),
      axis.text         = element_text(color = "#333333"),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      legend.key        = element_rect(fill = "#ffffff", color = NA),
      legend.text       = element_text(color = "#222222"),
      legend.title      = element_text(color = "#222222", face = "bold"),
      plot.title        = element_text(
        color = "#111111",
        face  = "bold",
        size  = 20
      ),
      plot.margin       = margin(10, 10, 10, 10)
    )
}

# SERVER ----------------------------------------------------------------------

server = function(input, output, session) {
  # Overview tab: filtered data by release year and type ----------------------
  filtered_data = reactive({
    req(input$year_range, input$type_filter)
    
    netflix %>%
      filter(
        !is.na(release_year),
        release_year >= input$year_range[1],
        release_year <= input$year_range[2],
        type %in% input$type_filter
      )
  })
  
  # Titles by year and type
  output$titles_by_year = renderPlotly({
    df = filtered_data() %>%
      count(release_year, type) %>%
      arrange(type, release_year)
    
    validate(need(nrow(df) > 0, "No titles under the current filters."))
    
    p = ggplot(df,
               aes(
                 x     = release_year,
                 y     = n,
                 color = type,
                 group = type,
                 text  = paste("Year:", release_year, "<br>Type:", type, "<br>Count:", n)
               )) +
      geom_line(linewidth = 0.6) +
      geom_point(size = 1.5) +
      scale_color_manual(values = c("Movie" = "#e50914", "TV Show" = "#111111")) +
      labs(x = "Release Year", y = "Number of Titles", color = "Type") +
      netflix_theme() +
      theme(legend.position = "top")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Overall count by type
  output$type_count = renderPlotly({
    df = filtered_data() %>%
      count(type)
    
    validate(need(nrow(df) > 0, "No titles under the current filters."))
    
    p = ggplot(df, aes(
      x    = type,
      y    = n,
      fill = type,
      text = paste("Type:", type, "<br>Count:", n)
    )) +
      geom_col(width = 0.7,
               color = "black",
               linewidth = 0.4) +
      scale_fill_manual(values = c("Movie" = "#db0000", "TV Show" = "#000000")) +
      labs(x = "Type", y = "Count") +
      netflix_theme() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Content ratings tab: distribution of ratings ------------------------------
  output$rating_dist = renderPlotly({
    req(input$rating_type_filter, input$rating_year_range)
    
    df = netflix %>%
      filter(
        type %in% input$rating_type_filter,!is.na(rating),!is.na(release_year),
        release_year >= input$rating_year_range[1],
        release_year <= input$rating_year_range[2]
      ) %>%
      count(rating, sort = TRUE)
    
    validate(need(nrow(df) > 0, "No titles with content ratings under the current filters."))
    
    cols = c(
      "#e41a1c",
      "#377eb8",
      "#4daf4a",
      "#984ea3",
      "#ff7f00",
      "#ffff33",
      "#a65628",
      "#f781bf",
      "#1b9e77",
      "#d95f02",
      "#7570b3",
      "#e7298a",
      "#66a61e",
      "#e6ab02",
      "#a6761d",
      "#666666",
      "#1f78b4",
      "#b2df8a",
      "#fb9a99",
      "#cab2d6"
    )
    pal = setNames(cols, sort(unique(df$rating)))
    
    p = ggplot(df, aes(
      x    = reorder(rating, n),
      y    = n,
      fill = rating,
      text = paste("Rating:", rating, "<br>Count:", n)
    )) +
      geom_col(width = 0.7,
               color = "black",
               linewidth = 0.3) +
      coord_flip(clip = "off") +
      scale_fill_manual(values = pal) +
      geom_text(
        data = df %>% filter(n < 5),
        aes(
          x = rating,
          y = max(n) * 0.3,
          label = n,
          color = rating
        ),
        size = 4,
        fontface = "bold"
      ) +
      scale_color_manual(values = pal, guide = "none") +
      labs(x = "Rating", y = "Count") +
      netflix_theme() +
      theme(legend.position = "none",
            plot.margin     = margin(20, 40, 20, 20))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Content ratings tab: rating-group mix over time ---------------------------
  output$rating_time = renderPlotly({
    req(input$rating_type_filter, input$rating_year_range)
    
    # Filter according to sidebar controls
    df = netflix %>%
      filter(
        !is.na(release_year),
        release_year >= input$rating_year_range[1],
        release_year <= input$rating_year_range[2],
        type %in% input$rating_type_filter,!is.na(rating_group)
      ) %>%
      # Fix the rating-group order for consistent plotting
      mutate(rating_group = factor(
        rating_group,
        levels = c("Kids", "Family / Teens", "Mature", "Unrated / Other")
      )) %>%
      # Count titles per year × rating_group
      count(release_year, rating_group, name = "n_titles") %>%
      arrange(release_year, rating_group) %>%
      group_by(release_year) %>%
      mutate(total = sum(n_titles), share = n_titles / total) %>%
      ungroup()
    
    # If nothing is left after filtering, show a message instead of an empty plot
    validate(need(nrow(df) > 0, "No titles under the current filters."))
    
    p = ggplot(
      df,
      aes(
        x     = release_year,
        y     = share,
        fill  = rating_group,
        group = rating_group,
        text  = paste0(
          "Year: ",
          release_year,
          "<br>Group: ",
          rating_group,
          "<br>Share: ",
          scales::percent(share, accuracy = 0.1)
        )
      )
    ) +
      geom_area(position = "stack", alpha = 0.8) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x    = "Release year", y    = "Share of titles", fill = "Content rating group") +
      netflix_theme() +
      theme(legend.position = "top")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Durations tab: movie runtime histogram ------------------------------------
  output$movie_duration_hist = renderPlotly({
    movies = netflix %>%
      filter(type == "Movie",!is.na(duration_min),
             duration_min <= input$max_movie_duration)
    
    # Users will receive a notification that no movies meet the criteria.
    validate(need(nrow(movies) > 0, "No movies under the current filters."))
    
    
    bins      = 30
    movie_cut = cut(movies$duration_min, breaks = bins)
    
    # Aggregate counts per bin and compute bin midpoints for plotting
    df_plot = movies %>%
      mutate(bin = movie_cut) %>%
      count(bin) %>%
      mutate(bin_mid =
               as.numeric(sub("\\((.+),(.+)\\]", "\\1", bin)) +
               (as.numeric(sub(
                 "\\((.+),(.+)\\]", "\\2", bin
               )) -
                 as.numeric(sub(
                   "\\((.+),(.+)\\]", "\\1", bin
                 ))) / 2)
    
    p = ggplot(df_plot, aes(
      x    = bin_mid,
      y    = n,
      text = paste("Duration bin:", bin, "<br>Count:", n)
    )) +
      geom_col(fill = "#db0000", color = "black") +
      labs(x = "Duration (minutes)", y = "Frequency") +
      netflix_theme()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Durations tab: number of seasons histogram --------------------------------
  output$season_hist = renderPlotly({
    shows = netflix %>%
      filter(type == "TV Show",!is.na(seasons),
             seasons <= input$max_seasons)
    
    # Provide a note when there is empty data
    validate(need(nrow(shows) > 0, "No TV shows under the current filters."))
    
    p = ggplot(shows, aes(x    = seasons, text = paste("Seasons:", seasons))) +
      geom_histogram(
        binwidth = 1,
        boundary = 0.5,
        closed   = "right",
        fill     = "#db0000",
        color    = "black"
      ) +
      scale_x_continuous(breaks = 1:input$max_seasons) +
      labs(x = "Number of seasons", y = "Frequency") +
      netflix_theme()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Scores tab: filtered data for IMDb analysis -------------------------------
  score_data = reactive({
    req(input$score_year_range,
        input$imdb_range,
        input$score_type_filter)
    
    netflix %>%
      filter(
        !is.na(imdb_rating),
        release_year >= input$score_year_range[1],
        release_year <= input$score_year_range[2],
        imdb_rating  >= input$imdb_range[1],
        imdb_rating  <= input$imdb_range[2],
        type %in% input$score_type_filter
      )
  })
  
  # Scores tab: average rating over time --------------------------------------
  output$imdb_by_year = renderPlotly({
    df = score_data() %>%
      group_by(release_year, type) %>%
      summarise(
        mean_rating = mean(imdb_rating, na.rm = TRUE),
        n           = n(),
        .groups     = "drop"
      )
    
    validate(need(nrow(df) > 0, "No titles with IMDb ratings under the current filters."))
    
    p = ggplot(df,
               aes(
                 x     = release_year,
                 y     = mean_rating,
                 color = type,
                 group = type,
                 text  = paste(
                   "Year:",
                   release_year,
                   "<br>Type:",
                   type,
                   "<br>Avg IMDb:",
                   round(mean_rating, 2),
                   "<br>N titles:",
                   n
                 )
               )) +
      geom_line(linewidth = 0.6) +
      geom_point(size = 1.8) +
      scale_y_continuous(limits = c(0, 10)) +
      labs(x = "Release year", y = "Average IMDb rating", color = "Type") +
      netflix_theme() +
      theme(legend.position = "top")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Scores tab: rating vs number of votes -------------------------------------
  output$rating_votes_scatter = renderPlotly({
    df = score_data() %>%
      filter(!is.na(imdb_votes), imdb_votes > 0)
    
    validate(need(nrow(df) > 0, "No titles with IMDb votes under the current filters."))
    
    p = ggplot(df,
               aes(
                 x     = imdb_rating,
                 y     = imdb_votes,
                 color = type,
                 text  = paste(
                   "Title:",
                   title,
                   "<br>Type:",
                   type,
                   "<br>Year:",
                   release_year,
                   "<br>IMDb rating:",
                   imdb_rating,
                   "<br>Votes:",
                   imdb_votes
                 )
               )) +
      geom_point(alpha = 0.7) +
      scale_y_log10() +
      labs(x    = "IMDb rating", y    = "Number of votes (log10 scale)", color = "Type") +
      netflix_theme() +
      theme(legend.position = "top")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Similar titles tab --------------------------------------------------------
  
  # Base data for similarity: use the scored dataset
  similarity_data = reactive({
    netflix_scores %>%
      filter(
        !is.na(z_rating),
        !is.na(z_logvotes)
      )
  })
  
  # Compute nearest neighbours to the chosen title
  sim_results = reactive({
    req(input$sim_base_title, input$sim_k)
    
    df = similarity_data()
    
    base_row = df %>%
      filter(title == input$sim_base_title) %>%
      slice(1)
    
    validate(
      need(nrow(base_row) > 0,
           "Could not find the selected title in the scored dataset.")
    )
    
    df %>%
      mutate(
        dist2 = (z_rating   - base_row$z_rating[1])^2 +
          (z_logvotes - base_row$z_logvotes[1])^2
      ) %>%
      filter(title != base_row$title[1]) %>%
      arrange(dist2) %>%
      mutate(rank = dplyr::row_number()) %>%
      select(
        rank,
        title,
        type,
        release_year,
        imdb_rating,
        imdb_votes,
        blockbuster_score,
        hidden_gem_score,
        dist2
      ) %>%
      head(input$sim_k)
  })
  
  # Scatter plot: show all titles, highlight selected + neighbours
  output$sim_scatter = renderPlotly({
    req(input$sim_base_title)
    
    df = similarity_data()
    
    base_row = df %>%
      filter(title == input$sim_base_title) %>%
      slice(1)
    
    validate(
      need(nrow(base_row) > 0,
           "Could not find the selected title in the scored dataset.")
    )
    
    neighbours = sim_results()
    
    df_plot = df %>%
      mutate(
        category = dplyr::case_when(
          title == base_row$title[1]          ~ "Selected title",
          title %in% neighbours$title        ~ "Similar title",
          TRUE                               ~ "Other titles"
        )
      )
    
    p = ggplot(
      df_plot,
      aes(
        x = z_logvotes,
        y = z_rating,
        color = category,
        text = paste0(
          "Title: ", title,
          "<br>Type: ", type,
          "<br>Year: ", release_year,
          "<br>IMDb rating: ", imdb_rating,
          "<br>Votes: ", imdb_votes
        )
      )
    ) +
      geom_point(alpha = 0.6, size = 1.7) +
      labs(
        x     = "Standardized log10(IMDb votes)",
        y     = "Standardized IMDb rating",
        color = ""
      ) +
      netflix_theme() +
      theme(legend.position = "top")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Table of similar titles
  output$sim_table = renderTable({
    sim_results() %>%
      select(-dist2)
  })
  
  # Languages tab: summary by original language -------------------------------
  lang_summary = reactive({
    req(input$lang_type_filter, input$lang_top_n)
    
    netflix %>%
      filter(!is.na(language_label), type %in% input$lang_type_filter) %>%
      mutate(lang = fct_lump_n(language_label, n = input$lang_top_n)) %>%
      group_by(lang) %>%
      summarise(
        n_titles   = n(),
        avg_rating = mean(imdb_rating, na.rm = TRUE),
        avg_votes  = mean(imdb_votes, na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      arrange(desc(n_titles))
  })
  
  output$lang_summary_text = renderText({
    df       = lang_summary()
    n_groups = nrow(df)
    n_titles = sum(df$n_titles)
    
    has_other = "Other" %in% df$lang
    
    if (has_other) {
      n_top = n_groups - 1
      
      paste0(
        "Currently showing top ",
        n_top,
        " languages plus an 'Other' group (",
        n_groups,
        " groups in total), covering ",
        n_titles,
        " titles under the selected type filters."
      )
    } else {
      paste0(
        "Currently showing ",
        n_groups,
        " languages with a total of ",
        n_titles,
        " titles under the selected type filters."
      )
    }
  })
  
  output$lang_count_plot = renderPlotly({
    df = lang_summary()
    
    p = ggplot(df, aes(
      x    = reorder(lang, n_titles),
      y    = n_titles,
      text = paste0("Language: ", lang, "<br>Titles: ", n_titles)
    )) +
      geom_col(fill = "#e50914") +
      geom_text(
        aes(
          label = n_titles,
          x     = lang,
          y     = n_titles * 1.02
        ),
        hjust = 0,
        size  = 3.5,
        color = "#333333"
      ) +
      coord_flip(clip = "off") +
      expand_limits(y = max(df$n_titles) * 1.15) +
      labs(x = NULL, y = "Number of titles") +
      netflix_theme()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$lang_rating_plot = renderPlotly({
    df = lang_summary()
    
    # Overall average rating, weighted by number of titles per language
    overall_mean = with(df, weighted.mean(avg_rating, w = n_titles, na.rm = TRUE))
    
    p = ggplot(df, aes(
      x    = reorder(lang, avg_rating),
      y    = avg_rating,
      text = paste0(
        "Language: ",
        lang,
        "<br>Avg IMDb: ",
        round(avg_rating, 2),
        "<br>N: ",
        n_titles
      )
    )) +
      geom_col(fill = "#00838f") +
      geom_hline(
        yintercept = overall_mean,
        linetype   = "dashed",
        linewidth  = 0.4,
        color      = "#9e9e9e"
      ) +
      coord_flip() +
      labs(x = NULL, y = "Average IMDb rating") +
      netflix_theme()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Languages tab: TMDb popularity vs IMDb votes ------------------------------
  output$popularity_votes_plot = renderPlot({
    req(input$lang_type_filter)
    
    df = netflix %>%
      filter(
        !is.na(tmdb_popularity),!is.na(imdb_votes),
        imdb_votes > 0,
        type %in% input$lang_type_filter
      )
    
    validate(need(nrow(df) > 0, "No titles with TMDb popularity and IMDb votes under the current filters."))
    
    ggplot(df, aes(x = imdb_votes, y = tmdb_popularity)) +
      geom_bin2d() +
      facet_wrap( ~ type) +
      scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
      scale_fill_gradient(low  = "#ffe5e5",
                          high = "#b30000",
                          name = "Number of titles") +
      labs(x = "IMDb votes (log10 scale)", y = "TMDb popularity") +
      netflix_theme()
  })
  
  # Genres tab: filtered long-format data -------------------------------------
  genre_data = reactive({
    req(input$genre_year_range, input$genre_type_filter)
    
    genres_long %>%
      filter(
        !is.na(release_year),
        release_year >= input$genre_year_range[1],
        release_year <= input$genre_year_range[2],
        type %in% input$genre_type_filter
      )
  })
  
  # Genres tab: popularity of genres over time --------------------------------
  output$genre_trend = renderPlotly({
    df0 = genre_data()
    
    # Count titles by year and genre
    df_counts = df0 %>%
      count(release_year, listed_in, name = "n_titles")
    
    validate(
      need(
        nrow(df_counts) > 0,
        "No titles under the current filters. Try widening the year range or including more types."
      )
    )
    
    # Genres available after filtering
    available_genres = sort(unique(df_counts$listed_in))
    
    # Handle the optional highlighted genres input
    highlight = input$genre_focus
    if (is.null(highlight))
      highlight = character(0)
    highlight = intersect(highlight, available_genres)
    
    # If user selected specific genres, keep only those;
    # otherwise automatically use the top 8 genres by frequency.
    if (length(highlight) > 0) {
      df = df_counts %>% filter(listed_in %in% highlight)
    } else {
      genre_summary = df_counts %>%
        group_by(listed_in) %>%
        summarise(total = sum(n_titles), .groups = "drop")
      
      n_top = min(8L, nrow(genre_summary))
      
      top_genres = genre_summary %>%
        slice_max(total, n = n_top) %>%
        pull(listed_in)
      
      df = df_counts %>% filter(listed_in %in% top_genres)
    }
    
    validate(
      need(
        nrow(df) > 0,
        "No titles left after applying the highlighted genres. Try clearing the highlight or adjusting filters."
      )
    )
    
    p = ggplot(
      df,
      aes(
        x     = release_year,
        y     = n_titles,
        color = listed_in,
        group = listed_in,
        text  = paste(
          "Year:",
          release_year,
          "<br>Genre:",
          listed_in,
          "<br>Count:",
          n_titles
        )
      )
    ) +
      geom_line(linewidth = 0.6) +
      geom_point(size = 1.8) +
      labs(x = "Release year", y = "Number of titles", color = "Genre") +
      netflix_theme() +
      theme(legend.position = "top")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Genres tab: genre mix by type ---------------------------------------------
  output$genre_type_share = renderPlotly({
    df = genre_data()
    
    df_counts = df %>%
      count(listed_in, type, name = "n_titles")
    
    genre_totals = df_counts %>%
      group_by(listed_in) %>%
      summarise(total = sum(n_titles), .groups = "drop")
    
    top_genres = genre_totals %>%
      slice_max(total, n = 12) %>%
      pull(listed_in)
    
    df_plot = df_counts %>%
      filter(listed_in %in% top_genres) %>%
      left_join(genre_totals, by = "listed_in")
    
    p = ggplot(df_plot,
               aes(
                 x    = reorder(listed_in, total),
                 y    = n_titles,
                 fill = type,
                 text = paste0(
                   "Genre: ",
                   listed_in,
                   "<br>Type: ",
                   type,
                   "<br>N titles: ",
                   n_titles
                 )
               )) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(x = NULL, y = "Number of titles", fill = "Type") +
      netflix_theme() +
      theme(legend.position = "top")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Discover tab: filtered scored data ----------------------------------------
  discover_data = reactive({
    req(
      input$disc_year_range,
      input$disc_type_filter,
      input$disc_rating_range,
      input$disc_min_votes
    )
    
    df = netflix_scores %>%
      filter(
        release_year >= input$disc_year_range[1],
        release_year <= input$disc_year_range[2],
        imdb_rating  >= input$disc_rating_range[1],
        imdb_rating  <= input$disc_rating_range[2],
        imdb_votes   >= input$disc_min_votes,
        type %in% input$disc_type_filter
      )
    
    # Optional language filter
    if (!is.null(input$disc_lang_filter) &&
        length(input$disc_lang_filter) > 0) {
      df = df %>%
        filter(tmdb_original_language %in% input$disc_lang_filter)
    }
    
    # Optional genre filter: keep titles that have at least one selected genre
    if (!is.null(input$disc_genre_filter) &&
        length(input$disc_genre_filter) > 0) {
      df = df %>%
        semi_join(
          genres_long %>%
            filter(listed_in %in% input$disc_genre_filter) %>%
            select(show_id) %>%
            distinct(),
          by = "show_id"
        )
    }
    
    if (nrow(df) == 0) {
      df$category <- character(0)
      return(df)
    }
    
    # Classify using medians within the filtered sample
    rating_med = median(df$imdb_rating, na.rm = TRUE)
    logv_med   = median(df$log_votes, na.rm = TRUE)
    
    df %>%
      mutate(
        category = case_when(
          imdb_rating >= rating_med & log_votes >= logv_med ~ "Blockbuster",
          imdb_rating >= rating_med &
            log_votes <  logv_med ~ "Hidden gem",
          imdb_rating <  rating_med &
            log_votes >= logv_med ~ "Cult / overrated?",
          TRUE                                              ~ "Obscure"
        )
      )
  })
  
  output$disc_scatter = renderPlotly({
    df = discover_data()
    
    validate(need(nrow(df) > 0, "No titles match the current filters."))
    
    med_rating = median(df$imdb_rating, na.rm = TRUE)
    med_logv   = median(df$log_votes, na.rm = TRUE)
    
    p = ggplot(df,
               aes(
                 x     = log_votes,
                 y     = imdb_rating,
                 color = category,
                 text  = paste0(
                   "Title: ",
                   title,
                   "<br>Type: ",
                   type,
                   "<br>Year: ",
                   release_year,
                   "<br>Rating: ",
                   imdb_rating,
                   "<br>Votes: ",
                   imdb_votes,
                   "<br>Category: ",
                   category
                 )
               )) +
      geom_point(alpha = 0.7, size = 1.8) +
      geom_vline(xintercept = med_logv,
                 linetype = "dashed",
                 linewidth = 0.4) +
      geom_hline(yintercept = med_rating,
                 linetype = "dashed",
                 linewidth = 0.4) +
      scale_x_continuous(name = "log10 IMDb votes") +
      scale_y_continuous(name = "IMDb rating", limits = c(0, 10)) +
      labs(color = "Category") +
      netflix_theme() +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$disc_table_gems = renderTable({
    df = discover_data() %>%
      filter(category == "Hidden gem") %>%
      arrange(desc(hidden_gem_score)) %>%
      head(10L) %>%
      select(
        Title    = title,
        Type     = type,
        Year     = release_year,
        Rating   = imdb_rating,
        Votes    = imdb_votes,
        Language = language_label
      )
    
    validate(need(nrow(df) > 0, "No hidden gems under the current filters."))
    df
  })
  
  output$disc_table_blocks = renderTable({
    df = discover_data() %>%
      filter(category == "Blockbuster") %>%
      arrange(desc(blockbuster_score)) %>%
      head(10L) %>%
      select(
        Title    = title,
        Type     = type,
        Year     = release_year,
        Rating   = imdb_rating,
        Votes    = imdb_votes,
        Language = language_label
      )
    
    validate(need(nrow(df) > 0, "No blockbusters under the current filters."))
    df
  })
  
  # Countries tab: aggregated country-level data ------------------------------
  country_data = reactive({
    req(input$country_year_range, input$country_type_filter)
    
    # Start from countries_long, where each row is (show_id, country)
    base_df = countries_long %>%
      filter(
        !is.na(release_year),
        release_year >= input$country_year_range[1],
        release_year <= input$country_year_range[2],
        type %in% input$country_type_filter
      )
    
    # If genres are selected: keep only titles that have at least one chosen genre
    if (!is.null(input$country_genre_filter) &&
        length(input$country_genre_filter) > 0) {
      base_df = base_df %>%
        semi_join(
          genres_long %>%
            filter(listed_in %in% input$country_genre_filter) %>%
            select(show_id) %>%
            distinct(),
          by = "show_id"
        )
    }
    
    # Join hidden_gem_score (imdb_rating is already in countries_long via netflix)
    df = base_df %>%
      left_join(netflix_scores %>%
                  select(show_id, hidden_gem_score), by = "show_id") %>%
      group_by(country) %>%
      summarise(
        n_titles   = n(),
        avg_rating = mean(imdb_rating, na.rm = TRUE),
        gem_score  = median(hidden_gem_score, na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      # Drop countries with very few titles to avoid unstable/degenerate rankings
      filter(n_titles >= input$country_min_titles)
    
    df
  })
  
  output$country_map = renderPlotly({
    df = country_data()
    
    validate(need(nrow(df) > 0, "No countries under the current filters."))
    
    metric = input$country_metric
    metric_label = dplyr::case_when(
      metric == "n_titles"   ~ "Number of titles",
      metric == "avg_rating" ~ "Average IMDb rating",
      metric == "gem_score"  ~ "Median hidden-gem score",
      TRUE                   ~ "Value"
    )
    
    df = df %>%
      mutate(
        metric_value = dplyr::case_when(
          metric == "n_titles"   ~ n_titles,
          metric == "avg_rating" ~ avg_rating,
          TRUE                   ~ gem_score
        ),
        hover_text = paste0(
          country,
          "<br>",
          metric_label,
          ": ",
          ifelse(is.na(metric_value), "NA", round(metric_value, 2)),
          "<br>Titles: ",
          n_titles
        )
      )
    
    plot_geo(df) %>%
      add_trace(
        locations    = ~ country,
        locationmode = "country names",
        z            = ~ metric_value,
        color        = ~ metric_value,
        text         = ~ hover_text,
        hoverinfo    = "text",
        colors       = "Reds",
        colorbar     = list(title = metric_label)
      ) %>%
      layout(
        geo = list(
          showframe      = FALSE,
          showcoastlines = FALSE,
          projection     = list(type = "natural earth")
        ),
        margin = list(
          l = 0,
          r = 0,
          t = 0,
          b = 0
        )
      )
  })
  
  output$country_table = renderTable({
    df = country_data()
    metric = input$country_metric
    
    df %>%
      arrange(dplyr::desc(.data[[metric]])) %>%
      mutate(avg_rating = round(avg_rating, 2),
             gem_score  = round(gem_score, 2)) %>%
      head(10L) %>%
      transmute(
        Country                   = country,
        Titles                    = n_titles,
        `Avg IMDb`                = avg_rating,
        `Median hidden-gem score` = gem_score
      )
  })
}

# Run the app -----------------------------------------------------------------

shinyApp(ui, server)
