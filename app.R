library(shiny)
library(bslib)
library(tidyr)
library(purrr)
library(lubridate)
library(scales)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)
library(ecotourism)

# Medium Task data loading and preprocessing ---------------------------------
data(glowworms, package = "ecotourism")
data(gouldian_finch, package = "ecotourism")
data(manta_rays, package = "ecotourism")
data(orchids, package = "ecotourism")
data(weather, package = "ecotourism")

normalize_occurrence_data <- function(df, organism_label) {
  required_cols <- c(
    "obs_lat", "obs_lon", "date", "month", "year", "ws_id",
    "sci_name", "record_type", "obs_state"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  
  df %>%
    mutate(
      organism = organism_label,
      date = as.Date(date),
      month = suppressWarnings(as.integer(month)),
      year = suppressWarnings(as.integer(year)),
      obs_lat = suppressWarnings(as.numeric(obs_lat)),
      obs_lon = suppressWarnings(as.numeric(obs_lon)),
      sci_name = as.character(sci_name),
      record_type = as.character(record_type),
      obs_state = as.character(obs_state),
      ws_id = as.character(ws_id)
    ) %>%
    select(all_of(c("organism", required_cols)), everything())
}

medium_occurrence_data <- bind_rows(
  normalize_occurrence_data(glowworms, "Glowworms"),
  normalize_occurrence_data(gouldian_finch, "Gouldian Finch"),
  normalize_occurrence_data(manta_rays, "Manta Rays"),
  normalize_occurrence_data(orchids, "Orchids")
)

weather_lookup <- weather %>%
  mutate(
    ws_id = as.character(ws_id),
    date = as.Date(date),
    month = suppressWarnings(as.integer(month)),
    year = suppressWarnings(as.integer(year))
  )

organism_choices <- sort(unique(medium_occurrence_data$organism))
month_choice_values <- c("all", as.character(1:12))
month_choice_labels <- c("All months", month.name)
month_choices <- stats::setNames(month_choice_values, month_choice_labels)
month_lookup <- stats::setNames(month.name, as.character(1:12))

# Hard Task data loading and helper functions -------------------------------
state_lookup <- c(
  ACT = "Australian Capital Territory",
  NSW = "New South Wales",
  NT  = "Northern Territory",
  QLD = "Queensland",
  SA  = "South Australia",
  TAS = "Tasmania",
  VIC = "Victoria",
  WA  = "Western Australia"
)

canonicalize_state <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  upper_x <- toupper(x)

  out <- rep(NA_character_, length(upper_x))

  abbr_match <- match(upper_x, names(state_lookup))
  out[!is.na(abbr_match)] <- unname(state_lookup[abbr_match[!is.na(abbr_match)]])

  full_match <- match(upper_x, toupper(unname(state_lookup)))
  out[is.na(out) & !is.na(full_match)] <- unname(state_lookup)[full_match[is.na(out) & !is.na(full_match)]]

  out[is.na(out)] <- stringr::str_to_title(tolower(upper_x[is.na(out)]))
  out
}

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    return(rep(1, length(x)))
  }
  (x - rng[1]) / diff(rng)
}

circular_day_distance <- function(day_a, day_b) {
  delta <- abs(as.integer(day_a) - as.integer(day_b))
  pmin(delta, 366 - delta)
}

safe_weighted_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w)
  if (!any(keep)) return(NA_real_)
  x <- x[keep]
  w <- w[keep]
  if (sum(w) <= 0) return(mean(x, na.rm = TRUE))
  weighted.mean(x, w, na.rm = TRUE)
}

weighted_sd <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w)
  if (!any(keep)) return(NA_real_)
  x <- x[keep]
  w <- w[keep]
  if (sum(w) <= 0) return(sd(x, na.rm = TRUE))
  mu <- weighted.mean(x, w)
  sqrt(sum(w * (x - mu)^2) / sum(w))
}

weighted_mode <- function(x, w = NULL) {
  if (length(x) == 0) return(NA)
  if (is.null(w)) w <- rep(1, length(x))
  mode_tbl <- tibble(value = x, weight = w) %>%
    filter(!is.na(value), !is.na(weight)) %>%
    group_by(value) %>%
    summarise(weight = sum(weight), .groups = "drop") %>%
    arrange(desc(weight))

  if (nrow(mode_tbl) == 0) return(NA)
  mode_tbl$value[[1]]
}

gaussian_score <- function(x, center, spread) {
  if (!is.finite(x) || !is.finite(center)) return(NA_real_)
  if (!is.finite(spread) || spread <= 0) spread <- max(abs(center) * 0.15, 1)
  exp(-((x - center)^2) / (2 * spread^2))
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

format_hour_label <- function(hour_value) {
  if (is.na(hour_value)) return("Time unavailable")
  sprintf("%02d:00", as.integer(hour_value))
}

fmt_num <- function(x, digits = 1, suffix = "") {
  ifelse(is.finite(x), paste0(formatC(x, format = "f", digits = digits), suffix), "—")
}

hard_organism_datasets <- list(
  Glowworms = glowworms,
  `Gouldian Finch` = gouldian_finch,
  `Manta Rays` = manta_rays,
  Orchids = orchids
)

station_meta <- ecotourism::weather_stations %>%
  transmute(
    ws_id = as.character(ws_id),
    stname = as.character(stname),
    stn_lat = as.numeric(stn_lat),
    stn_lon = as.numeric(stn_lon),
    stn_city = as.character(stn_city),
    stn_state = canonicalize_state(stn_state)
  )

tourism_lookup <- ecotourism::tourism_region %>%
  transmute(
    ws_id = as.character(ws_id),
    region_id = region_id,
    region = as.character(region),
    region_lat = as.numeric(lat),
    region_lon = as.numeric(lon)
  ) %>%
  distinct(ws_id, .keep_all = TRUE)

weather_tbl <- ecotourism::weather %>%
  transmute(
    ws_id = as.character(ws_id),
    date = as.Date(date),
    year = coalesce(as.integer(year), lubridate::year(as.Date(date))),
    month = coalesce(as.integer(month), lubridate::month(as.Date(date))),
    day = coalesce(as.integer(day), lubridate::day(as.Date(date))),
    weekday = as.character(weekday),
    dayofyear = coalesce(as.integer(dayofyear), lubridate::yday(as.Date(date))),
    temp = as.numeric(temp),
    min = as.numeric(min),
    max = as.numeric(max),
    rh = as.numeric(rh),
    prcp = as.numeric(prcp),
    rainy = dplyr::case_when(
      rainy %in% c(TRUE, 1, "1", "TRUE", "True", "yes", "Yes", "rainy", "Rainy") ~ 1,
      rainy %in% c(FALSE, 0, "0", "FALSE", "False", "no", "No") ~ 0,
      TRUE ~ NA_real_
    ),
    wind_speed = as.numeric(wind_speed),
    max_speed = as.numeric(max_speed)
  ) %>%
  left_join(station_meta, by = "ws_id")

prepare_occurrence_data <- function(df, label) {
  df %>%
    transmute(
      organism = label,
      obs_lat = as.numeric(obs_lat),
      obs_lon = as.numeric(obs_lon),
      date = as.Date(date),
      time = as.character(time),
      year = coalesce(as.integer(year), lubridate::year(as.Date(date))),
      month = coalesce(as.integer(month), lubridate::month(as.Date(date))),
      day = coalesce(as.integer(day), lubridate::day(as.Date(date))),
      hour = suppressWarnings(as.integer(hour)),
      weekday = as.character(weekday),
      dayofyear = coalesce(as.integer(dayofyear), lubridate::yday(as.Date(date))),
      sci_name = as.character(sci_name),
      record_type = as.character(record_type),
      obs_state = canonicalize_state(obs_state),
      ws_id = as.character(ws_id)
    ) %>%
    left_join(station_meta, by = "ws_id") %>%
    left_join(tourism_lookup, by = "ws_id") %>%
    mutate(
      state_std = coalesce(obs_state, stn_state),
      display_place = case_when(
        !is.na(region) ~ region,
        !is.na(stn_city) & !is.na(stn_state) ~ paste(stn_city, stn_state),
        !is.na(stname) ~ stname,
        TRUE ~ paste("Station", ws_id)
      )
    )
}

occurrence_prepared <- imap(hard_organism_datasets, prepare_occurrence_data)

recommend_spots <- function(selected_date,
                            selected_state,
                            organism_label,
                            top_n = 5,
                            window_days = 21) {
  occ <- occurrence_prepared[[organism_label]]
  if (is.null(occ) || nrow(occ) == 0) return(tibble())

  target_date <- as.Date(selected_date)
  target_doy <- lubridate::yday(target_date)

  if (!identical(selected_state, "All Australia")) {
    occ <- occ %>% filter(state_std == selected_state | stn_state == selected_state)
  }

  occ <- occ %>%
    filter(!is.na(ws_id), !is.na(date), !is.na(dayofyear)) %>%
    mutate(
      day_dist = circular_day_distance(dayofyear, target_doy),
      season_weight = exp(-0.5 * (day_dist / window_days)^2)
    )

  if (nrow(occ) == 0) return(tibble())

  occ_weather <- occ %>%
    inner_join(
      weather_tbl %>% select(ws_id, date, temp, prcp, rh, wind_speed, dayofyear),
      by = c("ws_id", "date")
    ) %>%
    mutate(
      wx_day_dist = circular_day_distance(dayofyear.y, target_doy),
      wx_weight = exp(-0.5 * (wx_day_dist / window_days)^2)
    )

  weather_pref <- occ_weather %>%
    summarise(
      temp_pref = safe_weighted_mean(temp, wx_weight),
      temp_sd = weighted_sd(temp, wx_weight),
      prcp_pref = safe_weighted_mean(prcp, wx_weight),
      prcp_sd = weighted_sd(prcp, wx_weight),
      rh_pref = safe_weighted_mean(rh, wx_weight),
      rh_sd = weighted_sd(rh, wx_weight),
      wind_pref = safe_weighted_mean(wind_speed, wx_weight),
      wind_sd = weighted_sd(wind_speed, wx_weight)
    )

  spot_summary <- occ %>%
    group_by(ws_id, stname, stn_city, stn_state, region, region_id, region_lat, region_lon) %>%
    summarise(
      location = dplyr::first(display_place),
      lat = coalesce(safe_mean(obs_lat), safe_mean(stn_lat), safe_mean(region_lat)),
      lon = coalesce(safe_mean(obs_lon), safe_mean(stn_lon), safe_mean(region_lon)),
      seasonal_sightings = sum(season_weight, na.rm = TRUE),
      nearby_records = sum(day_dist <= window_days, na.rm = TRUE),
      years_active = n_distinct(year),
      best_hour = weighted_mode(hour, season_weight),
      best_month = weighted_mode(month, season_weight),
      sci_name = dplyr::first(sci_name[!is.na(sci_name)], default = NA_character_),
      .groups = "drop"
    )

  candidate_weather <- weather_tbl %>%
    filter(ws_id %in% spot_summary$ws_id, !is.na(dayofyear)) %>%
    {if (!identical(selected_state, "All Australia")) filter(., stn_state == selected_state) else .} %>%
    mutate(
      day_dist = circular_day_distance(dayofyear, target_doy),
      season_weight = exp(-0.5 * (day_dist / window_days)^2)
    ) %>%
    group_by(ws_id) %>%
    summarise(
      typical_temp = safe_weighted_mean(temp, season_weight),
      typical_min = safe_weighted_mean(min, season_weight),
      typical_max = safe_weighted_mean(max, season_weight),
      typical_prcp = safe_weighted_mean(prcp, season_weight),
      typical_rh = safe_weighted_mean(rh, season_weight),
      typical_wind = safe_weighted_mean(wind_speed, season_weight),
      rainy_share = safe_weighted_mean(rainy, season_weight),
      .groups = "drop"
    )

  recs <- spot_summary %>%
    left_join(candidate_weather, by = "ws_id") %>%
    mutate(
      freq_score = rescale01(seasonal_sightings),
      consistency_score = rescale01(years_active),
      temp_score = map_dbl(typical_temp, ~ gaussian_score(.x, weather_pref$temp_pref, weather_pref$temp_sd)),
      prcp_score = map_dbl(typical_prcp, ~ gaussian_score(.x, weather_pref$prcp_pref, weather_pref$prcp_sd)),
      rh_score = map_dbl(typical_rh, ~ gaussian_score(.x, weather_pref$rh_pref, weather_pref$rh_sd)),
      wind_score = map_dbl(typical_wind, ~ gaussian_score(.x, weather_pref$wind_pref, weather_pref$wind_sd)),
      weather_score = purrr::pmap_dbl(
        list(temp_score, prcp_score, rh_score, wind_score),
        function(...) {
          vals <- c(...)
          vals <- vals[is.finite(vals)]
          if (!length(vals)) return(0)
          mean(vals)
        }
      ),
      raw_score = 0.55 * freq_score + 0.20 * consistency_score + 0.25 * weather_score,
      probability = 0.20 + 0.79 * rescale01(raw_score),
      probability_pct = percent(probability, accuracy = 1),
      best_time = format_hour_label(best_hour),
      weather_summary = ifelse(
        is.finite(typical_temp) | is.finite(typical_prcp) | is.finite(typical_wind),
        paste0(
          fmt_num(typical_temp, 1, "°C avg"), ", ",
          fmt_num(typical_prcp, 1, " mm rain"), ", ",
          fmt_num(typical_wind, 1, " km/h wind")
        ),
        "Weather not available in current package"
      ),
      recommendation_note = case_when(
        !is.finite(typical_temp) & !is.finite(typical_prcp) & !is.finite(typical_wind) ~ "Strong seasonal match; weather data unavailable",
        probability >= 0.75 ~ "Excellent seasonal match",
        probability >= 0.55 ~ "Strong historical pattern",
        probability >= 0.35 ~ "Moderate chance based on history",
        TRUE ~ "Possible, but not a peak-time location"
      )
    ) %>%
    arrange(desc(probability), desc(nearby_records)) %>%
    mutate(rank = row_number()) %>%
    slice_head(n = top_n)

  recs
}

compare_organisms <- function(selected_date, selected_state, window_days = 21) {
  map_dfr(names(occurrence_prepared), function(org_name) {
    top_pick <- recommend_spots(
      selected_date = selected_date,
      selected_state = selected_state,
      organism_label = org_name,
      top_n = 1,
      window_days = window_days
    )

    if (nrow(top_pick) == 0) return(tibble())

    top_pick %>%
      slice(1) %>%
      transmute(
        organism = org_name,
        best_place = location,
        best_time = best_time,
        probability = probability,
        probability_pct = probability_pct,
        typical_temp = typical_temp,
        typical_prcp = typical_prcp,
        note = recommendation_note
      )
  }) %>%
    arrange(desc(probability))
}

hard_state_choices <- c("All Australia", unname(state_lookup))
hard_organism_choices <- names(occurrence_prepared)

ui <- fluidPage(
  titlePanel("Ecotourism GSoC2026 Tasks"),
  tags$head(
    tags$style(HTML("
      .question-box {
        padding: 18px;
        margin-bottom: 20px;
        background-color: #f8f9fa;
        border-radius: 10px;
        border: 1px solid #ddd;
      }
      .answer-box {
        padding: 18px;
        margin-bottom: 25px;
        background-color: #eef7ee;
        border-radius: 10px;
        border: 1px solid #b7d8b7;
      }
      .run-output {
        background-color: #1e1e1e;
        color: #f1f1f1;
        padding: 12px;
        border-radius: 8px;
        white-space: pre-wrap;
        font-family: monospace;
        min-height: 80px;
      }
      textarea {
        font-family: monospace;
      }
      .medium-task {
        --medium-bg: #f4f8f2;
        --medium-surface: #ffffff;
        --medium-border: #d7e4d1;
        --medium-text: #1f3a2d;
        --medium-muted: #5f7668;
        --medium-accent: #2f6f4f;
        --medium-accent-soft: #e4efe7;
        --medium-shadow: 0 10px 28px rgba(40, 74, 52, 0.08);
        background: linear-gradient(180deg, #f6faf4 0%, #eef5ee 100%);
        border: 1px solid #dce9d7;
        border-radius: 22px;
        padding: 24px;
        margin: 16px 0 28px;
        color: var(--medium-text);
      }
      .medium-hero {
        margin-bottom: 18px;
      }
      .medium-kicker {
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        color: var(--medium-accent);
        margin-bottom: 6px;
      }
      .medium-hero h2 {
        margin-top: 0;
        margin-bottom: 8px;
        font-weight: 700;
      }
      .medium-hero p {
        color: var(--medium-muted);
        margin-bottom: 0;
      }
      .medium-panel {
        background: var(--medium-surface);
        border: 1px solid var(--medium-border);
        border-radius: 18px;
        box-shadow: var(--medium-shadow);
        padding: 18px;
        margin-bottom: 18px;
      }
      .medium-panel h4, .medium-panel h5 {
        margin-top: 0;
        color: var(--medium-text);
      }
      .medium-card {
        background: linear-gradient(180deg, #ffffff 0%, #f8fbf7 100%);
        border: 1px solid var(--medium-border);
        border-radius: 16px;
        padding: 16px;
        margin-bottom: 14px;
        min-height: 110px;
      }
      .medium-card-label {
        font-size: 12px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: var(--medium-muted);
        margin-bottom: 8px;
      }
      .medium-card-value {
        font-size: 30px;
        line-height: 1;
        font-weight: 700;
        color: var(--medium-text);
        margin-bottom: 8px;
      }
      .medium-card-note {
        font-size: 13px;
        color: var(--medium-muted);
      }
      .medium-insight {
        font-size: 14px;
        color: var(--medium-text);
        line-height: 1.5;
      }
      .medium-help {
        color: var(--medium-muted);
        font-size: 13px;
        margin-top: 8px;
      }
      .medium-empty {
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 140px;
        border-radius: 14px;
        background: #f7faf6;
        color: var(--medium-muted);
        text-align: center;
        padding: 16px;
      }
      .medium-plot {
        min-height: 320px;
      }
      .leaflet-container {
        border-radius: 14px;
      }

      .hard-task {
        background: linear-gradient(180deg, #f8fbff 0%, #eef4fb 100%);
        border: 1px solid #d9e4f0;
        border-radius: 22px;
        padding: 22px;
        margin: 16px 0 28px;
      }
      .hard-panel {
        background: #ffffff;
        border: 1px solid #d9e4f0;
        border-radius: 18px;
        box-shadow: 0 10px 28px rgba(38, 67, 100, 0.08);
        padding: 18px;
        margin-bottom: 18px;
      }
      .hard-panel h4 {
        margin-top: 0;
        margin-bottom: 14px;
        font-weight: 700;
        color: #243447;
      }
      .hard-value-title {
        font-size: 20px;
        font-weight: 700;
        line-height: 1.2;
        margin: 0 0 10px;
      }
      .hard-score {
        font-size: 16px;
        font-weight: 700;
        color: #1f73b7;
      }
      .hard-note {
        color: #5e6b78;
      }
      .hard-sidebar-help {
        color: #5e6b78;
        font-size: 13px;
        line-height: 1.55;
      }
      .hard-map .leaflet-container {
        border-radius: 14px;
      }
      @media (min-width: 992px) {
        .hard-sidebar-sticky {
          position: sticky;
          top: 16px;
        }
      }
      @media (max-width: 991px) {
        .medium-task {
          padding: 18px;
        }
      }
    "))
  ),
  
  tabsetPanel(
    
    tabPanel(
      "Easy Task",
      fluidRow(
        column(
          width = 10, offset = 1,
          
          br(),
          h2("Ecotourism Tutorial"),
          h4("Initial data exploration with ecotourism"),
          p(strong("Author:"), "Javad Vahdat Atashgah"),
          hr(),

          h3("Objectives"),
          p("Practice joining occurrence data with tourism data and weather data using the current version of the ecotourism package."),
          p("Focus on one organism, one tourism region, and one month."),
          
          h3("Preparation"),
          tags$ul(
            tags$li("Load tidyverse and the ecotourism package."),
            tags$li("Inspect the available occurrence, tourism, and weather datasets."),
            tags$li("Choose one organism, one region, and one month for analysis."),
            tags$li("Create one joined dataset that combines occurrence, tourism, and weather information.")
          ),
          
          hr(),
          h3("Exercises"),
          p("Use one organism, one region, and one month from the ecotourism package for all three questions below."),
          
          # Question 1
          div(
            class = "question-box",
            h4("Question 1"),
            p("Choose one organism, one region, and one month from the ecotourism package."),
            p("Filter the organism data to that month and region, then join it with weather_data using ws_id and date."),
            p("Show the first 6 rows."),
            actionButton("show1", "Show Answer")
          ),
          
          conditionalPanel(
            condition = "input.show1 % 2 == 1",
            div(
              class = "answer-box",
              h4("Answer: Question 1"),
              p("You can edit the code below and run it."),
              textAreaInput(
                "code1",
                "Editable Code",
                rows = 18,
                width = "100%",
                value = paste(
                  "library(tidyverse)",
                  "library(ecotourism)",
                  "",
                  "organism_data <- gouldian_finch",
                  "chosen_ws <- '941200-99999'",
                  "occ_month <- 8",
                  "",
                  "",
                  "joined_data <- organism_data %>%",
                  "  filter(",
                  "    ws_id == chosen_ws,",
                  "    month == occ_month",
                  "  ) %>%",
                  "  left_join(",
                  "    weather %>% select(ws_id, date, temp, prcp),",
                  "    by = c('ws_id', 'date')",
                  "  )",
                  "",
                  "head(joined_data)",
                  sep = "\n"
                )
              ),
              actionButton("run1", "Run Code"),
              br(), br(),
              strong("Output"),
              verbatimTextOutput("out1", placeholder = TRUE)
            )
          ),
          
          # Question 2
          div(
            class = "question-box",
            h4("Question 2"),
            p("Using the joined data from Question 1, calculate:"),
            p(" > total number of occurrence records"),
            p(" > average temperature"),
            p(" > total precipitation"),
            p("for that month"),

            actionButton("show2", "Show Answer")
          ),
          
          conditionalPanel(
            condition = "input.show2 % 2 == 1",
            div(
              class = "answer-box",
              h4("Answer: Question 2"),
              p("You can edit the code below and run it."),
              textAreaInput(
                "code2",
                "Editable Code",
                rows = 18,
                width = "100%",
                value = paste(
                  "library(tidyverse)",
                  "library(ecotourism)",
                  "",
                  "organism_data <- gouldian_finch",
                  "chosen_ws <- '941200-99999'",
                  "occ_month <- 8",
                  "",
                  "",
                  "joined_data <- organism_data %>%",
                  "  filter(",
                  "    ws_id == chosen_ws,",
                  "    month == occ_month",
                  "  ) %>%",
                  "  left_join(",
                  "    weather %>% select(ws_id, date, temp, prcp),",
                  "    by = c('ws_id', 'date')",
                  "  )",
                  "",
                  "joined_data %>%",
                  "  summarise(",
                  "    occurrences = n(),",
                  "    avg_temp = mean(temp, na.rm = TRUE),",
                  "    total_prcp = sum(prcp, na.rm = TRUE)",
                  "  )",
                  sep = "\n"
                )
              ),
              actionButton("run2", "Run Code"),
              br(), br(),
              strong("Output"),
              verbatimTextOutput("out2", placeholder = TRUE)
            )
          ),
          
          # Question 3
          div(
            class = "question-box",
            h4("Question 3"),
            p("Join the monthly occurrence-weather data with tourism data for the matching quarter."),
            p("Show the tourism trips value together with your summary."),
            actionButton("show3", "Show Answer")
          ),
          
          conditionalPanel(
            condition = "input.show3 % 2 == 1",
            div(
              class = "answer-box",
              h4("Answer: Question 3"),
              p("You can edit the code below and run it."),
              textAreaInput(
                "code3",
                "Editable Code",
                rows = 18,
                width = "100%",
                value = paste(
                  "library(tidyverse)",
                  "library(ecotourism)",
                  "",
                  "organism_data <- gouldian_finch",
                  "chosen_ws <- '941200-99999'",
                  "chosen_region_id <- 20",
                  "chosen_month <- 8",
                  "chosen_quarter <- ((chosen_month - 1) %/% 3) + 1",
                  "",
                  "joined_data <- organism_data %>%",
                  "  filter(",
                  "    ws_id == chosen_ws,",
                  "    month == chosen_month",
                  "  ) %>%",
                  "  mutate(",
                  "    quarter = chosen_quarter,",
                  "    region_id = chosen_region_id",
                  "  ) %>%",
                  "  left_join(",
                  "    weather %>% select(ws_id, date, temp, prcp),",
                  "    by = c('ws_id', 'date')",
                  "  ) %>%",
                  "  left_join(",
                  "    tourism_quarterly %>% filter(region_id == chosen_region_id),",
                  "    by = c('region_id', 'year', 'quarter')",
                  "  )",
                  "",
                  "",
                  "summary_data <- joined_data %>%",
                  "  summarise(",
                  "    occurrences = n(),",
                  "    avg_temp = mean(temp, na.rm = TRUE),",
                  "    total_prcp = sum(prcp, na.rm = TRUE),",
                  "    tourism_trips = first(trips)",
                  "  )",
                  "",
                  "summary_data",
                  sep = "\n"
                )
              ),
              actionButton("run3", "Run Code"),
              br(), br(),
              strong("Output"),
              verbatimTextOutput("out3", placeholder = TRUE)
            )
          ),
          
          hr(),
          h3("👌 Finishing up"),
          p("Write a short reflection on what you learned from joining the three datasets. Which join was easiest? Which part was hardest? Did the combined data help you understand ecotourism patterns better than using a single dataset alone?")
        )
      )
    ),
    
    tabPanel(
      "Medium Task",
      div(
        class = "medium-task",
        div(
          class = "medium-hero",
          div(class = "medium-kicker", "Medium Task Dashboard"),
          h2("Australian Wildlife Explorer"),
          p("Explore where wildlife sightings occur across Australia, when they peak through the year, and how local weather conditions align with those observations.")
        ),
        
        fluidRow(
          column(
            width = 3,
            # Medium Task controls
            div(
              class = "medium-panel",
              h4("Explore sightings"),
              selectInput(
                "medium_organism",
                "Organism",
                choices = organism_choices,
                selected = if ("Gouldian Finch" %in% organism_choices) "Gouldian Finch" else organism_choices[[1]]
              ),
              selectInput(
                "medium_month",
                "Month",
                choices = month_choices,
                selected = "all"
              ),
              uiOutput("medium_record_type_ui"),
              actionButton("medium_reset", "Reset filters", class = "btn-success"),
              div(
                class = "medium-help",
                "Use the same filters to update the map, key metrics, monthly patterns, weather summaries, and state rankings."
              )
            ),
            div(
              class = "medium-panel",
              h5("Quick note"),
              p(
                class = "medium-help",
                "Markers are clustered to keep dense occurrence areas readable. Zoom into regions to inspect individual sightings."
              )
            )
          ),
          column(
            width = 6,
            div(
              class = "medium-panel",
              h4("Sightings map"),
              leafletOutput("occurrence_map", height = 560)
            )
          ),
          column(
            width = 3,
            # Medium Task summary cards
            div(
              class = "medium-card",
              div(class = "medium-card-label", "Total sightings"),
              div(class = "medium-card-value", textOutput("kpi_total")),
              div(class = "medium-card-note", "Records matching the active filters.")
            ),
            div(
              class = "medium-card",
              div(class = "medium-card-label", "Unique locations"),
              div(class = "medium-card-value", textOutput("kpi_locations")),
              div(class = "medium-card-note", "Distinct latitude and longitude pairs.")
            ),
            div(
              class = "medium-card",
              div(class = "medium-card-label", "Years covered"),
              div(class = "medium-card-value", textOutput("kpi_years")),
              div(class = "medium-card-note", "Observed span for the filtered selection.")
            ),
            div(
              class = "medium-panel",
              h5("Insight"),
              uiOutput("medium_insight")
            ),
            div(
              class = "medium-panel",
              h5("Best time to visit"),
              uiOutput("best_time_ui")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            div(
              class = "medium-panel",
              h4("Sightings by month"),
              plotOutput("month_plot", height = 320)
            )
          ),
          column(
            width = 4,
            div(
              class = "medium-panel",
              h4("Weather at sighting time"),
              plotOutput("weather_plot", height = 320)
            )
          ),
          column(
            width = 4,
            div(
              class = "medium-panel",
              h4("Sightings over years"),
              plotOutput("year_plot", height = 320)
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "medium-panel",
              h4("State summary"),
              DTOutput("state_table")
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Hard Task",
      div(
        class = "hard-task",
        fluidRow(
          column(
            width = 4,
            div(
              class = "hard-panel hard-sidebar-sticky",
              h4("Plan your trip"),
              dateInput(
                inputId = "hard_trip_date",
                label = "Holiday date",
                value = as.Date("2026-06-20"),
                min = as.Date("2025-01-01"),
                max = as.Date("2030-12-31")
              ),
              selectInput(
                inputId = "hard_state",
                label = "State or territory",
                choices = hard_state_choices,
                selected = "Victoria"
              ),
              selectInput(
                inputId = "hard_organism",
                label = "Target organism",
                choices = hard_organism_choices,
                selected = "Gouldian Finch"
              ),
              sliderInput(
                inputId = "hard_window_days",
                label = "Seasonal matching window (days)",
                min = 7,
                max = 45,
                value = 21,
                step = 1
              ),
              numericInput(
                inputId = "hard_top_n",
                label = "How many places to rank",
                value = 5,
                min = 3,
                max = 10,
                step = 1
              ),
              actionButton("hard_plan_trip", "Plan my wildlife holiday", class = "btn-primary btn-block"),
              hr(),
              p(
                class = "hard-sidebar-help",
                "This planner uses historical wildlife sightings and historical weather in the ecotourism package. ",
                "For future dates it shows typical conditions for that time of year, not live forecasts."
              )
            )
          ),
          column(
            width = 8,
            fluidRow(
              column(
                width = 4,
                div(
                  class = "hard-panel",
                  h4("Best pick"),
                  uiOutput("hard_best_pick_ui")
                )
              ),
              column(
                width = 4,
                div(
                  class = "hard-panel",
                  h4("Typical weather"),
                  uiOutput("hard_weather_ui")
                )
              ),
              column(
                width = 4,
                div(
                  class = "hard-panel",
                  h4("Better wildlife options?"),
                  uiOutput("hard_alternative_ui")
                )
              )
            ),
            div(
              class = "hard-panel hard-map",
              h4("Recommended places on the map"),
              leafletOutput("hard_holiday_map", height = 520)
            ),
            div(
              class = "hard-panel",
              h4("Ranked trip recommendations"),
              DTOutput("hard_recommendation_table")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  run_user_code <- function(code_text) {
    tryCatch({
      env <- new.env(parent = globalenv())
      
      output_text <- capture.output({
        result <- eval(parse(text = code_text), envir = env)
        if (!is.null(result)) print(result)
      })
      
      if (length(output_text) == 0) {
        "Code ran successfully with no printed output."
      } else {
        paste(output_text, collapse = "\n")
      }
    }, error = function(e) {
      paste("Error:", e$message)
    })
  }
  
  result1 <- eventReactive(input$run1, {
    run_user_code(input$code1)
  })
  
  result2 <- eventReactive(input$run2, {
    run_user_code(input$code2)
  })
  
  result3 <- eventReactive(input$run3, {
    run_user_code(input$code3)
  })
  
  output$out1 <- renderText({
    req(result1())
    result1()
  })
  
  output$out2 <- renderText({
    req(result2())
    result2()
  })
  
  output$out3 <- renderText({
    req(result3())
    result3()
  })
  
  # Medium Task filter controls
  observeEvent(input$medium_reset, {
    updateSelectInput(session, "medium_organism", selected = if ("Gouldian Finch" %in% organism_choices) "Gouldian Finch" else organism_choices[[1]])
    updateSelectInput(session, "medium_month", selected = "all")
    updateSelectInput(session, "medium_record_type", selected = "all")
  })
  
  output$medium_record_type_ui <- renderUI({
    organism_data <- medium_occurrence_data %>%
      filter(organism == input$medium_organism)
    
    record_types <- organism_data %>%
      filter(!is.na(record_type), record_type != "") %>%
      distinct(record_type) %>%
      arrange(record_type) %>%
      pull(record_type)
    
    if (length(record_types) == 0) {
      selectInput(
        "medium_record_type",
        "Record type",
        choices = c("All record types" = "all"),
        selected = "all"
      )
    } else {
      selectInput(
        "medium_record_type",
        "Record type",
        choices = c("All record types" = "all", stats::setNames(record_types, record_types)),
        selected = "all"
      )
    }
  })
  
  # Medium Task shared reactive data pipelines
  medium_base_data <- reactive({
    medium_occurrence_data
  })
  
  medium_filtered_data <- reactive({
    req(input$medium_organism)
    
    filtered <- medium_base_data() %>%
      filter(organism == input$medium_organism)
    
    if (!is.null(input$medium_month) && input$medium_month != "all") {
      filtered <- filtered %>%
        filter(month == as.integer(input$medium_month))
    }
    
    if (!is.null(input$medium_record_type) && input$medium_record_type != "all") {
      filtered <- filtered %>%
        filter(record_type == input$medium_record_type)
    }
    
    filtered
  })
  
  medium_weather_data <- reactive({
    filtered <- medium_filtered_data()
    
    if (!all(c("ws_id", "date") %in% names(filtered))) {
      return(filtered[0, , drop = FALSE])
    }
    
    filtered %>%
      left_join(
        weather_lookup %>%
          select(ws_id, date, temp, prcp, rainy),
        by = c("ws_id", "date")
      )
  })
  
  medium_state_summary <- reactive({
    filtered <- medium_filtered_data()
    
    filtered %>%
      mutate(obs_state = ifelse(is.na(obs_state) | obs_state == "", "Unknown", obs_state)) %>%
      count(obs_state, name = "sightings", sort = TRUE)
  })
  
  medium_peak_months <- reactive({
    filtered <- medium_filtered_data()
    
    filtered %>%
      filter(!is.na(month), month >= 1, month <= 12) %>%
      count(month, name = "sightings", sort = TRUE) %>%
      mutate(month_label = month.name[month]) %>%
      slice_head(n = 3)
  })
  
  output$occurrence_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 134.5, lat = -25.5, zoom = 4)
  })
  
  observe({
    filtered <- medium_filtered_data() %>%
      filter(!is.na(obs_lat), !is.na(obs_lon))
    
    proxy <- leafletProxy("occurrence_map")
    proxy %>% clearMarkers() %>% clearMarkerClusters()
    
    if (nrow(filtered) == 0) {
      return()
    }
    
    popup_text <- paste0(
      "<strong>", filtered$organism, "</strong><br/>",
      ifelse(is.na(filtered$sci_name) | filtered$sci_name == "", "Scientific name unavailable", paste0("<em>", filtered$sci_name, "</em>")),
      "<br/>Date: ", ifelse(is.na(filtered$date), "Unknown", as.character(filtered$date)),
      "<br/>State: ", ifelse(is.na(filtered$obs_state) | filtered$obs_state == "", "Unknown", filtered$obs_state)
    )
    
    proxy %>%
      addCircleMarkers(
        lng = filtered$obs_lon,
        lat = filtered$obs_lat,
        popup = popup_text,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.8,
        color = "#2f6f4f",
        clusterOptions = markerClusterOptions()
      )
  })
  
  output$kpi_total <- renderText({
    format(nrow(medium_filtered_data()), big.mark = ",")
  })
  
  output$kpi_locations <- renderText({
    filtered <- medium_filtered_data() %>%
      filter(!is.na(obs_lat), !is.na(obs_lon)) %>%
      distinct(obs_lat, obs_lon)
    
    format(nrow(filtered), big.mark = ",")
  })
  
  output$kpi_years <- renderText({
    years <- medium_filtered_data() %>%
      filter(!is.na(year)) %>%
      pull(year)
    
    if (length(years) == 0) {
      return("No year data")
    }
    
    if (min(years) == max(years)) {
      as.character(min(years))
    } else {
      paste(min(years), max(years), sep = " - ")
    }
  })
  
  output$medium_insight <- renderUI({
    filtered <- medium_filtered_data()
    
    if (nrow(filtered) == 0) {
      return(div(class = "medium-empty", "No sightings match the current filters. Try another organism, month, or record type."))
    }
    
    top_state <- filtered %>%
      mutate(obs_state = ifelse(is.na(obs_state) | obs_state == "", "Unknown", obs_state)) %>%
      count(obs_state, sort = TRUE) %>%
      slice_head(n = 1)
    
    top_month <- filtered %>%
      filter(!is.na(month), month >= 1, month <= 12) %>%
      count(month, sort = TRUE) %>%
      mutate(month_label = month.name[month]) %>%
      slice_head(n = 1)
    
    insight_text <- paste0(
      input$medium_organism, " sightings are strongest in ",
      if (nrow(top_state) == 0) "Unknown" else top_state$obs_state[[1]],
      " and most often appear in ",
      if (nrow(top_month) == 0) "months without a clear peak" else top_month$month_label[[1]],
      "."
    )
    
    div(class = "medium-insight", insight_text)
  })
  
  output$best_time_ui <- renderUI({
    peaks <- medium_peak_months()
    weather_data <- medium_weather_data()
    
    if (nrow(peaks) == 0) {
      return(div(class = "medium-empty", "No monthly pattern is available for the current filters."))
    }
    
    month_text <- paste(peaks$month_label, collapse = ", ")
    narrative <- paste0("The strongest sighting months are ", month_text, ".")
    
    weather_subset <- weather_data %>%
      filter(!is.na(month), month %in% peaks$month)
    
    if (nrow(weather_subset) > 0 && any(!is.na(weather_subset$temp))) {
      avg_temp <- round(mean(weather_subset$temp, na.rm = TRUE), 1)
      rainfall_text <- ""
      if ("prcp" %in% names(weather_subset) && any(!is.na(weather_subset$prcp))) {
        rainfall_text <- paste0(" with average rainfall around ", round(mean(weather_subset$prcp, na.rm = TRUE), 1), " mm")
      }
      narrative <- paste0(
        narrative,
        " During those months, average sighting-day temperature is about ",
        avg_temp,
        "°C",
        rainfall_text,
        "."
      )
    } else {
      narrative <- paste0(narrative, " Weather detail is not available for this selection.")
    }
    
    div(class = "medium-insight", narrative)
  })
  
  output$month_plot <- renderPlot({
    filtered <- medium_filtered_data()
    
    month_counts <- filtered %>%
      filter(!is.na(month), month >= 1, month <= 12) %>%
      count(month, name = "sightings") %>%
      right_join(data.frame(month = 1:12), by = "month") %>%
      mutate(
        sightings = ifelse(is.na(sightings), 0, sightings),
        month_label = factor(month.name[month], levels = month.name)
      )
    
    validate(need(nrow(month_counts) > 0, "No month data available for the current filters."))
    
    ggplot(month_counts, aes(x = month_label, y = sightings)) +
      geom_col(fill = "#2f6f4f", width = 0.72) +
      labs(x = NULL, y = "Sightings") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
  output$weather_plot <- renderPlot({
    weather_data <- medium_weather_data()
    
    weather_summary <- weather_data %>%
      filter(!is.na(month), month >= 1, month <= 12, !is.na(temp)) %>%
      group_by(month) %>%
      summarise(avg_temp = mean(temp, na.rm = TRUE), .groups = "drop") %>%
      mutate(month_label = factor(month.name[month], levels = month.name))
    
    validate(need(nrow(weather_summary) > 0, "Weather data is unavailable for the current filters."))
    
    ggplot(weather_summary, aes(x = month_label, y = avg_temp, group = 1)) +
      geom_line(color = "#5a8f68", linewidth = 1) +
      geom_point(color = "#2f6f4f", size = 2.6) +
      labs(x = NULL, y = "Average temperature (°C)") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
  output$year_plot <- renderPlot({
    filtered <- medium_filtered_data()
    
    year_counts <- filtered %>%
      filter(!is.na(year)) %>%
      count(year, name = "sightings")
    
    validate(need(nrow(year_counts) > 0, "Year data is unavailable for the current filters."))
    
    ggplot(year_counts, aes(x = year, y = sightings)) +
      geom_line(color = "#2f6f4f", linewidth = 1) +
      geom_point(color = "#163426", size = 2.4) +
      labs(x = "Year", y = "Sightings") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
  output$state_table <- renderDT({
    summary_tbl <- medium_state_summary()
    
    validate(need(nrow(summary_tbl) > 0, "No state summary is available for the current filters."))
    
    datatable(
      summary_tbl,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 8,
        ordering = TRUE
      ),
      colnames = c("State", "Sightings")
    )
  })
  # Hard Task trip planner ---------------------------------------------------
  hard_trip_recommendations <- eventReactive(input$hard_plan_trip, {
    recommend_spots(
      selected_date = input$hard_trip_date,
      selected_state = input$hard_state,
      organism_label = input$hard_organism,
      top_n = input$hard_top_n,
      window_days = input$hard_window_days
    )
  }, ignoreInit = FALSE)

  hard_organism_comparison <- eventReactive(input$hard_plan_trip, {
    compare_organisms(
      selected_date = input$hard_trip_date,
      selected_state = input$hard_state,
      window_days = input$hard_window_days
    )
  }, ignoreInit = FALSE)

  output$hard_best_pick_ui <- renderUI({
    recs <- hard_trip_recommendations()
    if (nrow(recs) == 0) {
      return(tags$p("No matching historical records were found for this combination."))
    }

    best <- recs[1, ]
    tagList(
      tags$div(best$location, class = "hard-value-title"),
      tags$p(sprintf("%s chance score", best$probability_pct), class = "hard-score"),
      tags$p(sprintf("Best viewing time: %s", best$best_time)),
      tags$p(best$recommendation_note, class = "hard-note")
    )
  })

  output$hard_weather_ui <- renderUI({
    recs <- hard_trip_recommendations()
    if (nrow(recs) == 0) {
      return(tags$p("Weather summary unavailable."))
    }

    best <- recs[1, ]
    rainy_pct <- if (is.na(best$rainy_share)) "N/A" else percent(best$rainy_share, accuracy = 1)

    tagList(
      tags$p(sprintf("Average temperature: %s", fmt_num(best$typical_temp, 1, "°C"))),
      tags$p(sprintf("Typical daily range: %s to %s", fmt_num(best$typical_min, 1, "°C"), fmt_num(best$typical_max, 1, "°C"))),
      tags$p(sprintf("Average rainfall: %s", fmt_num(best$typical_prcp, 1, " mm"))),
      tags$p(sprintf("Average wind speed: %s", fmt_num(best$typical_wind, 1, " km/h"))),
      tags$p(sprintf("Share of rainy days near this date: %s", rainy_pct))
    )
  })

  output$hard_alternative_ui <- renderUI({
    comparison <- hard_organism_comparison()

    if (nrow(comparison) == 0) {
      return(tags$p("No alternative comparison available."))
    }

    selected_row <- comparison %>% filter(organism == input$hard_organism) %>% slice(1)
    best_row <- comparison %>% slice(1)

    if (nrow(selected_row) == 0) {
      return(tags$p("No alternative comparison available."))
    }

    if (nrow(best_row) > 0 && best_row$organism[[1]] == input$hard_organism) {
      tagList(
        tags$h4("Your choice is already one of the strongest options."),
        tags$p(sprintf("Top place: %s", best_row$best_place)),
        tags$p(sprintf("Expected best time: %s", best_row$best_time))
      )
    } else {
      tagList(
        tags$h4(sprintf("%s may be easier to see than %s", best_row$organism, input$hard_organism)),
        tags$p(sprintf("Top place: %s", best_row$best_place)),
        tags$p(sprintf("Chance score: %s", best_row$probability_pct)),
        tags$p(sprintf("Best time: %s", best_row$best_time))
      )
    }
  })

  output$hard_holiday_map <- renderLeaflet({
    recs <- hard_trip_recommendations()
    validate(need(nrow(recs) > 0, "No map data available for this selection."))

    pal <- colorNumeric("YlGnBu", domain = recs$probability)

    leaflet(recs) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~rescale(probability, to = c(8, 18)),
        stroke = TRUE,
        weight = 1,
        color = ~pal(probability),
        fillOpacity = 0.85,
        popup = ~paste0(
          "<strong>", location, "</strong><br/>",
          "Rank: ", rank, "<br/>",
          "Chance score: ", probability_pct, "<br/>",
          "Best time: ", best_time, "<br/>",
          "Weather: ", weather_summary, "<br/>",
          recommendation_note
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~probability,
        title = "Relative chance"
      )
  })

  output$hard_recommendation_table <- renderDT({
    recs <- hard_trip_recommendations()
    validate(need(nrow(recs) > 0, "No recommendations available."))

    recs %>%
      transmute(
        Rank = rank,
        Place = location,
        Region = region,
        Station = stname,
        `Chance score` = probability_pct,
        `Best time` = best_time,
        `Avg temp (°C)` = ifelse(is.finite(typical_temp), sprintf("%.1f", typical_temp), "—"),
        `Rain (mm)` = ifelse(is.finite(typical_prcp), sprintf("%.1f", typical_prcp), "—"),
        `Wind (km/h)` = ifelse(is.finite(typical_wind), sprintf("%.1f", typical_wind), "—"),
        Note = recommendation_note
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, dom = "tip"),
        class = "compact stripe hover"
      )
  })

}

shinyApp(ui, server)
