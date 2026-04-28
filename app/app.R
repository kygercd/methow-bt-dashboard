# app.R
# -------------------------------------------------------------
# Methow Bull Trout Dashboard - Shiny app
#
# Reads CSVs that were pulled by R/fetch_ptagis.R (committed
# daily by the GitHub Action). When run locally it loads from
# ../data/ ; when deployed to shinyapps.io it falls back to the
# raw GitHub URLs so the deployment doesn't need to bundle data.
#
# Data sources:
#   data/methow_basin_bt_tagging.csv          (PTAGIS)
#   data/wells_dam_bt_tagging.csv             (PTAGIS)
#   data/upper_columbia_bt_recapture.csv      (PTAGIS)
#   data/upper_columbia_bt_interrogation.csv  (PTAGIS)
#   data/wells_dam_counts.csv                 (DART, when wired)
#   data/last_update.csv                      (manifest)
#   config/site_metadata.csv                  (PTAGIS sites API)
# -------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(leaflet)
  library(plotly)
  library(DT)
})

# ---- Config -------------------------------------------------
GH_OWNER  <- "kygercd"
GH_REPO   <- "methow-bt-dashboard"
GH_BRANCH <- "main"
GH_RAW    <- sprintf("https://raw.githubusercontent.com/%s/%s/%s",
                     GH_OWNER, GH_REPO, GH_BRANCH)

# Where to look for files: local first, then GitHub raw.
local_root <- {
  # If run from app/ folder, project root is one level up.
  here <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("DESCRIPTION") |
                           rprojroot::has_dir(".github")),
    error = function(e) NULL
  )
  if (is.null(here)) getwd() else here
}

# load_csv("data/foo.csv") tries local first, then GH raw.
load_csv <- function(rel_path, ...) {
  local_fp <- file.path(local_root, rel_path)
  if (file.exists(local_fp)) {
    message("Loading local: ", local_fp)
    return(readr::read_csv(local_fp, show_col_types = FALSE, ...))
  }
  url <- paste0(GH_RAW, "/", rel_path)
  message("Loading remote: ", url)
  tryCatch(
    readr::read_csv(url, show_col_types = FALSE, ...),
    error = function(e) {
      warning("Failed to load ", rel_path, ": ", conditionMessage(e))
      NULL
    }
  )
}

# ---- Helpers ------------------------------------------------

# PTAGIS "site" columns are formatted like "WEA - Wells Dam Adult East".
# Extract just the site code (before the first " - ").
extract_site_code <- function(x) {
  if (is.null(x)) return(character())
  trimws(stringr::str_extract(as.character(x), "^[^ ]+"))
}

# Try to coerce date-ish columns. PTAGIS uses MM/DD/YYYY HH:MM:SS for
# datetimes and MM/DD/YYYY for dates.
parse_ptagis_dt <- function(x) {
  if (is.null(x)) return(as.POSIXct(NA))
  if (inherits(x, "POSIXt")) return(x)
  out <- suppressWarnings(lubridate::mdy_hms(x, quiet = TRUE, tz = "America/Los_Angeles"))
  if (all(is.na(out))) {
    out <- suppressWarnings(lubridate::mdy(x, quiet = TRUE))
    out <- as.POSIXct(out, tz = "America/Los_Angeles")
  }
  out
}

# Pick the first column name from `candidates` that exists in df,
# returning NA-vector of length nrow(df) if none match.
first_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(rep(NA, nrow(df)))
  df[[hit[1]]]
}

# ---- Reactive data loaders ---------------------------------
# Wrapped in functions so the app stays responsive during load.

load_all_data <- function() {
  list(
    tagging_methow      = load_csv("data/methow_basin_bt_tagging.csv"),
    tagging_wells       = load_csv("data/wells_dam_bt_tagging.csv"),
    recapture           = load_csv("data/upper_columbia_bt_recapture.csv"),
    interrogation       = load_csv("data/upper_columbia_bt_interrogation.csv"),
    wells_counts        = load_csv("data/wells_dam_counts.csv"),
    last_update         = load_csv("data/last_update.csv"),
    sites               = load_csv("config/site_metadata.csv")
  )
}

# Build a tidy, joinable detections frame from the interrogation
# summary. Standardises column names so the rest of the app can
# rely on: tag_code, site_code, site_name, species, first_obs,
# last_obs, obs_count.
tidy_interrogation <- function(df, sites = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble(
      tag_code   = character(),
      site_code  = character(),
      site_name  = character(),
      species    = character(),
      first_obs  = as.POSIXct(character()),
      last_obs   = as.POSIXct(character()),
      obs_count  = integer(),
      latitude   = numeric(),
      longitude  = numeric()
    ))
  }

  out <- tibble(
    tag_code  = first_col(df, c("tag_code", "tag", "pit_tag",
                                "tag_code_decimal", "tag_code_hex")),
    site_raw  = first_col(df, c("site", "site_name", "obs_site",
                                "interrogation_site")),
    species   = first_col(df, c("mark_species_name", "species",
                                "mark_species", "release_species_name")),
    first_obs = first_col(df, c("first_obs_date", "first_obs",
                                "min_obs_date", "first_obs_time",
                                "first_observation_date")),
    last_obs  = first_col(df, c("last_obs_date", "last_obs",
                                "max_obs_date", "last_obs_time",
                                "last_observation_date")),
    obs_count = first_col(df, c("obs_count", "n_obs", "detection_count",
                                "num_obs", "observation_count"))
  ) |>
    mutate(
      site_code = extract_site_code(site_raw),
      site_name = ifelse(grepl(" - ", site_raw, fixed = TRUE),
                         sub("^[^-]+-\\s*", "", site_raw),
                         site_raw),
      first_obs = parse_ptagis_dt(first_obs),
      last_obs  = parse_ptagis_dt(last_obs),
      obs_count = suppressWarnings(as.integer(obs_count))
    ) |>
    select(-site_raw)

  if (!is.null(sites) && nrow(sites) > 0) {
    join_cols <- intersect(c("site_code", "latitude", "longitude",
                             "name", "rkm"), names(sites))
    if ("site_code" %in% join_cols) {
      out <- out |>
        left_join(sites |> select(any_of(join_cols)),
                  by = "site_code")
    }
  }
  out
}

# ---- UI -----------------------------------------------------
ui <- page_navbar(
  title  = "Methow Bull Trout Dashboard",
  theme  = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = TRUE,

  # ---- Map tab ----------------------------------------------
  nav_panel(
    title = "Map",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        sliderInput("days_back", "Show detections in last N days:",
                    min = 7, max = 365, value = 60, step = 1),
        radioButtons("size_by", "Bubble size:",
                     choices = c("Detection count" = "count",
                                 "Unique tags"     = "tags"),
                     selected = "count"),
        checkboxInput("show_labels", "Show site labels", FALSE),
        hr(),
        textOutput("map_summary"),
        hr(),
        helpText("Click a site for details. Uses the Interrogation Summary export.")
      ),
      card(
        full_screen = TRUE,
        leafletOutput("det_map", height = "100%")
      )
    )
  ),

  # ---- Tag lookup tab ---------------------------------------
  nav_panel(
    title = "Tag lookup",
    layout_sidebar(
      sidebar = sidebar(
        title = "Find a tag",
        width = 320,
        textInput("tag_search", "PIT tag code:",
                  placeholder = "e.g. 3DD.003D5A1234"),
        helpText("Partial matches OK (last 4-6 chars)."),
        hr(),
        uiOutput("tag_pick")
      ),
      navset_card_tab(
        nav_panel("Detection history",
                  DTOutput("tag_history")),
        nav_panel("Track map",
                  leafletOutput("tag_map", height = 500)),
        nav_panel("Tagging info",
                  DTOutput("tag_release"))
      )
    )
  ),

  # ---- Wells counts tab -------------------------------------
  nav_panel(
    title = "Wells Dam counts",
    layout_sidebar(
      sidebar = sidebar(
        title = "Wells Dam adult passage",
        width = 280,
        helpText("Daily adult Bull Trout passage at Wells Dam."),
        helpText("Source: CBR DART (when wired in)."),
        sliderInput("wells_year_range", "Year range:",
                    min = 2010, max = as.integer(format(Sys.Date(), "%Y")),
                    value = c(as.integer(format(Sys.Date(), "%Y")) - 4,
                              as.integer(format(Sys.Date(), "%Y"))),
                    step = 1, sep = "")
      ),
      card(
        full_screen = TRUE,
        plotlyOutput("wells_plot", height = "100%")
      ),
      card(
        DTOutput("wells_table")
      )
    )
  ),

  # ---- About / status tab -----------------------------------
  nav_panel(
    title = "Data status",
    card(
      card_header("Last update"),
      DTOutput("last_update_tbl")
    ),
    card(
      card_header("How this works"),
      tags$div(
        tags$p("This dashboard reads CSVs that are pulled daily from PTAGIS by ",
               "a GitHub Action and committed to the repo:"),
        tags$ul(
          tags$li(tags$code("methow_basin_bt_tagging.csv"),
                  " - Bull Trout tagged in the Methow subbasin"),
          tags$li(tags$code("wells_dam_bt_tagging.csv"),
                  " - Bull Trout tagged at Wells Dam"),
          tags$li(tags$code("upper_columbia_bt_recapture.csv"),
                  " - Manual recaptures (Twisp weir, etc.)"),
          tags$li(tags$code("upper_columbia_bt_interrogation.csv"),
                  " - PIT-array detections")
        ),
        tags$p("Site coordinates are pulled from the PTAGIS sites API, ",
               "cached daily to ", tags$code("config/site_metadata.csv"), "."),
        tags$p("Repo: ",
               tags$a(href = sprintf("https://github.com/%s/%s", GH_OWNER, GH_REPO),
                      sprintf("%s/%s", GH_OWNER, GH_REPO)))
      )
    )
  ),

  nav_spacer(),
  nav_item(textOutput("freshness_badge", inline = TRUE))
)

# ---- Server -------------------------------------------------
server <- function(input, output, session) {

  # Load everything once at startup. Could be wrapped in
  # reactivePoll later if you want auto-refresh without redeploy.
  raw <- load_all_data()

  # ---- Tidy detections --------------------------------------
  detections <- reactive({
    tidy_interrogation(raw$interrogation, sites = raw$sites)
  })

  # ---- Freshness badge --------------------------------------
  output$freshness_badge <- renderText({
    lu <- raw$last_update
    if (is.null(lu) || nrow(lu) == 0) return("Data: unknown")
    latest <- suppressWarnings(max(as.POSIXct(lu$fetched), na.rm = TRUE))
    if (is.infinite(latest)) return("Data: unknown")
    sprintf("Data updated: %s", format(latest, "%Y-%m-%d %H:%M %Z"))
  })

  # ---- Map summary text -------------------------------------
  filtered_dets <- reactive({
    df <- detections()
    if (is.null(df) || nrow(df) == 0) return(df)
    cutoff <- Sys.time() - as.difftime(input$days_back, units = "days")
    df |> filter(!is.na(last_obs), last_obs >= cutoff)
  })

  output$map_summary <- renderText({
    df <- filtered_dets()
    if (is.null(df) || nrow(df) == 0) {
      return("No detections in this window.")
    }
    n_tags  <- length(unique(df$tag_code))
    n_sites <- length(unique(df$site_code))
    sprintf("%d unique tags across %d sites in last %d days",
            n_tags, n_sites, input$days_back)
  })

  # ---- Map --------------------------------------------------
  output$det_map <- renderLeaflet({
    df <- filtered_dets()

    base <- leaflet() |>
      addProviderTiles(providers$Esri.WorldTopoMap,
                       group = "Topo") |>
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "Imagery") |>
      addLayersControl(baseGroups = c("Topo", "Imagery"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      setView(lng = -120.0, lat = 48.4, zoom = 9)

    if (is.null(df) || nrow(df) == 0) return(base)

    df_geo <- df |>
      filter(!is.na(latitude), !is.na(longitude)) |>
      group_by(site_code, site_name, latitude, longitude) |>
      summarise(
        n_dets = sum(obs_count, na.rm = TRUE),
        n_tags = n_distinct(tag_code),
        last_seen = max(last_obs, na.rm = TRUE),
        .groups = "drop"
      )

    if (nrow(df_geo) == 0) return(base)

    size_var <- if (identical(input$size_by, "tags")) df_geo$n_tags
                else df_geo$n_dets
    radius   <- pmax(5, pmin(28, sqrt(size_var) * 2.5))

    pal_vals <- as.numeric(difftime(Sys.time(), df_geo$last_seen,
                                    units = "days"))
    pal <- colorNumeric(palette = "viridis", domain = pal_vals,
                        reverse = TRUE)

    m <- base |>
      addCircleMarkers(
        data = df_geo,
        lng  = ~longitude, lat = ~latitude,
        radius = radius,
        color = ~pal(pal_vals),
        stroke = TRUE, weight = 1, fillOpacity = 0.75,
        label = ~sprintf("%s (%s) - %d tags, %d detections",
                          site_name, site_code, n_tags, n_dets),
        popup = ~sprintf(
          "<b>%s</b><br/>Code: %s<br/>Tags: %d<br/>Detections: %d<br/>Last seen: %s",
          site_name, site_code, n_tags, n_dets,
          format(last_seen, "%Y-%m-%d %H:%M"))
      ) |>
      addLegend(position = "bottomright", pal = pal, values = pal_vals,
                title = "Days since<br/>last detection",
                opacity = 0.9)

    if (isTRUE(input$show_labels)) {
      m <- m |> addLabelOnlyMarkers(
        data = df_geo,
        lng = ~longitude, lat = ~latitude,
        label = ~site_code,
        labelOptions = labelOptions(noHide = TRUE, direction = "top",
                                    textsize = "11px"))
    }
    m
  })

  # ---- Tag lookup -------------------------------------------
  matched_tags <- reactive({
    q <- trimws(input$tag_search %||% "")
    if (!nzchar(q)) return(character())
    df <- detections()
    if (is.null(df) || nrow(df) == 0) return(character())
    hits <- unique(df$tag_code[
      grepl(q, df$tag_code, fixed = TRUE, ignore.case = TRUE)])
    head(hits, 50)
  })

  output$tag_pick <- renderUI({
    hits <- matched_tags()
    if (length(hits) == 0) return(helpText("No matches yet."))
    selectInput("tag_pick", sprintf("Matches (%d):", length(hits)),
                choices = hits, selected = hits[1])
  })

  selected_tag <- reactive({
    input$tag_pick %||% NA_character_
  })

  output$tag_history <- renderDT({
    tag <- selected_tag()
    if (is.na(tag)) return(datatable(data.frame()))
    df <- detections() |>
      filter(tag_code == tag) |>
      arrange(desc(last_obs)) |>
      select(any_of(c("tag_code", "site_code", "site_name",
                      "species", "first_obs", "last_obs", "obs_count")))
    datatable(df, options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE)
  })

  output$tag_map <- renderLeaflet({
    tag <- selected_tag()
    base <- leaflet() |>
      addProviderTiles(providers$Esri.WorldTopoMap) |>
      setView(lng = -120.0, lat = 48.4, zoom = 9)
    if (is.na(tag)) return(base)
    df <- detections() |>
      filter(tag_code == tag,
             !is.na(latitude), !is.na(longitude)) |>
      arrange(last_obs)
    if (nrow(df) == 0) return(base)
    base |>
      addPolylines(data = df, lng = ~longitude, lat = ~latitude,
                   weight = 2, opacity = 0.6) |>
      addCircleMarkers(data = df, lng = ~longitude, lat = ~latitude,
                       radius = 6, fillOpacity = 0.85, stroke = TRUE,
                       weight = 1,
                       label = ~sprintf("%s - %s",
                                         site_code,
                                         format(last_obs, "%Y-%m-%d")))
  })

  output$tag_release <- renderDT({
    tag <- selected_tag()
    if (is.na(tag)) return(datatable(data.frame()))
    rels <- bind_rows(
      raw$tagging_methow %||% tibble(),
      raw$tagging_wells  %||% tibble()
    )
    if (nrow(rels) == 0) return(datatable(data.frame()))
    tag_col <- intersect(c("tag_code", "tag", "pit_tag"), names(rels))
    if (length(tag_col) == 0) return(datatable(data.frame()))
    rels <- rels[rels[[tag_col[1]]] == tag, , drop = FALSE]
    datatable(rels, options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })

  # ---- Wells counts -----------------------------------------
  output$wells_plot <- renderPlotly({
    df <- raw$wells_counts
    if (is.null(df) || nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines") |>
               layout(title = "Wells Dam DART feed not yet wired in"))
    }
    df <- df |>
      mutate(date = as.Date(date),
             year = lubridate::year(date)) |>
      filter(year >= input$wells_year_range[1],
             year <= input$wells_year_range[2])
    plot_ly(df, x = ~date, y = ~count, color = ~as.factor(year),
            type = "scatter", mode = "lines") |>
      layout(yaxis = list(title = "Adult Bull Trout / day"),
             xaxis = list(title = ""),
             legend = list(title = list(text = "Year")))
  })

  output$wells_table <- renderDT({
    df <- raw$wells_counts
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(message = "DART feed not yet wired in.")))
    }
    datatable(df |> arrange(desc(date)),
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  })

  # ---- Data status table ------------------------------------
  output$last_update_tbl <- renderDT({
    df <- raw$last_update
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(message = "No manifest yet.")))
    }
    datatable(df, options = list(pageLength = 10, dom = "t"),
              rownames = FALSE)
  })
}

# ---- Tiny null-coalescing helper ----------------------------
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

shinyApp(ui, server)
