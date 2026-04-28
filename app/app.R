# app.R
# -------------------------------------------------------------
# Methow Bull Trout Dashboard - Shiny app
#
# Reads CSVs pulled daily by R/fetch_ptagis.R, R/fetch_dart_wells.R,
# and R/build_site_metadata.R. When run locally it loads from
# ../data/ ; on shinyapps.io it falls back to raw GitHub URLs so
# the deployed app picks up new data automatically.
#
# Map tab has three modes:
#   1. Last detection - one bubble per detection site, sized by
#      number of distinct tags last detected there. Click for the
#      per-fish detail.
#   2. Tagged         - one bubble per release site, sized by
#      tags released (filterable by year + life stage).
#   3. Recapture      - one bubble per recapture site, sized by
#      recap events (filterable by year + life stage).
#
# Life-stage classification (by length-mm at the relevant event):
#   <100  = Juvenile
#   100-399 = Sub-adult
#   >=400 = Adult
#   missing = Unknown
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

local_root <- {
  here <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("DESCRIPTION") |
                           rprojroot::has_dir(".github")),
    error = function(e) NULL
  )
  if (is.null(here)) getwd() else here
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

load_csv <- function(rel_path, ...) {
  local_fp <- file.path(local_root, rel_path)
  if (file.exists(local_fp)) {
    return(suppressWarnings(readr::read_csv(local_fp,
                                            show_col_types = FALSE, ...)))
  }
  url <- paste0(GH_RAW, "/", rel_path)
  tryCatch(
    suppressWarnings(readr::read_csv(url, show_col_types = FALSE, ...)),
    error = function(e) {
      warning("Failed to load ", rel_path, ": ", conditionMessage(e))
      NULL
    }
  )
}

# ---- Helpers ------------------------------------------------

# Site columns are formatted "CODE - Description". Extract code.
extract_site_code <- function(x) {
  if (is.null(x)) return(character())
  trimws(stringr::str_extract(as.character(x), "^[^ ]+"))
}

# PTAGIS: dates as "MM/DD/YYYY" or "MM/DD/YYYY HH:MM:SS [AM/PM]".
parse_dt <- function(x) {
  if (is.null(x)) return(as.POSIXct(NA))
  if (inherits(x, "POSIXt")) return(x)
  out <- suppressWarnings(lubridate::mdy_hms(x, quiet = TRUE,
                                             tz = "America/Los_Angeles"))
  miss <- is.na(out)
  if (any(miss)) {
    out2 <- suppressWarnings(lubridate::mdy(x[miss], quiet = TRUE))
    out[miss] <- as.POSIXct(out2, tz = "America/Los_Angeles")
  }
  out
}

life_stage <- function(len_mm) {
  len <- suppressWarnings(as.numeric(len_mm))
  dplyr::case_when(
    is.na(len)  ~ "Unknown",
    len <  100  ~ "Juvenile",
    len <  400  ~ "Sub-adult",
    TRUE        ~ "Adult"
  )
}

LIFE_STAGE_LEVELS <- c("Adult", "Sub-adult", "Juvenile", "Unknown")
LIFE_STAGE_COLORS <- c(Adult     = "#1f78b4",
                       `Sub-adult` = "#33a02c",
                       Juvenile  = "#ff7f00",
                       Unknown   = "#999999")

# Site coords merge: supplemental wins on conflict.
build_sites <- function(meta, supp) {
  if (is.null(meta) || nrow(meta) == 0) meta <- tibble(site_code = character())
  if (is.null(supp) || nrow(supp) == 0) supp <- tibble(site_code = character())
  bind_rows(supp, meta) |>
    filter(!is.na(site_code), nzchar(site_code)) |>
    distinct(site_code, .keep_all = TRUE) |>
    select(any_of(c("site_code","name","latitude","longitude",
                    "site_type","rkm")))
}

# ---- Loaders ------------------------------------------------
load_all_data <- function() {
  meta <- load_csv("config/site_metadata.csv")
  supp <- load_csv("config/site_coords_supplemental.csv")
  sites <- build_sites(meta, supp)

  list(
    tagging_methow      = load_csv("data/methow_basin_bt_tagging.csv"),
    tagging_wells       = load_csv("data/wells_dam_bt_tagging.csv"),
    recapture           = load_csv("data/upper_columbia_bt_recapture.csv"),
    interrogation       = load_csv("data/upper_columbia_bt_interrogation.csv"),
    wells_counts        = load_csv("data/wells_dam_counts.csv"),
    last_update         = load_csv("data/last_update.csv"),
    sites               = sites
  )
}

# Add lat/lon to a frame that already has site_code.
attach_coords <- function(df, sites) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (is.null(sites) || nrow(sites) == 0) {
    df$latitude  <- NA_real_
    df$longitude <- NA_real_
    df$site_name <- df$site_code
    return(df)
  }
  s <- sites |>
    select(site_code, latitude, longitude,
           site_name = name)
  left_join(df, s, by = "site_code")
}

# ---- Mode 1: Last detection (interrogation summary) ---------
tidy_last_detection <- function(df, sites) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  out <- tibble(
    tag_code     = df$tag,
    site_code    = extract_site_code(df$site),
    site_label   = df$site,
    first_obs    = parse_dt(df$first_time),
    last_obs     = parse_dt(df$last_time),
    species      = df$species_name,
    release_site = df$release_site,
    release_date = parse_dt(df$release_date),
    mark_length  = suppressWarnings(as.numeric(df$mark_length)),
    obs_count    = suppressWarnings(as.integer(df$count))
  ) |>
    mutate(life_stage = life_stage(mark_length)) |>
    attach_coords(sites)
  # Each tag may detect at multiple sites; keep all rows so
  # different sites get bubbles.
  out
}

# ---- Mode 2: Tagging ----------------------------------------
# The PTAGIS "Methow Basin" tagging export and the "Wells Dam"
# tagging export overlap heavily: every Wells-tagged fish is
# also in the Methow file (Wells sits in the Methow subbasin).
# A naive bind_rows() therefore double-counts the Wells fish.
# We distinct() on a tag identity key so each fish-tagging
# event appears exactly once, regardless of which source file
# (or files) it came from.
tidy_tagging <- function(df, sites) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  tibble(
    tag_code     = df$tag,
    site_code    = extract_site_code(df$release_site),
    site_label   = df$release_site,
    release_date = parse_dt(df$release_date),
    mark_date    = parse_dt(df$mark_date),
    length       = suppressWarnings(as.numeric(df$length)),
    species      = df$species_name,
    rear_type    = df$rear_type_name,
    mark_data_project = df$mark_data_project
  ) |>
    mutate(
      life_stage = life_stage(length),
      year = lubridate::year(release_date)
    ) |>
    filter(!is.na(site_code), nzchar(site_code)) |>
    # Defensive de-dup *within* one file. The cross-file de-dup
    # happens after bind_rows() in the server (see tagged_full).
    distinct(tag_code, mark_date, release_date, site_code, length,
             .keep_all = TRUE) |>
    attach_coords(sites)
}

# ---- Mode 3: Recapture --------------------------------------
# The recapture export occasionally produces exact duplicate
# rows (same tag, same recap_date, same recap_site, same length,
# same capture method) -- presumably from upstream re-imports.
# Distinct on the recapture-event key collapses these.
tidy_recapture <- function(df, sites) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  tibble(
    tag_code        = df$tag,
    site_code       = extract_site_code(df$recap_site),
    site_label      = df$recap_site,
    recap_date      = parse_dt(df$recap_date),
    recap_length    = suppressWarnings(as.numeric(df$recap_length)),
    recap_method    = df$recap_capture_method_name,
    species         = df$species_name,
    rear_type       = df$rear_type_name,
    mark_date       = parse_dt(df$mark_date),
    mark_site       = df$mark_site
  ) |>
    mutate(
      life_stage = life_stage(recap_length),   # at recap, per user
      year = lubridate::year(recap_date)
    ) |>
    filter(!is.na(site_code), nzchar(site_code)) |>
    distinct(tag_code, recap_date, site_code, recap_length,
             recap_method, .keep_all = TRUE) |>
    attach_coords(sites)
}

# Group → one row per site for plotting.
site_summary <- function(df, count_col = "tag_code") {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble(site_code = character(), latitude = numeric(),
                  longitude = numeric(), site_name = character(),
                  n = integer(), n_unique = integer()))
  }
  df |>
    filter(!is.na(latitude), !is.na(longitude)) |>
    group_by(site_code, site_name, latitude, longitude) |>
    summarise(
      n        = dplyr::n(),
      n_unique = dplyr::n_distinct(.data[[count_col]]),
      .groups  = "drop"
    )
}

# ---- UI -----------------------------------------------------
year_now <- as.integer(format(Sys.Date(), "%Y"))

ui <- page_navbar(
  title  = "Methow Bull Trout Dashboard",
  theme  = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = TRUE,

  # ---- Map tab ----------------------------------------------
  nav_panel(
    title = "Map",
    layout_sidebar(
      sidebar = sidebar(
        title = "Map options",
        width = 320,
        radioButtons("map_mode", "Mode:",
                     choices = c("Last detection" = "last",
                                 "Tagged"         = "tagged",
                                 "Recapture"      = "recap"),
                     selected = "last"),
        hr(),
        # Common: life stage filter
        checkboxGroupInput(
          "stages", "Life stage:",
          choices  = LIFE_STAGE_LEVELS,
          selected = c("Adult", "Sub-adult")
        ),
        # Mode-specific UI
        conditionalPanel(
          "input.map_mode == 'last'",
          sliderInput("days_back", "Last detection within (days):",
                      min = 30, max = 730, value = 180, step = 30)
        ),
        conditionalPanel(
          "input.map_mode == 'tagged'",
          sliderInput("tag_year_range", "Release year range:",
                      min = 1995, max = year_now,
                      value = c(year_now - 9, year_now), sep = "")
        ),
        conditionalPanel(
          "input.map_mode == 'recap'",
          sliderInput("recap_year_range", "Recapture year range:",
                      min = 1995, max = year_now,
                      value = c(year_now - 9, year_now), sep = "")
        ),
        hr(),
        textOutput("map_summary"),
        hr(),
        helpText("Click a circle to see the individual fish at that site.")
      ),
      # Give the map ~70% of the vertical space and the detail
      # table ~30%. fillable=TRUE on page_navbar makes both cards
      # respect these height hints inside layout_sidebar.
      card(
        full_screen = TRUE,
        height = "70vh",
        min_height = "480px",
        leafletOutput("det_map", height = "100%")
      ),
      card(
        height = "30vh",
        min_height = "220px",
        card_header(textOutput("detail_header")),
        DTOutput("detail_table")
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
        helpText("Daily adult Bull Trout passage counts at Wells Dam."),
        helpText("Source: CBR DART (adult_daily, project=WEL)."),
        sliderInput("wells_year_range", "Year range:",
                    min = 2010, max = year_now,
                    value = c(year_now - 4, year_now),
                    step = 1, sep = "")
      ),
      card(
        full_screen = TRUE,
        plotlyOutput("wells_plot", height = "100%")
      ),
      card(DTOutput("wells_table"))
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
        tags$p("Daily PTAGIS pulls + Wells Dam DART counts + ",
               "PTAGIS site metadata committed by a GitHub Action. ",
               "App reads the latest CSVs at runtime."),
        tags$p("Repo: ",
               tags$a(href = sprintf("https://github.com/%s/%s",
                                     GH_OWNER, GH_REPO),
                      sprintf("%s/%s", GH_OWNER, GH_REPO)))
      )
    )
  ),

  nav_spacer(),
  nav_item(textOutput("freshness_badge", inline = TRUE))
)

# ---- Server -------------------------------------------------
server <- function(input, output, session) {
  raw <- load_all_data()

  # Tidied data per mode
  last_det_full <- reactive(tidy_last_detection(raw$interrogation,
                                                raw$sites))
  tagged_full   <- reactive({
    bind_rows(
      tidy_tagging(raw$tagging_methow, raw$sites),
      tidy_tagging(raw$tagging_wells,  raw$sites)
    ) |>
      filter(!is.na(year)) |>
      # Wells-tagged fish appear in BOTH the Methow + Wells
      # exports (Wells sits inside the Methow subbasin).
      # Collapse to one row per unique tagging event.
      distinct(tag_code, mark_date, release_date, site_code, length,
               .keep_all = TRUE)
  })
  recap_full    <- reactive(tidy_recapture(raw$recapture, raw$sites) |>
                              filter(!is.na(year)))

  # Filtered for the current mode + UI inputs
  filtered <- reactive({
    stages <- input$stages %||% character()
    if (length(stages) == 0) return(NULL)

    if (input$map_mode == "last") {
      df <- last_det_full()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      cutoff <- Sys.time() - as.difftime(input$days_back, units = "days")
      df |>
        filter(life_stage %in% stages,
               !is.na(last_obs), last_obs >= cutoff)
    } else if (input$map_mode == "tagged") {
      df <- tagged_full()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df |>
        filter(life_stage %in% stages,
               year >= input$tag_year_range[1],
               year <= input$tag_year_range[2])
    } else {
      df <- recap_full()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df |>
        filter(life_stage %in% stages,
               year >= input$recap_year_range[1],
               year <= input$recap_year_range[2])
    }
  })

  # ---- Map summary text -------------------------------------
  output$map_summary <- renderText({
    df <- filtered()
    if (is.null(df) || nrow(df) == 0) return("No data in this filter.")
    n_total <- nrow(df)
    n_tags  <- length(unique(df$tag_code))
    n_sites <- length(unique(df$site_code))
    label <- switch(input$map_mode,
                    last   = "tags",
                    tagged = "releases",
                    recap  = "recap events")
    sprintf("%s %s, %s unique tag(s), at %s site(s).",
            format(n_total, big.mark = ","), label,
            format(n_tags, big.mark = ","), n_sites)
  })

  # ---- Map ---------------------------------------------------
  bubbles <- reactive({
    df <- filtered()
    if (is.null(df) || nrow(df) == 0) {
      return(tibble(site_code = character(), site_name = character(),
                    latitude = numeric(), longitude = numeric(),
                    n = integer(), n_unique = integer()))
    }
    site_summary(df, count_col = "tag_code")
  })

  output$det_map <- renderLeaflet({
    base <- leaflet() |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") |>
      addLayersControl(baseGroups = c("Topo", "Imagery"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      setView(lng = -120.0, lat = 48.4, zoom = 9)

    df <- bubbles()
    if (nrow(df) == 0) return(base)

    # Graduated symbol size from sqrt(n) so a 100-tag site
    # isn't comically larger than a 1-tag site.
    radius <- pmax(5, pmin(34, sqrt(df$n) * 3.0))

    base |>
      addCircleMarkers(
        data = df,
        lng = ~longitude, lat = ~latitude,
        radius = radius,
        color = "#1f4e79",
        stroke = TRUE, weight = 1, fillColor = "#3da5ff",
        fillOpacity = 0.7,
        layerId = ~site_code,
        label = ~sprintf("%s (%s) - %d", site_name %||% site_code,
                          site_code, n),
        popup = ~sprintf(
          "<b>%s</b><br/>Code: %s<br/>%s: %d<br/>Unique tags: %d",
          ifelse(is.na(site_name) | site_name == "", site_code, site_name),
          site_code,
          ifelse(input$map_mode == "last", "Detections",
                 ifelse(input$map_mode == "tagged", "Releases", "Recaps")),
          n, n_unique)
      )
  })

  # Click → detail
  click_state <- reactiveVal(NULL)
  observeEvent(input$det_map_marker_click, {
    click_state(input$det_map_marker_click$id)
  })
  # Reset detail when mode changes
  observe({
    input$map_mode
    click_state(NULL)
  })

  output$detail_header <- renderText({
    sc <- click_state()
    if (is.null(sc)) return("Click a site on the map to see fish details.")
    df <- filtered()
    name <- if (!is.null(df) && sc %in% df$site_code) {
      first_name <- df |> filter(site_code == sc) |> pull(site_name)
      first_name <- first_name[!is.na(first_name) & nzchar(first_name)][1]
      if (is.na(first_name)) sc else sprintf("%s (%s)", first_name, sc)
    } else sc
    sprintf("Detail: %s", name)
  })

  output$detail_table <- renderDT({
    sc <- click_state()
    df <- filtered()
    if (is.null(sc) || is.null(df)) {
      return(datatable(data.frame(), rownames = FALSE))
    }
    sub <- df |> filter(site_code == sc)
    cols <- switch(input$map_mode,
      last   = c("tag_code", "life_stage", "mark_length",
                 "first_obs", "last_obs", "obs_count",
                 "release_site", "release_date", "species"),
      tagged = c("tag_code", "life_stage", "length",
                 "release_date", "mark_date",
                 "rear_type", "mark_data_project", "species"),
      recap  = c("tag_code", "life_stage", "recap_length",
                 "recap_date", "recap_method",
                 "mark_site", "mark_date", "species")
    )
    sub <- sub |> select(any_of(cols))
    datatable(
      sub,
      options = list(pageLength = 25, scrollX = TRUE,
                     order = list(list(0, "asc"))),
      rownames = FALSE
    )
  })

  # ---- Freshness badge --------------------------------------
  output$freshness_badge <- renderText({
    lu <- raw$last_update
    if (is.null(lu) || nrow(lu) == 0) return("Data: unknown")
    latest <- suppressWarnings(max(as.POSIXct(lu$fetched), na.rm = TRUE))
    if (is.infinite(latest)) return("Data: unknown")
    sprintf("Data updated: %s", format(latest, "%Y-%m-%d %H:%M %Z"))
  })

  # ---- Wells counts -----------------------------------------
  output$wells_plot <- renderPlotly({
    df <- raw$wells_counts
    if (is.null(df) || nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines") |>
               layout(title = "Wells Dam DART feed not yet available"))
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
      return(datatable(data.frame(message = "DART feed not available."),
                       rownames = FALSE))
    }
    datatable(df |> arrange(desc(date)),
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  })

  # ---- Data status table ------------------------------------
  output$last_update_tbl <- renderDT({
    df <- raw$last_update
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(message = "No manifest yet."),
                       rownames = FALSE))
    }
    datatable(df, options = list(pageLength = 10, dom = "t"),
              rownames = FALSE)
  })
}

shinyApp(ui, server)
