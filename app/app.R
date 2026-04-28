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

# Standard date display: MM-DD-YYYY. Used everywhere a date
# (or datetime - the time component is dropped) is shown in
# a DT or a label.
fmt_date <- function(x) {
  if (is.null(x)) return(character())
  d <- suppressWarnings(as.Date(x))
  out <- format(d, "%m-%d-%Y")
  out[is.na(d)] <- ""
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

# ---- Subbasin classifier ------------------------------------
# Site codes are mapped to one of:
#   Methow, Wells Dam, Entiat, Wenatchee, Okanogan,
#   Columbia mainstem, Other.
# This is a pragmatic prefix-based mapping. Edit the lookup
# tables below as new sites appear in the data.
SUBBASIN_ORDER <- c("Methow", "Entiat", "Wenatchee",
                    "Okanogan", "Columbia mainstem", "Other")

# True for any PTAGIS site at Wells Dam (ladders, forebay,
# tailrace, hatchery channel). Used to flag fish that touched
# Wells Dam even though Wells Dam is grouped with the broader
# Columbia mainstem subbasin in classify_subbasin().
WELLS_DAM_CODES <- c("WEL", "WELLD1", "WELLD2", "WELTAL",
                     "WELFBY", "WELH",
                     "WEA", "WEH", "WEHC", "WL1", "WL2", "WLB")
is_wells_site <- function(site_code) {
  s <- toupper(as.character(site_code))
  s[is.na(s)] <- ""
  startsWith(s, "WEL") | s %in% WELLS_DAM_CODES
}

classify_subbasin <- function(site_code) {
  s <- toupper(as.character(site_code))
  s[is.na(s) | !nzchar(s)] <- NA_character_

  # Explicit code lists (short codes that don't follow a clean prefix).
  methow_codes <- c("TWR", "MRC", "MRW", "MRB", "MRT", "LMR",
                    "GLC", "BVC", "WFC", "CWT", "LBT", "LBC",
                    "LOR", "EMC", "HSU", "HSM", "HSL", "MTP",
                    "LIBBYC", "SGOLDC",
                    # Chewuch arrays + acclimation ponds
                    "CRW", "CRU", "CML", "CMU",
                    # Methow side channels & ponds
                    "18N", "LTP", "MSB", "MEM",
                    "3D1", "3D2", "3D3", "3D4",
                    # Spring Creek Acclimation Pond (Methow)
                    "SCP")
  wells_codes  <- c("WEA", "WEH", "WEHC", "WL1", "WL2", "WLB")
  entiat_codes <- c("ENL", "ENF", "ENA", "ENM", "ENS", "EWC")
  wenatchee_codes <- c("LWE", "UWE", "CHL", "CHU", "WTL", "LWN",
                       "MWE", "MWF", "MSH", "MCL",
                       "TUF", "NAL", "NAU",
                       "ICL", "ICM", "IC5",
                       "PEU",
                       "NAPEEC", "SILVEC", "TILLIC", "ETIENC",
                       "LEAV", "DRY",
                       # Chiwaukum Creek (Wenatchee trib) and
                       # neighbours coded with 3-letter shortforms
                       "CHW",
                       # Chiwawa-area arrays + ponds
                       "ROCK3C", "CCT", "CHIP", "UCT",
                       # Tyee Channel (TY1..TY4)
                       "TY1", "TY2", "TY3", "TY4",
                       # Leavenworth NFH ladder
                       "LNF",
                       # East Bank Hatchery (Wenatchee mainstem)
                       "EBO",
                       # Tillicum Creek temporary array
                       "TLT",
                       # Butcher Creek + Blackbird Pond (per user)
                       "BCP", "BBP")
  okanogan_codes <- c("OKL")
  cmain_codes    <- c("RIA", "RIS", "RRF", "RRJ", "RREBYP",
                      "MCN", "BON", "JDA", "TDA",
                      # Foster Creek + Chelan PUD facilities
                      # (Columbia mainstem near Wells / Chief Joseph)
                      "FST", "PD2", "CHEL")

  dplyr::case_when(
    is.na(s)                                       ~ NA_character_,
    # Methow + tributaries.
    # MET prefix catches METHR (Methow R), METTRP (Methow trap),
    # METRWT (Methow R weir), and any future MET* sites in this
    # subbasin. This is safe in our data because no non-Methow
    # PTAGIS site in the upper Columbia uses a MET prefix.
    stringr::str_starts(s, "MET")                  ~ "Methow",
    stringr::str_starts(s, "TWISP")                ~ "Methow",
    stringr::str_starts(s, "TWIT")                 ~ "Methow",
    stringr::str_starts(s, "CHEW")                 ~ "Methow",
    stringr::str_starts(s, "GOLD")                 ~ "Methow",
    stringr::str_starts(s, "BEAV")                 ~ "Methow",
    stringr::str_starts(s, "WOLF")                 ~ "Methow",
    stringr::str_starts(s, "FOG")                  ~ "Methow",
    stringr::str_starts(s, "LOST")                 ~ "Methow",
    stringr::str_starts(s, "EIGHT")                ~ "Methow",
    stringr::str_starts(s, "HANS")                 ~ "Methow",
    stringr::str_starts(s, "CHER")                 ~ "Methow",
    stringr::str_starts(s, "LBRI")                 ~ "Methow",
    s %in% methow_codes                            ~ "Methow",
    # Wells Dam is part of the Columbia mainstem. is_wells_site()
    # is the separate predicate used to flag tags that have been
    # tagged or detected at Wells (see Migratory Fish tab).
    stringr::str_starts(s, "WEL")                  ~ "Columbia mainstem",
    s %in% wells_codes                             ~ "Columbia mainstem",
    # Entiat
    stringr::str_starts(s, "ENT")                  ~ "Entiat",
    stringr::str_starts(s, "MAD")                  ~ "Entiat",
    s %in% entiat_codes                            ~ "Entiat",
    # Wenatchee + tributaries
    stringr::str_starts(s, "CHIW")                 ~ "Wenatchee",
    stringr::str_starts(s, "CHIK")                 ~ "Wenatchee",
    stringr::str_starts(s, "TUM")                  ~ "Wenatchee",
    stringr::str_starts(s, "NAS")                  ~ "Wenatchee",
    stringr::str_starts(s, "ICI")                  ~ "Wenatchee",
    stringr::str_starts(s, "PES")                  ~ "Wenatchee",
    stringr::str_starts(s, "WEN")                  ~ "Wenatchee",
    stringr::str_starts(s, "WHIT")                 ~ "Wenatchee",
    stringr::str_starts(s, "LEAV")                 ~ "Wenatchee",
    stringr::str_starts(s, "DRY")                  ~ "Wenatchee",
    s %in% wenatchee_codes                         ~ "Wenatchee",
    # Okanogan
    stringr::str_starts(s, "OKA")                  ~ "Okanogan",
    s %in% okanogan_codes                          ~ "Okanogan",
    # Other Columbia mainstem dams / bypass arrays
    s %in% cmain_codes                             ~ "Columbia mainstem",
    stringr::str_starts(s, "PRD")                  ~ "Columbia mainstem",
    TRUE                                           ~ "Other"
  )
}

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

  wells_counts <- load_csv("data/wells_dam_counts.csv")
  if (!is.null(wells_counts) && nrow(wells_counts) > 0) {
    # Known DART data error: 2024-10-28 reports 9 fish in
    # the middle of a long run of zeros. Drop it.
    wells_counts <- wells_counts |>
      mutate(date = suppressWarnings(as.Date(date))) |>
      filter(is.na(date) | date != as.Date("2024-10-28"))
  }

  list(
    tagging_methow      = load_csv("data/methow_basin_bt_tagging.csv"),
    tagging_wells       = load_csv("data/wells_dam_bt_tagging.csv"),
    recapture           = load_csv("data/upper_columbia_bt_recapture.csv"),
    interrogation       = load_csv("data/upper_columbia_bt_interrogation.csv"),
    wells_counts        = wells_counts,
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

  # ---- Migratory Fish tab -----------------------------------
  nav_panel(
    title = "Migratory Fish",
    layout_sidebar(
      sidebar = sidebar(
        title = "Migratory filters",
        width = 320,
        helpText("Fish whose journey spans 2+ subbasins ",
                 "(based on tagging, detection, and recapture ",
                 "sites). Wells Dam is part of the Columbia ",
                 "mainstem subbasin; the Wells column flags ",
                 "any tag actually detected at Wells Dam."),
        checkboxGroupInput(
          "mig_stages", "Life stage at tagging:",
          choices  = LIFE_STAGE_LEVELS,
          selected = c("Adult", "Sub-adult")
        ),
        checkboxInput("mig_wells_only",
                      "Only fish detected at Wells Dam",
                      value = FALSE),
        sliderInput("mig_year_range",
                    "Tagging year range:",
                    min = 1995, max = year_now,
                    value = c(1995, year_now),
                    step = 1, sep = ""),
        hr(),
        textOutput("mig_summary")
      ),
      card(
        card_header("Migratory fish (one row per tag)"),
        DTOutput("mig_table")
      )
    )
  ),

  # ---- Wells counts tab -------------------------------------
  nav_panel(
    title = "Wells Dam counts",
    layout_sidebar(
      sidebar = sidebar(
        title = "Wells Dam adult passage",
        width = 300,
        helpText("Daily adult Bull Trout passage counts at Wells Dam."),
        helpText("Source: CBR DART (adult_daily, project=WEL)."),
        sliderInput("wells_year_range", "Year range (plots):",
                    min = 2010, max = year_now,
                    value = c(2010, year_now),
                    step = 1, sep = ""),
        hr(),
        selectInput("wells_table_year", "Table year:",
                    choices = seq(year_now, 2010),
                    selected = year_now)
      ),
      card(
        card_header("Annual totals"),
        plotlyOutput("wells_annual_plot", height = "100%")
      ),
      card(
        card_header("Seasonal daily counts (overlay by year)"),
        plotlyOutput("wells_seasonal_plot", height = "100%")
      ),
      card(
        card_header(textOutput("wells_table_header")),
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
  # Tag -> mark length lookup (built from both tagging files,
  # one row per tag). Used to attach the original tagging
  # length onto recapture rows.
  tag_marklen <- reactive({
    tag_src <- bind_rows(raw$tagging_methow, raw$tagging_wells)
    if (is.null(tag_src) || nrow(tag_src) == 0) {
      return(tibble(tag_code = character(), mark_length = numeric()))
    }
    tag_src |>
      transmute(tag_code   = tag,
                mark_length = suppressWarnings(as.numeric(length))) |>
      filter(!is.na(tag_code), nzchar(tag_code)) |>
      distinct(tag_code, .keep_all = TRUE)
  })

  recap_full    <- reactive({
    tidy_recapture(raw$recapture, raw$sites) |>
      filter(!is.na(year)) |>
      left_join(tag_marklen(), by = "tag_code")
  })

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
                 "last_obs", "release_site", "release_date"),
      tagged = c("tag_code", "life_stage", "length",
                 "release_date"),
      recap  = c("tag_code", "life_stage", "recap_length",
                 "recap_date", "recap_method",
                 "mark_site", "mark_date", "mark_length")
    )
    sub <- sub |> select(any_of(cols))
    # MM-DD-YYYY display for any date column.
    date_cols <- intersect(c("last_obs", "release_date",
                             "mark_date", "recap_date"),
                           names(sub))
    for (cc in date_cols) sub[[cc]] <- fmt_date(sub[[cc]])
    datatable(
      sub,
      options = list(pageLength = 25, scrollX = TRUE,
                     order = list(list(0, "asc"))),
      rownames = FALSE
    )
  })

  # ---- Migratory Fish ---------------------------------------
  # Build one long table of (tag_code, site_code, event_kind,
  # event_date) drawn from tagging release/mark, interrogation
  # detection, and recapture mark/release/recap. Map every
  # site_code to a subbasin and aggregate per tag.
  mig_journey <- reactive({
    pieces <- list()

    add_tag <- function(df, src) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      tibble(
        tag_code  = df$tag,
        site_code = extract_site_code(df$release_site),
        event     = paste0("release_", src),
        event_dt  = parse_dt(df$release_date)
      )
    }
    add_mark <- function(df, src) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      tibble(
        tag_code  = df$tag,
        site_code = extract_site_code(df$mark_site),
        event     = paste0("mark_", src),
        event_dt  = parse_dt(df$mark_date)
      )
    }

    pieces$rel_methow  <- add_tag (raw$tagging_methow, "methow")
    pieces$mark_methow <- add_mark(raw$tagging_methow, "methow")
    pieces$rel_wells   <- add_tag (raw$tagging_wells,  "wells")
    pieces$mark_wells  <- add_mark(raw$tagging_wells,  "wells")

    if (!is.null(raw$interrogation) && nrow(raw$interrogation) > 0) {
      pieces$detect <- tibble(
        tag_code  = raw$interrogation$tag,
        site_code = extract_site_code(raw$interrogation$site),
        event     = "detection",
        event_dt  = parse_dt(raw$interrogation$last_time)
      )
    }
    if (!is.null(raw$recapture) && nrow(raw$recapture) > 0) {
      pieces$recap_at <- tibble(
        tag_code  = raw$recapture$tag,
        site_code = extract_site_code(raw$recapture$recap_site),
        event     = "recap",
        event_dt  = parse_dt(raw$recapture$recap_date)
      )
      pieces$recap_rel <- tibble(
        tag_code  = raw$recapture$tag,
        site_code = extract_site_code(raw$recapture$recap_release_site),
        event     = "recap_release",
        event_dt  = parse_dt(raw$recapture$recap_release_date)
      )
    }

    j <- bind_rows(pieces) |>
      filter(!is.na(tag_code), nzchar(tag_code),
             !is.na(site_code), nzchar(site_code))
    if (nrow(j) == 0) return(j)

    j |>
      mutate(subbasin = classify_subbasin(site_code))
  })

  # One row per tag with summary stats (visited subbasins, sites,
  # date range, life stage from tagging length).
  migratory_tbl <- reactive({
    j <- mig_journey()
    if (is.null(j) || nrow(j) == 0) {
      return(tibble(
        tag_code = character(), life_stage = character(),
        mark_subbasin = character(), mark_site = character(),
        mark_date = as.POSIXct(character()),
        mark_length = numeric(),
        n_subbasins = integer(), subbasins = character(),
        n_sites = integer(),
        first_event = as.POSIXct(character()),
        last_event  = as.POSIXct(character()),
        touched_wells = logical()
      ))
    }

    # Per-tag tagging info: prefer Methow file row (it carries
    # the same data, plus everything not in Wells), then Wells.
    tag_src <- bind_rows(raw$tagging_methow, raw$tagging_wells)
    if (!is.null(tag_src) && nrow(tag_src) > 0) {
      tag_info <- tag_src |>
        transmute(
          tag_code      = tag,
          mark_length   = suppressWarnings(as.numeric(length)),
          mark_date_raw = parse_dt(mark_date),
          mark_site_raw = extract_site_code(mark_site)
        ) |>
        filter(!is.na(tag_code), nzchar(tag_code)) |>
        distinct(tag_code, .keep_all = TRUE) |>
        mutate(life_stage    = life_stage(mark_length),
               mark_subbasin = classify_subbasin(mark_site_raw))
    } else {
      tag_info <- tibble(tag_code = character())
    }

    per_tag <- j |>
      mutate(at_wells = is_wells_site(site_code)) |>
      group_by(tag_code) |>
      summarise(
        subbasins      = paste(sort(unique(subbasin)), collapse = ", "),
        n_subbasins    = dplyr::n_distinct(subbasin),
        n_sites        = dplyr::n_distinct(site_code),
        first_event    = suppressWarnings(min(event_dt, na.rm = TRUE)),
        last_event     = suppressWarnings(max(event_dt, na.rm = TRUE)),
        detected_wells = any(at_wells),
        .groups = "drop"
      ) |>
      mutate(
        first_event = if_else(is.infinite(first_event),
                              as.POSIXct(NA), first_event),
        last_event  = if_else(is.infinite(last_event),
                              as.POSIXct(NA),  last_event)
      ) |>
      left_join(tag_info, by = "tag_code")

    # Migratory definition:
    #   (a) detected/recorded in 2+ subbasins, OR
    #   (b) touched Wells Dam AND any non-Wells site.
    # Both reduce to n_subbasins >= 2 (because Wells Dam is
    # itself a subbasin in our classifier), so this is the
    # single, unambiguous filter.
    per_tag |>
      filter(n_subbasins >= 2) |>
      arrange(desc(last_event))
  })

  output$mig_summary <- renderText({
    df <- migratory_tbl()
    stages <- input$mig_stages %||% character()
    if (is.null(df) || nrow(df) == 0) return("No migratory fish.")
    df <- df |>
      filter((life_stage %in% stages) | length(stages) == 0,
             lubridate::year(mark_date_raw) >= input$mig_year_range[1] |
               is.na(mark_date_raw),
             lubridate::year(mark_date_raw) <= input$mig_year_range[2] |
               is.na(mark_date_raw))
    if (isTRUE(input$mig_wells_only)) df <- df |> filter(detected_wells)
    sprintf("%s migratory fish, %s subbasin pair(s) represented.",
            format(nrow(df), big.mark = ","),
            format(length(unique(df$subbasins)), big.mark = ","))
  })

  output$mig_table <- renderDT({
    df <- migratory_tbl()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(message = "No migratory fish."),
                       rownames = FALSE))
    }
    stages <- input$mig_stages %||% character()
    df <- df |>
      filter(length(stages) == 0 | life_stage %in% stages,
             is.na(mark_date_raw) |
               (lubridate::year(mark_date_raw) >= input$mig_year_range[1] &
                lubridate::year(mark_date_raw) <= input$mig_year_range[2]))
    if (isTRUE(input$mig_wells_only)) df <- df |> filter(detected_wells)

    out <- df |>
      transmute(
        tag_code,
        life_stage,
        mark_length,
        mark_date     = fmt_date(mark_date_raw),
        mark_site     = mark_site_raw,
        mark_subbasin,
        subbasins,
        n_subbasins,
        n_sites,
        first_event   = fmt_date(first_event),
        last_event    = fmt_date(last_event),
        detected_wells
      )

    datatable(
      out,
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE,
      colnames = c("Tag", "Life stage", "Mark length (mm)",
                   "Mark date", "Mark site", "Mark subbasin",
                   "Subbasins visited", "# subbasins",
                   "# sites", "First event", "Last event",
                   "Detected at Wells Dam")
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

  # Tidy + annotate Wells counts once. Data is already cleaned
  # (2024-10-28 dropped) by load_all_data().
  wells_clean <- reactive({
    df <- raw$wells_counts
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df |>
      mutate(date  = as.Date(date),
             count = suppressWarnings(as.numeric(count)),
             year  = lubridate::year(date),
             md    = format(date, "%m-%d"),
             doy   = lubridate::yday(date)) |>
      filter(!is.na(date), !is.na(count))
  })

  # Annual totals: bar plot, one bar per year in range.
  output$wells_annual_plot <- renderPlotly({
    df <- wells_clean()
    if (is.null(df)) {
      return(plotly_empty(type = "bar") |>
               layout(title = "Wells Dam DART feed not yet available"))
    }
    df <- df |>
      filter(year >= input$wells_year_range[1],
             year <= input$wells_year_range[2]) |>
      group_by(year) |>
      summarise(total = sum(count, na.rm = TRUE), .groups = "drop")
    plot_ly(df, x = ~year, y = ~total, type = "bar",
            marker = list(color = "#1f4e79")) |>
      layout(yaxis = list(title = "Bull Trout (total / year)"),
             xaxis = list(title = "", dtick = 1),
             margin = list(l = 60, r = 20, t = 20, b = 40))
  })

  # Seasonal: x = day-of-year (anchored to 2000 so plotly
  # shows month tick labels), one line per year, colored.
  output$wells_seasonal_plot <- renderPlotly({
    df <- wells_clean()
    if (is.null(df)) {
      return(plotly_empty(type = "scatter", mode = "lines") |>
               layout(title = "Wells Dam DART feed not yet available"))
    }
    df <- df |>
      filter(year >= input$wells_year_range[1],
             year <= input$wells_year_range[2]) |>
      mutate(season_x = as.Date(paste0("2000-", md))) |>
      arrange(year, season_x)
    plot_ly(df, x = ~season_x, y = ~count, color = ~as.factor(year),
            type = "scatter", mode = "lines") |>
      layout(yaxis = list(title = "Bull Trout / day"),
             xaxis = list(title = "",
                          tickformat = "%b",
                          dtick = "M1"),
             legend = list(title = list(text = "Year")),
             margin = list(l = 60, r = 20, t = 20, b = 40))
  })

  # Table: daily counts for the selected year + a 10-year
  # historical average computed for each calendar day from
  # the previous 10 years' data. Columns: Date, Count, 10-yr avg.
  wells_table_data <- reactive({
    df <- wells_clean()
    yr <- suppressWarnings(as.integer(input$wells_table_year))
    if (is.null(df) || is.na(yr)) return(NULL)

    prior <- df |>
      filter(year < yr, year >= yr - 10) |>
      group_by(md) |>
      summarise(avg10 = mean(count, na.rm = TRUE), .groups = "drop")

    df |>
      filter(year == yr) |>
      left_join(prior, by = "md") |>
      arrange(date) |>
      transmute(
        Date          = fmt_date(date),
        Count         = count,
        `10-yr avg`   = round(avg10, 2)
      )
  })

  output$wells_table_header <- renderText({
    sprintf("Daily counts for %s", input$wells_table_year %||% "")
  })

  output$wells_table <- renderDT({
    df <- wells_table_data()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(message = "No data for this year."),
                       rownames = FALSE))
    }
    datatable(
      df,
      options = list(pageLength = 15, scrollX = TRUE,
                     order = list(list(0, "asc"))),
      rownames = FALSE
    )
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
