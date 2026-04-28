# fetch_dart_wells.R
# -------------------------------------------------------------
# Daily pull of Wells Dam adult-passage counts from CBR DART.
# Writes data/wells_dam_counts.csv with one row per date covering
# every year from 2010 through the current year.
#
# Source: CBR DART "Adult Daily Counts" report
#   https://www.cbr.washington.edu/dart/cs/php/rpt/adult_daily.php
# We hit the CSV output endpoint directly with one GET per year.
# The response includes counts for all species at Wells; we keep
# the Bull Trout column (BTrout) plus 10-yr average (BTrout10Yr)
# and water temperature (TempC).
#
# If a year has no data yet (e.g., the current year before the
# spring/summer passage window), DART returns a 302 -> HTML error
# page. We detect that and skip silently.
# -------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr2)
  library(readr)
  library(dplyr)
  library(fs)
  library(glue)
})

root     <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION") |
                                   rprojroot::has_dir(".github"))
data_dir <- path(root, "data")
dir_create(data_dir)
out_fp   <- path(data_dir, "wells_dam_counts.csv")

# ---- DART URL template --------------------------------------
# Verified working for years where Wells has BTrout data.
# `proj=WEL` = Wells Dam. The other params mirror what the
# website's form posts.
DART_URL_TEMPLATE <- paste0(
  "https://www.cbr.washington.edu/dart/cs/php/rpt/adult_daily.php",
  "?sc=1&outputFormat=csv&proj=WEL&span=no",
  "&startdate=1%2F1&enddate=12%2F31&run=&avg=1",
  "&year={year}&syear={year}&eyear={year}"
)

current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- 2010:current_year

# ---- Fetcher ------------------------------------------------
pull_year <- function(yr) {
  url <- glue(DART_URL_TEMPLATE, year = yr)
  message(glue("[{Sys.time()}] DART {yr}: {url}"))

  resp <- request(url) |>
    req_user_agent("methow-bt-dashboard (R httr2)") |>
    req_retry(max_tries = 3, backoff = ~ 5) |>
    req_timeout(120) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  body <- resp_body_string(resp)

  # When DART has no data for a year, redirect chain ends at an
  # HTML error page. Detect by checking for the expected CSV
  # header row.
  if (!grepl("^\\s*Project\\s*,\\s*Date", body)) {
    message(glue("  no DART data for {yr} (likely empty / pre-season)"))
    return(NULL)
  }

  df <- tryCatch(
    read_csv(I(body), show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      message("  parse failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Keep only the columns we use downstream. DART column set is
  # stable but we use any_of to be defensive.
  df <- df |>
    select(any_of(c("Project", "Date",
                    "BTrout", "BTrout10Yr", "TempC"))) |>
    rename_with(tolower)

  if (!"btrout" %in% names(df)) {
    message(glue("  {yr}: no BTrout column in response, skipping"))
    return(NULL)
  }
  if (!"btrout10yr" %in% names(df)) df$btrout10yr <- NA_real_
  if (!"tempc"      %in% names(df)) df$tempc      <- NA_real_

  df |>
    mutate(
      date       = suppressWarnings(as.Date(date)),
      btrout     = suppressWarnings(as.numeric(btrout)),
      btrout10yr = suppressWarnings(as.numeric(btrout10yr)),
      tempc      = suppressWarnings(as.numeric(tempc))
    ) |>
    filter(!is.na(date)) |>
    transmute(
      date,
      project        = "Wells",
      species        = "Bull Trout",
      count          = ifelse(is.na(btrout), 0L, as.integer(btrout)),
      count_10yr_avg = btrout10yr,
      temp_c         = tempc
    )
}

# ---- Main ---------------------------------------------------
all <- bind_rows(lapply(years, function(yr) {
  tryCatch(
    pull_year(yr),
    error = function(e) {
      message(sprintf("  !! year %d failed: %s", yr, conditionMessage(e)))
      NULL
    })
}))

if (is.null(all) || nrow(all) == 0) {
  message("DART returned no data for any year; writing empty stub.")
  all <- tibble(date = as.Date(character()),
                project = character(),
                species = character(),
                count = integer(),
                count_10yr_avg = numeric(),
                temp_c = numeric())
}

write_csv(all, out_fp)
cat(sprintf("\nWrote %d rows (%d distinct years) to %s\n",
            nrow(all),
            length(unique(format(all$date, "%Y"))),
            out_fp))
