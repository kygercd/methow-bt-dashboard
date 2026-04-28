# fetch_dart_wells.R
# -------------------------------------------------------------
# Daily pull of Wells Dam adult-passage counts from CBR DART.
# Writes data/wells_dam_counts.csv (one row per date+species).
#
# STATUS: PLACEHOLDER. The CBR DART form-based query system
# does not expose a stable direct CSV URL for adult_daily.
# To finalise this script you need to either:
#
#   (a) Submit the form once at
#       https://www.cbr.washington.edu/dart/query/adult_daily
#       with Project=WEL-Wells, output=CSV, and inspect the
#       browser network tab for the resulting CSV URL pattern.
#       Then paste it into DART_URL_TEMPLATE below.
#
#   (b) Or use the cbrshare R package (if available) which
#       wraps DART queries:
#         remotes::install_github("Columbia-Basin-Research/cbrshare")
#         dart_pull <- cbrshare::adult_daily(proj="WEL", year=YYYY)
#
# Until either is in place, this script logs that DART is not
# yet wired in and exits cleanly. The PTAGIS pull continues.
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

# ---- TODO: paste actual CSV URL here once verified ----------
DART_URL_TEMPLATE <- NA_character_   # e.g. "https://.../adult_daily.csv?proj=WEL&year={year}&species=All"

if (is.na(DART_URL_TEMPLATE)) {
  message("[fetch_dart_wells.R] DART URL not configured yet - skipping.")
  message("  See script header for how to finalise.")
  # Write an empty placeholder so the app's reactive file readers don't error.
  if (!file_exists(out_fp)) {
    tibble(date = as.Date(character()),
           project = character(),
           species = character(),
           count = integer()) |>
      write_csv(out_fp)
    message(glue("  wrote empty stub: {out_fp}"))
  }
  quit(status = 0)
}

current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- (current_year - 4):current_year   # last 5 years rolling

pull_year <- function(yr) {
  url <- glue(DART_URL_TEMPLATE, year = yr)
  message(glue("[{Sys.time()}] DART {yr}: {url}"))
  resp <- request(url) |>
    req_user_agent("methow-bt-dashboard (R httr2)") |>
    req_retry(max_tries = 3, backoff = ~ 5) |>
    req_timeout(120) |>
    req_perform()
  read_csv(resp_body_string(resp), show_col_types = FALSE)
}

all <- bind_rows(lapply(years, function(yr) {
  tryCatch(pull_year(yr),
           error = function(e) {
             message(sprintf("  !! year %d failed: %s", yr, conditionMessage(e)))
             NULL
           })
}))

write_csv(all, out_fp)
message(sprintf("\nWrote %d rows to %s", nrow(all), out_fp))
