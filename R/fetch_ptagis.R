# fetch_ptagis.R
# -------------------------------------------------------------
# Daily pull of Methow / Upper Columbia Bull Trout data from
# PTAGIS scheduled exports.
#
# PTAGIS-side setup (one-time, per query):
#   1. Build the saved query in Advanced Reporting.
#   2. Subscribe To -> File (CSV, daily). PTAGIS will write
#      the file to your user's file store on a schedule.
#   3. Add the query to config/queries.yml with the exact
#      filename PTAGIS uses.
#
# This script then retrieves each exported file from
#   https://api.ptagis.org/reporting/reports/<user>/file/<filename>
# and writes it to data/<name>.csv (re-encoded to UTF-8 for
# convenience downstream).
#
# Note on encoding: PTAGIS exports CSV as UTF-16 LE with a BOM
# (the SAP BusinessObjects default). We detect the BOM and
# re-encode to UTF-8 before writing.
#
# No PTAGIS credentials are required - the scheduled export
# files are served openly via the API. (If that ever changes,
# set env vars PTAGIS_USER / PTAGIS_PASS and the script will
# add them as basic auth automatically.)
# -------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr2)
  library(readr)
  library(dplyr)
  library(yaml)
  library(fs)
  library(glue)
  library(janitor)
})

# ---- Paths --------------------------------------------------
root       <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION") |
                                     rprojroot::has_dir(".github"))
config_fp  <- path(root, "config", "queries.yml")
data_dir   <- path(root, "data")
raw_dir    <- path(data_dir, "raw")
dir_create(raw_dir)

# ---- Config -------------------------------------------------
cfg <- yaml::read_yaml(config_fp)
stopifnot(!is.null(cfg$queries), length(cfg$queries) > 0,
          !is.null(cfg$ptagis_user), nzchar(cfg$ptagis_user))

ptagis_user <- cfg$ptagis_user
base_url    <- "https://api.ptagis.org/reporting/reports"

# ---- Optional credentials -----------------------------------
user <- Sys.getenv("PTAGIS_USER")
pass <- Sys.getenv("PTAGIS_PASS")
use_auth <- nchar(user) > 0 && nchar(pass) > 0

# ---- BOM-aware encoding detection ---------------------------
detect_encoding <- function(bytes) {
  if (length(bytes) >= 2 &&
      bytes[1] == as.raw(0xFF) && bytes[2] == as.raw(0xFE)) {
    return("UTF-16LE")
  }
  if (length(bytes) >= 2 &&
      bytes[1] == as.raw(0xFE) && bytes[2] == as.raw(0xFF)) {
    return("UTF-16BE")
  }
  if (length(bytes) >= 3 &&
      bytes[1] == as.raw(0xEF) && bytes[2] == as.raw(0xBB) &&
      bytes[3] == as.raw(0xBF)) {
    return("UTF-8")           # explicit BOM
  }
  "UTF-8"                       # default assumption
}

# ---- Fetch helper -------------------------------------------
fetch_report_file <- function(q, ptagis_user, use_auth, user, pass) {
  url <- sprintf("%s/%s/file/%s",
                 base_url,
                 utils::URLencode(ptagis_user),
                 utils::URLencode(q$filename, reserved = TRUE))

  message(glue("[{Sys.time()}] Fetching '{q$name}'"))
  message(glue("  URL: {url}"))

  req <- request(url) |>
    req_user_agent("methow-bt-dashboard (R httr2)") |>
    req_retry(max_tries = 3, backoff = ~ 5) |>
    req_timeout(600) |>
    req_error(is_error = function(resp) FALSE)   # we'll handle status

  if (use_auth) req <- req_auth_basic(req, user, pass)

  resp <- req_perform(req)
  status <- resp_status(resp)

  if (status == 404) {
    message("  !! 404 - file does not exist on PTAGIS yet. ",
            "Run the subscription on PTAGIS or wait for the next schedule.")
    return(NULL)
  }
  if (status >= 400) {
    message(glue("  !! HTTP {status}: {resp_status_desc(resp)}"))
    return(NULL)
  }

  bytes <- resp_body_raw(resp)
  enc   <- detect_encoding(bytes)
  message(glue("  encoding detected: {enc}"))

  # Write raw bytes to a tempfile and let readr decode it.
  tf <- tempfile(fileext = ".csv")
  writeBin(bytes, tf)
  on.exit(unlink(tf), add = TRUE)

  df <- readr::read_csv(
    tf,
    locale          = readr::locale(encoding = enc),
    show_col_types  = FALSE,
    progress        = FALSE
  ) |>
    janitor::clean_names()

  message(glue("  -> {nrow(df)} rows, {ncol(df)} columns"))
  df
}

# ---- Main loop ----------------------------------------------
stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
summary_rows <- list()

for (q in cfg$queries) {
  out <- tryCatch(
    fetch_report_file(q, ptagis_user, use_auth, user, pass),
    error = function(e) {
      message("  !! failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(out)) next

  latest_fp  <- path(data_dir, glue("{q$name}.csv"))
  archive_fp <- path(raw_dir, glue("{q$name}_{stamp}.csv"))
  # Always write UTF-8 so downstream tools (Shiny, git diffs) play nice.
  write_csv(out, latest_fp)
  write_csv(out, archive_fp)

  summary_rows[[q$name]] <- tibble(
    query   = q$name,
    rows    = nrow(out),
    cols    = ncol(out),
    fetched = Sys.time()
  )
}

manifest <- bind_rows(summary_rows)
write_csv(manifest, path(data_dir, "last_update.csv"))

cat("\nDone.\n")
print(manifest)
