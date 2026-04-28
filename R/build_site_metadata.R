# build_site_metadata.R
# -------------------------------------------------------------
# Pull the full PTAGIS site list (interrogation + MRR) and cache
# lat/lon to config/site_metadata.csv. Sites change rarely; a
# daily refresh costs nothing and means new arrays / release
# locations show up automatically.
#
# A small hand-curated config/site_coords_supplemental.csv lives
# alongside this file. The Shiny app reads both and lets the
# supplemental file win on conflicts -- so the app never breaks
# even if a particular API call fails.
# -------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(tibble)
  library(fs)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

root   <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION") |
                                 rprojroot::has_dir(".github"))
out_fp <- path(root, "config", "site_metadata.csv")

# ---- Helper: pull one PTAGIS sites endpoint as a tibble -----
fetch_sites <- function(endpoint) {
  url <- sprintf("https://api.ptagis.org/sites/%s", endpoint)
  message(sprintf("[%s] Fetching %s ...", Sys.time(), url))
  resp <- request(url) |>
    req_user_agent("methow-bt-dashboard (R httr2)") |>
    req_retry(max_tries = 3, backoff = ~ 5) |>
    req_timeout(120) |>
    req_perform()

  body <- resp_body_string(resp)
  # Use jsonlite directly for predictable simplification.
  raw <- jsonlite::fromJSON(body, simplifyVector = TRUE,
                            simplifyDataFrame = TRUE,
                            flatten = TRUE)

  if (!is.data.frame(raw)) {
    stop(sprintf("PTAGIS %s endpoint did not return a JSON array of objects",
                 endpoint))
  }

  df <- tibble::as_tibble(raw)
  message(sprintf("  -> %d rows", nrow(df)))

  # Standardise field names. PTAGIS returns camelCase.
  rename_map <- c(
    site_code = "siteCode",
    site_type = "type",
    name = "name",
    description = "description",
    latitude = "latitude",
    longitude = "longitude",
    rkm = "rkmMask",
    operations_organization_code = "operationsOrganizationCode",
    operations_organization_name = "operationsOrganizationName",
    active = "active",
    operational = "operational"
  )
  for (new in names(rename_map)) {
    old <- rename_map[[new]]
    if (old %in% names(df) && !(new %in% names(df))) {
      df[[new]] <- df[[old]]
    }
  }
  # Some endpoints use `siteType` instead of `type`.
  if (!"site_type" %in% names(df) && "siteType" %in% names(df)) {
    df$site_type <- df$siteType
  }
  # Some use `rkm` directly.
  if (!"rkm" %in% names(df) && "rkm" %in% names(raw)) {
    df$rkm <- raw$rkm
  }

  keep <- intersect(c("site_code", "name", "description", "site_type",
                      "active", "operational",
                      "operations_organization_code",
                      "operations_organization_name",
                      "latitude", "longitude", "rkm"),
                    names(df))
  df <- df[, keep, drop = FALSE]
  df$latitude  <- suppressWarnings(as.numeric(df$latitude))
  df$longitude <- suppressWarnings(as.numeric(df$longitude))
  if (!"rkm" %in% names(df)) df$rkm <- NA_character_
  df$rkm       <- as.character(df$rkm)
  df$source    <- endpoint
  df
}

# ---- Pull both endpoints, with per-endpoint error handling --
sites_list <- list()

for (ep in c("interrogation", "mrr")) {
  out <- tryCatch(fetch_sites(ep),
                  error = function(e) {
                    message(sprintf("  !! %s endpoint failed: %s",
                                    ep, conditionMessage(e)))
                    NULL
                  })
  if (!is.null(out)) sites_list[[ep]] <- out
}

if (length(sites_list) == 0) {
  stop("Both PTAGIS sites endpoints failed; nothing to write.")
}

# Combine; interrogation wins when site_code overlaps (it has
# array-specific names that are most useful for the map).
all_sites <- bind_rows(sites_list) |>
  filter(!is.na(site_code), nzchar(site_code)) |>
  arrange(source != "interrogation") |>      # interrogation first
  distinct(site_code, .keep_all = TRUE)

write_csv(all_sites, out_fp)

cat(sprintf("\nWrote %d sites to %s\n", nrow(all_sites), out_fp))
cat(sprintf("  with coords:  %d\n",
            sum(!is.na(all_sites$latitude) & !is.na(all_sites$longitude))))
cat(sprintf("  by source:\n"))
print(table(all_sites$source, useNA = "always"))
