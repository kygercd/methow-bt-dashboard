# build_site_metadata.R
# -------------------------------------------------------------
# Pull the full PTAGIS interrogation-site list (lat/lon, name,
# rkm, organization) and cache it to config/site_metadata.csv.
#
# Run by the daily GitHub Action. Sites change rarely, but a
# daily refresh costs nothing and means new arrays show up
# automatically.
# -------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr2)
  library(dplyr)
  library(readr)
  library(fs)
  library(janitor)
})

root        <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION") |
                                      rprojroot::has_dir(".github"))
out_fp      <- path(root, "config", "site_metadata.csv")

message(sprintf("[%s] Fetching PTAGIS interrogation sites ...", Sys.time()))

resp <- request("https://api.ptagis.org/sites/interrogation") |>
  req_user_agent("methow-bt-dashboard (R httr2)") |>
  req_retry(max_tries = 3, backoff = ~ 5) |>
  req_timeout(120) |>
  req_perform()

sites <- resp_body_json(resp, simplifyVector = TRUE) |>
  as_tibble() |>
  janitor::clean_names() |>
  select(any_of(c(
    "site_code", "name", "description", "site_type",
    "active", "operational",
    "operations_organization_code", "operations_organization_name",
    "latitude", "longitude", "rkm",
    "first_year", "last_year", "first_date", "last_date",
    "operation_period",
    "last_tag_code", "last_tag_time"
  ))) |>
  mutate(
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude),
    rkm       = as.character(rkm)
  )

# We also want MRR / release sites. Pull those too.
message("Fetching PTAGIS MRR sites (release / tagging locations) ...")
mrr_resp <- request("https://api.ptagis.org/sites/mrr") |>
  req_user_agent("methow-bt-dashboard (R httr2)") |>
  req_retry(max_tries = 3, backoff = ~ 5) |>
  req_timeout(120) |>
  req_perform()

mrr <- tryCatch({
  resp_body_json(mrr_resp, simplifyVector = TRUE) |>
    as_tibble() |>
    janitor::clean_names() |>
    select(any_of(c("site_code", "name", "latitude", "longitude", "rkm",
                    "site_type", "operations_organization_name"))) |>
    mutate(
      latitude  = as.numeric(latitude),
      longitude = as.numeric(longitude),
      rkm       = as.character(rkm)
    )
}, error = function(e) {
  message("  !! couldn't load MRR sites: ", conditionMessage(e))
  NULL
})

# Combine; interrogation sites win when site_code overlaps.
all_sites <- if (!is.null(mrr)) {
  bind_rows(
    sites |> mutate(source = "interrogation"),
    mrr   |> mutate(source = "mrr") |>
      filter(!site_code %in% sites$site_code)
  )
} else {
  sites |> mutate(source = "interrogation")
}

write_csv(all_sites, out_fp)

cat(sprintf("\nWrote %d sites to %s\n",
            nrow(all_sites), out_fp))
cat(sprintf("  with coords: %d\n",
            sum(!is.na(all_sites$latitude) & !is.na(all_sites$longitude))))
