# Methow Bull Trout Dashboard

Live dashboard of PIT-tag data for Bull Trout in the Methow
subbasin, Wells Dam, and the Twisp River weir, plus Wells Dam
adult-passage counts. PTAGIS data is refreshed daily by a
GitHub Action; the Shiny app reads the committed CSVs.

```
methow-bt-dashboard/
├── .github/workflows/daily-pull.yml   # cron -> R/fetch_*.R
├── R/
│   ├── fetch_ptagis.R                 # daily PTAGIS exports
│   ├── build_site_metadata.R          # PTAGIS sites (lat/lon)
│   └── fetch_dart_wells.R             # Wells Dam counts (placeholder)
├── config/
│   ├── queries.yml                    # PTAGIS user + filenames
│   └── site_metadata.csv              # auto-generated
├── data/                              # auto-updated CSVs
│   └── raw/                           # timestamped archive
├── app/
│   ├── app.R                          # Shiny dashboard
│   └── deploy.R                       # shinyapps.io deploy helper
└── DESCRIPTION
```

## How the data pipeline works

PTAGIS's Advanced Reporting (SAP BusinessObjects) doesn't give
saved queries a direct CSV download URL. The supported pattern
is:

1. **On PTAGIS:** save your query, then *Subscribe To -> File*
   to schedule it to write a CSV to your user's file store on a
   recurring basis.
2. **PTAGIS serves the file** at:
   `https://api.ptagis.org/reporting/reports/<user>/file/<filename>.csv`
3. **The GitHub Action** hits that URL daily, saves the CSV in
   the repo, and commits. The Shiny app reads those CSVs.

Site coordinates come from `https://api.ptagis.org/sites/...` and
are cached daily to `config/site_metadata.csv`. Wells Dam adult
Bull Trout counts come from CBR DART's Adult Daily Counts CSV
endpoint (`R/fetch_dart_wells.R`), one GET per year, 2010 through
the current year. Years with no data yet return an HTML error
page from DART; the script detects that and skips silently.

A bundled `data/wells_dam_bt_events_history.csv` holds historical
individual-passage records (ladder side + fish size) for
reference; the dashboard uses the aggregated daily counts.

## One-time setup

### 1. Build saved queries in PTAGIS

Log into <https://www.ptagis.org/> and open **Advanced Reporting**.
Currently configured queries (already in `config/queries.yml`):

- `MethowBasinBullTroutTagging` -- Tagging Details, Bull Trout,
  Methow subbasin
- `WellsDamBullTroutTagging` -- Tagging Details, Bull Trout,
  Wells Dam
- `Upper Columbia Bull Trout Recapture Detail` -- Recapture
  Detail, Upper Columbia Bull Trout
- `Upper Columbia Bull Trout Interrogation Summary` --
  Interrogation Summary, Upper Columbia Bull Trout

For each query: **Subscribe To -> File**, format **CSV** (single
file), frequency **Daily** at e.g. 02:00 PT.

### 2. Edit `config/queries.yml`

Set `ptagis_user` to the PTAGIS account that owns the
subscriptions. Each `filename` must match exactly what PTAGIS
writes (capitalisation and spaces matter).

### 3. (Optional) PTAGIS credentials

Scheduled-export files are served openly via the API, so no auth
is needed. If that ever changes, add `PTAGIS_USER` and
`PTAGIS_PASS` as repo secrets and the script will use them.

### 4. Push to GitHub and trigger the first run

In **Actions -> Daily PTAGIS pull -> Run workflow**. After it
succeeds, CSVs appear in `data/` committed by
`github-actions[bot]`. From then on it runs automatically every
day at 04:00 PT.

## Running the app locally

From R at the project root:

```r
# install deps once
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "readr",
                   "stringr", "lubridate", "leaflet", "plotly",
                   "DT", "rprojroot"))

shiny::runApp("app")
```

The app loads CSVs from `data/` if they exist locally, otherwise
falls back to the raw GitHub URLs.

## Deploying to shinyapps.io

### One-time

1. Create a free shinyapps.io account at
   <https://www.shinyapps.io/>.
2. **Account -> Tokens -> Show -> Show secret -> Copy to clipboard**.
   This puts a `rsconnect::setAccountInfo(...)` line on your
   clipboard.
3. In R, paste and run that line. You're now authenticated.

### Each deploy

```r
source("app/deploy.R")
```

This deploys just `app/app.R`. The app fetches data at runtime
from the raw GitHub URLs, so you don't need to redeploy when the
data updates -- the daily Action is enough. Re-deploy only when
the app code changes.

## Output files

- `data/<query_name>.csv` -- latest snapshot, overwritten each
  pull. The Shiny app reads these.
- `data/raw/<query_name>_<timestamp>.csv` -- archive of each
  pull. Prune periodically if the repo grows.
- `data/last_update.csv` -- one row per query with row count and
  fetch timestamp. The app shows this in the **Data status**
  tab.
- `config/site_metadata.csv` -- cached PTAGIS site list with
  lat/lon for the map.

## Notes / caveats

- **PTAGIS exports are UTF-16 LE with a BOM** (the SAP
  BusinessObjects default). The fetch script detects the BOM and
  re-encodes everything to UTF-8 before saving, so CSVs in
  `data/` are friendly UTF-8.
- PTAGIS has a reporting lag; "last detection" means "last
  detection PTAGIS has received and processed". The app surfaces
  the `last_update.csv` timestamp.
- DART URL pattern for `R/fetch_dart_wells.R`:
  `https://www.cbr.washington.edu/dart/cs/php/rpt/adult_daily.php?sc=1&outputFormat=csv&proj=WEL&span=no&startdate=1/1&enddate=12/31&run=&avg=1&year=YYYY&syear=YYYY&eyear=YYYY`
  Replace `proj=WEL` with another project code (e.g., `BON`,
  `MCN`) to point at a different dam.
- shinyapps.io free tier sleeps inactive apps -- first hit after
  idle takes a few seconds.
