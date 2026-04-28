# Methow Bull Trout Dashboard — data pipeline

Daily pull of PTAGIS data for Bull Trout in the Methow subbasin
(plus detections at Wells Dam and the Twisp River weir), driven
by a GitHub Actions cron job. The Shiny app (coming next) reads
the CSVs in `data/` and displays the current state.

```
methow-bt-dashboard/
├── .github/workflows/daily-pull.yml   # cron → runs R/fetch_ptagis.R
├── R/fetch_ptagis.R                   # retrieves scheduled PTAGIS exports
├── config/queries.yml                 # PTAGIS user + export filenames
├── data/                              # CSVs the app reads (auto-updated)
│   └── raw/                           # timestamped archive of each pull
└── DESCRIPTION                        # R deps
```

## How it works

PTAGIS's Advanced Reporting runs on SAP BusinessObjects, which
does not expose saved queries as direct CSV download URLs. The
supported pattern is:

1. **On PTAGIS:** save your query, then use *Subscribe To → File*
   to schedule it to run on a recurring basis and write the result
   as a CSV to your user's file store.
2. **PTAGIS serves the file** at a predictable URL:
   `https://api.ptagis.org/reporting/reports/<user>/file/<filename>.csv`
3. **Our GitHub Action** hits that URL each morning, saves the CSV
   into the repo, and commits. The Shiny app reads those CSVs.

We never "run" the query from our side — PTAGIS handles the
refresh on schedule. We just pick up the latest file.

## One-time setup

### 1. Build the two saved queries in PTAGIS

Log into <https://www.ptagis.org/> and open **Advanced Reporting**.
You'll build two queries and save each one.

**Query A — Methow Bull Trout tagging**

Report type: *Tagging Details* (or *Mark/Recapture Events*). Filters:

- Species → `Bull Trout` (code `3F`)
- Release Site Subbasin → `Methow` (`17020008`)

Save as something descriptive, e.g. `Methow_BT_Tagging`.

**Query B — Methow / Wells / Twisp Bull Trout interrogations**

Report type: *Complete Tag History* (preferred — gives every
detection event for qualifying tags) or *Interrogation Summary*.
Filters:

- Species → `Bull Trout`
- Interrogation Site Subbasin → `Methow` **OR** Site code in
  `WEA` (Wells Dam East ladder), plus any other Wells arrays you
  care about (`WEH`, `WEJ`, etc.), **OR** `TWR` (Twisp River weir)

Save as e.g. `Methow_BT_Interrogations`.

> Double-check Wells Dam site codes against PTAGIS's site list —
> there are several arrays (adult ladders, juvenile bypass). At
> minimum you want the adult arrays.

### 2. Schedule each query to export to a file

For each saved query, use **Subscribe To → File**:

- Format: **CSV** (single file — not a zip)
- Frequency: **Daily**, at a time earlier than your dashboard's
  refresh (e.g. 02:00 Pacific)
- Filename: e.g. `Methow_BT_Tagging.csv` and
  `Methow_BT_Interrogations.csv` — take note of the exact names
  including capitalization and spaces

### 3. Fill in `config/queries.yml`

Open [config/queries.yml](config/queries.yml) and set:

- `ptagis_user` — your PTAGIS username (the one that owns the
  scheduled reports)
- Each `filename` — must match exactly what PTAGIS wrote in step 2

### 4. (Optional) Add PTAGIS credentials as GitHub secrets

If PTAGIS serves your exported files publicly via the API, you
can skip this. If it challenges for auth, go to the repo's
**Settings → Secrets and variables → Actions** and add:

- `PTAGIS_USER` — your PTAGIS username
- `PTAGIS_PASS` — your PTAGIS password

The script picks these up automatically if set.

### 5. Push to GitHub and trigger the first run

```bash
git init
git add .
git commit -m "Initial commit"
git branch -M main
git remote add origin git@github.com:<you>/methow-bt-dashboard.git
git push -u origin main
```

In the repo's **Actions** tab → *Daily PTAGIS pull* → **Run
workflow**. Watch the log. If it succeeds, two CSVs appear in
`data/` committed by `github-actions[bot]`. After that it runs
automatically every day at 04:00 Pacific.

## Running locally

Put any credentials in a `.Renviron` in the project root
(gitignored):

```
PTAGIS_USER=yourname
PTAGIS_PASS=yourpassword
```

Then from R at the project root:

```r
source("R/fetch_ptagis.R")
```

## Output files

- `data/<query_name>.csv` — latest snapshot, overwritten on each
  run. This is what the Shiny app will read.
- `data/raw/<query_name>_<timestamp>.csv` — archive of each pull
  (useful for debugging and for detecting when a detection first
  appeared). Prune periodically if the repo gets large.
- `data/last_update.csv` — one row per query with row count and
  fetch timestamp. The app uses this to show "data current as of".

## Notes / caveats

- **PTAGIS exports are UTF-16 LE with a BOM** (the SAP
  BusinessObjects default). The fetch script detects the BOM
  and re-encodes everything to UTF-8 before saving, so the
  files in `data/` are friendly UTF-8 CSVs. If you ever run a
  PTAGIS export by hand and it looks like garbage in Excel /
  RStudio, that's why — open it with the UTF-16 encoding hint.
- PTAGIS has a reporting lag; "last detection" means "last
  detection PTAGIS has received and processed". Surface the
  `last_update.csv` timestamp in the app.
- The PTAGIS scheduled export determines how fresh the data can
  be. If PTAGIS runs the export at 02:00 PT and our Action runs
  at 04:00 PT, we get ~2-hour-old data at best.
- GitHub Actions is free for public repos and has a generous
  allowance for private ones — a daily 2-minute pull is nothing.
- If PTAGIS ever changes their URL format, you just update
  `config/queries.yml` or `R/fetch_ptagis.R` — no infra changes.
