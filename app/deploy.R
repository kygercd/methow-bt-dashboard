# deploy.R
# -------------------------------------------------------------
# One-time + repeat deployer for shinyapps.io.
#
# Run this from R at the project root:
#   source("app/deploy.R")
#
# It expects rsconnect to be installed and your shinyapps.io
# account to be registered (see the README for the one-time
# setup steps).
# -------------------------------------------------------------

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}
suppressPackageStartupMessages(library(rsconnect))

root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION") |
                               rprojroot::has_dir(".github"))
app_dir <- file.path(root, "app")

# Files inside app_dir to upload. Keep this minimal -- the app
# pulls data from the GitHub raw URLs at runtime, so we don't
# need to bundle CSVs.
app_files <- c("app.R")

rsconnect::deployApp(
  appDir         = app_dir,
  appFiles       = app_files,
  appName        = "methow-bt-dashboard",
  appTitle       = "Methow Bull Trout Dashboard",
  forceUpdate    = TRUE,
  launch.browser = TRUE
)
