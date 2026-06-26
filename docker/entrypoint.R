#!/usr/bin/env Rscript
# Container entrypoint for dwhr-runtime.
#
# Drives the per-session boot:
#   1. Optionally enable Shiny autoreload on file edits.
#   2. devtools::load_all() the dwhr source mounted at /workspaces/dwhr.
#   3. Patch dwhr::authenticate() to bypass auth (demo container only).
#   4. shiny::runApp() the example mounted at /app.
#
# Env vars (with defaults):
#   PORT             4815
#   HOST             0.0.0.0
#   APP_DIR          /app                          (mount point of chosen example)
#   DWHR_DIR         /workspaces/dwhr              (mount point of dwhr source)
#   SHINY_AUTORELOAD true                          ('false' to disable)

port    <- as.integer(Sys.getenv("PORT",            "4815"))
host    <- Sys.getenv("HOST",                       "0.0.0.0")
app_dir <- Sys.getenv("APP_DIR",                    "/app")
dwhr    <- Sys.getenv("DWHR_DIR",                   "/workspaces/dwhr")
ar      <- !identical(tolower(Sys.getenv("SHINY_AUTORELOAD", "true")), "false")

# Sanity checks — these are typical first-time-run failures.
if (!dir.exists(dwhr)) {
    stop(sprintf(
        "[entrypoint] dwhr source not mounted at %s — see docker-compose.yml volumes.",
        dwhr))
}
if (!dir.exists(app_dir)) {
    stop(sprintf(
        "[entrypoint] example app not mounted at %s — set EXAMPLE in docker-compose.yml.",
        app_dir))
}
if (!file.exists(file.path(app_dir, "server.R"))) {
    stop(sprintf(
        "[entrypoint] %s/server.R not found — is %s actually a Shiny app?",
        app_dir, app_dir))
}

cat(sprintf("[entrypoint] dwhr  : %s\n",  dwhr))
cat(sprintf("[entrypoint] app   : %s\n",  app_dir))
cat(sprintf("[entrypoint] port  : %d (host %s)\n", port, host))
cat(sprintf("[entrypoint] reload: %s\n",  if (ar) "on" else "off"))

if (ar) options(shiny.autoreload = TRUE)

# Load dwhr from source. Faster than R CMD INSTALL on each boot, and picks
# up R/ edits live (modulo Shiny autoreload only watching the app dir).
suppressPackageStartupMessages(library(devtools))
devtools::load_all(dwhr, quiet = TRUE)

# Replace dwhr::authenticate with a bypass stub. Demo only.
source("/auth-stub.R", local = TRUE)

# Hand off to Shiny.
shiny::runApp(app_dir, port = port, host = host, launch.browser = FALSE)
