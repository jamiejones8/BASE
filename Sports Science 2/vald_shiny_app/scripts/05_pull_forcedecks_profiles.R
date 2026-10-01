# scripts/05_pull_forcedecks_profiles.R
# Pull profiles via valdr and save to Data/profiles.rds

source("scripts/01_config.R")

suppressPackageStartupMessages({
  library(valdr)
  library(dplyr)
})

# --- Set credentials (safe to run repeatedly; valdr stores securely) ---
valdr::set_credentials(
  client_id     = Sys.getenv("VALD_USERNAME"),
  client_secret = Sys.getenv("VALD_PASSWORD"),
  tenant_id     = Sys.getenv("VALD_DUENDE_ID"),
  region        = Sys.getenv("VALD_REGION", "use")
)

# Pull profiles only
profiles <- valdr::get_profiles_only()

cat("Profiles rows:", nrow(profiles), "\n")
cat("Profile columns:\n")
print(names(profiles))

# Save
out <- file.path(DATA_DIR, "profiles.rds")
saveRDS(profiles, out)
cat("Saved profiles:", out, "\n")

# Optional: quick sanity check for external IDs
if ("externalId" %in% names(profiles)) {
  cat("Non-empty externalId:", sum(nzchar(profiles$externalId %||% "")), "\n")
}
