# 99_run_all.R
# ---------------------------------------------------------------------------
# Master script: sources all pipeline steps in order.
# Run from the project root (or open the .Rproj in RStudio first).
#
# Pipeline:
#   00_ingest.R   -> df_raw              (merged raw data)
#   01_clean.R    -> d_clean             (validated, deduplicated)
#   02_weights.R  -> svy_design          (post-stratified survey design)
#   04_q10.R      -> q10_by_region_wide  (open-ended text categorization)
#   03_tables.R   -> tables/all_survey_data.xlsx
#
# Checkpoints are saved as .rds files in checkpoint/ after each step,
# allowing restart from any point.
# ---------------------------------------------------------------------------

library(here)

t0 <- Sys.time()

message("========== CSI Q3 2025 Pipeline ==========\n")

source(here::here("script", "00_ingest.R"))
source(here::here("script", "01_clean.R"))
source(here::here("script", "02_weights.R"))
source(here::here("script", "04_q10.R"))
source(here::here("script", "03_tables.R"))

elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
message("\n========== Pipeline complete in ", elapsed, "s ==========")
