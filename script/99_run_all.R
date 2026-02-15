# 99_run_all.R
# ---------------------------------------------------------------------------
# Master script: sources all pipeline steps in order.
# Run from the project root (or open the .Rproj in RStudio first).
#
# Pipeline:
#   00_ingest_merge.R        -> df_all
#   01_standardize_reshape.R -> d_01
#   03_weights_design.R      -> svy_design, sample_data_weighted
#   04_q10_regex_tables.R    -> q10_by_region_wide
#   04_tables_simple.R       -> tables/all_survey_data.xlsx
# ---------------------------------------------------------------------------

library(here)

source(here::here("script", "00_ingest_merge.R"))
source(here::here("script", "01_standardize_reshape.R"))
source(here::here("script", "03_weights_design.R"))
# source(here::here("script", "04_q10_regex_tables.R"))
source(here::here("script", "04_tables_simple.R"))
