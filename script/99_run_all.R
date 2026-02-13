# 99_run_all.R
library(here)
source(here::here("script", "00_ingest_merge.R"))
source(here::here("script","01_standardize_reshape.R"))
# source(here::here("02_preQA_checks.R"))
source(here::here("script","03_weights_design.R"))
source(here::here("script","04_q10_regex_tables.R")) 
source(here::here("script","04_tables_simple.R"))
# source(here::here("06_q7_simplefreq.R"))     # <-- added
