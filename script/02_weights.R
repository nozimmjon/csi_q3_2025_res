# 02_weights.R
# ---------------------------------------------------------------------------
# Step 2: Construct survey weights and the survey design object.
#
# Methodology:
#   1. BASE WEIGHT — Inverse probability of selection within each district.
#      w_base_i = N_district / n_district
#      where N = population (from sample_size.xlsx, in thousands × 1000)
#      and   n = number of completed interviews in that district.
#
#   2. POST-STRATIFICATION — Adjusts the base-weighted design so that the
#      weighted sample matches known population margins by
#      district × age_group × gender.
#      Uses survey::postStratify(), which correctly adjusts BOTH point
#      estimates AND variance estimates (unlike manual ratio adjustment).
#
#   3. WEIGHT TRIMMING — Caps extreme weights at the 95th percentile and
#      re-normalizes so sum(trimmed_weights) = sum(original_weights).
#      This limits the influence of outlier observations while preserving
#      the population total.
#
#   4. DESIGN DECLARATION — Stratified design with districts as strata
#      and individuals as the sampling units (phone survey, no clustering).
#      Finite population correction (fpc) is supplied when population totals
#      are known, improving SE estimates in heavily-sampled strata.
#
# Requires: d_clean, code_to_name (from 01_clean.R)
# Outputs:
#   svy_design           — srvyr survey design object (.GlobalEnv)
#   sample_data_weighted — data frame with final_weight column (.GlobalEnv)
#   checkpoint/02_design.rds
# ---------------------------------------------------------------------------

library(dplyr)
library(readxl)
library(survey)
library(srvyr)
library(here)

stopifnot(exists("d_clean"), exists("code_to_name"))

# ===========================================================================
# 1) Load population data
# ===========================================================================
sample_size_data <- read_excel(here::here("sample_size.xlsx")) %>%
  mutate(
    code       = as.character(code),
    population = as.numeric(population)       # in thousands
  )

pop_age_gender <- readxl::read_xlsx(here::here("population_age_group.xlsx")) %>%
  dplyr::select(district_id, age_group, gender, population) %>%
  dplyr::mutate(
    district_id = as.character(district_id),
    district    = ifelse(
      district_id %in% names(code_to_name),
      code_to_name[district_id],
      district_id
    ),
    population = as.numeric(population)
  )

# ===========================================================================
# 2) Base weights
# ===========================================================================
# Merge population totals (population column is in 1000s)
district_pop <- sample_size_data %>%
  transmute(
    district = ifelse(
      code %in% names(code_to_name),
      code_to_name[code],
      code
    ),
    pop_total = population * 1000     # convert to actual persons
  )

sample_counts <- d_clean %>%
  count(district, name = "n_sampled")

weight_frame <- district_pop %>%
  inner_join(sample_counts, by = "district") %>%
  mutate(base_weight = pop_total / n_sampled)

# Merge onto respondent data
df <- d_clean %>%
  left_join(weight_frame %>% select(district, pop_total, n_sampled, base_weight),
            by = "district")

# Check for unmatched districts
n_no_weight <- sum(is.na(df$base_weight))
if (n_no_weight > 0) {
  unmatched <- unique(df$district[is.na(df$base_weight)])
  warning(
    "[02] ", n_no_weight, " rows without base weight (district not in sample_size.xlsx): ",
    paste(head(unmatched, 10), collapse = ", ")
  )
  # Assign weight = 1 as fallback for unmatched
  df <- df %>% mutate(base_weight = ifelse(is.na(base_weight), 1, base_weight))
}

# Add row ID for svydesign
df <- df %>% mutate(ID = dplyr::row_number())

# ===========================================================================
# 3) Initial survey design (before post-stratification)
# ===========================================================================
# Handle lonely PSUs gracefully
options(survey.lonely.psu = "adjust")

# Build stratified design:
#   strata  = district (sampling was stratified by district)
#   ids     = ~1       (no clustering — phone survey, individuals are SUs)
#   weights = base_weight (inverse probability of selection)
#   fpc     = pop_total   (finite population correction per stratum)
design_base <- svydesign(
  ids     = ~1,
  strata  = ~district,
  weights = ~base_weight,
  fpc     = ~pop_total,
  data    = df
)

message("[02] Base design: ", nrow(df), " respondents across ",
        length(unique(df$district)), " district strata")

# ===========================================================================
# 4) Post-stratification by district × age_group × gender
# ===========================================================================
# Build the population totals table for postStratify()
# Requires a data.frame with the stratifying variables and a Freq column
pop_margins <- pop_age_gender %>%
  filter(district %in% unique(df$district)) %>%
  group_by(district, age_group, gender) %>%
  summarise(Freq = sum(population), .groups = "drop") %>%
  as.data.frame()

# Check that all cells in the sample have corresponding population data
sample_cells <- df %>%
  filter(!is.na(age_group)) %>%
  distinct(district, age_group, gender)
pop_cells <- pop_margins %>% distinct(district, age_group, gender)
missing_cells <- anti_join(sample_cells, pop_cells,
                           by = c("district", "age_group", "gender"))

if (nrow(missing_cells) > 0) {
  warning(
    "[02] ", nrow(missing_cells),
    " sample cells (district x age x gender) have no population data. ",
    "Post-stratification will use partial= TRUE."
  )
}

# postStratify needs respondents to have non-NA levels of the
# stratifying variables. Rows with NA age_group get base weight only.
n_na_age <- sum(is.na(df$age_group))
if (n_na_age > 0) {
  message(
    "[02] ", n_na_age,
    " respondents have NA age_group — they keep base weight only (no post-strat)"
  )
}

# Apply post-stratification
design_ps <- tryCatch(
  postStratify(
    design  = design_base,
    strata  = ~ district + age_group + gender,
    population = pop_margins,
    partial = TRUE        # allows cells in sample with no pop data
  ),
  error = function(e) {
    warning("[02] postStratify() failed: ", e$message,
            "\n  Falling back to base-weighted design.")
    design_base
  }
)

message("[02] Post-stratification applied successfully")

# ===========================================================================
# 5) Weight trimming (cap at 95th percentile, re-normalize)
# ===========================================================================
raw_weights <- weights(design_ps)
p95 <- quantile(raw_weights, 0.95, na.rm = TRUE)
n_trimmed   <- sum(raw_weights > p95, na.rm = TRUE)

if (n_trimmed > 0) {
  message("[02] Trimming ", n_trimmed, " weights above 95th percentile (",
          round(p95, 1), ")")

  trimmed_weights <- pmin(raw_weights, p95)

  # Re-normalize: scale so sum(trimmed) = sum(original)
  scale_factor    <- sum(raw_weights, na.rm = TRUE) / sum(trimmed_weights, na.rm = TRUE)
  trimmed_weights <- trimmed_weights * scale_factor

  design_ps$variables$final_weight <- trimmed_weights
} else {
  design_ps$variables$final_weight <- raw_weights
}

# ===========================================================================
# 6) Weight diagnostics
# ===========================================================================
fw <- design_ps$variables$final_weight
message("\n[02] === WEIGHT DIAGNOSTICS ===")
message("  N respondents:       ", length(fw))
message("  Sum of weights:      ", format(round(sum(fw, na.rm = TRUE)), big.mark = ","))
message("  Mean weight:         ", round(mean(fw, na.rm = TRUE), 2))
message("  Median weight:       ", round(median(fw, na.rm = TRUE), 2))
message("  Min weight:          ", round(min(fw, na.rm = TRUE), 2))
message("  Max weight:          ", round(max(fw, na.rm = TRUE), 2))
message("  CV of weights:       ", round(sd(fw, na.rm = TRUE) / mean(fw, na.rm = TRUE), 3))
message("  Design effect (wt):  ",
        round(1 + (sd(fw, na.rm = TRUE) / mean(fw, na.rm = TRUE))^2, 2))
n_na <- sum(is.na(fw))
if (n_na > 0) message("  NA weights:          ", n_na)
message("==============================\n")

# Warn about small strata
strata_n <- design_ps$variables %>%
  count(district, name = "n_obs")
small <- strata_n %>% filter(n_obs < 10)
if (nrow(small) > 0) {
  warning(
    "[02] ", nrow(small), " strata with < 10 obs: ",
    paste(small$district, " (n=", small$n_obs, ")", sep = "", collapse = ", ")
  )
}

# ===========================================================================
# 7) Export objects
# ===========================================================================
# Re-build the final design with trimmed weights for proper variance estimation
sample_data_weighted <- design_ps$variables
sample_data_weighted$final_weight <- fw

svy_design <- svydesign(
  ids     = ~1,
  strata  = ~district,
  weights = ~final_weight,
  data    = sample_data_weighted
) %>%
  as_survey_design()

saveRDS(
  list(svy_design = svy_design, sample_data_weighted = sample_data_weighted),
  here::here("checkpoint", "02_design.rds")
)

assign("svy_design", svy_design, envir = .GlobalEnv)
assign("sample_data_weighted", sample_data_weighted, envir = .GlobalEnv)
message("[02] Survey design ready.")
