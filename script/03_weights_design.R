# 03_weights_design.R
# ---------------------------------------------------------------------------
# Builds the survey design object with two-stage weighting:
#   1. Base weight = population / sample_count per district
#   2. Post-stratification by district x age_group x gender
#
# Includes weight diagnostics: caps extreme post-stratification weights at
# 5× median, replaces NA/Inf weights with the district base weight, and
# warns about problematic strata.
#
# Requires: d_01 (from 01_standardize_reshape.R)
# Key outputs:
#   svy_design           — srvyr survey design object (.GlobalEnv)
#   sample_data_weighted — weighted data frame (.GlobalEnv)
# ---------------------------------------------------------------------------

library(dplyr)
library(readxl)
library(survey)
library(srvyr)
library(readr)

stopifnot(exists("d_01"))

# --- 1) Merge sample sizes -------------------------------------------------
sample_size_data <- read_excel(here::here("sample_size.xlsx")) %>%
  mutate(code = as.character(code))

# Validate population data has required columns
if (!all(c("code", "district", "population") %in% names(sample_size_data))) {
  stop("sample_size.xlsx must contain 'code', 'district' and 'population' columns")
}


# Convert numeric district codes to text names using sample_size.xlsx lookup
code_to_name <- setNames(sample_size_data$district, sample_size_data$code)
d_01 <- d_01 %>%
  mutate(
    district_code = as.character(district),
    district = ifelse(
      district_code %in% names(code_to_name),
      code_to_name[district_code],
      district_code
    )
  )

unmatched_svy    <- setdiff(d_01$district, sample_size_data$district)
unmatched_sample <- setdiff(sample_size_data$district, d_01$district)

if (length(unmatched_svy) > 0) {
  warning(
    length(unmatched_svy), " survey district(s) not found in sample_size.xlsx: ",
    paste(unmatched_svy, collapse = ", ")
  )
}

if (length(unmatched_sample) > 0) {
  message(
    length(unmatched_sample), " sample_size district(s) not in survey data: ",
    paste(unmatched_sample, collapse = ", ")
  )
}

final_survey_data <- d_01 %>%
  left_join(sample_size_data %>% select(district, population), by = "district") %>% 
  filter(!district %in% c("Давлатобод", "Ғозғон ш."))


# --- 2) Base weight by actual sample size -----------------------------------
reweigh_survey <- final_survey_data %>%
  group_by(district) %>%
  mutate(observation_count = n()) %>%
  ungroup() %>%
  mutate(
    ID         = dplyr::row_number(),
    new_weight = 1000 * population / observation_count
  )

# --- 3) Age groups ----------------------------------------------------------
sample_data <- reweigh_survey %>%
  mutate(
    age       = as.numeric(as.character(age)),
    age_group = cut(
      age,
      breaks = c(-Inf, 30, 50, 66, Inf),
      labels = c("18-29", "30-49", "50-65", "Over 65"),
      right  = FALSE
    )
  ) %>%
  select(-population)

# --- 4) Post-stratification using population_age_group.xlsx -----------------
population_data <- readxl::read_xlsx(here::here("population_age_group.xlsx")) %>%
  dplyr::select(district_id, age_group, gender, population) %>%
  dplyr::rename(Freq = population) %>%
  dplyr::mutate(
    district_id = as.character(district_id),
    district = ifelse(
      district_id %in% names(code_to_name),
      code_to_name[district_id],
      district_id
    )
  )

sample_districts <- unique(sample_data$district)
pop_f <- population_data %>%
  filter(district %in% sample_districts) %>%
  group_by(district) %>%
  mutate(pop_prop = Freq / sum(Freq)) %>%
  ungroup()

sample_props <- sample_data %>%
  count(district, age_group, gender) %>%
  group_by(district) %>%
  mutate(district_total = sum(n)) %>%
  ungroup() %>%
  mutate(sample_prop = n / district_total)

combined <- pop_f %>%
  left_join(sample_props, by = c("district", "age_group", "gender")) %>%
  mutate(
    post_weight = ifelse(is.na(sample_prop) | sample_prop == 0, 1, pop_prop / sample_prop)
  )

# --- 5) Join post-weights and compute final weight --------------------------
sample_data_weighted <- sample_data %>%
  left_join(
    combined %>% select(district, age_group, gender, post_weight),
    by = c("district", "age_group", "gender")
  ) %>%
  mutate(
    post_weight  = ifelse(is.na(post_weight), 1, post_weight),
    final_weight = new_weight * post_weight
  )

# --- 6) Weight diagnostics: cap extremes, fix NA/Inf -----------------------
median_wt <- median(sample_data_weighted$final_weight, na.rm = TRUE)
cap_threshold <- 5 * median_wt

n_extreme <- sum(sample_data_weighted$final_weight > cap_threshold, na.rm = TRUE)
if (n_extreme > 0) {
  message(n_extreme, " weights capped at 5× median (", round(cap_threshold, 1), ")")
  sample_data_weighted <- sample_data_weighted %>%
    mutate(final_weight = pmin(final_weight, cap_threshold))
}

n_bad <- sum(is.na(sample_data_weighted$final_weight) |
             is.infinite(sample_data_weighted$final_weight))
if (n_bad > 0) {
  warning(n_bad, " NA/Inf final_weight values replaced with base weight (new_weight)")
  sample_data_weighted <- sample_data_weighted %>%
    mutate(
      final_weight = ifelse(
        is.na(final_weight) | is.infinite(final_weight),
        ifelse(is.na(new_weight) | is.infinite(new_weight), 1, new_weight),
        final_weight
      )
    )
}

n_fallback <- sum(sample_data_weighted$post_weight == 1 &
                  !is.na(sample_data_weighted$new_weight))
if (n_fallback > 0) {
  message(
    n_fallback,
    " respondents received post_weight=1 (no population cell match or empty sample cell)"
  )
}

# Warn about strata with very few observations
strata_counts <- sample_data_weighted %>% count(district, name = "n_obs")
small_strata  <- strata_counts %>% filter(n_obs < 10)
if (nrow(small_strata) > 0) {
  warning(
    nrow(small_strata), " district strata have fewer than 10 observations: ",
    paste(small_strata$district, " (n=", small_strata$n_obs, ")", sep = "", collapse = ", ")
  )
}

# --- 7) Build survey design -------------------------------------------------
design <- svydesign(
  ids     = ~ID,
  strata  = ~district,
  weights = ~final_weight,
  data    = sample_data_weighted
)
svy_design <- design %>% as_survey_design()

assign("svy_design", svy_design, .GlobalEnv)
assign("sample_data_weighted", sample_data_weighted, .GlobalEnv)
message("✅ Survey design ready.")
