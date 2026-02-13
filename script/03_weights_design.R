# 03_weights_design.R
library(dplyr); library(readxl); library(survey); library(srvyr); library(readr)

stopifnot(exists("d_01"))

# Merge sample sizes
sample_size_data <- read_excel(here::here("sample_size.xlsx"))

unmatched_svy <- setdiff(d_01$district, sample_size_data$district)
unmatched_sample <- setdiff(sample_size_data$district, d_01$district)

if (length(unmatched_svy) > 0) {
  print("Unmatched districts in svy_unique:")
  print(unmatched_svy)
}

if (length(unmatched_sample) > 0) {
  print("Unmatched districts in sample_size_data:")
  print(unmatched_sample)
}


final_survey_data <- d_01 %>% left_join(sample_size_data, by = "district")

# Base weight by actual sample size
reweigh_survey <- final_survey_data %>%
  group_by(district) %>% mutate(observation_count = n()) %>% ungroup() %>%
  mutate(ID = dplyr::row_number(),
         new_weight = 1000 * population / observation_count)

# Age groups
sample_data <- reweigh_survey %>%
  mutate(age = as.numeric(as.character(age)),
         age_group = cut(age,
                         breaks = c(-Inf, 30, 50, 66, Inf),
                         labels = c("18-29","30-49","50-65","Over 65"),
                         right = FALSE)) %>%
  select(-population)

# Post-stratification using population_age_group.xlsx
population_data <- readxl::read_xlsx(here::here("population_age_group.xlsx")) %>%
  dplyr::select(district_id, district, age_group, gender, population) %>%
  dplyr::rename(Freq = population)

# Align factor levels
sample_districts <- unique(sample_data$district)
pop_f <- population_data %>%
  filter(district %in% sample_districts) %>%
  group_by(district) %>% mutate(pop_prop = Freq / sum(Freq)) %>% ungroup()

sample_props <- sample_data %>%
  count(district, age_group, gender) %>%
  group_by(district) %>% mutate(district_total = sum(n)) %>% ungroup() %>%
  mutate(sample_prop = n / district_total)

combined <- pop_f %>%
  left_join(sample_props, by = c("district","age_group","gender")) %>%
  mutate(
    post_weight = ifelse(is.na(sample_prop) | sample_prop == 0, 1, pop_prop / sample_prop)
  )

sample_data_weighted <- sample_data %>%
  left_join(combined %>% select(district, age_group, gender, post_weight),
            by = c("district","age_group","gender")) %>%
  mutate(
    post_weight  = ifelse(is.na(post_weight), 1, post_weight),
    final_weight = new_weight * post_weight
  )

n_fallback <- sum(sample_data_weighted$post_weight == 1 &
                  !is.na(sample_data_weighted$new_weight))
if (n_fallback > 0) {
  message(n_fallback,
          " respondents received post_weight=1 (no population cell match or empty sample cell)")
}

# Survey design (your structure)
design <- svydesign(
  ids = ~ID,
  strata = ~district,
  weights = ~final_weight,
  data = sample_data_weighted
)
svy_design <- design %>% as_survey_design()

assign("svy_design", svy_design, .GlobalEnv)
assign("sample_data_weighted", sample_data_weighted, .GlobalEnv)
message("✅ Survey design ready.")
